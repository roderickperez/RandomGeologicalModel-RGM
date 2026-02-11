import numpy as np
import scipy.ndimage
import scipy.signal
from dataclasses import dataclass, field
from typing import Tuple, Optional, Literal

# Try importing CuPy for GPU support
try:
    import cupy as cp
    import cupyx.scipy.ndimage
    import cupyx.scipy.signal
    # Check if a device is actually available
    try:
        cp.cuda.Device(0).compute_capability
        HAS_GPU = True
    except cp.cuda.runtime.CUDARuntimeError:
        HAS_GPU = False
        print("Warning: CuPy installed but no GPU detected. Falling back to CPU.")
except ImportError:
    cp = None
    HAS_GPU = False

@dataclass
class RGMConfig:
    # Geometry
    n1: int = 128  # Z (Vertical)
    n2: int = 128  # X (Crossline)
    n3: int = 128  # Y (Inline)
    
    # Layers
    nl: int = 20   # Number of layers
    refl_freq: float = 2.0 # Frequency of reflectors
    noise_amp: float = 0.5 # Amplitude of random noise for layers
    
    # Faults
    n_faults: int = 4
    fault_dip_range: Tuple[float, float] = (60.0, 80.0)
    fault_disp_range: Tuple[float, float] = (5.0, 15.0)
    
    # Physics
    f0: float = 25.0 # Ricker wavelet frequency
    dt: float = 0.001 # Sampling rate

class RGMGenerator:
    def __init__(self, config: RGMConfig, use_gpu: bool = True):
        self.cfg = config
        # Backend Selector (CPU vs GPU)
        if use_gpu and HAS_GPU:
            self.xp = cp
            self.ndi = cupyx.scipy.ndimage
            self.sig = cupyx.scipy.signal
            self.device = "GPU (NVIDIA A5000)"
        else:
            self.xp = np
            self.ndi = scipy.ndimage
            self.sig = scipy.signal
            self.device = "CPU"
            
    def _generate_random_surface(self, shape, smooth_sigma=10):
        """Generates a smooth random surface (Eq 1 in paper)."""
        # Create random noise
        noise = self.xp.random.rand(*shape).astype(self.xp.float32)
        # Smooth it to create geological folding
        surface = self.ndi.gaussian_filter(noise, sigma=smooth_sigma)
        # Normalize
        surface = (surface - surface.mean()) / (surface.std() + 1e-6)
        return surface

    def generate_layers(self):
        """
        Generates Vp model by integrating thickness variations (Eq 15-18).
        """
        xp = self.xp
        
        # 1. Base Layer Thickness (uniform)
        base_thickness = self.cfg.n1 / self.cfg.nl
        
        # 2. Generate Random Thickness Variations
        # Shape: (n_layers, n2, n3)
        thickness_map = xp.ones((self.cfg.nl, self.cfg.n2, self.cfg.n3), dtype=xp.float32) * base_thickness
        
        # Add random perturbations (stratigraphic variability)
        noise = xp.random.normal(0, self.cfg.noise_amp, size=(self.cfg.nl, self.cfg.n2, self.cfg.n3))
        # Smooth thickness variations laterally
        for i in range(self.cfg.nl):
            noise[i] = self.ndi.gaussian_filter(noise[i], sigma=5.0)
            
        thickness_map += noise
        thickness_map = xp.abs(thickness_map) # Thickness must be positive

        # 3. Integrate to get Depth (Z) of interfaces (Cumsum simulates Eq 15 integral)
        interfaces = xp.cumsum(thickness_map, axis=0) # (nl, n2, n3)
        
        # 4. Fill the Volume (Vp)
        # We create a 3D volume where value = layer_index
        # This acts as the "Medium Property Model"
        vp_model = xp.zeros((self.cfg.n1, self.cfg.n2, self.cfg.n3), dtype=xp.float32)
        
        # Create coordinate grid
        Z, _, _ = xp.meshgrid(xp.arange(self.cfg.n1), 
                              xp.arange(self.cfg.n2), 
                              xp.arange(self.cfg.n3), indexing='ij')
        
        # Assign velocities based on depth relative to interfaces
        # Simplified "filling" strategy for speed
        for i in range(self.cfg.nl):
            # If Z > interface_depth, assign new velocity
            # We interpolate the 2D interface to 3D volume for comparison
            # In Python, broadcasting handles this comparison efficiently
            layer_depth = interfaces[i][None, :, :]
            mask = Z > layer_depth
            # Velocity: 2000 + layer_idx * 100
            vp_model[mask] = 2000.0 + (i * 100.0)
            
        return vp_model

    def apply_faults(self, volume):
        """
        Applies faults by shifting the volume coordinates (Eq 38-46).
        Uses reverse mapping (interpolation) for high performance.
        """
        xp = self.xp
        n1, n2, n3 = volume.shape
        
        # Create coordinate grids
        coords = xp.meshgrid(xp.arange(n1), xp.arange(n2), xp.arange(n3), indexing='ij')
        z_coords, x_coords, y_coords = coords[0].astype(float), coords[1].astype(float), coords[2].astype(float)
        
        for i in range(self.cfg.n_faults):
            # Random Fault Parameters
            center_x = xp.random.randint(0, n2)
            center_y = xp.random.randint(0, n3)
            strike = xp.random.uniform(0, 2 * xp.pi)
            dip = xp.deg2rad(xp.random.uniform(*self.cfg.fault_dip_range))
            displacement = xp.random.uniform(*self.cfg.fault_disp_range)
            
            # Normal vector to fault plane: n = (sin(dip)cos(strike), sin(dip)sin(strike), cos(dip))
            nx = xp.sin(dip) * xp.cos(strike)
            ny = xp.sin(dip) * xp.sin(strike)
            nz = xp.cos(dip)
            
            # Plane Equation: nx(x-cx) + ny(y-cy) + nz(z-cz) = 0
            # Calculate distance from every voxel to the fault plane
            dist = nx * (x_coords - center_x) + ny * (y_coords - center_y) + nz * (z_coords - n1/2)
            
            # Define Mask: The "hanging wall" (one side of the fault)
            mask = dist > 0
            
            # Calculate Shift Vector (Simple dip-slip for example)
            # Paper Eq 44-46 describe specific trigonometry for shift based on rake
            # Simplified: shift along dip direction
            shift_z = displacement * xp.cos(dip)
            shift_x = displacement * xp.sin(dip) * xp.cos(strike)
            shift_y = displacement * xp.sin(dip) * xp.sin(strike)
            
            # Apply shift to COORDINATES (Reverse Mapping)
            # If we want the value at X, and we shifted by +D, we look at X-D
            # Be careful with cupy arrays in boolean indexing
            if HAS_GPU and isinstance(mask, cp.ndarray):
               z_coords[mask] -= shift_z.item() if isinstance(shift_z, cp.ndarray) else shift_z
               x_coords[mask] -= shift_x.item() if isinstance(shift_x, cp.ndarray) else shift_x
               y_coords[mask] -= shift_y.item() if isinstance(shift_y, cp.ndarray) else shift_y
            else:
               z_coords[mask] -= shift_z
               x_coords[mask] -= shift_x
               y_coords[mask] -= shift_y
            
        # Interpolate the volume at the new distorted coordinates
        # This handles the "shifting" of pixels without holes
        distorted_vol = self.ndi.map_coordinates(
            volume, 
            [z_coords, x_coords, y_coords], 
            order=1, 
            mode='nearest'
        )
        
        return distorted_vol

    def compute_seismic(self, vp_model):
        """
        Generates seismic image via Reflectivity + Convolution (Eq 53-55).
        """
        xp = self.xp
        
        # 1. Calculate Reflectivity (RC) - Eq 53
        # RC = (Z2 - Z1) / (Z2 + Z1), assume Density is constant or proportional to Vp
        # Differentiate vertically
        vp_top = vp_model[:-1, :, :]
        vp_bot = vp_model[1:, :, :]
        rc = (vp_bot - vp_top) / (vp_bot + vp_top + 1e-6)
        # Pad back to original size
        rc = xp.pad(rc, ((1,0), (0,0), (0,0)), mode='constant')
        
        # 2. Generate Ricker Wavelet (1D)
        t = xp.arange(-100, 100) * self.cfg.dt
        pi_f_t = xp.pi * self.cfg.f0 * t
        ricker = (1 - 2 * pi_f_t**2) * xp.exp(-pi_f_t**2)
        
        # 3. 3D Convolution (Eq 55)
        # We broadcast the 1D wavelet to a 3D kernel (Z-axis only for simple convolution)
        # Or use separable convolution for speed
        if self.device.startswith("GPU"):
            # On GPU, FFT convolution is extremely fast
            # Reshape ricker for broadcasting
            kernel = ricker[:, None, None]
            seismic = self.sig.fftconvolve(rc, kernel, mode='same')
        else:
            # CPU fallback
            kernel = ricker[:, None, None]
            seismic = self.sig.fftconvolve(rc, kernel, mode='same')
            
        return seismic

    def to_cpu(self, array):
        if HAS_GPU and isinstance(array, cp.ndarray):
            return array.get()
        return array
