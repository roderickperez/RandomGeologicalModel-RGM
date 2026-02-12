import cupy as cp
import sys

print(f"Python: {sys.version}")
print(f"CuPy version: {cp.__version__}")

try:
    print("Checking CUDA device...")
    dev = cp.cuda.Device(0)
    print(f"Device 0: {dev}")
    print(f"Compute Capability: {dev.compute_capability}")
except Exception as e:
    print(f"\n❌ ERROR: {e}")
    import traceback
    traceback.print_exc()

print("\nCUDA Path info:")
import os
print(f"CUDA_PATH: {os.environ.get('CUDA_PATH', 'Not Set')}")
print(f"LD_LIBRARY_PATH: {os.environ.get('LD_LIBRARY_PATH', 'Not Set')}")
