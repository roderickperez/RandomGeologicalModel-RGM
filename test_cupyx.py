
print("Importing cupy...")
import cupy as cp
print("Importing cupyx.scipy.ndimage...")
import cupyx.scipy.ndimage
print("Importing cupyx.scipy.signal...")
import cupyx.scipy.signal

print("Checking Device...")
try:
    dev = cp.cuda.Device(0)
    dev.use()
    print(f"✅ GPU Detected: {dev.compute_capability}")
except Exception as e:
    print(f"❌ GPU Check Failed: {e}")
