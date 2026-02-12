
print("Step 1: Importing rich...")
from rich.console import Console

print("Step 2: Importing matplotlib...")
import matplotlib.pyplot as plt

print("Step 3: Importing cupy...")
import cupy as cp

try:
    print("Step 4: Checking Device...")
    cp.cuda.Device(0).compute_capability
    print("✅ GPU Detected!")
except Exception as e:
    print(f"❌ GPU Check Failed: {e}")
