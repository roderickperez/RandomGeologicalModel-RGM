from rich.console import Console
from rich.progress import track
import matplotlib.pyplot as plt
from rgm_core import RGMConfig, RGMGenerator
import argparse

def main():
    console = Console()
    
    parser = argparse.ArgumentParser(description="Random Geological Model Generator")
    parser.add_argument("--no-gpu", action="store_true", help="Force CPU usage")
    args = parser.parse_args()
    
    use_gpu = not args.no_gpu

    console.print("[bold green]Initializing RGM...[/bold green]")

    # 1. Configuration
    cfg = RGMConfig(
        n1=128, n2=256, n3=256, 
        n_faults=6,
        nl=25
    )
    
    # 2. Initialize Generator (Selects CuPy automatically if available and requested)
    rgm = RGMGenerator(cfg, use_gpu=use_gpu)
    console.print(f"Backend: [bold yellow]{rgm.device}[/bold yellow]")

    # 3. Pipeline
    with console.status("Generating Layers..."):
        vp = rgm.generate_layers()
    console.print("✅ Vp Model Generated")

    with console.status("Applying Faults..."):
        vp_faulted = rgm.apply_faults(vp)
    console.print("✅ Faults Applied")

    with console.status("Computing Seismic Response..."):
        seismic = rgm.compute_seismic(vp_faulted)
    console.print("✅ Seismic Image Generated")

    # 4. Visualization (Move to CPU for Matplotlib)
    console.print("Plotting results...")
    vp_cpu = rgm.to_cpu(vp_faulted)
    seismic_cpu = rgm.to_cpu(seismic)

    fig, ax = plt.subplots(1, 2, figsize=(12, 6))
    
    # Plot Inline slice (middle of Y)
    mid_idx = cfg.n3 // 2
    
    # VP Plot
    im0 = ax[0].imshow(vp_cpu[:, :, mid_idx], cmap='jet', aspect='auto', origin='lower')
    ax[0].set_title(f"Faulted Vp Model (Inline {mid_idx})")
    ax[0].set_xlabel("Crossline (X)")
    ax[0].set_ylabel("Depth (Z)")
    plt.colorbar(im0, ax=ax[0], label='Velocity (m/s)')
    
    # Seismic Plot
    im1 = ax[1].imshow(seismic_cpu[:, :, mid_idx], cmap='gray', aspect='auto', origin='lower')
    ax[1].set_title(f"Seismic Image (Inline {mid_idx})")
    ax[1].set_xlabel("Crossline (X)")
    ax[1].set_ylabel("Depth (Z)")
    plt.colorbar(im1, ax=ax[1], label='Amplitude')
    
    plt.tight_layout()
    output_file = "rgm_output.png"
    plt.savefig(output_file, dpi=300)
    console.print(f"[bold green]Done! Saved to {output_file}[/bold green]")

if __name__ == "__main__":
    main()
