from rich.console import Console
from rich.panel import Panel
from rich.text import Text
from rich.prompt import Prompt, IntPrompt, Confirm, FloatPrompt
from rich.progress import Progress, SpinnerColumn, TextColumn, BarColumn, TimeElapsedColumn
import matplotlib.pyplot as plt
from rgm_core import RGMConfig, RGMGenerator
import argparse
import sys

def display_banner(console):
    banner_text = Text(r"""
  _____   _____ __  __ 
 |  __ \ / ____|  \/  |
 | |__) | |  __| \  / |
 |  _  /| | |_ | |\/| |
 | | \ \| |__| | |  | |
 |_|  \_\\_____|_|  |_|
""", style="bold cyan")
    
    subtitle = Text("\nRandom Geological Model (RGM) generator", style="bold yellow")
    
    panel = Panel(
        Text.assemble(banner_text, subtitle),
        subtitle="[bold white]v1.0.0[/bold white]",
        border_style="bright_blue",
        padding=(1, 2)
    )
    console.print(panel)

def get_interactive_config(console):
    console.print("\n[bold cyan]--- Parameter Configuration ---[/bold cyan]")
    n1 = IntPrompt.ask("Depth (n1)", default=128)
    n2 = IntPrompt.ask("Crossline (n2)", default=256)
    n3 = IntPrompt.ask("Inline (n3)", default=256)
    nl = IntPrompt.ask("Number of layers (nl)", default=25)
    n_faults = IntPrompt.ask("Number of faults", default=6)
    f0 = FloatPrompt.ask("Ricker Wavelet Frequency (Hz)", default=25.0)
    
    return RGMConfig(
        n1=n1, n2=n2, n3=n3,
        nl=nl,
        n_faults=n_faults,
        f0=f0
    )

def run_simulation(cfg, use_gpu, console):
    # Initialize Generator
    rgm = RGMGenerator(cfg, use_gpu=use_gpu)
    console.print(f"\n[bold]Backend:[/bold] [bold yellow]{rgm.device}[/bold yellow]")

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TimeElapsedColumn(),
        console=console
    ) as progress:
        
        t1 = progress.add_task("[cyan]Generating Layers...", total=1)
        vp = rgm.generate_layers()
        progress.update(t1, advance=1)
        
        t2 = progress.add_task("[magenta]Applying Faults...", total=1)
        vp_faulted = rgm.apply_faults(vp)
        progress.update(t2, advance=1)
        
        t3 = progress.add_task("[green]Computing Seismic Response...", total=1)
        seismic = rgm.compute_seismic(vp_faulted)
        progress.update(t3, advance=1)

    # Visualization
    console.print("\n[bold blue]Plotting results...[/bold blue]")
    vp_cpu = rgm.to_cpu(vp_faulted)
    seismic_cpu = rgm.to_cpu(seismic)

    fig, ax = plt.subplots(1, 2, figsize=(14, 7))
    mid_idx = cfg.n3 // 2
    
    # VP Plot
    im0 = ax[0].imshow(vp_cpu[:, :, mid_idx], cmap='jet', aspect='auto', origin='lower')
    ax[0].set_title(f"Faulted Vp Model (Inline {mid_idx})", fontweight='bold')
    ax[0].set_xlabel("Crossline (X)")
    ax[0].set_ylabel("Depth (Z)")
    plt.colorbar(im0, ax=ax[0], label='Velocity (m/s)')
    
    # Seismic Plot
    im1 = ax[1].imshow(seismic_cpu[:, :, mid_idx], cmap='gray', aspect='auto', origin='lower')
    ax[1].set_title(f"Seismic Image (Inline {mid_idx})", fontweight='bold')
    ax[1].set_xlabel("Crossline (X)")
    ax[1].set_ylabel("Depth (Z)")
    plt.colorbar(im1, ax=ax[1], label='Amplitude')
    
    plt.tight_layout()
    output_file = "rgm_output.png"
    plt.savefig(output_file, dpi=300)
    console.print(f"✅ [bold green]Success! View output: {output_file}[/bold green]")

def main():
    console = Console()
    display_banner(console)
    
    parser = argparse.ArgumentParser(description="Random Geological Model Generator")
    parser.add_argument("--no-gpu", action="store_true", help="Force CPU usage")
    parser.add_argument("--batch", action="store_true", help="Run with default values without interaction")
    args = parser.parse_args()
    
    use_gpu = not args.no_gpu

    while True:
        if args.batch:
            cfg = RGMConfig()
        else:
            cfg = get_interactive_config(console)
        
        run_simulation(cfg, use_gpu, console)
        
        if args.batch or not Confirm.ask("\nRun again with different parameters?", default=False):
            break

    console.print("\n[bold green]RGM Session Ended. Goodbye![/bold green]")

if __name__ == "__main__":
    main()

