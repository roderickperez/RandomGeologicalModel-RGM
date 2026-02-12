# Random Geological Model (RGM) Generation

This project implements a framework for generating random geological models using multi-randomization techniques, specifically designed for use in machine learning datasets.

## Credits

This project is based on the original repository from Los Alamos National Laboratory:
[https://github.com/lanl/rgm](https://github.com/lanl/rgm)

Reference paper: *Generation of Random Geological Models Using Multi Randomization for Machine Learning* (Found in `reference/`).

## Project Structure

The project is organized into several key directories:

- **`Fortran/`**: Contains the core numerical simulation and model generation logic written in Fortran.
    - `src/`: Source code for the geological modeling engine.
    - `example/`: Example scripts and configurations.
- **`Python/`**: Python interface and convenience wrappers for interacting with the Fortran backend.
    - `rgm_core.py`: Core Python API.
    - `main.py`: Entry point for common generation tasks.
- **`flit/`**: Supporting libraries and utilities used by the Fortran components (derived from the original LANL source).
- **`reference/`**: Documentation, including the original research paper that describes the methodology.
- **`requirements.txt`**: List of Python dependencies required to run the wrappers and visualization scripts.

## File Overview

- `fix_macros.py`: Script to handle macro expansions and code adjustments.
- `rgm_output.png`: Sample output visualization of a generated model.
- `test_macro*.f90`: Test files for verifying code generation and macro handling.

## Getting Started

1. **Install Dependencies**:
   ```bash
   pip install -r requirements.txt
   ```
2. **Setup Fortran Environment**:
   Ensure you have a compatible Fortran compiler (e.g., `gfortran` or `ifort`) installed to compile the sources in `Fortran/src`.

---
*Note: This repository is a curated version of the original RGM package for specific machine learning research purposes.*

## Running the RGM CLI (Python)

To launch the premium Python interface with interactive parameter selection:

1. **Activate the Environment**:
   ```bash
   source rgm_env/bin/activate
   ```

2. **Run the CLI**:
   ```bash
   python Python/main.py
   ```

3. **Install Dependencies (if needed)**:
    If running for the first time or if dependencies are missing:
   ```bash
   ./rgm_env/bin/pip install rich
   ```

## Generating 3D Cube Models

You have two options for generating RGM cubes:

### Option 1: Premium Python CLI (Recommended for Visualization)
This runs the Python implementation with an interactive menu and generates a visualization (`rgm_output.png`).

```bash
# Activate the environment
source rgm_env/bin/activate

# Run the CLI
python Python/main.py
```

### Option 2: High-Performance Fortran Binary
This runs the compiled Fortran executable (`x_generate_mtl_3d`) which generates the raw 3D data files in the `dataset3_mtl` directory.

```bash
cd Fortran/example
./bin/x_generate_mtl_3d
```
