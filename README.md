# PIP PES
A collection of Permutationally Invariant Polynomial Potential Energy Surfaces written in Fortran and interfaced with i-PI, unless indicated as "standalone" (which can be found in the folder `standalone`. This collection will be regularly expanded and updated. Virtually all are at the CCSD(T) level and for single gas-phase molecules, unless stated explicitly. 

## Installation
(1) Install i-PI and set up the `IPI_ROOT` environment variable by running the `env.sh` script in i-PI.

(2) Download this repository (can be downloaded to a separate location from i-PI). Each folder in this repository contains a potential surface and the corresponding driver. The source code of the potential is located in `src` in each folder.

(3) Use the Makefile inside each potential to compile and install the Fortran driver. The Makefile assumes gcc and gfortran compiler.

## Running i-PI
Copy the `coeff` folder from the potential you use into the folder where i-PI is run, and then you can run i-PI as usual:
```bash
i-pi input.xml &
sleep 5
i-pi-driver -u -h driver -m $(POTENTIAL) -o $(ADDITIONAL_PARAM)
```

See below for the options available.

## Available Potentials:

* q-AQUA-pol (general many-body water potential): can be used for both small gas-phase clusters and condensed phases. Use option `-m q-aqua-pol`. The total number of water molecules must be supplied by the -o option, e.g., `-o 256`.

* Protonated oxalate anion: use option `-m oxalate`.

* CH<sub>5</sub><sup>+</sup>: use option `-m ch5p`.

* Ethanol: use option `-m ethanol`.

* N-methylacetamide: use option `-m NMA`.

* Hydrated proton and dipole moment surface (standalone). 

## Example Inputs
Example i-PI input files for short MD simulations and/or harmonic frequency analysis and initial geometries are provided in `examples`.
