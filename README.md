# PIP PES
A collection of Permutationally Invariant Polynomial Potential Energy Surfaces written in Fortran and interfaced with i-PI. This collection will be regularly expanded and updated.  Virtually all are at the CCSD(T) level.

## Installation
(1) Install i-PI and set up the `IPI_ROOT` environment variable by running the `env.sh` script in i-PI.

(2) Download this repository (can be downloaded to a separate location from i-PI). Each folder in this repository contains a potential surface and the corresponding driver. The source code of the potential is located in `src` in each folder.

(3) Go to `src/pes_shell.f90` inside the potential you'd like to run, and modify the variable `coef_path` to the actual path of the `coeff` directory on your machine.

(4) Use the Makefile inside each potential to compile and install the Fortran driver. The Makefile assumes gcc and gfortran compiler.

## Running i-PI
After installation, you can run i-PI as usual:
```bash
i-pi input.xml &
sleep 5
i-pi-driver -u -h driver -m $(POTENTIAL)
```

See below for the options available.

## Available Potentials:

* q-AQUA-pol (general many-body water potential): use option `-m q-aqua-pol`. The total number of water molecules must be supplied by the -o option, e.g., `-o 256`.

* Protonated oxalate anion: use option `-m oxalate`.

* CH<sub>5</sub><sup>+</sup>: use option `-m ch5p`.
