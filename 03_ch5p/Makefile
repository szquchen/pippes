# Makefile for the driver tests
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

.PHONY: all install modules clean
CC=gcc
FLAGS=-g -O3 -Wall
CFLAGS=$(FLAGS)

# Fortran Compiling Flags for gfortran and intel Fortran
FC=gfortran
FFLAGS=$(FLAGS) -ffree-line-length-none -ffixed-line-length-none -Wno-maybe-uninitialized -fdefault-real-8 -fdefault-double-8 -fopenmp
# FC=ifx
# FFLAGS=-r8 -O3 -qopenmp -mcmodel=large

MODULES=distance.f90
PES=src/bemsa.f90 src/pes_shell.f90

OBJECTS=$(MODULES:%.f90=%.o) $(PES:%.f90=%.o)
all: install

sockets.o: sockets.c
	$(CC) $(CFLAGS) -c -o sockets.o sockets.c

ipi-interface.x: $(OBJECTS) sockets.o fsockets.o ipi-interface.o | $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^

install: ipi-interface.x
	ln -fs $(CURDIR)/ipi-interface.x $(IPI_ROOT)/bin/i-pi-driver

#driver_pure.x: $(OBJECTS) fsockets_pure.o driver.o | $(OBJECTS)
#	$(FC) $(FFLAGS) -o driver_pure.x $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f *.o *.mod *.x */*.mod */*.o $(IPI_ROOT)/bin/i-pi-driver
