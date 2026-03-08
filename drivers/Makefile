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

.PHONY: all modules clean
FC  := ifort -r8 -O3 -qopenmp -mcmodel=large -ftz -fpp -Bstatic
FFLAGS  := -qmkl=parallel

MODULES=distance.f90
PES = q_aqua_pol/constants.f90 q_aqua_pol/math.f90 q_aqua_pol/nasa_mod.f90 q_aqua_pol/t_ewald.f90 q_aqua_pol/t_model_mod.f90 q_aqua_pol/pot_ttm.f90 q_aqua_pol/bemsa2b.f90 q_aqua_pol/bemsa3b.f90 q_aqua_pol/bemsa4b.f90 q_aqua_pol/potential.f90 q_aqua_pol/pes_shell.f90
OBJECTS=$(MODULES:%.f90=%.o) $(PES:%.f90=%.o)

CC=gcc -g -O3 -Wall
all: driver.x #driver_pure.x

sockets.o: sockets.c
	$(CC) $(CFLAGS) -c -o sockets.o sockets.c

driver.x: $(OBJECTS) sockets.o fsockets.o driver.o | $(OBJECTS)
	$(FC) $(FFLAGS) -o driver.x $^
	ln -fs ../drivers/f90/driver.x ../../bin/i-pi-driver

#driver_pure.x: $(OBJECTS) fsockets_pure.o driver.o | $(OBJECTS)
#	$(FC) $(FFLAGS) -o driver_pure.x $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f *.o *.mod *.x */*.mod */*.o ../../bin/i-pi-driver
