#Generic makefile for fortran90
FC = ifx -r8 -O
LIBS = -qmkl=sequential

OBJPES := bemsa_aspirin.o  pes_shell.o  testpot.o

testpot.x : $(OBJPES)
	$(FC) -o $@ $^ $(FFLAGS) $(LIBS)

%.o : %.f90
	$(FC) -c $(FFLAGS) $(LIBS) $<

clean:
	rm -f *.o *.mod *.x	
