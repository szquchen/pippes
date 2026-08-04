#Generic makefile for fortran90
LIBDIR := ../lib/pes_lib
LIBDIR2 := ../lib/watlib
MODDIR := ../lib/pes_lib/mod
MODDIR2 := ../lib/watlib/mod3b

FC := gfortran
LIBS :=   -L$(LIBDIR) -L$(LIBDIR2) -lpes3b -lpes2b -ldms2b -lpes -lpx
FFLAGS := -O -fdefault-real-8 -fdefault-double-8  -I$(MODDIR) -I$(MODDIR2)

vpath %.f90 src

OBJ := \
constants.o\
math.o  smear.o  ttm3f_mod2.o  mnasa_mod.o  mnasa.o  ttm3f_mb.o \
pot_monomer_mod.o  pot_monomer.o  pes3b.o  shell_w.o  \
chg.o ltp2011_dms.o pes_h.o dms_h.o shell_h.o  \
h5o2.pes4B-dms4B.o  shell_hw.o  \
bemsa.o shell_hww.o\
shell_hwww.o\
pes_shell_wt.o \
getpot.o \

# Targets:
.PHONY : clean 

%.o : %.f90
	$(FC) -c $(FFLAGS) $< 
%.o : %.f
	$(FC) -c $(FFLAGS) $< 

all : getpot

getpot :  $(OBJ) 
	$(FC) -o $@  $^ $(FFLAGS) $(LIBS)

clean :
	rm *.o *.mod 
