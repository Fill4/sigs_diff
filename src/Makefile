# no console output during compilation:
#.SILENT:

# *********************************************************
# ***					Directories						***
# *********************************************************
BASE := $(abspath ./)

# library
lib := $(abspath ../lib/)

# *********************************************************
# ***					Macros							***
# *********************************************************
FC	= gfortran
FFLAGS = -O2 -ffree-form -ffast-math
LINK = -lmodules -llapack

# *******************************************************
# *** Regra que por defeito produz os ficheiros .o **
# *******************************************************
%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $*.f -L$(lib) -I$(lib) $(LINK) 
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $*.f90 $(LINK) -L$(lib) -J$(lib)

#----------------------------------------------------------

FILES = \
$(BASE)/types.o \
$(BASE)/commonvar.o \
$(BASE)/commonarray.o \
$(BASE)/fun.o \
$(BASE)/components.o \
$(BASE)/fit_d2_genetic.o \
$(BASE)/openfiles.o \
$(BASE)/output.o \
$(BASE)/deffreq.o \
$(BASE)/read_inputs.o \
$(BASE)/second_diff.o \
$(BASE)/automatic_interval.o \
$(BASE)/init.o \
$(BASE)/skpcom.o \
$(BASE)/help.o \
$(BASE)/main.o

# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sigs_diff: $(FILES)
	$(FC) $(FILES) -o sigs_diff -L$(lib) -I$(lib) $(LINK)

clean:
	rm -f $(FILES) *~ *.mod *.o sigs_diff
