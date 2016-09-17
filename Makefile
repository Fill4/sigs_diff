# no console output during compilation:
#.SILENT:

# *******************************************************
# ***          Comecar por limpar os sufixos          ***
# *******************************************************
.SUFFIXES:

# *******************************************************
# ***     Especificar os sufixos para .f .o .do       ***
# *******************************************************
#.SUFFIXES: .f90 .o


# *******************************************************
# ***   Especificar as directorias com as subrotinas  ***
# *******************************************************
BASE := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# library
lib := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))../lib-fortran)

# *******************************************************
# ***                       Macros                    ***
# *******************************************************
# gfortran
FC	= gfortran
FFLAGS = -O3 -ffree-form -ffast-math -fexternal-blas -march=native -funroll-loops -Wuninitialized -Werror
LINK = -lmodules -llapack -lblas -L/usr/lib
#dbx     = -O5 -r8 -g
#profil  = -p -O5 -r8 
#samedir = .
#FTN     = ftnchek


#
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
$(BASE)/sig_bcz_d2.o \
$(BASE)/openfiles.o \
$(BASE)/output.o \
$(BASE)/deffreq.o \
$(BASE)/parameters.o \
$(BASE)/second_diff.o \
$(BASE)/init.o \
$(BASE)/skpcom.o \
$(BASE)/help.o \
$(BASE)/fit_d2_genetic.o

# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sigs_diff: $(FILES)
	$(FC) $(FILES) -o sigs_diff -L$(lib) -I$(lib) $(LINK) -O3

clean:
	rm -f $(FILES) *~ *.mod *.o sigs_diff