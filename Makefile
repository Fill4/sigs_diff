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
BASE = /home/fill/Documents/repos/glitch2

# library
lib = /home/fill/Documents/repos/lib-fortran

# *******************************************************
# ***                       Macros                    ***
# *******************************************************
#FC = /usr/bin/f77
#FC	= g77 -Wall
FC	= gfortran
FFLAGS = -O3 -ffree-form -ffast-math -fexternal-blas
#dbx     = -O5 -r8 -g
#profil  = -p -O5 -r8 
#samedir = .
#FTN     = ftnchek
LINK = -lmodules -llapack -lblas -L/usr/lib

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
$(BASE)/star_par.o \
$(BASE)/fit_d2_genetic.o

# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sig_bcz_d2: $(FILES)
	$(FC) $(FILES) -o $@ -L$(lib) -I$(lib) $(LINK) -O3

clean:
	rm -f $(FILES) *~ *.mod *.o sig_bcz_d2
