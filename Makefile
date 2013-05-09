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
BASE = /home/joao/Programs/CODE_freqFit

path_main = $(BASE)/2ndDifferences
path_common = $(BASE)/2ndDifferences/common
path_basic = $(BASE)/2ndDifferences/basic

# library
lib = /home/joao/Programs/fortran/lib

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
$(path_main)/types.o \
$(path_main)/commonvar.o \
$(path_main)/commonarray.o \
$(path_main)/fun.o \
$(path_main)/components.o \
$(path_main)/sig_bcz_d2.o \
$(path_main)/openfiles.o \
$(path_main)/output.o \
$(path_main)/deffreq.o \
$(path_main)/parameters.o \
$(path_main)/error_covariance.o \
$(path_main)/second_diff.o \
$(path_main)/fit_d2.o \
$(path_main)/fit_d2_genetic.o
#$(path_main)/fun2.o


FCOM = \
$(path_common)/init.o

# $(JFA)/minimize.o \
# $(JFA)/nl2sol.o \
# $(JFA)/resid.o\

FBASIC = \
$(path_basic)/num_to_text.o \
$(path_basic)/skpcom.o \
$(path_basic)/length.o





# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sig_bcz_d2: $(FILES) $(FCOM) $(FBASIC) 
	$(FC) $(FILES) $(FCOM) $(FBASIC) -o $@ -L$(lib) -I$(lib) $(LINK) -O3

clean:
	rm -f $(FILES) $(FCOM) $(FBASIC) *~ *.mod *.o sig_bcz_d2
