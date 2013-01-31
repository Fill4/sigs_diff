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
JF = /home/joao/Programs/CODE_freqFit/2ndDifferences
JFA = /home/joao/Programs/CODE_freqFit/2ndDifferences/common
JFB = /home/joao/Programs/CODE_freqFit/2ndDifferences/basic
JFC = /home/joao/Programs/CODE_freqFit/nl2sol_fit/subroutines/nl2sol
# plot library:
JFp = /home/joao/utils/gnuFor2
# spline library:
JFspl = /home/joao/Programs/CODE_freqFit/dierckx
lib = /home/joao/Programs/fortran/lib

# *******************************************************
# ***                       Macros                    ***
# *******************************************************
#FC = /usr/bin/f77
#FC	= g77 -Wall
FC	= gfortran
FFLAGS = -O1 -ffree-form -ffast-math
#dbx     = -O5 -r8 -g
#profil  = -p -O5 -r8 
#samedir = .
#FTN     = ftnchek
LINK = -lmodules -llapack -L/usr/lib

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
$(JF)/types.o \
$(JF)/commonvar.o \
$(JF)/commonarray.o \
$(JF)/fun.o \
$(JF)/sig_bcz_d2.o \
$(JF)/output.o \
$(JF)/deffreq.o \
$(JF)/parameters.o \
$(JF)/second_diff.o \
$(JF)/fit_d2.o \
$(JF)/fit_d2_genetic.o
#$(JF)/fun2.o


FCOM = \
$(JFA)/init.o

# $(JFA)/minimize.o \
# $(JFA)/nl2sol.o \
# $(JFA)/resid.o\

FBASIC = \
$(JFB)/num_to_text.o \
$(JFB)/skpcom.o \
$(JFB)/length.o

Fnl2sol = \
$(JFC)/interfaces.o \
$(JFC)/minimize.o \
$(JFC)/nl2sol.o \
$(JFC)/resid.o



FPLOT = \
$(JFp)/gnufor2.o


#FSPLINE = \
#$(JFspl)/concur.o \
#$(JFspl)/curev.o \
#$(JFspl)/fpadpo.o \
#$(JFspl)/fpback.o \
#$(JFspl)/fpbspl.o \
#$(JFspl)/fpched.o \
#$(JFspl)/fpcons.o \
#$(JFspl)/fpdisc.o \
#$(JFspl)/fpgivs.o \
#$(JFspl)/fpinst.o \
#$(JFspl)/fpknot.o \
#$(JFspl)/fppocu.o \
#$(JFspl)/fprati.o \
#$(JFspl)/fprota.o 


# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sig_bcz_d2: $(FPLOT) $(FILES) $(FCOM) $(FBASIC) 
	$(FC) $(FILES) $(FCOM) $(FBASIC) $(FPLOT) -o $@ -L$(lib) -I$(lib) $(LINK) 

clean:
	rm -f $(FILES) $(FCOM) $(FBASIC) $(FPLOT) *~ *.mod *.o sig_bcz_d2
