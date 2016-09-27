# no console output during compilation:
#.SILENT:

# *******************************************************
# ***                       Macros                    ***
# *******************************************************
FC = gfortran
FFLAGS = -O3 -ffast-math -ffree-form -fbacktrace
LINK = -llapack

lib = ./

# *******************************************************
# *** Regra que por defeito produz os ficheiros .o **
# *******************************************************
%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $*.f $(LINK)
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $*.f90
%.o: %.c
	gcc -O2 -Ofast -c -o $@ $*.c

# *******************************************************
# ***                Object files                     ***
# *******************************************************
OBJS = \
lib_array.o\
lib_assert.o \
lib_pikaia12.o \
lib_plot.o \
lib_regression.o \
lib_statistics.o

# **********************************************************
# ***                     Compile                        *** 
# **********************************************************

libmodules.a: $(OBJS)
	ar r $@ $^

clean:
	rm -f $(OBJS) libmodules.a *~
