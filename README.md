# SIGS - Seismic Inferences for Glitches in Stars (using second differences)

Fit acoustic glitches in the second differences of low-degree p-mode frequencies using the PIKAIA genetic algorithm

This program is independent but was developed alongside [sigs_freq](https://github.com/Fill4/sigs_freq) which fits acoustic glitches in the frequencies.

## Description

This program fits the signal due to the acoustic glitches at the base of the convection zone and at the Helium second ionization region, by removing a smooth component from the second differences of the frequencies and fitting the remaining residuals to a known function.

The signal used to describe the oscillatory signature of the glitches, adapted from the work of Faria [2013], is of the form:

![equation](http://mathurl.com/hbjsx4o.png?raw=true)

More details regarding the development of the methods from this repository can be found here: __to be added__

## Dependencies

This program requires the LAPACK and BLAS libraries and gfortran for the compilation
It also needs gnuplot to use option -p that shows plots of the results

In Linux these can be installed by running the command:

```
sudo apt-get install liblapack-dev
sudo apt-get install gfortran
sudo apt-get install gnuplot
# Install gnuplot-x11 if you are using an x11 server to show the plot windows

```

## Compiling

```
# Clone the repo
git clone https://github.com/Fill4/sigs_diff
# Move to repository folder
cd /path/to/repo/
# Compile
make
```
## Usage

To use the code it is necessary a file with the format of [sun.freqs](tests/sun.freqs) with the frequencies of oscillation of a star
Then run the command:
```
./sigs_diff -v -p -a
```
and when prompted, select the location of the frequencies file and click Enter.
The command -p shows the plots of the final fit and the final parameters will be printed to a file results_diff.
More commands and ways of executing the code are available and can be consulted here: __to be added__

## Testing
To test the compilation go to the tests folder and execute

```
sigs_diff -v -p -a < freqs
```

The plots will appear at the end of execution and the file results_diff will have the final parameters of the fitted function

## Bibliography

Faria, J. P. (2013), Asteroseismology of 16 cyg a and b, Master’s thesis ([link](http://hdl.handle.net/10216/69506))
