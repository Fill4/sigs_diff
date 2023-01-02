# SIGS - Seismic Inferences for Glitches in Stars (second differences)
Fit acoustic glitches in the second differences of low-degree p-mode frequencies using the PIKAIA genetic algorithm. \
This code was developed alongside [sigs_freq](https://github.com/Fill4/sigs_freq), which fits acoustic glitches using the stellar oscillation frequencies directly.

A full description of the methods used and the development of the code can be found in [Pereira, F. (2016)](https://hdl.handle.net/10216/90991).

## Description

Finds the signals corresponding to the acoustic glitches at the base of the convection zone and at the helium second ionization region. \
Starts by removing a smooth component from the second differences of the oscillation frequencies and then fits the residuals to a parametric function.

The function adopted to describe the oscillatory signature of the glitches in the second differences follows the following form:

$$
\begin{align*} 
\delta \Delta_2 \nu \simeq & \ A_{bcz} \left( \frac{\nu_r}{\nu} \right)^2 \sin \left( 4 \pi \tau_{bcz} \nu + 2 \phi_{bcz} \right) \ + \\
                         + & \ A_{HeII} \left( \frac{\nu_r}{\nu} \right) \exp \left[ -\beta_{HeII} \frac{\nu_r}{\nu} \right]^2 \sin \left( 4 \pi \tau_{HeII} \nu + 2 \phi_{HeII} \right)
\end{align*}
$$

where $\tau_{bcz}$ and $\tau_{HeII}$ represent the acoustic depth of the base of the convective zone and of the helium second ionization zone, respectively.

More details regarding the adopted methods can be found in Sections 3 and 4 of [Pereira, F. (2016)](https://hdl.handle.net/10216/90991).

## Dependencies

This program requires the LAPACK and BLAS libraries and gfortran for the compilation. \
It also needs gnuplot to use option -p that shows plots of the results.

In Linux these can be installed by running the command:
```
sudo apt-get install liblapack-dev
sudo apt-get install gfortran
sudo apt-get install gnuplot
# Install gnuplot-x11 if you are using an x11 server to show the plot windows

```

## Compilation

```
# Clone the repo
git clone https://github.com/Fill4/sigs_diff.git
# Move to repository folder
cd /path/to/repo/folder
# Compile
make
```
## Usage

To use the code, a file with the same format as [sun.freqs](tests/sun.freqs) is required, containing stellar oscillation frequencies, with their respective angular degree and mode numbers. \
The second differences are calculated from the frequencies. \
Then run the command:
```
./sigs_diff -v -p -a
```
and when prompted, select the location of the frequencies file and click Enter. 

The command -p shows the plots of the final fit and the final parameters will be printed to a file results_diff. \
More commands and ways of running the code are available and can be consulted in Appendix A of [Pereira, F. (2016)](https://hdl.handle.net/10216/90991).

## Example

To test the compilation go to the tests folder and run:
```
sigs_diff -v -p -a < freqs
```

The plots should appear at the end of execution and the file results_diff should have been created and contain the final parameters of the fitted function.

## References

[Pereira, F. (2016)](https://hdl.handle.net/10216/90991), Development of automatic tools for measuring acoustic glitches in seismic data of solar-type stars, MSc thesis \
[Faria, J. P. (2013)](http://hdl.handle.net/10216/69506), Asteroseismology of 16 cyg a and b, MSc thesis