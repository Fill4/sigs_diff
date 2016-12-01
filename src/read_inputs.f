subroutine read_inputs(options_file)
! This subroutine parses through the options_file assigning the input values to 
! all variables.

	use commonvar
	implicit none

	character (len=80), intent(in)  :: options_file

	!Defining namelist opt_namelist for easy input of a list of variables
	namelist / opt_namelist / lmin, lmax, nlmin, nmin, nmax, use_error_chi2, ssmax,&
								degree, lambda, ftol, smooth_iter_max,&
								pikaia_pop, pikaia_gen,&
								w0ref, large_sep, teff,&
								upper_tau_bcz, lower_tau_bcz, upper_tau_he2, lower_tau_he2,&
								bcz_interval, he2_interval

	integer :: ierr = 1
	integer :: unit1 = 8
	character (len=256)            :: message

	! Open user defined options file
	if (verbose) write (6,*) " Reading the parameters from file: ", options_file
	!Open Options File
	open(unit=unit1, file=options_file, action='read', delim='quote', iostat=ierr)

	! Read user defined options file (overwrites default values)
	read(unit1, nml=opt_namelist, iostat=ierr, iomsg=message)  
	close (unit1)
	if (ierr /= 0) write(*,*) "Failed reading ", trim(options_file), " with error code ", ierr, '/', message

	return

end subroutine read_inputs
