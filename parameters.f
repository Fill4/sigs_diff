!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine parameters(options_file)
! This subroutine parses through the options_file assigning the input values to 
! all variables.

	use commonvar
	implicit none
	
	character(len=80), intent(in)  :: options_file

	! Defining namelist sig_bcz_controls for easy input of a list of variables
	namelist / sig_bcz_controls / pikaia_popd,pikaia_gend,&
								w0refd,&
								vrightd,vleftd,&
								nlmind,&
								use_error_chi2d,&
								ssmaxd,&
								lmind,lmaxd
	
	! Variables to control pikaia execution
	integer		:: pikaia_popd = -1, pikaia_gend = -1
	! Reference frequency
	real		:: w0refd = 2100.0
	! Borders to ignore in frequency (right and left)
	real		:: vrightd = 0.0, vleftd = 0.0
	! Minimum number of modes with same degree
	integer		:: nlmind = 5
	! Whether to use errors or not
	logical		:: use_error_chi2d = .FALSE.
	! Upper limit for error
	real		:: ssmaxd = 0.500
	! Range in degree
	integer		:: lmind = 0,lmaxd = 2
	! Degree of the polynomial smooth function
	integer		:: degreed = 2

	integer                        :: ierr = 1
	integer                        :: unit1 = 8
	character (len=256)            :: message
	
	! Open Options File
	if (verbose) write (6,*) " Reading input parameters from file: ", options_file
	open(unit=unit1, file=options_file, &
				  action='read', delim='quote', &
				  iostat=ierr)

	! Read Options File
	read(unit1, nml=sig_bcz_controls, iostat=ierr, iomsg=message)
	close (unit1)
	if (ierr /= 0) write(*,*) " Failed reading ", trim(options_file), &
				  " with error code ", ierr, '/', message		

	! Constants
	pi = 4.0_dp*atan(1.0_dp)
	fac = 2.0d-6*pi
	
	! Assgn input values to all variables
	pikaia_pop = pikaia_popd
	pikaia_gen = pikaia_gend	
	
	w0ref = w0refd
	
	lmin = lmind
	lmax = lmaxd
	nlmin = nlmind
	isel = 0

	if (isel.eq.0) then
	vleft = vleftd
	vright = vrightd
	else if (isel.eq.1) then
	nleft = vleftd !! fix
	nright = vrightd !! fix
	else 
	write(*,*) "ERROR: ISEL must be 0 or 1!"
	endif

	use_error_chi2 = use_error_chi2d
	ssmax = ssmaxd

	return

end subroutine parameters
