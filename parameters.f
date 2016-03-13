!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
	subroutine parameters(filename)
!	The input data necessary to fit the signal is read here

		use commonvar
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		
		character (len=6)   :: aline
		character(len=80), intent(in)  :: filename

		! sig_bcz_controls
		namelist / sig_bcz_controls / xinitd, ftold,&
			iterinitd,iterfitd,&
			w0refd,&
			vrightd,vleftd,&
			nlmind,&
			use_error_chi2d, &
			ssmaxd,&
			lmind,lmaxd
		
		! smoothing and fitting control parameters -
		real    :: xinitd, ftold
		! fitting procedure -
		integer :: iterinitd,iterfitd
		! reference values -
		real    :: w0refd
		! borders to ignore in frequency (right and left) -
		real     :: vrightd,vleftd
		! minimum number of modes with same degree -
		integer  :: nlmind
		! wether it should use the errors or not -
		logical  :: use_error_chi2d
		! upper limit for error -
		real     :: ssmaxd
		! range in degree -
		integer  :: lmind,lmaxd

		!--- Read from namelist input file -
		integer                        :: ierr = 1
		integer                        :: unit1 = 8
		character (len=256)            :: message
		integer :: unit
		
		! initialize controls to default values -
		include "options_default.dat"
		
		!write (6,*) ' '
		write (6,*) " Reading input parameters from file: ", filename
		! open the file -
		open(unit=unit1, file=filename, &
					  action='read', delim='quote', &
					  iostat=ierr)
		! read input -
		read(unit1, nml=sig_bcz_controls, iostat=ierr, iomsg=message)
		close (unit1)
			if (ierr /= 0) write(*,*) " --> failed in ", trim(filename), &
					  " with error code ", ierr, '/', message		

		pi = 4.0_dp*atan(1.0_dp)
		fac = 2.0d-6*pi

		xinit = xinitd
		ftol = ftold
		tolfit = tolfitd
		
		dc = 	dcd
		iterinit = iterinitd
		iterfit = iterfitd
		
		nu0 = nu0d * 1.0d-6 ! convert from muHz (input) to Hz
		
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
