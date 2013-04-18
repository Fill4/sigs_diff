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
		
		! smoothing and fitting control parameters -
		real    :: xinitd, ftold, tolfitd, dcd
		! fitting procedure -
		integer :: iterinitd,iterfitd
		! reference values -
		real    :: w0refd,xl0d, nu0d
		
		! initial guesses of the parameters are declared in commonvar
				
		! type of input for frequency files -
		real    :: intyped
		! set iprint to -
		!	0 - no output, just the final values for the parameters
		!	1 - the coefficients are written in a file
		!	2 - as 1, and with the residuals being written on the screen
		!	3 - as 2, and with the end signal+fit being written in a file
		!	4 - as 3, and with the signal+fit being written in a file for each iteration
		!	5 - only the signal for the first iteration is written
		integer  :: iprintd
		! output second differences used in calculation?
		logical  :: write_d2
		! borders to ignore in frequency (right and left) -
		real     :: vrigthd,vleftd
		! minimum number of modes with same degree -
		integer  :: nlmind
		! wether it should use the errors or not -
		character(len=10) :: include_errorsd, use_error_chi2d
		integer  :: isigd
		! upper limit for error -
		real     :: ssmaxd
		! range in degree -
		integer  :: lmind,lmaxd
		namelist / sig_bcz_controls / xinitd, ftold, tolfitd, dcd,&
             iterinitd,iterfitd,&
             w0refd,xl0d,nu0d,&
             amp_bcz, tau_bcz, phi_bcz, amp1_he, amp2_he, tau_he, phi_he, poly0, &
             intyped,&
             iprintd,&
             write_d2, write_final, &
             vrigthd,vleftd,&
             nlmind,&
             isigd, include_errorsd, use_error_chi2d, &
             ssmaxd,&
             lmind,lmaxd
     
		!--- Read from namelist input file -
		integer                        :: ierr = 1
		integer                        :: unit1 = 8
		character (len=256)            :: message
		integer :: unit
		
		! initialize controls to default values -
		include "sig_bcz_controls_default.dek"
		
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

!		if( iprintd .ne. 0 ) then
!			write (6,'( a,/ )') "  Input parameters are:"
!			write (6, 6001) xinitd, xamp0d, tau0refd, phi0refd, iprintd, include_errorsd
! 6001		format(4x, "XINIT = ", es10.4, /, &
!                   4x, "AMP0 = ", f5.3, 4x, "TAU0 = ", f9.2, 4x, "PHI0 = ", f5.3, /, &
!                   4x, "IPRINT = ", i1, 4x, "ERRORS = ", a1, / )
!		end if
		
!		write(6,'( a,/ )') "  Input parameters are:"
!		write(6,nml=sig_bcz_controls)
		

		pi = 4.0_dp*atan(1.0_dp)
		pi_sq = pi*pi
		twopi = 2.0_dp*pi
		fac = 2.0_dp-6*pi

        tau0_houdek = 80.0d0
        tau0_houdek_sq = 6400.0d0

		icalc=0
		ipara=0
		ioutp=0
		idata=0

		xinit = xinitd
		ftol = ftold
		tolfit = tolfitd
        
		dc = 	dcd
		iterinit = iterinitd
		iterfit = iterfitd

		icalc=1
		
		nu0 = nu0d * 1.0d-6 ! convert from muHz (input) to Hz
		w0 = nu0 * twopi
		
		w0ref = w0refd
		xl0 = xl0d
			xl02=xl0*(xl0+1.0d0)

		
		intype = intyped
           if (intype.eq.0) then
              xmass = 1.0
              xrad = 1.0
           endif
           
		ipara=1
		
		
		iprint = iprintd
		write_d2_to_file = write_d2
		
		ioutp=1
		
		lmin = lmind
		lmax = lmaxd
		nlmin = nlmind
		isel = 0

		if (isel.eq.0) then
		vleft = vleftd
		vrigth = vrigthd
		else if (isel.eq.1) then
		nleft = vleftd !! fix
		nrigth = vrightd !! fix
		else 
		write(*,*) "ERROR: ISEL must be 0 or 1!"

		endif
		  
		include_errors = include_errorsd 
		use_error_chi2 = use_error_chi2d
		isig = isigd
		ssmax = ssmaxd

		idata=1

		if (icalc*ipara*ioutp*idata.eq.0) then
			write (*,*) 'ERROR: data not read correctly!'
			stop
		endif

		return
	
	end subroutine parameters
