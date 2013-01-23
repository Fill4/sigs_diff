!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
	subroutine parameters()
!	The input data necessary to fit the signal is read here

		use commonvar
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
        
		character (len=6)   :: aline
		character (len=80)  :: afilepar


		! sig_bcz_controls
		
		! smoothing and fitting control parameters -
		real    :: xinitd, ftold, tolfitd, dcd
		! fitting procedure -
		integer :: iterinitd,iterfitd
		! reference values -
		real    :: w0refd,xl0d
		! initial guesses of the parameters -
		real    :: xamp0d, tau0refd, phi0refd
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
             w0refd,xl0d,&
             xamp0d,tau0refd,phi0refd,&
             intyped,&
             iprintd,&
             vrigthd,vleftd,&
             nlmind,&
             isigd, include_errorsd, use_error_chi2d, &
             ssmaxd,&
             lmind,lmaxd
     
		!--- Read from namelist input file -
		character (len=*), parameter   :: filename = 'sig_bcz_input'
		integer                        :: ierr = 1
		integer                        :: unit1 = 8
		character (len=256)            :: message
		integer :: unit
		
		! initialize controls to default values -
		include "sig_bcz_controls_default.dek"
		
		!write (6,*) ' '
		write (6,*) " Reading input parameters from file: ", filename
		! open the file -
		open(unit=unit1, file="/home/joao/Documents/FCUP/TESE/bcz_fit/sig_bcz_input", &
                      action='read', delim='quote', &
                      iostat=ierr)
     	! read input -
     	read(unit1, nml=sig_bcz_controls, iostat=ierr)  
		close (unit1)
          	if (ierr /= 0) write(*,*) " --> failed in ", trim(filename), &
                      " with error code ", ierr

		if( iprintd .ne. 0 ) then
			write (6,'( a,/ )') "  Input parameters are:"
			write (6, 6001) xinitd, xamp0d, tau0refd, phi0refd, iprintd, include_errorsd
 6001		format(4x, "XINIT = ", es10.4, /, &
                   4x, "AMP0 = ", f5.3, 4x, "TAU0 = ", f9.2, 4x, "PHI0 = ", f5.3, /, &
                   4x, "IPRINT = ", i1, 4x, "ERRORS = ", a1, / )
		end if
		
!		write(6,'( a,/ )') "  Input parameters are:"
!		write(6,nml=sig_bcz_controls)
		

		pi  = 4.0d0*atan(1.0d0)
		fac = 2.0d-6*pi


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
		
		w0ref = w0refd
		xl0 = xl0d
			xl02=xl0*(xl0+1.0d0)
			
		xamp0 = xamp0d
		tau0ref = tau0refd
		phi0ref = phi0refd
		
		intype = intyped
           if (intype.eq.0) then
              xmass = 1.0
              xrad = 1.0
           endif
           
		ipara=1
		
		
		iprint = iprintd
		
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
