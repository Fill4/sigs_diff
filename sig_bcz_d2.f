<<<<<<< HEAD
!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
   program main ! main only calls sig_bcz_d2
    call sig_bcz_d2
   end program main
    
    
   subroutine sig_bcz_d2

    use types_and_interfaces
    use commonvar
    use commonarray
    use lib_array
        
    use gnufor2
    
    implicit none
    
    character (len=80)   :: afile
    character (len=1)    :: amess
    character (len=80)   :: parameter_file
    
    integer :: argcount

    real(dp)                 :: varlim
    real(dp)                 :: res

    afile='00000'
    write (6,'(/, a, //)')"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"
    
!+++++++++++++++++++++++++++++++++++++++++++
!--- process command line argument
    argcount = iargc()
    if (argcount /= 1) then
        write(*,'(a,x,/)') "ERROR! usage: sig_bcz_d2 parameter_file"
        stop
    endif
    call getarg(1, parameter_file)
    
!--- Number of parameters to fit -
    nconst=7
    allocate(c(nconst))
    allocate(polyc(4))
    
!--- Read file with input parameters -
     call parameters (parameter_file)
     
!--- Initializing all quantities, read in frequencies and output files -
    call deffreq (afile)
    call init (afile) ! also calculates 2nd differences
    write(*,*) ' '

    !call openfiles (afile,xinitm)
    call flush (6)
    varlim = 0.2d0

    
    !call polyreg(dble(w_d2(1:nd2)), dble(d2(1:nd2)), 2, coeff)
    !write(*,*) coeff
    
    !call linspace(dble(w_d2(1)), dble(w_d2(nd2)), xx)
    !polyfun = coeff(3) * xx * xx + coeff(2) * xx + coeff(1)
    
    !call plot(dble(w_d2(1:nd2)), dble(d2(1:nd2)), xx, polyfun,' 5. 1-',color2='dark-yellow',color1='#40e0d0')
    
!--- Finding the best parameters -
    amess = ' '
!    call fit_d2(res)
    call fit_d2_genetic(res)
!--- plot results -

    
!    call linspace(dble(w_d2(1)), dble(w_d2(nd2)), xx)
!    do i=1,100
!        resultfun(i) = fun(c, xx(i), 0)
!    end do
!    write(*,*) w_d2(1), xx(1), resultfun(50), fun(c, w_d2(1), 1)
!    call plot(dble(w_d2(1:nd2)), dble(d2(1:nd2)), xx, resultfun,' 5. 1-',color2='dark-yellow',color1='#40e0d0')
    
!--- Writing the results -
    ! variation in tau0 relative to initial value
!    var = abs(((c(3)/(w0*fac))-tau0)/tau0)
!    if (var .gt. varlim .and. amess(1:1) .eq. ' ') then
!       amess='.'
!       write (*,*)"  ==> WARNING: Value of taud not admissible! [.]"
!    endif
!    
    call output (afile, res)

!    if (iprint.ge.1) close (3)
    write (6,*)"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"
=======
!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
program sigs_d2
! This program isolates an oscillatory signal that is present in the second 
! differences of the oscillation frequencies and associated with regions where 
! the sound speed has a discontinuity.
!
! The general form of the signal is a known function of frequency that depends 
! on a set of parameters C. 
!
! The differences between the observed 2nd differences and the model ones is 
! minimized in the least squares sense, providing the best fit parameters

	use types_and_interfaces
	use commonvar
	use commonarray
	use lib_array
	implicit none

	integer				:: narg, iarg
	character (len=80)	:: afile, options_file, star_file
	character (len=20)	:: name
	real(dp)			:: chi2

	! Check if arguments are found
	narg=command_argument_count()

	if(narg>0)then
	! Loop across options
		do iarg=1,narg
			call get_command_argument(iarg,name)
			select case(adjustl(name))
			case('-h','--help')
				call help
			case('-v','--verbose')
				verbose = .TRUE.
			case('-p','--plots')
				show_plots = .TRUE.
			case default
				write(6,*)"Option ",adjustl(name),"unknown"
			end select
		end do
	end if

	afile='00000'
	options_file = 'options_file'
	if (verbose) write (6,*)"---------------------> PROGRAM SIGS_D2 <---------------------"

	! Number of parameters to fit
1	nconst=7
	allocate(c(nconst))

	! Read options_file with input parameters
	call parameters(options_file)	

	! Initializing all quantities, read in frequencies and create output files
	call deffreq (afile) 	! Reads freqs_list file
	call init (afile) 		! Reads frequencias and calculates 2nd differences
	call openfiles			! Prepare output files

	call flush (6)

	! Removes the smooth component and finds the best parameters
	call fit_d2_genetic(chi2)
	! Output the results
	call output (afile, chi2)

	if (verbose) write (6,*)"---------------------> PROGRAM SIGS_D2 <---------------------"

	call flush (6)
	deallocate(c)

	goto 1
>>>>>>> Automatic_Approach

end program sigs_d2
