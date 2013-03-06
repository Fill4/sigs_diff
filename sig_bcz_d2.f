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
	use lib_opt
	use lib_array
		
	use gnufor2
	
    implicit none
	
	character (len=80)   :: afile
	character (len=1)    :: amess
	character (len=80)   :: parameter_file
	
	integer :: i, argcount

	real                     :: varlim, var
	real(dp)                 :: res
	real(dp), dimension(3)   :: coeff 
    real(dp), dimension(100) :: xx, resultfun


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
	!nconst=8
	nconst=11
	allocate(c(nconst))
	
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
!	call fit_d2(res)
    call fit_d2_genetic(res)
!--- plot results -

    
!	call linspace(dble(w_d2(1)), dble(w_d2(nd2)), xx)
!	do i=1,100
!	    resultfun(i) = fun(c, xx(i), 0)
!	end do
!	write(*,*) w_d2(1), xx(1), resultfun(50), fun(c, w_d2(1), 1)
!	call plot(dble(w_d2(1:nd2)), dble(d2(1:nd2)), xx, resultfun,' 5. 1-',color2='dark-yellow',color1='#40e0d0')
	
!--- Writing the results -
	! variation in tau0 relative to initial value
!	var = abs(((c(3)/(w0*fac))-tau0)/tau0)
!	if (var .gt. varlim .and. amess(1:1) .eq. ' ') then
!	   amess='.'
!	   write (*,*)"  ==> WARNING: Value of taud not admissible! [.]"
!	endif
!	
	call output (afile, res)

!	if (iprint.ge.1) close (3)
	write (6,*)"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"

    deallocate(c)
    
   end subroutine sig_bcz_d2
