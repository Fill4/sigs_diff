<<<<<<< HEAD
!--------------------------------------------------------------------
  subroutine fit_d2_genetic (chi2)
!	 this subroutine iterates until the relative variation of the residuals is 
!    smaller than TOLFIT. The final value of the parameters is in C and the 
!    residuals are returned in chi2

        use types_and_interfaces
        use commonvar
        use commonarray, only : nd2, w_d2, d2, sigd2, c, polyc
        use lib_pikaia12
        use lib_simplex
        use lib_regression, only: polyreg
        use lib_array
        use lib_plot

        implicit none
        
		real(dp)                 :: chi2

        real(dp)                    :: object, simp, stopcr
        real(dp), dimension(nconst) :: p, step, var
        integer                     :: ier, print_every, iquad, maxf, nloop, nop
        logical                     :: first

        real        :: ctrl(12), x(nconst), f
        integer     :: seed, status
        integer     :: degree
        
        character(len=80) :: outfile
        
        
        real(dp), dimension(150) :: xx, result_fun, result_smooth, result_he, result_bcz
        real(dp)                :: min_xx, max_xx 

        
        ! The polynomial fit is done before the signal fit 
        degree = 3
        
        call polyreg(1.0_dp/w_d2(1:nd2), d2(1:nd2), degree, polyc)
        
        write(*,*) 'Polynomial fit: '
        write(*,'(16x, es10.2)') polyc(1)
        write(*,'(16x, es10.2, a)') polyc(2), ' x^-1'
        write(*,'(16x, es10.2, a)') polyc(3), ' x^-2'
        write(*,'(16x, es10.2, a)') polyc(4), ' x^-3'
       
        ! GA minimization ******************************************************
        
        !     First, initialize the random-number generator
        seed=13590
        call rninit(seed)
        
        !     Set control variables
        ctrl(1:12) = -1
        ctrl(1) = 120
        ctrl(2) = iterfit
        ctrl(5) = 5 ! one-point+creep, adjustable rate based on fitness
        !ctrl(12) = 2
        outfile = 'evolution_par_ga.dat'
        
        !   Now call pikaia
        !CALL pikaia(objfun_ga, nconst, ctrl, x, f, status, outfile)
        CALL pikaia(objfun_ga, nconst, ctrl, x, f, status)  ! if using PIKAIA 1.2

        ! rescaling parameters
        call rescale(x, c)
        
        ! residuals of best parameters
        chi2 = 1.0d12/objfun_ga(nconst, x)
        
        !     Print the results
        WRITE(*,*) ' status: ', STATUS
        WRITE(*,*) '      x: ', c
        WRITE(*,*) '      x: ', x
        WRITE(*,*) '  chi^2: ', 1./f
!        WRITE(*,20) ctrl
!        20 FORMAT(   '    ctrl: ', 6F11.6/ t11, 6F11.6)
        
        
        ! Nelder-Mead minimization to refine minimum ***************************
        
        !   set up starting values (from GA minimization)
        p = c
        !   set max # of function evaluations, print every iprint
        maxf = iterfit
        print_every = -100
        !   set value for stopping criterion.   Stopping occurs when the
        !   standard deviation of the values of the objective function at
        !   the points of the current simplex < stopcr
        stopcr = tolfit
        nloop = 100
        !   now call MINIM to do the work
        first = .true.
        CALL minim(p, nconst, object, maxf, print_every, stopcr, nloop, &
                   iquad, simp, var, objfun, ier)
        !   successful termination
		c = p
        
        
        ! Plots ****************************************************************
        
        ! create array with smooth function		
		min_xx = minval(w_d2(1:nd2)) - 1.0d-4
		max_xx = maxval(w_d2(1:nd2)) + 1.0d-4
        call linspace(min_xx, max_xx, xx)
        result_fun = fun(xx)
        result_smooth = smooth_comp(xx)
        result_he = he_comp(xx)
        result_bcz = bcz_comp(xx)


        call plot(xx*1.0d6,result_he*1.0d6, &
                  xx*1.0d6,result_bcz*1.0d6, &
                  xx*1.0d6,(result_smooth+result_he+result_bcz)*1.0d6, &
                  ' 1-00-10-',color3='black',color2='red',color1='blue')!, &
                  !terminal='png')
                  !yrange=(/-3.0d0,3.0d0/) ) 


        call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, &
                  xx*1.0d6, result_fun*1.0d6, &
                  xx*1.0d6,result_smooth*1.0d6, &
                  ' 5.00-10-',color3='green',color2='black',color1='red')!, &
                  !errors=sigd2(1:nd2)*1.0d6)
                  !terminal='png')
                  !yrange=(/-3.0d0,3.0d0/) )
                  
        call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, &
                  xx*1.0d6, result_fun*1.0d6, &
                  ' 5.00-',color2='black',color1='red', &
                  errors=sigd2(1:nd2)*1.0d6)
                  !terminal='png')
                  !yrange=(/-3.0d0,3.0d0/) )                
                  
!        call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, &
!                  xx*1.0d6, result_fun*1.0d6, &
!                  ' 5.00-',color2='black',color1='green', &
!                  errors=sigd2(1:nd2)*1.0d6)!, &
!                  !terminal='png')
!                  !yrange=(/-3.0d0,3.0d0/) )
                  

		return
=======
!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine fit_d2_genetic (chi2)
! This subroutine initially removes the smooth component from the second 
! differences.
!
! Next it runs PIKAIA to try to fit the second differences to the funtion
! in the fun.f file using a iteratively reweighted least squares method.

	use types_and_interfaces
	use commonvar
	use commonarray, only : nd2, w_d2, d2, sigd2, l_d2, c, polyc, weight, pre_d2
	use lib_pikaia12
	use lib_regression, only: polyreg
	use lib_statistics, only: median

	implicit none
		
	real(dp), intent(inout)		:: chi2
	real(dp), dimension(nconst)	:: c0, p, step, var

	real						:: ctrl(12), x(nconst), f
	integer						:: seed, status
	integer						:: new_unit, i
	
	character(len=80)			:: outfile, filename
	real(dp), dimension(nd2)	:: smooth

	! Polynomial fit of the second differences. The degree is previously defined
	if (degree .eq. 0) then
		polyc(1) = median(d2(1:nd2))
	else
		call polyreg(1.0_dp/(w_d2(1:nd2)*w0ref), d2(1:nd2), degree, polyc)
	end if

	! Get smooth function and remove it from the second differences
	smooth(1:nd2) = smooth_comp(w_d2(1:nd2)*w0ref)
	pre_d2(1:nd2) = d2(1:nd2)
	d2(1:nd2) = d2(1:nd2) - smooth(1:nd2)

	! Define number of re-weight iterations based on error usage 
	!if (use_error_chi2) then
	!	maxIter = 4
	!else
	!	maxIter = 1
	!end if

	! First, initialize the random-number generator
	seed = TIME()
	call rninit(seed)
	
	! Set control variables
	ctrl(1:12) = -1
	ctrl(1) = pikaia_pop 
	ctrl(2) = pikaia_gen
	ctrl(5) = 5 ! one-point+creep, adjustable rate based on fitness
	
	! Either run PIKAIA once for a no errors run or iterate through various weights for
	! each point until convergence or until MaxIter
	!do iterIRLS=1,maxIter
	call pikaia(objfun_ga, nconst, ctrl, x, f, status)  ! if using PIKAIA 1.2
		!call rescale(x, c)
		!if (all(abs(c0(1:2)-c(1:2)) < 0.2_dp * c(1:2)) .and. all(abs(c0(4:6)-c(4:6)) < 0.2_dp * c(4:6))) exit
		!c0 = c
	!end do
		
	! Rescale parameters
	call rescale(x, c)

	!Check exit_status for errors during PIKAIA execution and stop if there were any
	if (status /= 0) then
		write(6,*) "Error in PIKAIA"
		stop
	endif
	
	! Residuals of best parameters
	chi2 = 1.0/f

	return
>>>>>>> Automatic_Approach
		
end subroutine fit_d2_genetic
  
  
function objfun_ga(n, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal is calculated in FUN for the current values of the parameters p, 
! and subtracted from the second differences

<<<<<<< HEAD
        ! rescaling parameters
        call rescale(p, c)

        ww = w_d2(1:nd2)
		signal = fun(ww)
		resid = sum( ((d2(1:nd2)-signal)/(sigd2(1:nd2)))**2 )

		fun_val = sngl(1.0 / resid)
		
!		resid_vector(:,1) = d2(1:nd2)-signal
!		chi2 = matmul( & 
!		         matmul(transpose(resid_vector), icov), &
!		               resid_vector)
!		fun_val = sngl(1.0 / chi2(1,1))
		                              
		!write(*,*) resid, 1.0d12*chi2
		
		return
		
  end function objfun_ga
=======
	use types_and_interfaces, only: dp, fun, rescale
	use commonvar, only : nconst, pi, iterIRLS, use_error_chi2
	use commonarray, only : nd2, w_d2, d2, c, l, sigd2, weight
	implicit none

	integer, intent(in)			:: n     ! size of parameter space
	real, intent(in)			:: p(:)
	real						:: fun_val
	real(dp), dimension(nd2)	:: ww, signal, weight0, resid
	real(dp)					:: final_resid
	integer, parameter			:: Q = 4

	! Rescale parameters
	call rescale(p, c)

	ww = w_d2(1:nd2)
	signal = fun(ww)
	! If we are using errors enter IRLS
	if (use_error_chi2) then
		resid = ((d2(1:nd2)-signal(1:nd2))/sigd2(1:nd2))**2
	!	weight0 = 1.0_dp / sigd2(1:nd2)**2
	!	resid = (d2(1:nd2)-signal)**2 * weight0(1:nd2)
	!	if (iterIRLS>1) then
	!		weight(1:nd2) = Q / ( sigd2(1:nd2)**2 * (resid + Q) )
	!	else 
	!		weight(1:nd2) = weight0
	!	end if
	!	resid = (d2(1:nd2)-signal(1:nd2))**2 * weight(1:nd2)
	! Else just do normal least square calculation
	else if (.not. use_error_chi2) then
		resid = (d2(1:nd2)-signal(1:nd2))**2
	end if
	final_resid = sum(resid)
	fun_val = sngl(1.0 / final_resid)
	
	return

end function objfun_ga
>>>>>>> Automatic_Approach
  
  
subroutine rescale(array_in, array_out)
! Rescales the parameters that come out of pikaia ARRAY_IN to their physical
! values ARRAY_OUT
<<<<<<< HEAD
        
        use types_and_interfaces, only: dp
        use commonvar, only: pi
        
        implicit none
        
        real, dimension(:), intent(in)      :: array_in
        real(dp), dimension(:), intent(out) :: array_out
        
        
        ! polynomial
!        array_out(1) = -1.62013420d-5
!        array_out(9) = 1.54115760d-2
!        array_out(10) = -4.83646351
!        array_out(11) = 5.08562576d2
        
!        array_out(1) = -1.85558899d-5
!        array_out(9) = 1.63208828d-2
!        array_out(10) = -4.13947874
!        array_out(11) = 2.63645116d2
        
        ! bcz
        array_out(1) = dble(array_in(1))! * 0.5d0
        !array_out(1) = 0.2
        array_out(2) = dble(array_in(2)) * (4000._dp - 1900._dp) + 1900._dp
        array_out(3) = dble(array_in(3)) * pi !2.0_dp * pi
        
        ! heII
        array_out(4) = dble(array_in(4))! * 0.1d0
        array_out(5) = dble(array_in(5)) * 150.0_dp ! Delta_II in sec
        array_out(6) = dble(array_in(6)) * (1800._dp - 300._dp) + 300._dp
        array_out(7) = dble(array_in(7)) * pi !2.0_dp * pi
=======
	
	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)			:: array_in
	real(dp), dimension(:), intent(out) 	:: array_out

	
	! Bcz
	array_out(1) = dble(array_in(1)) * 10
	array_out(2) = dble(array_in(2)) * (upper_tau_bcz - lower_tau_bcz) + lower_tau_bcz
	array_out(3) = dble(array_in(3)) * pi
	
	! HeII
	array_out(4) = dble(array_in(4)) * 10
	array_out(5) = dble(array_in(5)) * 10
	array_out(6) = dble(array_in(6)) * (upper_tau_he2 - lower_tau_he2) + lower_tau_he2
	array_out(7) = dble(array_in(7)) * pi
>>>>>>> Automatic_Approach
  
end subroutine rescale
  
