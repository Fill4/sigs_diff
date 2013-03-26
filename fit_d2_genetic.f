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
		
  end subroutine fit_d2_genetic
  
  
  function objfun_ga(n, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal is calculated in FUN for the current values of the parameters p, 
! and subtracted from the second differences
        use types_and_interfaces, only: dp, fun, rescale
        use commonvar, only : nconst, pi
        use commonarray, only : nd2, w_d2, d2, c, l, sigd2, icov
    
        implicit none

        integer, intent(in)     :: n     ! size of parameter space
        real, intent(in)        :: p(:)
        real                    :: fun_val

!        real(dp)                    :: signal
        real(dp), dimension(nd2)    :: ww, signal
        real(dp)                    :: resid, resid_vector(nd2,1), chi2(1,1)

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
  
  
  subroutine rescale(array_in, array_out)
! Rescales the parameters that come out of pikaia ARRAY_IN to their physical
! values ARRAY_OUT
        
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
  
  end subroutine rescale
  
