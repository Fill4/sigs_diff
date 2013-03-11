!--------------------------------------------------------------------
  subroutine fit_d2_genetic (chi2)
!	 this subroutine iterates until the relative variation of the residuals is 
!    smaller than TOLFIT. The final value of the parameters is in C and the 
!    residuals are returned in chi2

        use types_and_interfaces
        use commonvar
        use commonarray, only : nd2, w_d2, d2, sigd2, c, polyc
        use lib_pikaia12
        use lib_regression, only: polyreg
        use lib_array
        use lib_plot

        implicit none
        
		real(dp)                 :: chi2

        real(dp)                    :: object, simp, stopcr
        real(dp), dimension(nconst) :: p, step, var

        real        :: ctrl(12), x(nconst), f
        integer     :: seed, status
        integer     :: degree
        
        character(len=80) :: outfile
        
        
        real(dp), dimension(150) :: xx, result_fun, result_smooth, result_he, result_bcz
        real(dp)                :: min_xx, max_xx 

        
        ! The polynomial fit is done before the signal fit 
        degree = 3
        
        call polyreg(w_d2(1:nd2), d2(1:nd2), degree, polyc)
        
        write(*,*) 'Polynomial fit: '
        write(*,'(16x, es10.2)') polyc(1)
        write(*,'(16x, es10.2, a)') polyc(2), ' x'
        write(*,'(16x, es10.2, a)') polyc(3), ' x^2'
        write(*,'(16x, es10.2, a)') polyc(4), ' x^3'
       
        
        !     First, initialize the random-number generator
        seed=13578
        call rninit(seed)
        
        !     Set control variables
        ctrl(1:12) = -1
        ctrl(1) = 120
        ctrl(2) = iterfit
        ctrl(5) = 5 ! one-point+creep, adjustable rate based on fitness
        !ctrl(12) = 2
        outfile = 'evolution_par_ga.dat'
        
        !     Now call pikaia
        !CALL pikaia(objfun_ga, nconst, ctrl, x, f, status, outfile)
        CALL pikaia(objfun_ga, nconst, ctrl, x, f, status)  ! if using PIKAIA 1.2

        ! rescaling parameters
        call rescale(x, c)
        
        ! residuals of best parameters
        chi2 = 1.0/objfun_ga(nconst, x)
        
        !     Print the results
        WRITE(*,*) ' status: ', STATUS
        WRITE(*,*) '      x: ', c
        WRITE(*,*) '  chi^2: ', 1./f
        !WRITE(*,20) ctrl


        20 FORMAT(   '    ctrl: ', 6F11.6/ t11, 6F11.6)
        
        ! create array with smooth function		
		min_xx = minval(w_d2(1:nd2)) - 2.0d-4
		max_xx = maxval(w_d2(1:nd2)) + 2.0d-4
        call linspace(min_xx, max_xx, xx)
        result_fun = fun(xx)
        result_smooth = smooth_comp(xx)
        result_he = he_comp(xx)
        result_bcz = bcz_comp(xx)


        call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, &
                  xx*1.0d6, result_fun*1.0d6, &
                  ' 5.00-',color2='black',color1='green', &
                  errors=sigd2(1:nd2)*1.0d6)
                  !terminal='png')
                  !yrange=(/-3.0d0,3.0d0/) )
                  
        call plot(xx*1.0d6,result_he*1.0d6, &
                  xx*1.0d6,result_bcz*1.0d6, &
                  xx*1.0d6,result_smooth*1.0d6, &
                  ' 1-00- 2-',color3='green',color2='red',color1='blue')!, &
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
        use commonarray, only : nd2, w_d2, d2, c, l, sigd2
    
        implicit none

        integer, intent(in)     :: n     ! size of parameter space
        real, intent(in)        :: p(:)
        real                    :: fun_val

!        real(dp)                    :: signal
        real(dp), dimension(nd2)    :: ww, signal
        real(dp)                    :: resid

        ! rescaling parameters
        call rescale(p, c)

        ww = w_d2(1:nd2)
		signal = fun(ww)
		resid = sum( ((d2(1:nd2)-signal)/(sigd2(1:nd2)))**2 )

!		do i=1,nd2
!			ll = l(i)
!			signal = fun(ww(i))
!			resid = resid + ((d2(i)-signal)/sigd2(i))**2
!		end do

		fun_val = sngl(1.0 / resid)
		
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
        array_out(1) = dble(array_in(1)) * 10.0_dp
        array_out(2) = dble(array_in(2)) * (3000._dp - 1200._dp) + 1200._dp
        array_out(3) = dble(array_in(3)) * 2.0_dp * pi
        
        ! heII
        array_out(4) = dble(array_in(4)) * 0.1_dp
        array_out(5) = dble(array_in(5)) * 100.0_dp ! Delta_II in sec
        array_out(6) = dble(array_in(6)) * (1000._dp - 500._dp) + 500._dp
        array_out(7) = dble(array_in(7)) * 2.0_dp * pi

  
  end subroutine rescale
  
