!--------------------------------------------------------------------
  subroutine fit_d2 (resd)
!	 this subroutine iterates until the relative variation of the residuals is 
!    smaller than TOLFIT. The final value of the parameters is returned in C 
!    and the residuals in RESD

        use types_and_interfaces, only: dp
        use commonvar
        use commonarray, only : nd2, w_d2, d2, c
        use lib_simplex
        use lib_array
        use lib_plot


		real(dp), intent(inout)   :: resd

        real(dp)                    :: object, simp, stopcr
        real(dp), dimension(nconst) :: p, step, var
        integer            :: ier, print_every, iquad, maxf, nloop, nop
        logical            :: first
        !logical, dimension(:), pointer :: vary
        
        real(dp), dimension(80) :: xx, resultfun
        real(dp)                :: min_xx, max_xx 

        ! provide interface to the objective function
        interface
          subroutine objfun(p, func)
            implicit none
            integer, parameter     :: dp = selected_real_kind(p=15,r=307)
            real (dp), intent(in)  :: p(:)
            real (dp), intent(out) :: func
          end subroutine objfun
        end interface
        ! and to the function that calculates the signal
        interface 
            real(kind=8) function fun (w_d2)
	            implicit none
	            real(kind=8), intent(in)  :: w_d2
            end function fun
        end interface 


        ! Set up starting values
        c(1) = poly0*1.0d-6    ! input in muHz
        c(2) = amp_bcz*1.0d-12  ! input in muHz
        c(3) = tau_bcz  ! input in sec
        c(4) = phi_bcz
        c(5) = amp1_he*1.0d-3  ! input in muHz
        c(6) = amp2_he
        c(7) = tau_he   ! input in sec
        c(8) = phi_he
        
        p = c

        ! Set max. no. of function evaluations, print every iprint
        maxf = iterfit
        print_every = -100

        ! Set value for stopping criterion.   Stopping occurs when the
        ! standard deviation of the values of the objective function at
        ! the points of the current simplex < stopcr.
        stopcr = tolfit
        nloop = 100

        ! Fit a quadratic surface to be sure a minimum has been found.
        iquad = 1

        ! As function value is being evaluated in REAL (dp), it
        ! should be accurate to about 15 decimals.   If we set simp = 1.d-6,
        ! we should get about 9 dec. digits accuracy in fitting the surface.
        simp = 1.d-6

        ! Now call MINIM to do the work.
        first = .true.

          CALL minim(p, nconst, object, maxf, print_every, stopcr, nloop,   &
                     iquad, simp, var, objfun, ier)



        ! Successful termination.
		c = p
		call objfun(c, resd)
			
		! create array with smooth function		
		min_xx = minval(w_d2(1:nd2)) - 1.0d-4
		max_xx = maxval(w_d2(1:nd2)) + 1.0d-4
        call linspace(min_xx, max_xx, xx)
        do i=1,80
            resultfun(i) = fun(xx(i))
        end do
        
        ! rescale parameters back to more sensible values
		c(1) = c(1) * 1.0d6
		c(2) = c(2) * 1.0d12
		c(5) = c(5) * 1.0d3

		
!        write(*,*) w_d2(1), xx(100), resultfun(50), resultfun(1)
        write(*,'(8(f13.3,","))') c
        
        
        call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, xx*1.0d6, resultfun*1.0d6, &
                  ' 5.00-',color2='dark-yellow',color1='#40e0d0', &
                  yrange=(/-2.d0,2.d0/))
        !call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, &
        !          ' 5.', yrange=(/-2.d0,2.d0/))
	
	
		return
		
  end subroutine fit_d2
  
  
  subroutine objfun(p, func)
! This subroutine calculates the objective function value, i.e. the chi^2.
! The signal is calculate in FUN for the current values of the parameters p, and
! subtracted from the second differences
        use types_and_interfaces, only: dp
        use commonvar, only : use_error_chi2, nconst
        use commonarray, only : nd2, w_d2, d2, c, l, sigd2
    
        implicit none

        real(dp), intent(in)   :: p(:)
        real(dp), intent(out)  :: func
        
        integer :: i, ll
        real(dp) :: ww, signal, resid, fun

        c = p
		
		resid = 0.0d0
		! if not weighting by errors -
		if (.NOT. use_error_chi2) then
			do i=1,nd2
				ww = w_d2(i)
				ll = l(i)
				signal = fun(ww)
				resid = resid + (d2(i)-signal)**2
			end do
		! if weighting by errors -
		else if (use_error_chi2) then
			do i=1,nd2
				ww = w_d2(i)
				ll = l(i)
				signal = fun(ww)
				resid = resid + ((d2(i)-signal)/sigd2(i))**2
			end do
		endif
		
		func = resid
		
  end subroutine objfun

