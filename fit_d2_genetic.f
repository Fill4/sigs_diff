!--------------------------------------------------------------------
  subroutine fit_d2_genetic (resd)
!	 this subroutine iterates until the relative variation of the residuals is 
!    smaller than TOLFIT. The final value of the parameters is in C and the 
!    residuals are returned in RESD

        use types_and_interfaces
        use commonvar
        use commonarray, only : nd2, w_d2, d2, c
        use lib_pikaia10
        use lib_array
        use gnufor2


		real(kind=8)                 :: resd

        real(dp)                    :: object, simp, stopcr
        real(dp), dimension(nconst) :: p, step, var

        real        :: ctrl(12), x(nconst), f
        integer     :: seed, status
        character(len=80) :: outfile
        
        
        real(dp), dimension(80) :: xx, resultfun
        real(dp)                :: min_xx, max_xx 

!        ! provide interface to the objective function
!        interface
!          function objfun_ga(n, p) result(fun_val)
!            implicit none
!            integer, intent(in)    :: n
!            real, intent(in)       :: p(:)
!            real                   :: fun_val
!          end function objfun_ga
!        end interface
!        
!        ! to the function that calculates the signal
!        interface 
!            real(kind=8) function fun (w_d2)
!	            implicit none
!	            real(kind=8), intent(in)  :: w_d2
!            end function fun
!        end interface 
!        
!        ! and to the parameter rescaling subroutine
!        interface
!          subroutine rescale(array_in, array_out)
!            implicit none
!            real, dimension(:), intent(in)      :: array_in
!            real(kind=8), dimension(:), intent(out) :: array_out
!          end subroutine rescale
!        end interface
        
        
        !     First, initialize the random-number generator
        seed=13579
        call rninit(seed)
        
        !     Set control variables
        ctrl(1:12) = -1
        ctrl(1) = 120
        ctrl(2) = 5000
        !ctrl(12) = -1
        outfile = 'param_file'
        
         
        !     Now call pikaia
        CALL pikaia(objfun_ga, nconst, ctrl, x, f, status, outfile)

        ! rescaling parameters
        call rescale(x, c)
        
        !     Print the results
        WRITE(*,*) ' status: ', STATUS
        WRITE(*,*) '      x: ', c
        WRITE(*,*) '  chi^2: ', 1./f
        WRITE(*,20) ctrl


        20 FORMAT(   '    ctrl: ', 6F11.6/ t11, 6F11.6)
        
        ! create array with smooth function		
		min_xx = minval(w_d2(1:nd2)) - 1.0d-4
		max_xx = maxval(w_d2(1:nd2)) + 1.0d-4
        call linspace(min_xx, max_xx, xx)
        do i=1,80
            resultfun(i) = fun(xx(i))
        end do

        write(*,*) 'calling plot'
        call plot(w_d2(1:nd2)*1.0d6, d2(1:nd2)*1.0d6, xx*1.0d6, resultfun*1.0d6, &
                  ' 5.00-',color2='dark-yellow',color1='#40e0d0')


		return
		
  end subroutine fit_d2_genetic
  
  
  function objfun_ga(n, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal is calculate in FUN for the current values of the parameters p, and
! subtracted from the second differences
        use types_and_interfaces, only: dp, fun, rescale
        use commonvar, only : use_error_chi2, nconst, pi
        use commonarray, only : nd2, w_d2, d2, c, l, sigd2
    
        implicit none

        integer, intent(in)     :: n     ! size of parameter space
        real, intent(in)        :: p(:)
        real                    :: fun_val
                
        integer     :: i, ll
        real(dp)    :: ww, signal, resid

        ! rescaling parameters
        call rescale(p, c)
        
		
		resid = 0.0d0
!		! if not weighting by errors -
!		if (use_error_chi2 == 'no' .or. use_error_chi2 == 'n') then
!			do i=1,nd2
!				ww = w_d2(i)
!				ll = l(i)
!				signal = fun(ww)
!				resid = resid + (d2(i)-signal)**2
!			end do

!        let's assume we always weight by errors to remove evaluation (31/01/2013)
		
		! if weighting by errors -
!		else if (use_error_chi2 == 'yes' .or. use_error_chi2 == 'y') then
			do i=1,nd2
				ww = w_d2(i)
				ll = l(i)
				signal = fun(ww)
				resid = resid + ((d2(i)-signal)/sigd2(i))**2
			end do
!		endif
		
		fun_val = sngl(1.0 / resid)
		
		return
		
  end function objfun_ga
  
  
  subroutine rescale(array_in, array_out)
        
        use types_and_interfaces, only: dp
        use commonvar, only: pi
        
        implicit none
        
        real, dimension(:), intent(in)      :: array_in
        real(dp), dimension(:), intent(out) :: array_out
        
        array_out(1) = dble(array_in(1)) * 2.0d-6
        array_out(2) = dble(array_in(2)) * 1.0d-12
        array_out(3) = dble(array_in(3)) * (5000. - 2000.) + 2000.
        array_out(4) = dble(array_in(4)) * 2. * pi
        array_out(5) = dble(array_in(5)) * 2.0d-3
        array_out(6) = dble(array_in(6)) * 1.0d8
        array_out(7) = dble(array_in(7)) * (2000. - 500.) + 500.
        array_out(8) = dble(array_in(8)) * 2. * pi
  
  
  end subroutine rescale

