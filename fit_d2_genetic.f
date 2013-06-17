!--------------------------------------------------------------------
  subroutine fit_d2_genetic (chi2)
!	 this subroutine iterates until the relative variation of the residuals is 
!    smaller than TOLFIT. The final value of the parameters is in C and the 
!    residuals are returned in chi2

    use types_and_interfaces
    use commonvar
    use commonarray, only : nd2, w_d2, d2, sigd2, l_d2, c, polyc, weight
    use lib_pikaia12
    use lib_regression, only: polyreg
    use lib_statistics, only: mean, median
    use lib_array
    use lib_plot
    use lib_io

    implicit none
        
	real(dp), intent(inout)     :: chi2

    real(dp)                    :: object, simp, stopcr
    real(dp), dimension(nconst) :: c0, p, step, var

    real        :: ctrl(12), x(nconst), f
    integer     :: seed, status
    integer     :: degree, new_unit, i
    
    character(len=80) :: outfile, filename
    
    
    real(dp), dimension(150) :: xx, result_fun, result_smooth, result_he, result_bcz
    real(dp)                :: min_xx, max_xx 

        
    !     The polynomial fit is done before the signal fit 
    ! if fitting a higher degree polynomial, uncomment following two lines
!    degree = 2
!    call polyreg(1.0_dp/w_d2(1:nd2), d2(1:nd2), degree, polyc)
    
    ! if just fitting a constant we can calculate the mean
!    polyc(1) = mean(d2(1:nd2))
    ! or, in a more robust way, consider instead the median 
    polyc(1) = median(d2(1:nd2))
    
    write(*,*) 'Polynomial fit: '
    write(*,'(16x, es10.2)') polyc(1)
!        write(*,'(16x, es10.2, a)') polyc(2), ' x^-1'
!        write(*,'(16x, es10.2, a)') polyc(3), ' x^-2'
!        write(*,'(16x, es10.2, a)') polyc(4), ' x^-3'
       
    
    !     First, initialize the random-number generator
    seed=13568
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
    
    do iterIRLS=1,15
        CALL pikaia(objfun_ga, nconst, ctrl, x, f, status)  ! if using PIKAIA 1.2
        write(*,*) weight(1:nd2)*1.0d-12
        write(*,*)
        call rescale(x, c)
        WRITE(*,*) '      x: ', c
        WRITE(*,*) '  chi^2: ', 1./f
        write(*,*) all(abs(c0-c) < 0.2_dp * c)
        if (all(abs(c0(1:2)-c(1:2)) < 0.2_dp * c(1:2)) .and. all(abs(c0(4:6)-c(4:6)) < 0.2_dp * c(4:6))) exit
        c0 = c
        write(*,*)
    end do
        
    ! rescaling parameters
    call rescale(x, c)
    
    ! residuals of best parameters
    chi2 = 1.0/objfun_ga(nconst, x)
    
!    !     Print the results
!    WRITE(*,*) ' status: ', STATUS
!    WRITE(*,*) '      x: ', c
!    WRITE(*,*) '  chi^2: ', 1./f

    
    ! create array with smooth function		
	min_xx = minval(w_d2(1:nd2)) - 2.0d-4
	max_xx = maxval(w_d2(1:nd2)) + 2.0d-4
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

!!    call hist( abs((d2(1:nd2)-fun(w_d2(1:nd2)))/sigd2(1:nd2)), 15, &
!!               color='#779944',pause=-1.0)


    ! output signal to file -
    if (write_final) then
        write(filename, '("signal_",i4,"-",i0.4,".dat")') int(c(2)), int(c(6))
        write(*,*) filename
        new_unit = next_unit()
        open (new_unit, file=filename, status='unknown')
        write(new_unit,'(a)') '# observed second differences'
        write(new_unit,'(a)') '# N'
        write(new_unit,'(a, x, a, 3a12)') '#', 'l', 'nu(muHz)', 'D2nu(muHz)', 'err(muHz)'
        write(new_unit,'(i3)') nd2
        write(new_unit,'(i3, 3f12.4)') (l_d2(i), w_d2(i)*1.0d6, d2(i)*1.0d6, sigd2(i)*1.0d6, i=1,nd2)
        write(new_unit,'(a)') '# fitted signals'
        write(new_unit,'(a, x, 4a12)') '#', 'nu(muHz)', 'bcz', 'he', 'sum'
        write(new_unit,'(2x, 4f12.5)') (xx(i)*1.0d6, result_bcz(i)*1.0d6, &
                                        result_he(i)*1.0d6, result_fun(i)*1.0d6, i=1,150)
        close(new_unit)
    endif
     

	return
		
  end subroutine fit_d2_genetic
  
  
  function objfun_ga(n, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal is calculated in FUN for the current values of the parameters p, 
! and subtracted from the second differences
        use types_and_interfaces, only: dp, fun, rescale
        use commonvar, only : nconst, pi, iterIRLS
        use commonarray, only : nd2, w_d2, d2, c, l, sigd2, icov, weight
    
        implicit none

        integer, intent(in)     :: n     ! size of parameter space
        real, intent(in)        :: p(:)
        real                    :: fun_val

!        real(dp)                    :: signal
        real(dp), dimension(nd2)    :: ww, signal, weight0
        real(dp)                    :: resid(nd2), sr, resid_vector(nd2,1), chi2(1,1)
        integer, parameter :: Q = 4

        ! rescaling parameters
        call rescale(p, c)

        ww = w_d2(1:nd2)
		signal = fun(ww)
		weight0 = 1.0_dp / sigd2(1:nd2)**2
		resid = (d2(1:nd2)-signal)**2 * weight0
		
		if (iterIRLS>1) then
		    weight(1:nd2) = Q / ( sigd2(1:nd2)**2 * (resid + Q) )
		else 
            weight(1:nd2) = weight0
		end if 
		
		resid = (d2(1:nd2)-signal)**2 * weight(1:nd2)
        sr = sum(resid)
		fun_val = sngl(1.0 / sr)
		
!		resid_vector(:,1) = d2(1:nd2)-signal
!		chi2 = matmul( & 
!		         matmul(transpose(resid_vector), icov), &
!		               resid_vector)
!		fun_val = sngl(1.0 / chi2(1,1))
		                              
		!write(*,*) resid, chi2
		
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

        
        ! bcz
        array_out(1) = dble(array_in(1)) * 5.0d-12
        array_out(2) = dble(array_in(2)) * (4000._dp - 1900._dp) + 1900._dp
        array_out(3) = dble(array_in(3)) * pi
        
        ! heII
        array_out(4) = dble(array_in(4)) * 20.0d-3
        array_out(5) = dble(array_in(5)) * 1.0d8
        array_out(6) = dble(array_in(6)) * (1800._dp - 500._dp) + 500._dp
        array_out(7) = dble(array_in(7)) * pi
  
  end subroutine rescale
  
