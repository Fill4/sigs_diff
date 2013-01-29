!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
   subroutine second_diff
   
    use types
    use commonvar, only: w0, write_d2_to_file
    use commonarray, only: npt, w, sig, n, l, d2, w_d2, sigd2, nd2
    
    use gnufor2, only: plot
    
    implicit none
    
    integer :: i, j
    real(dp), dimension(npt)   :: d2work, error
    
    d2work = 0.
    error = 0.
    w = w*1.0d-6     ! convert frequencies to Hz
    sig = sig*1.0d-6 ! and errors too
        
    do i=2,n
        if (l(i-1) == l(i) .and. l(i) == l(i+1)) then
            d2work(i) = w(i-1) - 2.0*w(i) + w(i+1)    ! in Hz
            error(i) = dsqrt(sig(i-1)**2 + 4.0*sig(i)**2 + sig(i+1)**2) ! propagate uncertainty
        end if
    end do
    
    ! remove zeros and wrong values
    j = 1
    do i=1,n
        if (d2work(i) /= 0. .and. d2work(i) < 10. ) then
            d2(j) = d2work(i)   ! in Hz
            w_d2(j) = w(i)  ! in Hz
            sigd2(j) = error(i) ! in Hz
            j = j+1
        end if
    end do
    
    ! print in muHz unscaled 
    if (write_d2_to_file) then
        ! open the file -
		open(unit=33, file='d2.data', action='write')
		write(33,'(f18.8, f18.8, f18.8, /)') (w_d2(i)*1.0d6, d2(i)*1.0d6, sigd2(i)*1.0d6, i=1,j-1)
    endif
    
    ! number of second differences
    nd2 = j-1
	write (6,'(7x, a, i3)') "# of second differences: ", nd2
    
    
    !call plot(dble(w_d2(1:j-1)), dble(d2(1:j-1)), ' 5.')
   
   end subroutine second_diff
   
   
   
   
   
