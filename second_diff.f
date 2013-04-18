!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
   subroutine second_diff
   
    use types_and_interfaces
    use commonvar, only: w0, write_d2_to_file
    use commonarray, only: npt, w, sig, n, l, l_d2, d2, w_d2, sigd2, nd2, icov, &
                           nd2_l0, nd2_l1, nd2_l2, nd2_l3
    
    use gnufor2, only: plot
    use lib_assert
    
    implicit none
    
    integer :: i, j
    real(dp), dimension(npt)   :: d2work, error
    real(dp), dimension(npt)   :: ww, sigw  ! work arrays so don't change w & sig

    
    d2work = 0.
    error = 0.
    
    nd2_l0 = 0
    nd2_l1 = 0
    nd2_l2 = 0
    nd2_l3 = 0
    
    ww = w*1.0d-6     ! convert frequencies to Hz
    sigw = sig*1.0d-6 ! and errors too
        
    do i=2,n
        if (l(i-1) == l(i) .and. l(i) == l(i+1)) then
            d2work(i) = ww(i-1) - 2.0*ww(i) + ww(i+1)    ! in Hz
            error(i) = sqrt(sigw(i-1)**2 + 4.0*sigw(i)**2 + sigw(i+1)**2) ! propagate uncertainty
        end if
    end do
    
    ! remove zeros and wrong values
    j = 1
    do i=1,n
        if (d2work(i) /= 0. .and. abs(d2work(i)) < 15.d-6 ) then
            d2(j) = d2work(i)   ! in Hz
            w_d2(j) = ww(i)  ! in Hz
            sigd2(j) = error(i) ! in Hz
            l_d2(j) = l(i)
            if(l(i)==0) nd2_l0 = nd2_l0 + 1
            if(l(i)==1) nd2_l1 = nd2_l1 + 1
            if(l(i)==2) nd2_l2 = nd2_l2 + 1
            if(l(i)==3) nd2_l3 = nd2_l3 + 1
            
            j = j+1
        end if
    end do
    
    !write(*,*) nd2_l0, nd2_l1, nd2_l2, nd2_l3
    
    ! print in muHz
    if (write_d2_to_file) then
        ! open the file -
		open(unit=33, file='d2.data', action='write')
		write(33,*) "#  nu (muHz)", "d2 (muHz)", "err_d2 (muHz)"
		write(33,'(f18.8, f18.8, f18.8)') (w_d2(i)*1.0d6, d2(i)*1.0d6, sigd2(i)*1.0d6, i=1,j-1)
		close(33)
    endif
    
    ! number of second differences
    nd2 = j-1
	write (6,'(7x, a, i3)') "# of second differences: ", nd2
    
    ! calculate 2nd differences' covariance matrix
    allocate(icov(nd2, nd2))
    call error_covariance(sigw(1:n), icov)
    
    call assert(size(icov, dim=1) == size(icov, dim=2), 'Covariance matrix is not square')
    call assert(size(icov, dim=1) == nd2, 'Covariance matrix has wrong dimensions')

   
   end subroutine second_diff
   
   
   
   
   
