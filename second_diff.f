!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
   subroutine second_diff
   
    use commonvar, only: w0     ! reference frequency
    use commonarray, only: npt, w, n, l, d2, w_d2, nd2
    
    use gnufor2, only: plot
    
    implicit none
    
    integer :: i, j
    real, dimension(npt)   :: d2work
    
    d2work = 0.
    
    do i=2,n
        if (l(i-1) == l(i) .and. l(i) == l(i+1)) then
            d2work(i) = (w(i-1) - 2.0*w(i) + w(i+1)) * w0
        end if
    end do
    
    ! remove zeros and wrong values
    j = 1
    do i=1,n
        if (d2work(i) /= 0. .and. d2work(i) < 10. ) then
            d2(j) = d2work(i)
            w_d2(j) = w(i)*w0
            j = j+1
        end if
    end do
    
        
    write(*,'(f10.4, i3, f10.4, /)') (w(i)*w0, l(i), d2work(i), i=1,n)
    write(*,'(f10.4, f10.4, /)') (d2(i), w_d2(i), i=1,j-1)
    
    write(*,*) j-1
    nd2 = j-1
    
    !call plot(dble(w_d2(1:j-1)), dble(d2(1:j-1)), ' 5.',terminal='jpeg')
   
   end subroutine second_diff
   
   
   
   
   
