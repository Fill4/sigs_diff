!****************************************************************************
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!****************************************************************************
subroutine second_diff
   
	use types_and_interfaces
	use commonvar
	use commonarray

	use lib_assert
	
	implicit none
	
	integer :: i, j
	real(dp), dimension(npt)	:: d2work, error

	d2work = 0.
	error = 0.

	! If frequencies and from sequential n's and are from the same l mode then calculate de 2nd difference
	do i=2,n
		if (l(i-1) == l(i) .and. l(i) == l(i+1) .and. xn(i)-xn(i-1) == 1 .and. xn(i)-xn(i-1) == 1) then
			d2work(i) = w(i-1) - 2.0*w(i) + w(i+1)    ! in Hz
			if (use_error_chi2) then
				error(i) = sqrt(sig(i-1)**2 + 4.0*sig(i)**2 + sig(i+1)**2) ! propagate uncertainty
			end if
		end if
	end do

	! Remove zeros and wrong values
	j = 1
	do i=1,n
		! Trying new method without transforming units - Filipe
		!if (d2work(i) /= 0. .and. abs(d2work(i)) < 15.d-6 ) then
		if (d2work(i) /= 0. .and. abs(d2work(i)) < 10) then
			d2(j) = d2work(i)   ! in Hz
			w_d2(j) = w(i)  ! in Hz
			l_d2(j) = l(i)
			if (use_error_chi2) then
				sigd2(j) = error(i) ! in Hz
			end if
			
			j = j+1
		end if
	end do

	! Number of second differences
	nd2 = j-1
	write (6,'(7x, a, i3)') "# of second differences: ", nd2

	! Write second differences to file
	if (use_error_chi2) then
		open(unit=33, file='d2.data', action='write')
		write(33,*) "#  nu (muHz)", "d2 (muHz)", "err_d2 (muHz)"
		write(33,'(f18.8, f18.8, f18.8)') (w_d2(i)*w0ref, d2(i)*w0ref, sigd2(i), i=1,nd2)
		close(33)
	else
		open(unit=33, file='d2.data', action='write')
		write(33,*) "#  nu (muHz)", "d2 (muHz)"
		write(33,'(f18.8, f18.8)') (w_d2(i)*w0ref, d2(i)*w0ref, i=1,nd2)
		close(33)
	end if

end subroutine second_diff