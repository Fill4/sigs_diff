!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine second_diff
! This subroutine calculates the second differences from the oscillation
! frequencies.

	use types_and_interfaces
	use commonvar
	use commonarray
	use lib_assert
	implicit none
	
	integer :: i, j
	real(dp), dimension(npt)	:: d2work, error

	d2work(1:npt) = 0.0
	error(1:npt) = 0.0

	if (verbose) write(*,1015) 'l', 'w', 'd2', 'sig'
 1015 format (a4, a12, a12, a9)
	! If frequencies are from sequential n's and are from the same l mode then calculate de 2nd difference
	do i=2,n-1
		if (l(i) == l(i) .and. l(i) == l(i+1) .and. xn(i)-xn(i-1) == 1 .and. xn(i)-xn(i-1) == 1) then
			d2work(i) = w(i-1) - 2.0*w(i) + w(i+1)
			if (use_error_chi2) then
				error(i) = sqrt(sig(i-1)**2 + 4.0*sig(i)**2 + sig(i+1)**2) !Propagate uncertainty
			end if
		end if
	end do

	! Remove zeros and wrong values.
	j = 1
	do i=1,n
		if (d2work(i) /= 0. .and. abs(d2work(i)) < 20) then
			d2(j) = d2work(i)
			w_d2(j) = w(i)/w0ref
			l_d2(j) = l(i)
			if (use_error_chi2) then
				sigd2(j) = error(i)
			end if
			if (verbose) then
				write(*,1010) l_d2(j), w_d2(j)*w0ref, d2(j), sigd2(j)
			endif
			j = j+1
		end if
	end do
 1010 format (i4, f12.4, f12.4, f9.4)

	! Number of second differences
	nd2 = j-1
	if (verbose) write (*,*) ' '
	if (verbose) write (6,'(2x, a, i3)') "# of second differences: ", nd2

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