!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine init (afile)
! This subroutine reads frequency data from file AFILE, and divides it in
! groups of modes with same degree l.

	use types_and_interfaces, only: dp
	use commonvar
	use commonarray
	implicit none
	
	character(len=80), intent(inout) :: afile
	real(dp) :: dw, fw, ww, ss, wlower, wupper, wmax, wmin
	integer  :: ll, nd, nn
	integer  :: i, j, k

	close (1)
	open (1,file=afile,status='old')
	call skpcom (1)

	wmin = 1.0d6
	wmax = 0.0d0
	n=0

	! A first loop over the frequencies is performed to define the maximum
	! and minimum frequencies accepted from the data.
10	if (.NOT. use_error_chi2) then
		read (1,*,end=20) ll,nn,ww
	else if (use_error_chi2) then
		read (1,*,end=20) ll,nn,ww,ss
		if (ss.gt.ssmax) goto 10
	endif

	if (nn.gt.nmax.or.nn.lt.nmin) goto 10
	if (ll.gt.lmax.or.ll.lt.lmin) goto 10
	if (w0ref.eq.-1) then
		if (nn.eq.18 .and. ll.eq.0) then
			w0ref = ww
		endif
	endif
	if (ww.gt.wmax) wmax = ww
	if (ww.lt.wmin) wmin = ww
	goto 10

20	rewind (1)

	call skpcom (1)

	if (w0ref.eq.-1) then
		write(*,*) ' No w0ref defined and could not attribute frequency of n=18 to w0ref'
		write(*,*) ' Defining w0ref as 2500 nuHz. Consider defining a refence frequency in the options file'
	endif

	dw = (wmax-wmin)
	wlower = wmin + dw*vleft
	wupper = wmax - dw*vright
 
	if (verbose) then
		write (*,1013) 'Range in frequencies:', wlower, wupper
		write (*,1014) 'Reference frequency :', w0ref
		write(*,*) ' '
	endif
1013 format (2x, a, f10.4, x, '-', x, f9.4)
1014 format (2x, a, f10.4)

	! Check if reference frequency is adequate
	fw = (w0ref-wlower)/(wupper-wlower)
	if (fw.lt.0.1d0.or.fw.gt.0.9d0) then
		write (*,*) 'WARNING: Reference w is inadequate for data!'
		write (*,*) ' '
	endif

	if (verbose) write(*,1015) 'l', 'n', 'w', 'sig'
1015 format (a4, a5, a12, a9)

	! Redo cycle to read the frequencies and its data to the arrays
 11 continue

	if (.NOT. use_error_chi2) then
		read (1,*,end=21) ll,nn,ww
	else if (use_error_chi2) then
		read (1,*,end=21) ll,nn,ww,ss
		if (ss.gt.ssmax) goto 11
	endif

	if (ww.gt.wupper.or.ww.lt.wlower) goto 11
	if (nn.lt.nmin) goto 11
	if (nn.gt.nmax) goto 11
	if (ll.gt.lmax.or.ll.lt.lmin) goto 11

	n = n+1
	l(n) = ll
	sig(n) = ss
	w(n) = ww
	xn(n) = dble(nn)

	if (verbose) write(*,1016) l(n), xn(n), w(n), sig(n)
 1016 format (i3, i4, f12.4, f9.4)
	
	goto 11

 21 close (1)

	! Cycle through every l to make sure they have more than nlmin lines
 39 nnp=1
	np(1)=1
	do i=2,n
		if (l(i).ne.l(i-1)) then
			np(nnp+1)=i
			nnp=nnp+1
			if (np(nnp)-np(nnp-1).le.nlmin) then
				write (6,*) "WARNING: Too few points for l=", l(i-1)
				nd=np(nnp)-np(nnp-1)
				do j=np(nnp-1),n-nd
					l(j)=l(j+nd)
					w(j)=w(j+nd)
					xn(j)=xn(j+nd)
					if (use_error_chi2) sig(j)=sig(j+nd)
				end do
				n=n-nd
				goto 39
			endif
		endif
	end do

	! Check if last l has more than nlmin points
	if ((n+1)-np(nnp) .lt. nlmin) then
		write (6,*) "WARNING: Too few points for l=", l(n)
		n=np(nnp)-1
		nnp=nnp-1
	endif

	! Final check. If all l's had fewer lines than nlmin then the program cannot run.
	if (n .lt. nlmin) then
		write (6,*) "Not enough points to run program. Stopping"
		stop
	endif
	! Add final value to np to later iterate through all l's again
	np(nnp+1) = n+1

	if (verbose) write(*,*) ' '
	if (verbose) write (6,1017) "Frequencies read: ", n
1017 format (2x, a, i3)
	if (verbose) write(*,*) ' '

	! Call second_diff to calcualte the second differences from the read frequencies
	call second_diff

	return

end subroutine init
