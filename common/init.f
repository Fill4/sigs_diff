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
	integer  :: i, j

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
		if (verbose) print *, ll, nn, ww
	else if (use_error_chi2) then
		read (1,*,end=20) ll,nn,ww,ss
		if (verbose) print *, ll, nn, ww, ss
		if (ss.gt.ssmax) goto 10
	endif

	if (nn.lt.6) goto 10
	if (ll.gt.lmax.or.ll.lt.lmin) goto 10
	if (isel.eq.1) then
		if (nn.lt.nleft) goto 10
		if (nn.gt.nright) goto 10
	endif

	if (ww.gt.wmax) wmax = ww
	if (ww.lt.wmin) wmin = ww
	goto 10

20	rewind (1)

	call skpcom (1)

	dw = (wmax-wmin)
	wlower = wmin + dw*vleft
	wupper = wmax - dw*vright

	if (verbose) write (*,1013) 'Range in frequencies:', wlower, wupper
1013 format (7x, a, f10.4, x, '-', x, f9.4)
 
	if (verbose) write (*,1014) 'Reference frequency :', w0ref
1014 format (7x, a, f10.4)

	! Check if reference frequency is adequate
	fw = (w0ref-wlower)/(wupper-wlower)
	if (fw.lt.0.1d0.or.fw.gt.0.9d0) then
		write (*,*) 'WARNING: Reference w is inadequate for data!'
	endif

	! Redo cycle to read the frequencies and its data to the arrays
11	continue

	if (.NOT. use_error_chi2) then
		read (1,*,end=21) l(n+1),nn,ww
	else if (use_error_chi2) then
		read (1,*,end=21) l(n+1),nn,ww,sig(n+1)
		if (sig(n+1).gt.ssmax) goto 11
	endif

	if (nn.lt.0) goto 11
	if (isel.eq.0) then
		if (ww.gt.wupper.or.ww.lt.wlower) goto 11
		else if (isel.eq.1) then
			if (nn.lt.nleft) goto 11
			if (nn.gt.nright) goto 11
		else
			write (*,*) 'WARNING: Wrong option for ISEL!'
		endif

	if (l(n+1).gt.lmax.or.l(n+1).lt.lmin) goto 11

	n = n+1
	w(n) = ww/w0ref
	xn(n) = dble(nn)
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

	write (6,1015) "Frequencies read: ", n
1015 format (7x, a, i3)

	! Call second_diff to calcualte the second differences from the read frequencies
	call second_diff

	return

end subroutine init
