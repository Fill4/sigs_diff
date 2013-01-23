!--------------------------------------------------------------------
	subroutine init (afile)
!	 this subroutine reads frequency data from file AFILE,
!	 and divides it in groups of modes with same degree "l"

		use commonvar
		! contains npt, n, l, sd, sig, xn, w -
		use commonarray
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		character(len=80), intent(inout)    :: afile
		parameter (nsteps=100)
		dimension np(nsteps)
		
		common /blockval09/nnp
		common /blockval10/np

		
		close (1)
		open (1,file=afile,status='old')
		call skpcom (1)

		wmin = 1.0d6
		wmax = 0.0d0
		
		! if not using errors -
 10		if (include_errors == 'no' .or. include_errors == 'n') then
			read (1,*,end=20) ll,nn,ww
		! if using errors -
		else if (include_errors == 'yes' .or. include_errors == 'y') then
			read (1,*,end=20) ll,nn,ww,ss
			if (ss.gt.ssmax) goto 10
		endif

		if (nn.lt.0) goto 10
		if (ll.gt.lmax.or.ll.lt.lmin) goto 10
		if (isel.eq.1) then
			if (nn.lt.nleft) goto 10
			if (nn.gt.nrigth) goto 10
		endif

		if (ww.gt.wmax) wmax = ww
		if (ww.lt.wmin) wmin = ww
		goto 10

 20		rewind (1)


		call skpcom (1)
		
		n=0
		if (isel.eq.1) then
			vleft = 0.0d0
			vrigth = 0.0d0
		endif

		dw = (wmax-wmin)
		wlower = wmin + dw*vleft
		wupper = wmax - dw*vrigth
		
		write (*,1013) 'Range in frequencies:', wlower, wupper
 1013	format (7x, a, f10.4, x, '-', x, f9.4)	
 
 		write (*,1014) 'Reference frequency :', w0
 1014	format (7x, a, f10.4)


		fw = (w0-wlower)/(wupper-wlower)
		if (fw.lt.0.1d0.or.fw.gt.0.9d0) then
			write (*,*) 'WARNING: Reference w is inadequate for data!'
		endif
		
		
 11		continue



		! if not using errors -
		if (include_errors == 'no' .or. include_errors == 'n') then
			read (1,*,end=21) l(n+1),nn,ww
		! if using errors -
		else if (include_errors == 'yes' .or. include_errors == 'y') then
			read (1,*,end=21) l(n+1),nn,ww,sig(n+1)
			if (sig(n+1).gt.ssmax) goto 11
		endif
		
		if (nn.lt.0) goto 11
		if (isel.eq.0) then
			if (ww.gt.wupper.or.ww.lt.wlower) goto 11
			else if (isel.eq.1) then
				if (nn.lt.nleft) goto 11
				if (nn.gt.nrigth) goto 11
			else
				write (*,*) 'WARNING: Wrong option for ISEL!'
			endif

		if (l(n+1).gt.lmax.or.l(n+1).lt.lmin) goto 11

		n = n+1
		w(n) = ww/w0
		xn(n) = dble(nn)
		goto 11

 21		close (1)


 39		nnp=1
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
						if (isig.gt.0) sig(n)=sig(j+nd)
					end do

					n=n-nd
					goto 39
				
				endif
			
			endif
		end do

		
		if ((n+1)-np(nnp).le.nlmin) then
			write (6,*) "WARNING: Too few points for l=", l(n)
			nnp=nnp-1
			n=n-1
		endif
		
		np(nnp+1)=n+1

		write (6,1015) "Points read: ", n
 1015	format (7x, a, i3)

        ! calculate second differences
        call second_diff
        
        return
	
	end subroutine init
