!--------------------------------------------------------------------
	subroutine writeout (iout,c)
!	 this subroutine writes out to the output files

		use commonvar, only : nconst,fac,w0
		! contains npt, n, l, sd, sig, xn, w -
		use commonarray
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		integer, parameter       :: ncp = 20
		dimension c(ncp)

		
		double precision   :: ww
		integer            :: ll
		double precision, dimension(ncp,npt) :: temp



		! writing the final signal to SIG -
		if (iout.eq.1) then
		
			write (10,*) "#"
			write (10,*) "# with final parameters:"
			write (10,'( x, a, x, f7.2 )') "#     taud =", c(1)/(w0*fac)
			write (10,'( x, a, x, f7.5 )') "#     phi  =", c(2)
			write (10,'( x, a, x, f7.5 )') "#     Ad   =", c(3)
			write (10,*) "#"
			write (10,*) "#   v        v-vs   l  n   sig     fit"
			write (10,*) "#------------------------------------------"


			do i=1,n
				ww = w(i)
				ll = l(i)
				temp(1,i) = w0*ww
				temp(2,i) = sd(i)
				temp(3,i) = fun(c,ww,ll)
				temp(4,i) = ll
				temp(5,i) = xn(i)
				temp(6,i) = sig(i)
			enddo

 
!	   call ord (temp,5,n,1)
!	   call order (1,temp,5,n,ncp,npt)

			do i=1,n
				write (10,'(f9.3,f10.6,2i3,f7.3,f10.6)') , &
                              (temp(j,i),j=1,2),l(i),int(temp(5,i)), &
                              temp(6,i),temp(3,i)		
     
     		end do	


 1310	   format (f9.3,f10.6,2i3,f7.3,f10.6)
			close (10)
		endif
		
		
		
		! writing the parameters of this iteration to COF file -
		if (iout.eq.2) then
			write (3,'(f8.2,3x,f8.5,3x,f8.5)'), &
      			    c(1)/(w0*fac),(c(i),i=2,nconst)

			call flush (3)
		endif
		
		! writing the signal for this iteration to QFT file -
		if (iout.eq.3) then
			write (7,*) "#"
			write (7,*) "#   v        v-vs   l  n   sig     fit"
			write (7,*) "#------------------------------------------"
			
			do i=1,n
				ww=w(i)
				ll=l(i)
				write (7,1310) w0*ww,sd(i),ll,int(xn(i)),sig(i),fun(c,ww,ll)
			enddo


			close (7)
		endif
		
	return
	
	end subroutine writeout
