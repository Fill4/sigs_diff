!--------------------------------------------------------------------
!  Mario Monteiro: May 2000
!  File last changed:
!--------------------------------------------------------------------
	subroutine num_to_text (num,aword,jn)
!	 this subroutine converts an integer number NUM into a character
!	 string AWORD which has JN characters.

		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		character aword*80
		character(len=10)  :: abase
		parameter (na=40)
		dimension lnum(na)

		abase = '0123456789'

		jn=1
		nval=0
	
		do j=1,na
			nexp = na-j
			lnum(jn) = int((num-nval)/10.0d0**nexp)
			if (lnum(jn).eq.0 .and. jn.eq.1) cycle
			nval = nval+int(10.0d0**nexp*lnum(jn))
			jn = jn+1
		enddo

		jn=jn-1

		do i=1,jn
			np = lnum(i)+1
			aword(i:i) = abase(np:np)
		enddo

		do i=jn+1,na
			aword(i:i)=' '
		enddo

		return
	
	end subroutine num_to_text
