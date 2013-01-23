!--------------------------------------------------------------------
!	joao faria: 20/08/2012 
!		remove INTYPE as an argument, it is contained in the module
!		COMMONVAR
!--------------------------------------------------------------------
	subroutine openfiles (afile,xinit)
!	 open the files necessary for the OUTPUT

	use commonvar
	
	implicit double precision (b-h,o-z)
	implicit integer (i-n)
	
	character(len=80)   :: afile
	integer             :: nunit
	logical             :: res_exists

	data iwrite/0/


	! RES file (unit = 9) -
	!inquire( file="/home/joao/Documents/FCUP/TESE/bcz_fit/res", exist=res_exists)
	
	if (iwrite.eq.0) then
		iwrite=1
		open (9, file='res', status='unknown')
		write (9,*) ' '
		close (9)
		open (9,file='res',status='old')
		! write to terminal that RES was created -	
		write (6,*) "  File RES   [Model,C1,C2,...] (all final values)"

		! header -
		write (9,9001) "# SIG_BCZ results (", nconst, "parameters)"
 9001	format (x, a, i1, x, a, /) 
           
     	write (9,9002) "# Frequency data from: ", afile
 9002	format (x, a, x, a20, /)


!           write (9,1202) xinit,ftol,tolfit,dc,iterinit,iterfit
! 1202      format ('# Calculation: XINIT,FTOL,TOLFIT; DC,ITERINIT,',
!     *             'ITERFIT',/,
!     *             '#     ',d8.2,2d10.2,/,
!     *             '#     ',f5.3,2x,i1,2x,i1)
     
!           xl0=(-1.0d0+sqrt(1.0d0+4.0d0*xl02))/2.0d0
!           write (9,1206) w0ref,xl0,xamp0ref,tau0ref,phi0ref,intype
! 1206      format ('# Parameters: W0REF,XL0; XAMP0REF,TAUD0REF,',
!     *             'PHI0REF; INTYPE; (XMASS),(XRAD)',/
!     *             '#     ',f6.1,2x,f4.1,/,
!     *             '#     ',f4.2,2x,f6.1,2x,f4.2/,
!     *             '#     ',i1)

!	   if (intype.eq.0) then
!	      write (9,1205) xmass,xrad
! 1205	      format ('#     ',f6.4,2x,f6.4)
!	   endif
!           write (9,1210) iprint
! 1210      format ('# Output: IPRINT',/,
!     *             '#     ',i1)
!           if (isel.eq.0) then
!              write (9,1212) lmin,lmax,nlmin,isel,vleft,vrigth,
!     *                       isig,ssmax
! 1212         format ('# Data: LMIN,LMAX,NLMIN,ISEL; VLEFT,VRIGTH; ',
!     *                'ISIG,SSMAX',/,
!     *                '#     ',i1,2x,i1,2x,i1,2x,i1,/,
!     *                '#     ',f3.1,2x,f3.1,/,
!     *                '#     ',i1,2x,f5.3)
!           else if (isel.eq.1) then
!              write (9,1213) lmin,lmax,nlmin,isel,nleft,nrigth,
!     *                       isig,ssmax
! 1213         format ('# Data: LMIN,LMAX,NLMIN,ISEL; NLEFT,NRIGTH; ',
!     *                'ISIG,SSMAX',/,
!     *                '#     ',i1,2x,i1,2x,i1,2x,i1,/,
!     *                '#     ',i2,2x,i2,/,
!     *                '#     ',i1,2x,f5.3)
!           else
!              write (*,*) 'ERROR: Option ISEL not valid!'
!              stop
!           endif


		if (intype.eq.0) then
              write (9,*) "# Mod  Tau_d       Phi      A_d"
              write (9,*) "#------------------------------------"
              
     
           else if (intype.eq.1) then
              write (9,1065)
 1065         format('#',/,'# Mod  Tau_d       Phi  ',&
                  '    A_d           cldovs',/,&
                  '#----------------------------------------',&
                  '-------------------------')
           else if (intype.eq.2) then
              write (9,1066)
 1066         format('#',/,'# M   Tau_d     Phi  ',&
                  '    A_d      mass   w_ref   fw',/,&
                  '#--------------------------------------------',&
                  '-------------------------')
		else
              write (*,*) 'WARNING: No output to COF!'
           endif
	endif

	! COF file (unit = 3) -
	if (iprint.ge.1 .and. iprint.le.4) then
		close (3)
		open (3, file='cof', status='unknown')
		write (3,*) ' '
		close (3)
		open (3, file='cof', status='unknown')
		! write to terminal that COF was created -	
		write (6,*) "  File COF   [C1,C2,...] (each iteration)"
		
		write (3,*) "# tau_d     phi        A_d   "
		write (3,*) '#-----------------------------------'
		call flush (3)
	endif
	

	! SIG file (unit = 10) -
	if (iprint.ge.3.and.iprint.le.4) then
		close (10)
		open (10, file='sig', status='unknown')
		write (10,*) ' '
		close (10)
		open (10, file='sig', status='unknown')
		! write to terminal that SIG was created -	
		write (6,*) "  File SIG   [v, v-smooth, l, n, sigma, fit] "
		
		write (10,*) "# Final signal isolated by SIG_BCZ!"
		write (10,'(x,a,a20)') "# in frequencies from: ", afile
	endif


	! QFT file (unit = 7) -
	if (iprint.eq.5) then
		close (7)
		open (7, file='qft', status='unknown')
		write (7,*) ' '
		close (7)
		open (7, file='qft', status='unknown')
		! write to terminal that QFT was created -	
		write (6,*) "  File QFT   [v, v-smooth, l, n, sigma, fit] (1st iteration only) "
		
		write (7,*) "# Signal isolated in the 1st iteration"
		write (7,'(x,a,a20)') "# in frequencies from: ", afile
	endif


	return
	
	end subroutine openfiles
