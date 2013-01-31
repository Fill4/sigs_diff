!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
	subroutine deffreq (afile)
!	 Define the reference values of the parameters C
        
        use types_and_interfaces
		use commonvar
		use commonarray, only: c
		
		
		implicit none
		
		character(len=80), intent(inout)    :: afile
		character(len=80)    :: afile0	

		

		w0 = w0ref
		tau0 = tau0ref
	
	
		if (afile(1:5).eq.'00000') then
			afile0='freqs'
		else
			afile0='stop'
		endif
		
		write(*,*) ' '
		
		if (use_error_chi2 == 'yes' .or. use_error_chi2 == 'y') then
			write (*,'(2x, a)', advance = "no") "name of input file (l,n,v,sigma) --> "
		else if (use_error_chi2 == 'no' .or. use_error_chi2 == 'n') then
			write (*,'(2x, a)', advance = "no") "name of input file (l,n,v) --> "
		endif

		read (*,'(a80)') afile

		write (*,*) ' '

		if (afile(1:1).eq.' ') afile = afile0
		if (afile(1:4).eq.'stop') stop

		write (*,*) "  Reading frequencies from File: ", afile

		return
	end
