<<<<<<< HEAD
!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
    subroutine deffreq (afile)
!     Define the reference values of the parameters C
        
        use types_and_interfaces
        use commonvar
       
        implicit none
        
        character(len=80), intent(inout)    :: afile
        character(len=80)    :: afile0    

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
=======
!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine deffreq (afile)
! This subroutine reads all the frequencies files on afile.
	
	use commonvar
	implicit none
	
	character(len=80), intent(inout)	:: afile
	character(len=80)					:: afile0

	if (afile(1:5).eq.'00000') then
		afile0='freqs'
	else
		afile0='stop'
	endif
	
	if (use_error_chi2) then
		if (verbose) write (*,'(2x, a)', advance = "no") "Name of input file (l,n,v,sigma) --> "
	else if (.NOT. use_error_chi2) then
		if (verbose) write (*,'(2x, a)', advance = "no") "Name of input file (l,n,v) --> "
	endif

	read (*,'(a80)') afile

	if (verbose) write (*,*) ' '

	if (afile(1:1).eq.' ') afile = afile0
	if (afile(1:4).eq.'stop') stop

	if (verbose) then
		write (*,*) " Reading frequencies from File: ", afile
		write (*,*) ' '
	end if

	return
end
>>>>>>> Automatic_Approach
