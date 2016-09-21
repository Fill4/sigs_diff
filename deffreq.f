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
	if (afile(1:4).eq.'stop') then
		call cpu_time(finish)
		write(*,*) ' Time elapsed:   ', finish-start, 's'
		stop
	endif

	if (verbose) then
		write (*,*) " Reading frequencies from File: ", afile
		write (*,*) ' '
	end if

	return
end
