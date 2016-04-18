!****************************************************************************
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!****************************************************************************
! Main calls sig_bcz_d2
program main 
	call sig_bcz_d2
end program main

subroutine sig_bcz_d2

	use types_and_interfaces
	use commonvar
	use commonarray
	use lib_array


	implicit none

	character (len=80)   :: afile
	character (len=1)    :: amess
	character (len=80)   :: options_file

	real(dp)                 :: chi2

	afile='00000'
	options_file = 'options_file'
	write (6,'(/, a, //)')"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"

	!--- Number of parameters to fit
 1	nconst=7
	allocate(c(nconst))

	!--- Read options_file with input parameters -
	call parameters(options_file)	

	!--- Initializing all quantities, read in frequencies and create output files -
	call deffreq (afile) ! Reads freqs_list file
	call init (afile) ! Reads frequencias and calculates 2nd differences
	call openfiles () ! Prepare output files

	call flush (6)

	!--- Find best parameters
	call fit_d2_genetic(chi2)
	!--- Output the results
	call output (afile, chi2)

	write (6,*)"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"
	call flush (6)
	deallocate(c)

	goto 1

end subroutine sig_bcz_d2
