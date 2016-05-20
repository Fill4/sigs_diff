!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
program sig_bcz_d2
! This program isolates an oscillatory signal that is present in the second 
! differences of the oscillation frequencies and associated with regions where 
! the sound speed has a discontinuity.
!
! The general form of the signal is a known function of frequency that depends 
! on a set of parameters C. 
!
! The differences between the observed 2nd differences and the model ones is 
! minimized in the least squares sense, providing the best fit parameters

	use types_and_interfaces
	use commonvar
	use commonarray
	use lib_array
	implicit none

	integer				:: narg, iarg
	character (len=80)	:: afile, options_file, star_file
	character (len=20)	:: name
	real(dp)			:: chi2

	! Check if arguments are found
	narg=command_argument_count()

	if(narg>0)then
	! Loop across options
		do iarg=1,narg
			call get_command_argument(iarg,name)
			select case(adjustl(name))
			case('-h','--help')
				call help
			case('-v','--verbose')
				verbose = .TRUE.
			case('-p','--plots')
				show_plots = .TRUE.
			case('-f','-fgong')
				is_model = .TRUE.
			case default
				write(6,*)"Option ",adjustl(name),"unknown"
			end select
		end do
	end if

	afile='00000'
	options_file = 'options_file'
	if (verbose) write (6,*)"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"

	! Number of parameters to fit
1	nconst=7
	allocate(c(nconst))

	! Read options_file with input parameters
	call parameters(options_file)	

	! Initializing all quantities, read in frequencies and create output files
	call deffreq (afile) 	! Reads freqs_list file
	! Necessary change for fgong freq files
	if (is_model) then
		star_file = afile
		afile = trim(afile) // '.freq'
		call star_par (star_file)
	end if
	call init (afile) 		! Reads frequencias and calculates 2nd differences
	call openfiles			! Prepare output files

	call flush (6)

	! Removes the smooth component and finds the best parameters
	call fit_d2_genetic(chi2)
	! Output the results
	call output (afile, chi2)

	if (verbose) write (6,*)"---------------------> PROGRAM SIG_BCZ_D2 <---------------------"

	call flush (6)
	deallocate(c)

	goto 1

end program sig_bcz_d2
