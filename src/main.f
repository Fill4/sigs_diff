!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
! CHANGE
!----------------------------------------------------------------------------
program main
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
	implicit none

	integer				:: narg, iarg
	character (len=80)	:: afile, options_file, star_file
	character (len=20)	:: name

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
			case('-a','--auto')
				automatic = .TRUE.
			case default
				write(6,*)"Option ",adjustl(name),"unknown"
			end select
		end do
	end if

	call cpu_time(start)

	afile='00000'
	options_file = 'options_file'
	if (verbose) write (6,*)"---------------------> PROGRAM SIGS_DIFF <---------------------"

	! Number of parameters to fit
1	nconst=7
	allocate(c(nconst))

	! Read options_file with input parameters
	call read_inputs(options_file)	

	! Initializing all quantities, read in frequencies and create output files
	call deffreq (afile) 	! Reads freqs_list file
	call init (afile) 		! Reads frequencias and calculates 2nd differences
	call openfiles			! Prepare output files
	call flush (6)

	! Removes the smooth component and finds the best parameters
	if (automatic) then
		call automatic_interval
	endif
	call fit_d2_genetic

	! Output the results
	call output (afile)

	if (verbose) write (6,*)"---------------------> PROGRAM SIGS_DIFF <---------------------"
	call flush (9)
	call flush (6)
	deallocate(c)

	goto 1

end program main
