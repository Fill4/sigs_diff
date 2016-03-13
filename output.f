!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
  subroutine output (frequency_file, resd)
!	 write the results to the terminal

        use types_and_interfaces
		use commonvar
		use commonarray, only: c, n
		
		character(len=80), intent(in) :: frequency_file
		real(dp), intent(in)          :: resd
	
		real(dp)    :: tauBCZ, tauHe, a, a_bcz, a_he, beta
		real(dp)    :: chi2, chi2norm
		
		integer     :: nfile


		tauBCZ = c(2)
		tauHe = c(6)
		
		write(*,*) w0ref, fac
		
		a_bcz = c(1)*1.0d18 / (w0ref)**2  
		a_he = c(4) * w0ref * exp(-c(5)*(w0ref*1.0d-6)**2)
		beta = c(5) * (w0ref*fac)**2  
		
		a = tauHe / (sqrt(2.0_dp*pi)*w0ref*c(6))
		
		
		write (6,*) "  Frequencies from file: ", frequency_file
	
		write (6,1010) 'Results:', &
		                   'tau_BCZ = ', tauBCZ, &
		                   'phi_bcz = ', c(3), 'A_bcz = ', a_bcz, &
                           'tau_He = ', tauHe, &
                           'phi_he = ', c(7), 'A_he = ', a_he, beta, &
                           'A_II / sqrt(2pi)*nu0*Delta_II = ', a, &
                           'chi2 = ', resd, 'chi2norm = ', resd/(n-nconst)
                           
 1010	format (3x, a, //, &
                6x, a, f9.4, //, &
                6x, a, f9.2, 6x, a, f10.4, //, &
                6x, a, f9.4, //, &
                6x, a, f9.2, 6x, a, f10.4, 2x, f15.3, //, &
                6x, a, f15.8, //, &
                6x, a, f15.10, 5x, a, f15.10, /)

        
        ! output to "res" file -
        !nfile = length(frequency_file)
        if (intype.eq.0) then
            write (9,9003) &
               frequency_file, tauBCZ, c(3), a_bcz, tauHe, c(7), a_he, beta
     9003   format (x, a24, 7f10.4)
        endif
        close(9)

        return
        
  end subroutine output
