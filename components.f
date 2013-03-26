!**********************************************************
  elemental real(dp) function smooth_comp (nu_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi
        use commonarray, only: polyc

		implicit none
	
		real(dp), intent(in)  :: nu_d2
        
		smooth_comp = polyc(1) !+ polyc(2)*nu_d2 + polyc(3)*(nu_d2**2) + polyc(4)*(nu_d2**3)

  end function smooth_comp


!**********************************************************
  elemental real(dp) function he_comp (nu_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq
        use commonarray, only: c, nd2

		implicit none
	
		real(dp), intent(in)  :: nu_d2
        real(dp) :: xarg
        
	  	! HeIIZ
	  	!   c(4) = A_II (c0 from M12)
	  	!   c(5) = c2 from M12
	  	!   c(6) = tau_he
	  	!   c(7) = phi_he
	  	
	  	xarg = 4.0_dp*pi*c(6)*nu_d2 + 2.0_dp*c(7)
	  	he_comp = (c(4) * nu_d2 * exp(-c(5) * nu_d2**2)) * sin(xarg)

  end function he_comp


!**********************************************************
  elemental real(dp) function bcz_comp (nu_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, tau0_houdek, tau0_houdek_sq, nu0
        use commonarray, only: c, nd2

		implicit none
	
		real(dp), intent(in)  :: nu_d2
		real(dp) :: xarg
        
		! BCZ signal
		!   c(1) = A_c (b2 from M12)
		!   c(2) = tau_c (tau_bcz from M12)
		!   c(3) = phi_c (phi_bcz from M12)
		
		xarg = 4.0d0*pi*c(2)*nu_d2 + 2.0_dp*c(3)
	  	bcz_comp  = ( c(1) / (nu_d2**2) ) * sin(xarg)

  end function bcz_comp














