!**********************************************************
  elemental real(dp) function smooth_comp (w_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi
        use commonarray, only: polyc, nd2

		implicit none
	
		real(dp), intent(in)  :: w_d2
        
		!smooth_comp = c(1) + c(9)/w_d2 + c(10)/(w_d2**2) + c(11)/(w_d2**3)
		smooth_comp = polyc(1) + polyc(2)*w_d2 + polyc(3)*w_d2**2 + polyc(4)*w_d2**3


  end function smooth_comp


!**********************************************************
  elemental real(dp) function he_comp (w_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq
        use commonarray, only: c, nd2

		implicit none
	
		real(dp), intent(in)  :: w_d2
        real(dp) :: xarg, bcz, heiiz
        
	  	! HeIIZ
	  	!   c(7) = tau_he
	  	!   c(8) = phi_he
	  	xarg = 2.0d0 * ( 2.d0*pi*c(6)*w_d2 + c(7) )
	  	heiiz = (c(4) * w_d2 * exp(-8.0_dp*pi_sq*(c(5)**2)*(w_d2**2))) * &
	  	        cos(xarg)
        
        he_comp = heiiz

  end function he_comp


!**********************************************************
  elemental real(dp) function bcz_comp (w_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, tau0_houdek, tau0_houdek_sq, nu0
        use commonarray, only: c, nd2

		implicit none
	
		real(dp), intent(in)  :: w_d2
		real(dp) :: xarg, factor, bcz, heiiz
    	real(dp) :: tau0
        
		! BCZ signal
		!   c(3) = tau_c
		!   c(4) = phi_c
		!   c(2) = A_c
		xarg = 2.0d0 * ( 2.d0*pi*c(2)*w_d2 + c(3) )
		factor = 1.0_dp / sqrt(1.0_dp + 0.0625_dp/(pi_sq*tau0_houdek_sq*w_d2*w_d2))
	  	bcz  = ( c(1) * (nu0**3) / (w_d2**2) ) * &
	  	       factor * &
	  	       cos(xarg + atan(4.0_dp*pi*tau0_houdek*w_d2))
        
        bcz_comp = bcz

  end function bcz_comp














