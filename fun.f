!**********************************************************
  elemental real(dp) function fun (w_d2)
!	 this is the function to be fitted.

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, tau0_houdek, tau0_houdek_sq, nu0, w0
        use commonarray, only: c, polyc

		implicit none
	
		real(dp), intent(in)  :: w_d2

		real(dp) :: xarg, factor, bcz, heiiz
		real(dp) :: a1, a2, b1, b2, alpha1, alpha2, delta, d, dd
		real(dp) :: poly
		
		
		! w_d2 is (cyclic) frequency nu in Hz 
		
		
		! eqs (C8) and (C9) of Houdek 2007 *************************************
		d = c(5)**2
		dd = d**2
		! HeII
		alpha1 = 2.0_dp * w0 * c(6)
		a1 = - 4.0_dp * d * w_d2 * w0 + w0/w_d2
		b1 = 8.0_dp * dd * w0**2 * w_d2**2 - 6.0_dp * d * w0**3
		
		! eq (C12) of Houdek 2007 **********************************************
		alpha2 = 2.0_dp * w0 * c(2)
        a2 = - 2.0_dp * (w0 / w_d2) * &
             (1.0_dp + 0.125_dp/(tau0_houdek_sq * w_d2**2)) / &
             (1.0_dp + 0.25_dp/(tau0_houdek_sq * w_d2**2))
        b2 = (w0 / w_d2)**2 * &
             ( (3.0_dp + 0.625_dp/(tau0_houdek_sq * w_d2**2)) / &
               (1.0_dp + 0.25_dp/(tau0_houdek_sq * w_d2**2))**2 &
             )
		
		! third degree polynomial in nu^-1
		!   polyc
!		poly = polyc(1) + polyc(2)*w_d2 + polyc(3)*w_d2**2 + polyc(4)*w_d2**3
        poly = polyc(1) + polyc(2)/w_d2 + polyc(3)/(w_d2**2) + polyc(4)/(w_d2**3)
        
        !poly = c(1) + c(9)*w_d2 + c(10)*w_d2**2 + c(11)*w_d2**3
		!poly = c(1) + c(9)/w_d2 + c(10)/(w_d2**2) + c(11)/(w_d2**3)
		
		
	  	
	  	! HeIIZ
	  	!   c(4) = A_II
	  	!   c(5) = Delta_II
	  	!   c(6) = tau_he
	  	!   c(7) = phi_he
	  	delta = atan( (a1*sin(alpha1)) / &
	  	              (1.0_dp - ((1.0_dp + b1) * cos(alpha1))) &
	  	            )
	  	xarg = 2.0_dp * ( 2.0_dp*pi*c(6)*w_d2 + c(7) ) - delta

	  	heiiz = (c(4) * w_d2 * exp(-8.0_dp*pi_sq*(c(5)**2)*(w_d2**2))) * &
	  	        cos(xarg)
	  	        
	  	        
		! BCZ signal
		!   c(2) = tau_c
		!   c(3) = phi_c
		!   c(1) = A_c
		delta = atan( (a2*sin(alpha2)) / &
	  	              (1.0_dp - ((1.0_dp + b2) * cos(alpha2))) &
	  	            )
		xarg = 2.0d0 * ( 2.d0*pi*c(2)*w_d2 + c(3) ) - delta
		factor = 1.0_dp / sqrt(1.0_dp + 0.0625_dp/(pi_sq*tau0_houdek_sq*w_d2*w_d2))
	  	bcz  = ( c(1) * (nu0**3) / (w_d2**2) ) * &
	  	       factor * &
	  	       cos(xarg + atan(4.0_dp*pi*tau0_houdek*w_d2))
	  	



	  	
	  	fun = poly + bcz + heiiz

		
  end function fun



















