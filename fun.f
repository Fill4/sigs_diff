!**********************************************************
  elemental real(dp) function fun (nu_d2)
!	 this is the function to be fitted. Expression from Mazumdar 2012 (M12)

        use types_and_interfaces, only: dp
        use commonvar
        use commonarray

		implicit none
	
		real(dp), intent(in)  :: nu_d2

		real(dp) :: xarg, factor, bcz, heiiz
		real(dp) :: a1, a2, b1, b2, alpha1, alpha2, delta, d, dd
		real(dp) :: poly
		
		
		! nu_d2 is (cyclic) frequency nu in Hz 
		
		

		
		! polynomial in nu (can be up to second degree)
		!   polyc
        poly = polyc(1) !+ polyc(2)/nu_d2 + polyc(3)/(nu_d2**2)

	  	
	  	! HeIIZ
	  	!   c(4) = A_II (c0 from M12)
	  	!   c(5) = c2 from M12
	  	!   c(6) = tau_he
	  	!   c(7) = phi_he
	  	
	  	xarg = 4.0_dp*pi*c(6)*nu_d2 + 2.0_dp*c(7)
	  	heiiz = (c(4) * nu_d2 * exp(-c(5) * nu_d2**2)) * sin(xarg)
	  	        
	  	        
		! BCZ signal
		!   c(1) = A_c (b2 from M12)
		!   c(2) = tau_c (tau_bcz from M12)
		!   c(3) = phi_c (phi_bcz from M12)
		
		xarg = 4.0d0*pi*c(2)*nu_d2 + 2.0_dp*c(3)
	  	bcz  = ( c(1) / (nu_d2**2) ) * sin(xarg)
	  	


	  	fun = poly + bcz + heiiz

		
  end function fun



















