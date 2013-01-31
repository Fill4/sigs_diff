!**********************************************************
  real(dp) function fun (w_d2)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone

        use types_and_interfaces, only: dp
        use commonvar, only: pi
        use commonarray, only: c

		implicit none
	
		real(dp), intent(in)  :: w_d2

		real(dp) :: xarg, bcz, heiiz
		real(dp) :: poly
        
		poly = c(1) !+ c(2)*w_d2 + c(3)*w_d2*w_d2
		
		! BCZ
		xarg = 2.0d0 * ( 2.d0*pi*c(3)*w_d2 + c(4) )
	  	bcz  = ( c(2)/w_d2**2 ) * dsin(xarg)
	  	
	  	! HeIIZ
	  	xarg = 2.0d0 * ( 2.d0*pi*c(7)*w_d2 + c(8) )
	  	heiiz = (c(5)*w_d2*dexp(-c(6)*w_d2**2)) * dsin(xarg)
	  	
	  	fun = poly + bcz + heiiz

    
		return
		
  end function fun



















