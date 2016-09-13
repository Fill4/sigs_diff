<<<<<<< HEAD
!**********************************************************
  elemental real(dp) function fun (nu_d2)
!     this is the function to be fitted.

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, twopi, piover4, &
                             tau0_houdek, tau0_houdek_sq, nu0, w0
        use commonarray, only: c, polyc

        implicit none
    
        real(dp), intent(in)  :: nu_d2

        real(dp) :: xarg, factor, bcz, heiiz
        real(dp) :: a1, a2, b1, b2, alpha1, alpha2, F1, F2, delta, d, dd
        real(dp) :: tau_c_tilde, k_c, psi_c
        real(dp) :: poly
        
        
        ! nu_d2 is (cyclic) frequency nu in Hz 
        
        
        d = c(5)**2
        dd = d**2
        ! HeII *****************************************************************
        ! eqs (C8) and (C9) of Houdek 2007
        alpha1 = 2.0_dp * w0 * c(6)
        a1 = - 8.0_dp * pi * d * nu_d2 * w0 + w0/(twopi*nu_d2)
        b1 = 32.0_dp * pi_sq * dd * w0**2 * nu_d2**2 - 6.0_dp * d * w0**2
        ! eq (C6) of Houdek 2007
        F1 = 2.0_dp * sqrt((1.0_dp - ((1.0_dp + b1) * cos(alpha1)))**2 + (a1*sin(alpha1))**2)
        
        
        ! BCZ ******************************************************************
        ! eq (C12) of Houdek 2007
        alpha2 = 2.0_dp * w0 * c(2)
        a2 = - 2.0_dp * (w0 / (twopi*nu_d2)) * &
             (1.0_dp + 0.03125_dp/(tau0_houdek_sq * pi_sq * nu_d2**2)) / &
             (1.0_dp + 0.0625_dp/(tau0_houdek_sq * pi_sq * nu_d2**2))
        b2 = (w0 / (twopi*nu_d2))**2 * &
             ((3.0_dp + 0.15625_dp/(tau0_houdek_sq * pi_sq * nu_d2**2)) / &
               (1.0_dp + 0.0625_dp/(tau0_houdek_sq * pi_sq * nu_d2**2))**2 &
             )
        ! eq (C6) of Houdek 2007
        F2 = 2.0_dp * sqrt((1.0_dp - ((1.0_dp + b2) * cos(alpha2)))**2 + (a2*sin(alpha2))**2)
        
        
        ! third degree polynomial in nu^-1
        !   polyc
!        poly = polyc(1) + polyc(2)*nu_d2 + polyc(3)*nu_d2**2 + polyc(4)*nu_d2**3
        poly = polyc(1) + polyc(2)/nu_d2 + polyc(3)/(nu_d2**2) + polyc(4)/(nu_d2**3)
        
        
          
          ! HeIIZ
          !   c(4) = A_II
          !   c(5) = Delta_II
          !   c(6) = tau_he
          !   c(7) = phi_he
          delta = atan( (a1*sin(alpha1)) / &
                        (1.0_dp - ((1.0_dp + b1) * cos(alpha1))) &
                      )
          xarg = 2.0_dp * ( 2.0_dp*pi*c(6)*nu_d2 + c(7) ) - delta

          heiiz = (F1 * c(4) * nu_d2 * exp(-8.0_dp*pi_sq*(c(5)**2)*(nu_d2**2))) * &
                  cos(xarg)
     
                  
        ! BCZ signal
        !   c(2) = tau_c
        !   c(3) = phi_c
        !   c(1) = A_c
        delta = atan( (a2*sin(alpha2)) / &
                        (1.0_dp - ((1.0_dp + b2) * cos(alpha2))) &
                      )
          
          ! this is for the diagnostic D0 of Houdek2007
!        xarg = 2.0_dp * ( 2.d0*pi*c(2)*nu_d2 + c(3) ) &
!               + atan(4.0_dp*pi*tau0_houdek*nu_d2) &
!               - delta 

        ! this is the improved D1 (appearing also in D2); m=3.5 is hardcoded
        tau_c_tilde = c(2) + c(3)/(twopi*nu_d2)
        k_c = sqrt( 1.0_dp - (20.25_dp)/(tau_c_tilde**2 * (twopi*nu_d2)**2) )
        psi_c = k_c*tau_c_tilde*twopi*nu_d2 &
                - 4.5_dp * acos(4.5_dp/(tau_c_tilde*twopi*nu_d2)) &
                + piover4
        xarg = 2.0_dp * psi_c &
               + atan(4.0_dp*pi*tau0_houdek*nu_d2) &
               - delta 
        !***               
        factor = 1.0_dp / sqrt(1.0_dp + 1.0_dp/(16.0_dp*pi_sq*tau0_houdek_sq*nu_d2*nu_d2))
        
          bcz  = ( F2 * c(1) * (nu0**3) / (nu_d2**2) ) * &
                 factor * &
                 cos(xarg)


         ! the final signal
          fun = poly + heiiz + bcz


        
  end function fun
=======
!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
elemental real(dp) function fun (nu_d2)
! This is the theoretical function to be fitted

	use types_and_interfaces, only: dp
	use commonarray, only: c
	use commonvar, only: w0ref, pi
	implicit none

	real(dp), intent(in)  :: nu_d2
	real(dp) :: xarg, yarg, bcz, heii, factor
	
	! HeIIZ
	!   c(4) = A_he
	!   c(5) = beta
	!   c(6) = tau_he
	!   c(7) = phi_he
	
	xarg = 4.0_dp*pi*c(6)*nu_d2*w0ref*1.0d-6 + 2.0_dp*c(7)
	heii = (c(4)*nu_d2*exp(-c(5)*nu_d2**2)) * sin(xarg)

	! BCZ signal
	!   c(1) = A_bcz
	!   c(2) = tau_bcz
	!   c(3) = phi_bcz

	yarg = 4.0d0*pi*c(2)*nu_d2*w0ref*1.0d-6 + 2.0_dp*c(3)
	bcz  = ( c(1)/nu_d2**2 ) * sin(yarg)

	fun = bcz + heii

	return

end function fun
>>>>>>> Automatic_Approach



















