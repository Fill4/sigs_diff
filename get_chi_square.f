!**********************************************************
! 
!
!
!**********************************************************
  subroutine get_chi_square (chi2, chi2norm)
   
    use types
    ! contains npt, n, l, sd, sig, xn, w -
    use commonarray, only: nd2, d2, w_d2
    use commonvar, only: nconst

    
    real, intent(inout)         :: chi2, chi2norm
    real(dp)                :: chi2term, chi2sum
    real(dp)                :: fit, ww
    real(dp)                :: signal_err = 1.0d0
    integer                     :: i, chi2N
    integer                     :: ll
    
    chi2sum = 0.0d0
    chi2N = 0
    
    do i=1,nd2
        ww = w_d2(i)

        fit = fun(ww)
        chi2term = ((d2(i) - fit) / signal_err)**2
	    chi2sum = chi2sum + chi2term
	    chi2N = chi2N + 1
	enddo
	
	chi2 = chi2sum
	! normalized chi^2:
	chi2norm = chi2sum / (chi2N - nconst)
	
	return
	
  end subroutine
