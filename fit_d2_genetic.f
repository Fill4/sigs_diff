!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine fit_d2_genetic (chi2)
! This subroutine initially removes the smooth component from the second 
! differences.
!
! Next it runs PIKAIA to try to fit the second differences to the funtion
! in the fun.f file using a iteratively reweighted least squares method.

	use types_and_interfaces
	use commonvar
	use commonarray, only : nd2, w_d2, d2, sigd2, l_d2, c, polyc, weight, pre_d2
	use lib_pikaia12
	use lib_regression, only: polyreg
	use lib_statistics, only: median

	implicit none
		
	real(dp), intent(inout)		:: chi2
	real(dp), dimension(nconst)	:: c0, p, step, var

	real						:: ctrl(12), x(nconst), f
	integer						:: seed, status
	integer						:: new_unit, i
	
	character(len=80)			:: outfile, filename
	real(dp), dimension(nd2)	:: smooth

	! Polynomial fit of the second differences. The degree is previously defined
	if (degree .eq. 0) then
		polyc(1) = median(d2(1:nd2))
	else
		call polyreg(1.0_dp/(w_d2(1:nd2)*w0ref), d2(1:nd2), degree, polyc)
	end if

	! Get smooth function and remove it from the second differences
	smooth(1:nd2) = smooth_comp(w_d2(1:nd2)*w0ref)
	pre_d2(1:nd2) = d2(1:nd2)
	d2(1:nd2) = d2(1:nd2) - smooth(1:nd2)

	! Define number of re-weight iterations based on error usage 
	!if (use_error_chi2) then
	!	maxIter = 4
	!else
	!	maxIter = 1
	!end if

	! First, initialize the random-number generator
	seed = TIME()
	call rninit(seed)
	
	! Set control variables
	ctrl(1:12) = -1
	ctrl(1) = pikaia_pop 
	ctrl(2) = pikaia_gen
	ctrl(5) = 5 ! one-point+creep, adjustable rate based on fitness
	
	! Either run PIKAIA once for a no errors run or iterate through various weights for
	! each point until convergence or until MaxIter
	!do iterIRLS=1,maxIter
	call pikaia(objfun_ga, nconst, ctrl, x, f, status)  ! if using PIKAIA 1.2
		!call rescale(x, c)
		!if (all(abs(c0(1:2)-c(1:2)) < 0.2_dp * c(1:2)) .and. all(abs(c0(4:6)-c(4:6)) < 0.2_dp * c(4:6))) exit
		!c0 = c
	!end do
		
	! Rescale parameters
	call rescale(x, c)

	!Check exit_status for errors during PIKAIA execution and stop if there were any
	if (status /= 0) then
		write(6,*) "Error in PIKAIA"
		stop
	endif
	
	! Residuals of best parameters
	chi2 = 1.0/f

	return
		
end subroutine fit_d2_genetic
  
  
function objfun_ga(n, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal is calculated in FUN for the current values of the parameters p, 
! and subtracted from the second differences

	use types_and_interfaces, only: dp, fun, rescale
	use commonvar, only : nconst, pi, iterIRLS, use_error_chi2
	use commonarray, only : nd2, w_d2, d2, c, l, sigd2, weight
	implicit none

	integer, intent(in)			:: n     ! size of parameter space
	real, intent(in)			:: p(:)
	real						:: fun_val
	real(dp), dimension(nd2)	:: ww, signal, weight0, resid
	real(dp)					:: final_resid
	integer, parameter			:: Q = 4

	! Rescale parameters
	call rescale(p, c)

	ww = w_d2(1:nd2)
	signal = fun(ww)
	! If we are using errors enter IRLS
	if (use_error_chi2) then
		resid = ((d2(1:nd2)-signal(1:nd2))/sigd2(1:nd2))**2
	!	weight0 = 1.0_dp / sigd2(1:nd2)**2
	!	resid = (d2(1:nd2)-signal)**2 * weight0(1:nd2)
	!	if (iterIRLS>1) then
	!		weight(1:nd2) = Q / ( sigd2(1:nd2)**2 * (resid + Q) )
	!	else 
	!		weight(1:nd2) = weight0
	!	end if
	!	resid = (d2(1:nd2)-signal(1:nd2))**2 * weight(1:nd2)
	! Else just do normal least square calculation
	else if (.not. use_error_chi2) then
		resid = (d2(1:nd2)-signal(1:nd2))**2
	end if
	final_resid = sum(resid)
	fun_val = sngl(1.0 / final_resid)
	
	return

end function objfun_ga
  
subroutine rescale(array_in, array_out)
! Rescales the parameters that come out of pikaia ARRAY_IN to their physical
! values ARRAY_OUT
	
	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)			:: array_in
	real(dp), dimension(:), intent(out) 	:: array_out

	
	! Bcz
	array_out(1) = dble(array_in(1)) * 0.6_dp
	array_out(2) = dble(array_in(2)) * (upper_tau_bcz - lower_tau_bcz) + lower_tau_bcz
	array_out(3) = dble(array_in(3)) * pi
	
	! HeII
	array_out(4) = dble(array_in(4)) * (5.0_dp - 0.5_dp) + 0.5_dp
	array_out(5) = dble(array_in(5)) * 5.0_dp
	array_out(6) = dble(array_in(6)) * (upper_tau_he2 - lower_tau_he2) + lower_tau_he2
	array_out(7) = dble(array_in(7)) * pi
  
end subroutine rescale
  
