!-------------------------------------------------------------------------------
subroutine error_covariance(errors, invcov)
! this routine calculates the inverse of the error covariance matrix 

    use types_and_interfaces, only: dp
    use commonarray, only: n
    use lib_matrix, only: inverse
    
	implicit none
	
	real(dp), intent(in) :: errors(n)
	real(dp), dimension(n,n), intent(out) :: invcov
	
	real(dp), dimension(n,n) :: jac, k, cov
	integer :: i, j
	
	
	jac = 0
	do i=1,n
        do j=1,n
            if (i==j) jac(i,j) = -2
            if (i==j+1) jac(i,j) = 1
            if (i==j-1) jac(i,j) = 1
        end do
    end do
	
	do i=1,n
        do j=1,n
            k(i,j) = jac(i,j) * errors(i)
        end do
    end do
		
	cov = matmul(k, transpose(k))
	call inverse(cov, invcov)
	
	return
	
end subroutine error_covariance
