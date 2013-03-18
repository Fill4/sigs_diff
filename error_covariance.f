!-------------------------------------------------------------------------------
subroutine error_covariance(errors, invcov)
! this routine calculates the inverse of the error covariance matrix 

    use types_and_interfaces, only: dp
    use commonarray, only: n, nd2, l
    use lib_matrix
    use lib_assert
    
	implicit none
	
	real(dp), intent(in) :: errors(n)
	real(dp), dimension(nd2,nd2), intent(out) :: invcov
	
	real(dp), allocatable :: jac(:,:), sigma(:,:), cov(:,:)
	real(dp), allocatable :: tmpl0(:,:), tmpl1(:,:), tmpl2(:,:), tmpl3(:,:)
	real(dp) :: cond
    integer :: nl0, nl1, nl2, nl3
	integer :: i, j
	
	! jacobian, dimensions are n x nd2 to build it and then we transpose
	allocate(jac(n, nd2))
    jac = 0
	! "input covariance matrix" (of the frequencies), dimensions: n x n
	allocate(sigma(n, n))

    nl0 = 0
    nl1 = 0
    nl2 = 0
    nl3 = 0
    do i=1,n
        if (l(i) == 0) nl0 = nl0+1
        if (l(i) == 1) nl1 = nl1+1
        if (l(i) == 2) nl2 = nl2+1
        if (l(i) == 3) nl3 = nl3+1
    end do

    ! jacobian for l=0
    allocate(tmpl0(nl0, nl0-2))
    tmpl0=0
    do i=1,nl0-2
        tmpl0(i,i) = 1
        tmpl0(i+1,i) = -2
        tmpl0(i+2,i) = 1
    end do
    ! jacobian for l=1
    allocate(tmpl1(nl1, nl1-2))
    tmpl1=0
    do i=1,nl1-2
        tmpl1(i,i) = 1
        tmpl1(i+1,i) = -2
        tmpl1(i+2,i) = 1
    end do
    ! jacobian for l=2
    allocate(tmpl2(nl2, nl2-2))
    tmpl2=0
    do i=1,nl2-2
        tmpl2(i,i) = 1
        tmpl2(i+1,i) = -2
        tmpl2(i+2,i) = 1
    end do
    ! jacobian for l=3
    allocate(tmpl3(nl3, nl3-2))
    tmpl3=0
    do i=1,nl3-2
        tmpl3(i,i) = 1
        tmpl3(i+1,i) = -2
        tmpl3(i+2,i) = 1
    end do

    ! full jacobian matrix
    jac = blkdiag(blkdiag(blkdiag(tmpl0, tmpl1), tmpl2),tmpl3)
    jac = transpose(jac)
!    write(*,*) size(jac, dim=1), size(jac, dim=2)
    
    ! build input covariance matrix, assuming independent errors in the frequencies
    ! note that it is in muHz
    sigma = 0
    do i=1,n
        sigma(i,i) = (1d6*errors(i))**2
    end do
    
    ! "output covariance matrix" (of the second differences), dimension: nd2 x nd2
    ! note that it is in muHz
    allocate(cov(nd2, nd2))
    cov = 0
    cov = matmul(jac, matmul(sigma, transpose(jac)))
!    write(*,'(71f5.2)') cov

    call condition(cov, cond)
    call assert(cond < 1.0_dp/epsilon(1.0_dp), 'Covariance matrix cannot be inverted')
!    write(*,'(a,es10.3,3x,a,es10.3)') 'condition number: ', cond, 'eps^-1: ', 1.0_dp/epsilon(1.0_dp)

	call inverse(cov, invcov)

	return
	
end subroutine error_covariance
