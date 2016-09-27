! Regression related routines 
! João Faria (c) 2013


module lib_regression
  
  use lib_assert
  
  implicit none
  save

  private
  
  integer,parameter :: idp = selected_int_kind(13)
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)
  
  public :: linreg
  interface linreg
    module procedure linreg_sp
    module procedure linreg_dp
  end interface linreg
  
  public :: polyreg
  interface polyreg
    module procedure polyreg_sp
    module procedure polyreg_dp
  end interface polyreg
  
contains

  subroutine linreg_dp(x, y, m, b, r)
  ! performs linear regression analysis for a set of (x,y) data
  ! input:  array x,y
  ! output: real m (slope)
  !         real b (intercept)
  !         real r (correlation coefficient squared)
  !-----------------------------------------------------------------------------
    implicit none

    real(dp), dimension(:), intent(in) :: x, y

    real(dp),intent(out)  ::  b  ! y-intercept of best fit line
    real(dp),intent(out)  ::  m  ! slope of best fit line
    real(dp),intent(out)  ::  r   ! squared correlation coefficient

    integer   ::  n
    real(dp)  ::  sumx  = 0.0d0 ! sum of x
    real(dp)  ::  sumx2 = 0.0d0 ! sum of x**2
    real(dp)  ::  sumxy = 0.0d0 ! sum of x * y
    real(dp)  ::  sumy  = 0.0d0 ! sum of y
    real(dp)  ::  sumy2 = 0.0d0 ! sum of y**2


    n = size(y)
    call assert(n == size(x), 'sizes of arrays must match')
    
    sumx = sum(x)
    sumy = sum(y)
    sumx2 = sum(x*x)
    sumy2 = sum(y*y)
    sumxy = sum(x*y)


    m = (n * sumxy  -  sumx * sumy) / (n * sumx2 - sumx**2) ! slope
    b = (sumy * sumx2  -  sumx * sumxy) / (n * sumx2  -  sumx**2) ! intercept
    ! correlation coefficient
    r = (sumxy - sumx * sumy / n) / &
                   dsqrt((sumx2 - sumx**2/n) * (sumy2 - sumy**2/n))

    return
    
  end subroutine linreg_dp
  
  subroutine linreg_sp(x, y, m, b, r)
  ! performs linear regression analysis for a set of (x,y) data
  ! input:  array x,y
  ! output: real m (slope)
  !         real b (intercept)
  !         real r (correlation coefficient squared)
  !-----------------------------------------------------------------------------
    implicit none

    real(sp), dimension(:), intent(in) :: x, y

    real(sp)  ::  b  ! y-intercept of best fit line
    real(sp)  ::  m  ! slope of best fit line
    real(sp)  ::  r   ! squared correlation coefficient

    integer   ::  n
    real(sp)  ::  sumx  = 0.0d0 ! sum of x
    real(sp)  ::  sumx2 = 0.0d0 ! sum of x**2
    real(sp)  ::  sumxy = 0.0d0 ! sum of x * y
    real(sp)  ::  sumy  = 0.0d0 ! sum of y
    real(sp)  ::  sumy2 = 0.0d0 ! sum of y**2
    
    n = size(y)
    call assert(n == size(x), 'sizes of arrays must match')
    
    sumx = sum(x)
    sumy = sum(y)
    sumx2 = sum(x*x)
    sumy2 = sum(y*y)
    sumxy = sum(x*y)

    m = (n * sumxy  -  sumx * sumy) / (n * sumx2 - sumx**2) ! slope
    b = (sumy * sumx2  -  sumx * sumxy) / (n * sumx2  -  sumx**2) ! intercept
    ! correlation coefficient
    r = (sumxy - sumx * sumy / n) / &
                   sqrt((sumx2 - sumx**2/n) * (sumy2 - sumy**2/n))

    return
    
  end subroutine linreg_sp
  
  
  subroutine polyreg_dp(x, y, degree, coeff)
  ! performs polynomial regression analysis for a set of (x,y) data
  ! NOTE: needs LAPACK library 
  ! input:  array   x, y
  !         integer degree
  ! output: array coeff (fitted coefficients) size degree+1
  !-----------------------------------------------------------------------------
    implicit none
    
    integer, intent(in) :: degree

    real(dp), dimension(degree+1), intent(out)  :: coeff
    real(dp), dimension(:), intent(in)          :: x, y
    real(dp), dimension(:,:), allocatable :: mX
    real(dp), dimension(:,:), allocatable :: mXT
    real(dp), dimension(:,:), allocatable :: mXTX
    integer :: i, j
    integer :: n, lda, lwork
    integer :: info
    integer, dimension(:), allocatable :: ipiv
    real(dp), dimension(:), allocatable :: work
 
    n = degree+1
    lda = n
    lwork = n
 
    allocate(ipiv(n))
    allocate(work(lwork))
    allocate(mXT(n, size(x)))
    allocate(mX(size(x), n))
    allocate(mXTX(n, n))
 
    ! prepare the matrix
    do i = 0, degree
       do j = 1, size(x)
          mX(j, i+1) = x(j)**i
       end do
    end do
 
    mXT  = transpose(mX)
    mXTX = matmul(mXT, mX)
 
    ! calls to LAPACK subroutines DGETRF and DGETRI
    call DGETRF(n, n, mXTX, lda, ipiv, info)
    if ( info /= 0 ) then
       print *, "problem in lapack DGETRF"
       return
    end if
    
    call DGETRI(n, mXTX, lda, ipiv, work, lwork, info)
    if ( info /= 0 ) then
       print *, "problem in lapack DGETRI"
       return
    end if
 
    coeff = matmul( matmul(mXTX, mXT), y)
 
    deallocate(ipiv)
    deallocate(work)
    deallocate(mX)
    deallocate(mXT)
    deallocate(mXTX)
    
    return
 
  end subroutine polyreg_dp
  
  subroutine polyreg_sp(x, y, degree, coeff)
  ! performs polynomial regression analysis for a set of (x,y) data
  ! note: needs LAPACK
  ! input:  array   x, y
  !         integer degree
  ! output: array coeff (fitted coefficients) size degree+1
  !-----------------------------------------------------------------------------
    implicit none
    
    integer, intent(in) :: degree

    real(sp), dimension(degree+1), intent(out)  :: coeff
    real(sp), dimension(:), intent(in)          :: x, y
    real(sp), dimension(:,:), allocatable :: mX
    real(sp), dimension(:,:), allocatable :: mXT
    real(sp), dimension(:,:), allocatable :: mXTX
    integer :: i, j
    integer :: n, lda, lwork
    integer :: info
    integer, dimension(:), allocatable :: ipiv
    real(sp), dimension(:), allocatable :: work
 
    n = degree+1
    lda = n
    lwork = n
 
    allocate(ipiv(n))
    allocate(work(lwork))
    allocate(mXT(n, size(x)))
    allocate(mX(size(x), n))
    allocate(mXTX(n, n))
 
    ! prepare the matrix
    do i = 0, degree
       do j = 1, size(x)
          mX(j, i+1) = x(j)**i
       end do
    end do
 
    mXT  = transpose(mX)
    mXTX = matmul(mXT, mX)
 
    ! calls to LAPACK subroutines SGETRF and SGETRI
    call SGETRF(n, n, mXTX, lda, ipiv, info)
    if ( info /= 0 ) then
       print *, "problem in lapack DGETRF"
       return
    end if
    
    call SGETRI(n, mXTX, lda, ipiv, work, lwork, info)
    if ( info /= 0 ) then
       print *, "problem in lapack DGETRI"
       return
    end if
 
    coeff = matmul( matmul(mXTX, mXT), y)
 
    deallocate(ipiv)
    deallocate(work)
    deallocate(mX)
    deallocate(mXT)
    deallocate(mXTX)
    
    return
 
  end subroutine polyreg_sp


end module lib_regression
