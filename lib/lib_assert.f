! Assertion related routines
! João Faria (c) 2013

module lib_assert
!  Defines the following routines:
!    - assert: basic assertion routine with message to stderr

  implicit none
  save

  private
  
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)
  
  public :: assert
  public :: assert_almost_equal
  
contains

!*******************************************************************************
! assert if COND is true, else write MSG and stop program
!*******************************************************************************
  subroutine assert(cond, msg)
    logical, intent(in) :: cond
    character(len=*), intent(in) :: msg
    if (.not. cond) then
      write(0,*) 'ASSERT: ', msg
      stop
    endif
  end subroutine assert

!*******************************************************************************
! assert whether the difference between A and B is smaller than the acceptable
! error (EPS or machine precision if not specified), determined by the smaller 
! of A or B. Knuth's "essentiallyEqual" algorithm.
!*******************************************************************************
  subroutine assert_almost_equal(a, b, msg, eps)

    real(dp), intent(in)    :: a, b
    real(dp), optional      :: eps
    character(len=*), intent(in) :: msg
    real(dp) :: error
    logical :: is_almost_equal

    ! use machine epsilon if eps is not specified
    error = epsilon(0.0_dp)
    if (present(eps)) error = eps

    ! we use MERGE as a ternary operator of the form
    !   variable = merge(value if true, value if false, condition)
    is_almost_equal = abs(a-b) <= merge(abs(b), abs(a), abs(a) > abs(b))*error

    if (.not. is_almost_equal) then
      write(0,*) 'ASSERT_ALMOST_EQUAL: ', msg
      stop
    endif
    
  end subroutine assert_almost_equal



end module lib_assert
