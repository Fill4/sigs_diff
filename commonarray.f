!--------------------------------------------------------------------
!	Joao Faria: 21/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common arrays that some subroutines
!	 need to share. 

module commonarray

	use types_and_interfaces, only: dp
	
	implicit none
	
	integer, parameter, public       :: npt = 2000
	integer, public                  :: n   ! number of frequencies
	integer, public                  :: nd2 ! number of second differences
	
	integer, dimension(npt), public  :: l, l_d2   ! angular degrees

	integer, public, dimension(100)		:: np
	integer, public						:: nnp
	
	real(dp), dimension(npt), public     :: sd      ! signal
	real(dp), dimension(npt), public     :: sig     ! errors in frequencies
	real(dp), dimension(npt), public     :: sigd2   ! errors in second diff
	real(dp), dimension(npt), public     :: weight  ! iteratively adjusted weights for the 2nd differences
	real(dp), dimension(npt), public     :: xn
	real(dp), dimension(npt), public     :: w   ! frequencies

    real(dp), dimension(npt), public     :: d2      ! second differences
    real(dp), dimension(npt), public     :: w_d2    ! central frequency of second difference
    integer, public                      :: nd2_l0, nd2_l1, nd2_l2, nd2_l3
    
    real(dp), allocatable, public   :: c(:) ! parameters of the fit
    real(dp), allocatable, public   :: polyc(:) ! extra parameters of polynomial
    
    real(dp), allocatable, public   :: icov(:,:) ! inverse of covariance matrix
    
end module commonarray
