!--------------------------------------------------------------------
!	Joao Faria: 21/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common arrays that some subroutines
!	 need to share. 

module commonarray

	use types
	
	implicit none
	
	integer, parameter, public       :: npt = 2000
	integer, public                  :: n   ! number of frequencies
	integer, public                  :: nd2 ! number of second differences
	
	integer, dimension(npt), public  :: l   ! angular degrees
	
	real, dimension(npt), public     :: sd  ! signal
	real, dimension(npt), public     :: sig ! errors
	real, dimension(npt), public     :: xn
	real, dimension(npt), public     :: w   ! frequencies

    real(dp), dimension(npt), public     :: d2      ! second differences
    real(dp), dimension(npt), public     :: w_d2    ! central frequency of second difference
    
    real(dp), allocatable, public   :: c(:) ! parameters of the fit
    
end module commonarray
