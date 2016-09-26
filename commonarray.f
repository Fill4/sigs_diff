module commonarray
! Module that has all the array variables that need to be shared between subroutines 

	use types_and_interfaces, only: dp
	implicit none
	
	integer, parameter			:: npt = 500
	integer						:: n			! number of frequencies
	integer						:: nd2			! number of second differences
	
	integer, dimension(npt)		:: l, l_d2		! angular degrees
	integer, dimension(npt)		:: xn

	real(dp), dimension(npt)	:: w		! frequencies
	real(dp), dimension(npt)	:: d2		! second differences
	real(dp), dimension(npt)	:: pre_d2		! second differences
	real(dp), dimension(npt)	:: w_d2		! central frequency of second difference
	
	real(dp), dimension(npt)	:: sig		! errors in frequencies
	real(dp), dimension(npt)	:: sigd2	! errors in second diff
	real(dp), dimension(npt)	:: weight	! iteratively adjusted weights for the 2nd differences

	integer, dimension(100)		:: np
	integer						:: nnp
	
	real(dp), allocatable		:: c(:)			! parameters of the fit
	real(dp), dimension(4)		:: polyc(4) 	! extra parameters of polynomial
	
end module commonarray
