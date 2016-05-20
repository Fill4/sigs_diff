!--------------------------------------------------------------------
!	Joao Faria: 21/08/2012  |	Revised: Filipe Pereira - Abr 2016
!--------------------------------------------------------------------
module commonarray
!	Module that contains the common arrays that some subroutines
!	need to share. 

	use types_and_interfaces, only: dp
	implicit none
	
	integer, parameter, public		:: npt = 500
	integer, public					:: n			! number of frequencies
	integer, public					:: nd2			! number of second differences
	
	integer, dimension(npt), public		:: l, l_d2		! angular degrees
	real(dp), dimension(npt), public	:: xn

	real(dp), dimension(npt), public	:: w		! frequencies
	real(dp), dimension(npt), public	:: d2		! second differences
	real(dp), dimension(npt), public	:: pre_d2		! second differences
	real(dp), dimension(npt), public	:: w_d2		! central frequency of second difference
	
	real(dp), dimension(npt), public	:: sig		! errors in frequencies
	real(dp), dimension(npt), public	:: sigd2	! errors in second diff
	real(dp), dimension(npt), public	:: weight	! iteratively adjusted weights for the 2nd differences

	integer, public, dimension(100)		:: np
	integer, public						:: nnp
	
	real(dp), allocatable, public	:: c(:)			! parameters of the fit
	real(dp), dimension(4), public	:: polyc(4) 	! extra parameters of polynomial
	
end module commonarray
