!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common variables that all subroutines
!	 need to share. 

module commonvar
	
	implicit none
	
	character(len=10), public  :: include_errors, use_error_chi2
	
	integer, public      :: nconst			! number of parameters to fit
	integer, public      :: iprint
	integer, public      :: intype			! type of frequency input
	integer, public      :: lmin, lmax, nlmin	! lmin - min degree l to consider
												! lmax - max degree l to consider
												! nlmin - min # of l points
	integer, public      :: iterinit,iterfit
	integer, public      :: itermod
	integer, public      :: isig, isel
	integer, public      :: nleft,nrigth
	
	real, public         :: fac,pi
	
	real, public         :: xinit
	real, public         :: ftol,tolfit,dc
	real, public         :: valtype
	real, public         :: vleft,vrigth
	real, public         :: xmass,xrad			! mass & radius of star
	real, public         :: ssmax				! max error allowed in frequencies
	real, public         :: w0,xl0,xamp0,tau0	! initial values
	real, public         :: w0ref,xamp0ref,tau0ref,phi0ref	! reference values
	

end module commonvar
