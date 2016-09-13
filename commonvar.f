!--------------------------------------------------------------------
<<<<<<< HEAD
!    Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!     Module that contains the common variables that all subroutines
!     need to share. 

module commonvar
    
    use types_and_interfaces, only: dp
    
    implicit none
    
    character(len=10), public  :: include_errors, use_error_chi2
    
    integer, public      :: nconst            ! number of parameters to fit
    integer, public      :: iprint
    integer, public      :: intype            ! type of frequency input
    integer, public      :: lmin, lmax, nlmin    ! lmin - min degree l to consider
                                                ! lmax - max degree l to consider
                                                ! nlmin - min # of l points
    integer, public      :: iterinit,iterfit
    integer, public      :: itermod
    integer, public      :: isig, isel
    integer, public      :: nleft,nrigth
    
    logical, public      :: write_d2_to_file
    
    real(dp), public     :: fac, pi, pi_sq, twopi, piover4
    real(dp), public     :: tau0_houdek, tau0_houdek_sq  
    
    real, public         :: xinit
    real, public         :: ftol,tolfit,dc
    real, public         :: valtype
    real(dp), public     :: vleft,vrigth
    real, public         :: xmass,xrad            ! mass & radius of star
    real, public         :: ssmax                ! max error allowed in frequencies
                            ! initial values:   
    real, public         :: amp_bcz, tau_bcz, phi_bcz, &
                            amp1_he, amp2_he, tau_he, phi_he, &
                            poly0
    real, public         :: tau0, xamp0ref,tau0ref,phi0ref    ! reference values
    real(dp), public     :: nu0, w0, w0ref
    
=======
!	Joao Faria: 20/08/2012	|	Revised: Filipe Pereira - Abr 2016
!--------------------------------------------------------------------
module commonvar
!	Module that contains the common variables that all subroutines
!	need to share. 
	
	use types_and_interfaces, only: dp
	implicit none
	
	logical, public		:: use_error_chi2 		! Use errors from frequencies
	logical, public		:: show_plots = .FALSE.
	logical, public		:: verbose = .FALSE.	! Toggle print information during execution
	
	integer, public		:: nconst  				! Number of parameters to fit
	real(dp), public	:: pi 					! Constants
	real(dp), public	:: w0ref                ! Reference frequency

	integer, public		:: pikaia_pop			! Controls the initial population size of pikaia
	integer, public		:: pikaia_gen			! Controls the number of generations for pikaia
	integer, public		:: iterIRLS, maxIter	! Control the IRLS procedure (not implemented)

	integer, public		:: degree				! Degree of polynomial fitted to 2nd differences

	integer, public		:: lmin, lmax 			! Min and max degree l to consider
	integer, public		:: nlmin				! Minimum number of l points
	integer, public		:: nmin, nmax			! Min and max radial order n to consider
	real(dp), public	:: vleft,vright			! Percentage of left and right bounds of frequencies to consider
	real(dp), public	:: ssmax				! Max error allowed in frequencies

	!Variables to use in rescale function
	real(dp), public	:: upper_tau_bcz, lower_tau_bcz
	real(dp), public	:: upper_tau_he2, lower_tau_he2
	real(dp), public	:: upper_beta, lower_beta
	
	reaL, public		:: large_sep, teff, lum
>>>>>>> Automatic_Approach

end module commonvar
