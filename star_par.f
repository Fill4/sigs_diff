!----------------------------------------------------------------------------
! Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine star_par(star_file)

	use types_and_interfaces, only: dp
	use commonvar
	implicit none

	character(len=80), intent(inout)	:: star_file
	real(dp)			:: c4pi, ca, cc, fa, csig
	real(dp)			:: solar_m, solar_r, solar_l
	real(dp)			:: xdatain(200)
	integer				:: i, ndimin(200)
	character(len=80)	:: aheaderin(10)

	! Numerical constants:
	c4pi = 16.0d0*atan(1.0d0)
	ca = 7.565912199839849d-15
	cc = 2.99792458d10
	csig = ca*cc/4.0d0
	fa = c4pi*csig

	! Solar values (CGS):
	solar_m = 1.98919D+33
	solar_r = 6.9599D+10
	solar_l = 3.846D+33

	open (1,file=star_file,form='formatted',status='old')

	! Header:
	do i=1,4
	   read (1,1010) aheaderin(i)
	end do
1010	format (a80)

	! Global parameters:
	read (1,1020) (ndimin(i),i=1,4)
1020	format (4i10)

	read (1,1030) (xdatain(i),i=1,ndimin(2))
1030	format (1p5e16.9)

	! Attribute star parameters
	star_mass = xdatain(1)/solar_m
	star_rad = xdatain(2)/solar_r
	star_lum = xdatain(3)/solar_l
	star_teff = exp(0.25*log(xdatain(3)/(fa*xdatain(2)**2)))
	star_age = xdatain(13)*1.0d-9

	if (verbose) then
		write (*,*) '      M/M_sun  = ', star_mass
		write (*,*) '      R/R_sun  = ', star_rad
		write (*,*) '      L/L_sun  = ', star_lum
		write (*,*) '      Teff (K) = ', star_teff
		write (*,*) '      Age (Gy) = ', star_age
		write (*,*) ' '
	end if

end subroutine star_par