!--------------------------------------------------------------------
!	Joao Faria: Jan 2013
!--------------------------------------------------------------------
!	 Module that contains the common types

module types_and_interfaces

    implicit none
    
    public
    
    integer,parameter :: sp = selected_real_kind(p=6,r=37)
    integer,parameter :: dp = selected_real_kind(p=15,r=307)

    ! provide interface to the objective function (genetic algorthim type)
    interface
      function objfun_ga(n, p) result(fun_val)
        implicit none
        integer, intent(in)    :: n
        real, intent(in)       :: p(:)
        real                   :: fun_val
      end function objfun_ga
    end interface
    
    ! provide interface to the objective function (simplex type)
    interface objfun
      subroutine objfun(p, func)
        implicit none
        integer, parameter     :: dp = selected_real_kind(p=15,r=307)
        real (dp), intent(in)  :: p(:)
        real (dp), intent(out) :: func
      end subroutine objfun
    end interface objfun

    ! interface to the function that calculates the signal
    interface fun
        elemental real(kind=8) function fun (w_d2)
            implicit none
            real(kind=8), intent(in)  :: w_d2
        end function fun
    end interface fun
    
    ! to the parameter rescaling subroutine
    interface rescale
      subroutine rescale(array_in, array_out)
        implicit none
        real, dimension(:), intent(in)      :: array_in
        real(kind=8), dimension(:), intent(out) :: array_out
      end subroutine rescale
    end interface rescale
    
    ! interface to the function that calculates the smooth component
    interface smooth_comp
        elemental real(kind=8) function smooth_comp (w_d2)
            implicit none
            real(kind=8), intent(in)  :: w_d2
        end function smooth_comp
    end interface smooth_comp
    
    ! interface to the function that calculates the he component
    interface he_comp
        elemental real(kind=8) function he_comp (w_d2)
            implicit none
            real(kind=8), intent(in)  :: w_d2
        end function he_comp
    end interface he_comp
    
        ! interface to the function that calculates the bcz component
    interface bcz_comp
        elemental real(kind=8) function bcz_comp (w_d2)
            implicit none
            real(kind=8), intent(in)  :: w_d2
        end function bcz_comp
    end interface bcz_comp


end module types_and_interfaces
