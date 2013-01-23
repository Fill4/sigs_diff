!--------------------------------------------------------------------
!	Joao Faria: Jan 2013
!--------------------------------------------------------------------
!	 Module that contains the common types

module types

    implicit none
    
    public
    
    integer,parameter :: sp = selected_real_kind(p=6,r=37)
    integer,parameter :: dp = selected_real_kind(p=15,r=307)

end module types
