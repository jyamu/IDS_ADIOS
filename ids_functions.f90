module ids_functions

  use ids_types

  interface


    subroutine ids_core_profile_allocate (NRTM, N_Ions, cp)
        implicit none
        integer,        intent(in)  :: NRTM
        integer,        intent(in)  :: N_Ions
        TYPE (ids_core_profile_type), intent(out) :: cp
    end subroutine

 end interface

end module ids_functions

