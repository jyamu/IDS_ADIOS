module ids_functions

  !use ids_types, only : ids_core_profile_type
  use ids_types

  contains

    subroutine ids_core_profile_allocate (timesteps, NRTM, N_Ions, properties, cp)
        implicit none
        integer,        intent(in)  :: timesteps
        integer,        intent(in)  :: NRTM
        integer,        intent(in)  :: N_Ions
        TYPE(ids_properties_type), intent(in) :: properties
        TYPE(ids_core_profile_type), intent(out) :: cp

        cp%NRTM = NRTM
        cp%N_Ions = N_Ions
        cp%ids_properties = properties

        allocate (cp%vacuum_toroidal_field%b0(timesteps))
        allocate (cp%rho_tor_norm(NRTM, timesteps))
        
    end subroutine

    subroutine ids_properties_set (statusof, comment, creator, properties)
        implicit none
        character, dimension(*), intent(in) :: statusof, comment, creator
        TYPE(ids_properties_type), intent(out) :: properties

        integer :: l = LEN(statusof)
        allocate (properties%status_of(l))
        properties%status_of(1:l) = statusof(1:l)
        
    end subroutine

end module ids_functions

