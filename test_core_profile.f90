!! Test the ids_type core_profile structure, write/read to file
program ids_test_core_profile
    use ids_types
    implicit none

    TYPE (ids_core_profile_type) :: cp

    cp%ids_properties%status_of = 'partial' 
    cp%NRTM = 5;
    cp%vacuum_toroidal_field%r0 = 1.0;
    cp%vacuum_toroidal_field%b0 = 1.0;
    
    


end program ids_test_core_profile

