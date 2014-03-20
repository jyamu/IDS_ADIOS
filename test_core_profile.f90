!! Test the ids_type core_profile structure, write/read to file
program ids_test_core_profile
    use ids_types
    use ids_functions
    implicit none

    TYPE (ids_core_profile_type) :: cp
    TYPE (ids_properties_type) :: prop 

    call ids_properties_set ("partial", "This is a test!", "Anonymous", prop)
    call ids_core_profile_allocate (2, 5, 5, prop, cp)

    cp%ids_properties%status_of = 'partial' 
    cp%vacuum_toroidal_field%r0 = 1.0;
    cp%vacuum_toroidal_field%b0 = 1.0;
    

    


end program ids_test_core_profile

