!! Test the ids_type core_profile structure, write/read to file
program ids_test_core_profile
    use ids_types
    use ids_functions
    implicit none

    TYPE (ids_core_profile_type) :: cp
    TYPE (ids_properties_type) :: prop 
    TYPE (code_type) :: code
    TYPE (global_quantities_type) :: quantity
    integer(4), parameter :: NRTM=2
    integer(4), parameter :: timesteps=5
    integer(4), parameter :: N_Ions=4
    TYPE(ions_type), dimension(N_Ions) :: iz
    real(4), dimension(NRTM,timesteps) :: ni, nifast, ti, pi, tori, poli

    integer(4) :: i
    integer(4) :: j
    integer(4) :: k

    call ids_properties_create ("partial", "This is a test!", "Anonymous", "2014-03-21", "ITER", "EPSI", 5, 6, prop)
    call global_quantities_create (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, quantity)
    call code_create ("phi", "1.0", "pot", "diagnostics", "flag", code)
    call ids_core_profile_allocate (timesteps, NRTM, 5, prop, quantity, code, cp)

    cp%ids_properties%status_of = 'partial' 
    cp%vacuum_toroidal_field%r0 = 1.0;
    cp%vacuum_toroidal_field%b0 = 1.0;
    
    do i=1, N_Ions
        do j=1, NRTM
            do k=1, timesteps
                cp%ions(i)%n_i(j, k) = j+k+0.1+i
                cp%ions(i)%n_i_fast(j, k) = ni(j, k)+1+i
                cp%ions(i)%t_i(j, k) = i*0.5+i
                cp%ions(i)%p_i(j, k) = j*3.3+i
                cp%ions(i)%v_tor_i(j, k) = j/k+i
                cp%ions(i)%v_pol_i(j, k) = tori(j, k)+0.2+i
            end do
        end do
        cp%ions(i)%a = 1.5+i
        cp%ions(i)%z_ion = 2.8+i
        cp%ions(i)%z_n = 3.5+i
        cp%ions(i)%multiple_charge_state_flag = 0
        cp%ions(i)%label = "H+"
    end do

    do i=1, NRTM
        do j=1, timesteps
            cp%n_i_total_over_n_e = i/j+i+j
            cp%momentum_tor = i/j+i+j
            cp%zeff = i/j+i+j
            cp%p_e = i/j+i+j
            cp%p_i_total = i/j+i+j
            cp%pressure_thermal = i/j+i+j
            cp%pressure_perpendicular = i/j+i+j
            cp%pressure_parallel = i/j+i+j
            cp%j_total = i/j+i+j
            cp%j_tor = i/j+i+j
            cp%j_ohmic = i/j+i+j
            cp%j_non_inductive = i/j+i+j
            cp%j_bootstrap = i/j+i+j
            cp%conductivity_parallel = i/j+i+j
            cp%e_field_parallel = i/j+i+j
            cp%q = i/j+i+j
            cp%magnetic_shear = i/j+i+j
        end do
    end do

    do i=1, timesteps
        cp%time(i) = i
    end do

    print '("Core profile: timesteps=",i0," NRTM=",i0)', cp%timesteps, cp%NRTM
    print '("  properties: status=[",a,"] creator=[",a,"]")', cp%ids_properties%status_of, cp%ids_properties%creator_of

end program ids_test_core_profile

