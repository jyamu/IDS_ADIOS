module ids_functions

  !use ids_types, only : ids_core_profile_type
  use ids_core_profiles

  contains

    subroutine ids_core_profile_allocate (timesteps, NRTM, N_Ions, properties, quantities, code, cp)
        implicit none
        integer,        intent(in)  :: timesteps
        integer,        intent(in)  :: NRTM
        integer,        intent(in)  :: N_Ions
        TYPE(ids_properties_type), intent(in) :: properties
        TYPE (global_quantities_type), intent(in) :: quantities
        TYPE (code_type) :: code
        TYPE(ids_core_profile_type), intent(out) :: cp
        integer :: i

        cp%NRTM = NRTM
        cp%timesteps = timesteps
        cp%N_Ions = N_Ions
        cp%ids_properties = properties

        allocate (cp%vacuum_toroidal_field%b0(timesteps))
        allocate (cp%rho_tor_norm(NRTM, timesteps))
        allocate (cp%psi(NRTM, timesteps))        
        allocate (cp%t_e(NRTM, timesteps))
        allocate (cp%t_i_average(NRTM, timesteps))
        allocate (cp%n_e(NRTM, timesteps))
        allocate (cp%n_e_fast(NRTM, timesteps))

        allocate(cp%ions(N_Ions))
        do i = 1, N_Ions
            allocate (cp%ions(i)%n_i(NRTM, timesteps))
            allocate (cp%ions(i)%n_i_fast(NRTM, timesteps))
            allocate (cp%ions(i)%t_i(NRTM, timesteps))
            allocate (cp%ions(i)%p_i(NRTM, timesteps))
            allocate (cp%ions(i)%v_tor_i(NRTM, timesteps))
            allocate (cp%ions(i)%v_pol_i(NRTM, timesteps))
            allocate (cp%ions(i)%label(20)) 
        end do

        allocate (cp%n_i_total_over_n_e(NRTM, timesteps))
        allocate (cp%momentum_tor(NRTM, timesteps))
        allocate (cp%zeff(NRTM, timesteps))
        allocate (cp%p_e(NRTM, timesteps))
        allocate (cp%p_i_total(NRTM, timesteps))
        allocate (cp%pressure_thermal(NRTM, timesteps))
        allocate (cp%pressure_perpendicular(NRTM, timesteps))
        allocate (cp%pressure_parallel(NRTM, timesteps))
        allocate (cp%j_total(NRTM, timesteps))
        allocate (cp%j_tor(NRTM, timesteps))
        allocate (cp%j_ohmic(NRTM, timesteps))
        allocate (cp%j_non_inductive(NRTM, timesteps))
        allocate (cp%j_bootstrap(NRTM, timesteps))
        allocate (cp%conductivity_parallel(NRTM, timesteps))
        allocate (cp%e_field_parallel(NRTM, timesteps))
        allocate (cp%q(NRTM, timesteps))
        allocate (cp%magnetic_shear(NRTM, timesteps))

        cp%global_quantities = quantities
        cp%code = code
        allocate (cp%time(timesteps))

    end subroutine

    subroutine ids_properties_create (statusof, comment, creator, dateof, source, reference, time, coco, properties)
        implicit none
        character(*), intent(in) :: statusof, comment, creator, dateof, source, reference
        integer(4), intent(in) :: time, coco
        TYPE(ids_properties_type), intent(out) :: properties

        properties%status_of = statusof
        properties%comment_of = comment
        properties%creator_of = creator
        properties%date_of = dateof
        properties%source_of = source
        properties%reference_of = reference
        
        properties%homogeneous_time = time
        properties%cocos = coco

        !integer :: l
        !l = LEN(statusof)
        !print '("  properties: l=",i0," status in=[",a,"] out=[",a,"]")', l, statusof, properties%status_of

    end subroutine

!    subroutine ions_create (timesteps, NRTM, aa, zion, zn, lb, ni, nifast, ti, pi, tori, poli, flag, iz)
!        implicit none
!        character, dimension(*), intent(in) :: lb
!        real(4), dimension(:,:), intent(in) :: ni, nifast, ti, pi, tori, poli
!        integer(4), intent(in) :: NRTM, timesteps
!        real(4), intent(in) :: aa, zion, zn
!        integer(4), intent(in) :: flag
!        TYPE(ions_type), intent(out) :: iz
!
!        integer :: l = LEN(lb)
!        allocate (iz%label(l))
!        iz%label(1:l) = lb(1:l)
!
!        allocate (iz%n_i(NRTM, timesteps))        
!        iz%n_i(1:NRTM, 1:timesteps) = ni(1:NRTM, 1:timesteps)
!        allocate (iz%n_i_fast(NRTM, timesteps)) 
!       iz%n_i_fast(1:NRTM, 1:timesteps) = nifast(1:NRTM, 1:timesteps)
!        allocate (iz%t_i(NRTM, timesteps)) 
!        iz%t_i(1:NRTM, 1:timesteps) = ti(1:NRTM, 1:timesteps)
!        allocate (iz%p_i(NRTM, timesteps)) 
!        iz%p_i(1:NRTM, 1:timesteps) = pi(1:NRTM, 1:timesteps)
!        allocate (iz%v_tor_i(NRTM, timesteps)) 
!        iz%v_tor_i(1:NRTM, 1:timesteps) = tori(1:NRTM, 1:timesteps)
!        allocate (iz%v_pol_i(NRTM, timesteps))     
!        iz%v_pol_i(1:NRTM, 1:timesteps) = poli(1:NRTM, 1:timesteps)

!        iz%a = aa
!        iz%z_ion = zion
!        iz%z_n = zn
!        iz%multiple_charge_state_flag = flag

!    end subroutine

    subroutine global_quantities_create (ip, inductive, bootstrap, loop, li, tor, tor_norm, pol, diamagnetic, quantities)
        implicit none
        real(4), intent(in) :: ip, inductive, bootstrap, loop, li, tor, tor_norm, pol, diamagnetic
        TYPE (global_quantities_type), intent(out) :: quantities
    end subroutine

    subroutine code_create (nm, version, para, diag, flag, code)
        implicit none
        character, dimension(*), intent(in) :: nm, version, para, diag, flag
        TYPE (code_type), intent(out) :: code

        integer :: l = LEN(nm)
        allocate (code%name(l))
        code%name(1:l) = nm(1:l)

        l = LEN(version)
        allocate (code%version(l))
        code%version(1:l) = version(1:l)

        l = LEN(para)
        allocate (code%parameters(l))
        code%parameters(1:l) = para(1:l)

        l = LEN(diag)
        allocate (code%output_diagnostics(l))
        code%output_diagnostics(1:l) = diag(1:l)

        l= LEN(flag)
        allocate (code%output_flag(l))
        code%output_flag(1:l) = flag(1:l)
    end subroutine

end module ids_functions

