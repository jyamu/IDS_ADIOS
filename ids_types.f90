module ids_types

TYPE :: ids_properties_type
    character(:), allocatable   :: status_of !! OK, partial, incomplete
    character(:), allocatable   :: comment_of
    character(:), allocatable   :: creator_of
    character(:), allocatable   :: date_of
    character(:), allocatable   :: source_of
    character(:), allocatable   :: reference_of
    integer*4                   :: homogeneous_time
    integer*4                   :: cocos
END TYPE ids_properties_type

TYPE :: vacuum_toroidal_field_type
    real(4)                                 :: r0
    real(4), dimension(:), allocatable      :: b0    !! time
END TYPE  vacuum_toroidal_field_type 

TYPE :: ions_charge_states_type
    real(4)                                :: z_min
    real(4)                                :: z_max
    character, dimension(:), allocatable   :: label
    real(4), dimension(:,:), allocatable   :: n_z
    real(4), dimension(:,:), allocatable   :: n_z_fast
    real(4), dimension(:,:), allocatable   :: t_z
    real(4), dimension(:,:), allocatable   :: v_tor_z
    real(4), dimension(:,:), allocatable   :: v_pol_z
END TYPE ions_charge_states_type

TYPE :: ions_type
    real(4)                                :: a
    real(4)                                :: z_ion
    real(4)                                :: z_n
    character, dimension(:), allocatable   :: label 
    real(4), dimension(:,:), allocatable   :: n_i       !! NRTM x time
    real(4), dimension(:,:), allocatable   :: n_i_fast  !! NRTM x time
    real(4), dimension(:,:), allocatable   :: t_i       !! NRTM x time
    real(4), dimension(:,:), allocatable   :: p_i       !! NRTM x time
    real(4), dimension(:,:), allocatable   :: v_tor_i   !! NRTM x time
    real(4), dimension(:,:), allocatable   :: v_pol_i   !! NRTM x time
    integer(4)                             :: multiple_charge_state_flag  !! 0|1
!    TYPE (ions_charge_states_type), dimension(:), allocatable :: charge_states
    !! max size 6
END TYPE ions_type

TYPE :: global_quantities_type
    real(4)                                :: ip
    real(4)                                :: current_non_inductive
    real(4)                                :: current_bootstrap
    real(4)                                :: v_loop
    real(4)                                :: li_3
    real(4)                                :: beta_tor
    real(4)                                :: beta_tor_norm
    real(4)                                :: beta_pol
    real(4)                                :: energy_diamagnetic
END TYPE global_quantities_type

TYPE :: code_type
    character, dimension(:), allocatable   :: name
    character, dimension(:), allocatable   :: version
    character, dimension(:), allocatable   :: parameters
    character, dimension(:), allocatable   :: output_diagnostics
    character, dimension(:), allocatable   :: output_flag
END TYPE code_type

TYPE :: ids_core_profile_type
    TYPE (ids_properties_type)             :: ids_properties
    integer*4                              :: NRTM          !! 1st dimension of many arrays below
    integer*4                              :: timesteps     !! 2nd dimension of many arrays below
    real*4, dimension(:,:), allocatable    :: rho_tor_norm  !! NRTM x time
    TYPE (vacuum_toroidal_field_type)      :: vacuum_toroidal_field
    real*4, dimension(:,:), allocatable    :: psi           !! NRTM x time
    real*4, dimension(:,:), allocatable    :: t_e           !! NRTM x time
    real*4, dimension(:,:), allocatable    :: t_i_average   !! NRTM x time
    real*4, dimension(:,:), allocatable    :: n_e           !! NRTM x time
    real*4, dimension(:,:), allocatable    :: n_e_fast      !! NRTM x time
    integer*4                              :: N_Ions        !! number of ions (in structure below)
    TYPE (ions_type), dimension(:), allocatable :: ions !! max size 5
    real(4), dimension(:,:), allocatable   :: n_i_total_over_n_e 
    real(4), dimension(:,:), allocatable   :: momentum_tor
    real(4), dimension(:,:), allocatable   :: zeff
    real(4), dimension(:,:), allocatable   :: p_e
    real(4), dimension(:,:), allocatable   :: p_i_total
    real(4), dimension(:,:), allocatable   :: pressure_thermal
    real(4), dimension(:,:), allocatable   :: pressure_perpendicular
    real(4), dimension(:,:), allocatable   :: pressure_parallel
    real(4), dimension(:,:), allocatable   :: j_total
    real(4), dimension(:,:), allocatable   :: j_tor
    real(4), dimension(:,:), allocatable   :: j_ohmic
    real(4), dimension(:,:), allocatable   :: j_non_inductive
    real(4), dimension(:,:), allocatable   :: j_bootstrap
    real(4), dimension(:,:), allocatable   :: conductivity_parallel
    real(4), dimension(:,:), allocatable   :: e_field_parallel
    real(4), dimension(:,:), allocatable   :: q
    real(4), dimension(:,:), allocatable   :: magnetic_shear
    TYPE (global_quantities_type)          :: global_quantities
    TYPE (code_type)                       :: code
    real(4), dimension(:), allocatable     :: time
END TYPE ids_core_profile_type


end module ids_types

