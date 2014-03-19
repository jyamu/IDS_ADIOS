module ids_types

TYPE :: ids_properties_type
    character(10)             :: status_of !! OK, partial, incomplete
    character(256)            :: comment_of
    character(100)            :: creator_of
END TYPE ids_properties_type

TYPE :: ids_vacuum_toroidal_field_type
    real*4  :: r0
    real*4, dimension(:), allocatable  :: b0    !! time
END TYPE  ids_vacuum_toroidal_field_type 

TYPE :: ids_ions_type
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
    integer                                :: multiple_charge_state_flag
    !! charge_states/* missing here
END TYPE ids_ions_type


TYPE :: ids_core_profile_type
    TYPE (ids_properties_type)             :: ids_properties
    integer                                :: NRTM          !! 1st dimension of many arrays below
    real, dimension(:,:), allocatable      :: rho_tor_norm  !! NRTM x time
    TYPE (ids_vacuum_toroidal_field_type)  :: vacuum_toroidal_field
    real, dimension(:,:), allocatable      :: psi           !! NRTM x time
    real, dimension(:,:), allocatable      :: t_e           !! NRTM x time
    real, dimension(:,:), allocatable      :: t_i_average   !! NRTM x time
    real, dimension(:,:), allocatable      :: n_e           !! NRTM x time
    real, dimension(:,:), allocatable      :: n_e_fast      !! NRTM x time

    integer                                :: N_Ions        !! number of ions (in structure below)
    TYPE (ids_ions_type), dimension(:), allocatable :: ions !! max size 5

    !! lot of data missing here

END TYPE ids_core_profile_type


end module ids_types

