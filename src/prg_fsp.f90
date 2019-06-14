!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- prg_fsp - (January - 2013)
!!
!!- author@mail: Nicolas.Grima@univ-brest.fr
!!
!!- Modified, improved by:
!!- Date                 :
!!
!!   (-:  Some examples, ideas and bonus at the end of this file :-)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM prg_fsp

  !===================!
  !- USE association -!
  !===================!
  USE mod_precision
  USE mod_configure
  USE mod_cst
  USE mod_lun
  USE mod_memory
  USE mod_namelist
  USE mod_inout

  !================!
  !- DECLARATIONS -!
  !================!
  IMPLICIT NONE 

  INTEGER(kind = iprec), DIMENSION(:,:), ALLOCATABLE :: dummy_i2D
  REAL   (kind = rprec), DIMENSION(:,:), ALLOCATABLE :: dummy_r2D

  !=============!
  !- MAIN PART -!
  !=============!

  !-----------------!
  !- Print version -!
  !-----------------!
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'===================================================='
  WRITE(lun_standard,*)'= -o0)   '//TRIM(APPNAME)//' v'//TRIM(VERSION)//'     (0o- ='
  WRITE(lun_standard,*)'===================================================='
  WRITE(lun_standard,*)''

  !=====================!
  !- Machine Precision -!
  !=====================!
  CALL sub_machine_precision(lun_standard)

  !======================!
  !- Read namelist file -!
  !======================!
  CALL sub_read_namelist(.TRUE.)

  !=========================!
  !- Open  memory log file -!
  !=========================!
  CALL sub_open_memory_file()

  !=============================!
  !- Dynamic memory Allocation -!
  !=============================!
  Write(lun_standard,*)''
  Write(lun_standard,*)'======================'
  Write(lun_standard,*)'= Memory Allocations ='
  Write(lun_standard,*)'======================'
  Write(lun_standard,*)'  - dummy_i2D'
  CALL sub_memory_allocate(dummy_i2D,ifsp,ifsp+iFive)
  Write(lun_standard,*)'  - dummy_r2D'
  CALL sub_memory_allocate(dummy_r2D,iparam,iparam+iFive)

  !=========!
  !- Title -!
  !=========!

  !-------------!
  !-  Subtitle -!
  !-------------!

  !- comments -!

  !==========! 
  !- NETCDF -!
  !==========!
!!$  CALL sub_netcdf_write_fast(dummy_i2D)
!!$  CALL sub_netcdf_write_fast               ( &
!!$       dummy_r2D                           , & ! 2D array of reals.
!!$       variable_name       = 'dummy_r2D'   , & ! netcdf variable name (opt).
!!$       netcdf_file_name    = 'dummy_r2D.nc', & ! netcdf file name (opt).
!!$       nb_netcdf_file_name = 0             )   ! add number in the netcdf file name (opt).

  !===============================!
  !- Dynamic memory deAllocation -!
  !===============================!
 Write(lun_standard,*)''
  Write(lun_standard,*)'========================'
  Write(lun_standard,*)'= Memory DeAllocations ='
  Write(lun_standard,*)'========================'
  Write(lun_standard,*)'  - dummy_i2D'
  CALL sub_memory_deallocate(dummy_i2D)
  Write(lun_standard,*)'  - dummy_r2D'
  CALL sub_memory_deallocate(dummy_r2D)

  !==========================!
  !- Close  memory log file -!
  !==========================!
  CALL sub_close_memory_file()

  !-----------------!
  !- Print version -!
  !-----------------!
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)'= -o0)    '//TRIM(APPNAME)//' v'//TRIM(VERSION)//'     (0o- ='
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)''

END PROGRAM prg_fsp

!!====================================================================!!
!!---------------------- EXAMPLES - IDEAS - BONUS --------------------!!
!!====================================================================!!
!!
!! !================!
!! !- DECLARATIONS -!
!! !================!
!! INTEGER(kind = iprec) ::
!! INTEGER(kind = iprec), PARAMETER ::
!! INTEGER(kind = iprec), DIMENSION(:)      , ALLOCATABLE ::
!! INTEGER(kind = iprec), DIMENSION(:,:)    , ALLOCATABLE ::
!! INTEGER(kind = iprec), DIMENSION(:,:,:)  , ALLOCATABLE ::
!! INTEGER(kind = iprec), DIMENSION(:,:,:,:), ALLOCATABLE ::
!!
!! REAL   (kind = rprec) ::
!! REAL   (kind = rprec), PARAMETER ::
!! REAL   (kind = rprec), DIMENSION(:)      , ALLOCATABLE ::
!! REAL   (kind = rprec), DIMENSION(:,:)    , ALLOCATABLE ::
!! REAL   (kind = rprec), DIMENSION(:,:,:)  , ALLOCATABLE ::
!! REAL   (kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE ::
!!
!! LOGICAL               ::
!! CHARACTER(len = )     :: 
!!
!!
!!====================================================================!!
