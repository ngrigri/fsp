!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- mod_netcdf_write_fast - (Month - Year)
!!
!!- Author: Nicolas.Grima@univ-brest.fr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Write easely and simply  a vector or anarray of integers, 
!! reals or logicals in a NetCDF file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_netcdf_write_fast

  !===================!
  !- USE association -!
  !===================!
  USE mod_precision
  USE mod_cst
  USE mod_lun
  USE netcdf

  !===============================!
  !- DECLARATIONS of GLOBAL DATA -!
  !===============================!
  IMPLICIT NONE

  !====================================!
  !- DECLARATIONS of INTERFACE BLOCKS -!
  !====================================!
  INTERFACE sub_netcdf_write_fast
     MODULE PROCEDURE                 &
          sub_netcdf_write_fast_i1D , &
          sub_netcdf_write_fast_i2D , &
          sub_netcdf_write_fast_r1D , &
          sub_netcdf_write_fast_r2D , &
          sub_netcdf_write_fast_l2D
  END INTERFACE

CONTAINS

  !********************************************************************!
  !- sub_netcdf_write_fast_i1D - (October - 2012)
  !
  !- Author: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Create a NetCDF File and store in it a vector of integers
  !
  !********************************************************************!
  SUBROUTINE sub_netcdf_write_fast_i1D( &
       array_i1D                      , & ! Vector of integers.
       variable_name                  , & ! NetCDF variable name (opt).
       netcdf_file_name               , & ! NetCDF file name (opt).
       nb_netcdf_file_name            )   ! Add a number in the NetCDF 
    !                                     ! file name (opt).

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), DIMENSION(:), INTENT(in) :: array_i1D
    CHARACTER(len = *)   ,     OPTIONAL, INTENT(in) :: variable_name
    CHARACTER(len = *)   ,     OPTIONAL, INTENT(in) :: netcdf_file_name
    INTEGER(kind = iprec),     OPTIONAL, INTENT(in) :: nb_netcdf_file_name

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    INTEGER(kind = iprec), PARAMETER :: nb_digit = iFour

    CHARACTER(len=48)     :: char_format
    INTEGER(kind = iprec) :: size_string

    CHARACTER(len = 32)   :: final_netcdf_file_name
    CHARACTER(len = 32)   :: final_variable_name
    INTEGER(kind = iprec) :: nb_final_netcdf_file_name
    INTEGER(kind = iprec) :: is_err

    INTEGER(kind = iprec) :: dim1

    INTEGER(kind = iprec) :: nc_id
    INTEGER(kind = iprec) :: var_id
    INTEGER(kind = iprec) :: dim1_id

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!

    !======================================!
    !- SECTION 1: test optional arguments -!
    !======================================!
    IF (PRESENT(variable_name)) THEN
       final_variable_name = TRIM(variable_name)
    ELSE
       final_variable_name = 'array_i1D'
    END IF

    IF (PRESENT(netcdf_file_name)) THEN
       final_netcdf_file_name = TRIM(netcdf_file_name)
    ELSE
       final_netcdf_file_name = 'netcdf_file.nc'
    END IF

    !- Test if the end of the netcdf file is ".nc".   -!
    !- If not stop the program with an error message. -!
    size_string = LEN_TRIM(final_netcdf_file_name)
    IF (final_netcdf_file_name(size_string-iThree:size_string) /= '.nc') THEN
       WRITE(lun_error,*)'Error: mod_netcdf_write_fast, &
            &sub_netcdf_write_fast_i1D'
       WRITE(lun_error,*)'NetCDF file name has to finish with .nc'
       WRITE(lun_error,*)' final_netcdf_file_name(size_string-iThree:size_string) =', &
                           final_netcdf_file_name(size_string-iThree:size_string)
       STOP
    END IF

    IF ( PRESENT(nb_netcdf_file_name)) THEN
       nb_final_netcdf_file_name = nb_netcdf_file_name
    ELSE
       nb_final_netcdf_file_name = -iOne
    END IF

    !- Add a digit number in the NetCDF file name -!
    !- just before ".nc"                          -!
    IF (nb_final_netcdf_file_name /= -iOne) THEN
       WRITE(char_format, 1000) size_string-iThree, nb_digit, nb_digit
       WRITE(final_netcdf_file_name,FMT=TRIM(char_format)) &
            final_netcdf_file_name(1:size_string-iThree) , &
            nb_final_netcdf_file_name,'.nc'
    END IF

1000 FORMAT('(A',I2,',I',I2,'.',I2,',A3)')

    !======================!
    !- CREATE NetCDF FILE -!
    !======================!
    is_err = nf90_create                     ( &
         path  = TRIM(final_netcdf_file_name), & ! NetCDF filename created.
         cmode = NF90_64BIT_OFFSET           , & ! 64_bits record, Large File
         ncid  = nc_id                       )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-----------------!
    !- Set Dimension -!
    !-----------------!
    dim1 = SIZE(array_i1D,1)

    is_err=NF90_def_dim ( &
         ncid = nc_id   , &
         name  = 'dim1' , &
         len   = dim1   , &
         dimid = dim1_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-----------------------!
    !- Variable Definition -!
    !-----------------------!
    is_err=NF90_def_var                    ( &
         ncid   = nc_id                    , &
         name   = TRIM(final_variable_name), &
         xtype  = NF90_INT                 , &
         dimids = (/ dim1_id /)            , &
         varid  = var_id                   )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !------------------------------!
    !- Close the header defintion -!
    !------------------------------!
    is_err =  NF90_enddef(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-------------------------------!
    !- Put data in the NetCDF file -!
    !-------------------------------!
    is_err = NF90_put_var  ( &
         ncid   = nc_id    , &
         varid  = var_id   , &
         values = array_i1D)

    !-------------------------!
    !- Close the netcdf file -!
    !-------------------------!
    is_err = nf90_close(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

  END SUBROUTINE sub_netcdf_write_fast_i1D

  !********************************************************************!
  !- sub_netcdf_write_fast_i2D - (October - 2012)
  !
  !- Author: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Create a NetCDF File and store in it a 2D array of integers.
  !
  !********************************************************************!
  SUBROUTINE sub_netcdf_write_fast_i2D( &
       array_i2D                      , & ! 2D array of integers.
       variable_name                  , & ! NetCDF variable name (opt).
       netcdf_file_name               , & ! NetCDF file name (opt).
       nb_netcdf_file_name            )   ! Add a number in the NetCDF 
    !                                     ! file name (opt).

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), DIMENSION(:,:), INTENT(in) :: array_i2D
    CHARACTER(len = *),          OPTIONAL, INTENT(in) :: variable_name
    CHARACTER(len = *),          OPTIONAL, INTENT(in) :: netcdf_file_name
    INTEGER(kind = iprec),       OPTIONAL, INTENT(in) :: nb_netcdf_file_name

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    INTEGER(kind = iprec), PARAMETER :: nb_digit = iFour

    CHARACTER(len=48)     :: char_format
    INTEGER(kind = iprec) :: size_string

    CHARACTER(len = 32)   :: final_netcdf_file_name
    CHARACTER(len = 32)   :: final_variable_name
    INTEGER(kind = iprec) :: nb_final_netcdf_file_name
    INTEGER(kind = iprec) :: is_err

    INTEGER(kind = iprec) :: dim1
    INTEGER(kind = iprec) :: dim2

    INTEGER(kind = iprec) :: nc_id
    INTEGER(kind = iprec) :: var_id
    INTEGER(kind = iprec) :: dim1_id
    INTEGER(kind = iprec) :: dim2_id

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(*,*)' -db- Enter in sub_netcdf_write_fast_i2D'

    !======================================!
    !- SECTION 1: test optional arguments -!
    !======================================!
    IF (PRESENT(variable_name)) THEN
       final_variable_name = TRIM(variable_name)
    ELSE
       final_variable_name = 'array_i2D'
    END IF

    IF (PRESENT(netcdf_file_name)) THEN
       final_netcdf_file_name = TRIM(netcdf_file_name)
    ELSE
       final_netcdf_file_name = 'netcdf_file.nc'
    END IF

    !- Test if the end of the netcdf file is ".nc".   -!
    !- If not stop the program with an error message. -!
    size_string = LEN_TRIM(final_netcdf_file_name)
    IF (final_netcdf_file_name(size_string-iTwo:size_string) /= '.nc') THEN
       WRITE(lun_error,*)'Error: mod_netcdf_write_fast, &
            &sub_netcdf_write_fast_i2D'
       WRITE(lun_error,*)'NetCDF file name has to finish with .nc'
       WRITE(lun_error,*)' final_netcdf_file_name(size_string-iTwo:size_string) =', &
            final_netcdf_file_name(size_string-iTwo:size_string)
       STOP
    END IF

    IF ( PRESENT(nb_netcdf_file_name)) THEN
       nb_final_netcdf_file_name = nb_netcdf_file_name
    ELSE
       nb_final_netcdf_file_name = -iOne
    END IF

    !- Add a digit number in the NetCDF file name -!
    !- just before ".nc"
    IF (nb_final_netcdf_file_name /= -iOne) THEN
       WRITE(char_format, 1000) size_string-iTwo, nb_digit, nb_digit
       WRITE(final_netcdf_file_name,FMT=TRIM(char_format)) &
            final_netcdf_file_name(1:size_string-iTwo)   , &
            nb_final_netcdf_file_name,'.nc'
    END IF

1000 FORMAT('(A',I2,',I',I2,'.',I2,',A3)')

    !======================!
    !- CREATE NetCDF FILE -!
    !======================!
    is_err = nf90_create                     ( &
         path  = TRIM(final_netcdf_file_name), & ! NetCDF filename created.
         cmode = NF90_64BIT_OFFSET           , & ! 64_bits record, Large File
         ncid  = nc_id                       )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !------------------!
    !- Set Dimensions -!
    !------------------!
    dim1 = SIZE(array_i2D,1)
    dim2 = SIZE(array_i2D,2)

    is_err=NF90_def_dim ( &
         ncid  = nc_id  , &
         name  = 'dim1' , &
         len   = dim1   , &
         dimid = dim1_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    is_err=NF90_def_diM ( &
         ncid  = nc_id  , &
         name  = 'dim2' , &
         len   = dim2   , &
         dimid = dim2_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-----------------------!
    !- Variable Definition -!
    !-----------------------!
    is_err=NF90_def_var                    ( &
         ncid   = nc_id                    , &
         name   = TRIM(final_variable_name), &
         xtype  = NF90_INT                 , &
         dimids = (/ dim1_id,dim2_id /)    , &
         varid  = var_id                  )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !------------------------------!
    !- Close the header defintion -!
    !------------------------------!
    is_err =  NF90_enddef(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-------------------------------!
    !- Put data in the NetCDF file -!
    !-------------------------------!
    is_err = NF90_put_var   ( &
         ncid   = nc_id      , &
         varid  = var_id     , &
         values = array_i2D )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-------------------------!
    !- Close the netcdf file -!
    !-------------------------!
    is_err = nf90_close(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !!write(lun_dbg,*)' -db- Exit sub_netcdf_write_fast_i2D'

  END SUBROUTINE sub_netcdf_write_fast_i2D

  !********************************************************************!
  !- sub_netcdf_write_fast_r1D - (October - 2012)
  !
  !- Author: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Create a NetCDF File and store in it a vector of reals
  !
  !********************************************************************!
  SUBROUTINE sub_netcdf_write_fast_r1D( &
       array_r1D                      , & ! Vector of reals.
       variable_name                  , & ! NetCDF variable name (opt).
       netcdf_file_name               , & ! NetCDF file name (opt).
       nb_netcdf_file_name            )   ! Add a number in the NetCDF 
    !                                     ! file name (opt).

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL(kind = rprec),    DIMENSION(:), INTENT(in) :: array_r1D
    CHARACTER(len = *),        OPTIONAL, INTENT(in) :: variable_name
    CHARACTER(len = *),        OPTIONAL, INTENT(in) :: netcdf_file_name
    INTEGER(kind = iprec),     OPTIONAL, INTENT(in) :: nb_netcdf_file_name

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    INTEGER(kind = iprec), PARAMETER :: nb_digit = iFour

    CHARACTER(len=48)     :: char_format
    INTEGER(kind = iprec) :: size_string

    CHARACTER(len = 32)   :: final_netcdf_file_name
    CHARACTER(len = 32)   :: final_variable_name
    INTEGER(kind = iprec) :: nb_final_netcdf_file_name
    INTEGER(kind = iprec) :: is_err

    INTEGER(kind = iprec) :: dim1

    INTEGER(kind = iprec) :: nc_id
    INTEGER(kind = iprec) :: var_id
    INTEGER(kind = iprec) :: dim1_id

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!

    !======================================!
    !- SECTION 1: test optional arguments -!
    !======================================!
    IF (PRESENT(variable_name)) THEN
       final_variable_name = TRIM(variable_name)
    ELSE
       final_variable_name = 'array_r1D'
    END IF

    IF (PRESENT(netcdf_file_name)) THEN
       final_netcdf_file_name = TRIM(netcdf_file_name)
    ELSE
       final_netcdf_file_name = 'netcdf_file.nc'
    END IF

    !- Test if the end of the netcdf file is ".nc".   -!
    !- If not stop the program with an error message. -!
    size_string = LEN_TRIM(final_netcdf_file_name)
    IF (final_netcdf_file_name(size_string-iTwo:size_string) /= '.nc') THEN
       WRITE(lun_error,*)'Error: mod_netcdf_write_fast'
       WRITE(lun_error,*)'NetCDF file name has to finish with .nc'
       WRITE(lun_error,*)' final_netcdf_file_name(size_string-iThree:size_string) =', &
            final_netcdf_file_name(size_string-iThree:size_string)
       STOP
    END IF

    IF ( PRESENT(nb_netcdf_file_name)) THEN
       nb_final_netcdf_file_name = nb_netcdf_file_name
    ELSE
       nb_final_netcdf_file_name = -iOne
    END IF

    !- Add a digit number in the NetCDF file name -!
    !- just before ".nc"           
    IF (nb_final_netcdf_file_name /= -iOne) THEN
       WRITE(char_format, 1000) size_string-iTwo, nb_digit, nb_digit
       WRITE(final_netcdf_file_name,FMT=TRIM(char_format)) &
            final_netcdf_file_name(1:size_string-iThree) , &
            nb_final_netcdf_file_name,'.nc'
    END IF

1000 FORMAT('(A',I2,',I',I2,'.',I2,',A3)')


    !======================!
    !- CREATE NetCDF FILE -!
    !======================!
    is_err = nf90_create                     ( &
         path  = TRIM(final_netcdf_file_name), & ! NetCDF filename created.
         cmode = NF90_64BIT_OFFSET           , & ! 64_bits record, Large File
         ncid  = nc_id                       )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-----------------!
    !- Set Dimension -!
    !-----------------!
    dim1 = SIZE(array_r1D,1)

    is_err=NF90_def_dim ( &
         ncid  = nc_id  , &
         name  = 'dim1' , &
         len   = dim1   , &
         dimid = dim1_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-----------------------!
    !- Variable Definition -!
    !-----------------------!
    is_err=NF90_def_var                    ( &
         ncid   = nc_id                    , &
         name   = TRIM(final_variable_name), &
         xtype  = NF90_DOUBLE              , &
         dimids = (/ dim1_id /)            , &
         varid  = var_id                   )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !------------------------------!
    !- Close the header defintion -!
    !------------------------------!
    is_err =  NF90_enddef(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-------------------------------!
    !- Put data in the NetCDF file -!
    !-------------------------------!
    is_err = NF90_put_var   ( &
         ncid   = nc_id     , &
         varid  = var_id    , &
         values = array_r1D )

    !-------------------------!
    !- Close the netcdf file -!
    !-------------------------!
    is_err = nf90_close(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

  END SUBROUTINE sub_netcdf_write_fast_r1D

  !********************************************************************!
  !- sub_netcdf_write_fast_r2D - (October - 2012)
  !
  !- Author: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Create a NetCDF File and store in it a 2D array of reals
  !
  !********************************************************************!
  SUBROUTINE sub_netcdf_write_fast_r2D( &
       array_r2D                      , & ! 2D array of reals.
       variable_name                  , & ! NetCDF variable name (opt).
       netcdf_file_name               , & ! NetCDF file name (opt).
       nb_netcdf_file_name            )   ! Add a number in the NetCDF 
    !                                     ! file name (opt).

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL(kind = rprec), DIMENSION(:,:), INTENT(in) :: array_r2D
    CHARACTER(len = *),        OPTIONAL, INTENT(in) :: variable_name
    CHARACTER(len = *),        OPTIONAL, INTENT(in) :: netcdf_file_name
    INTEGER(kind = iprec),     OPTIONAL, INTENT(in) :: nb_netcdf_file_name
 
    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    INTEGER(kind = iprec), PARAMETER :: nb_digit = iFour

    CHARACTER(len=48)   :: char_format
    INTEGER(kind = iprec) :: size_string

    CHARACTER(len = 32)   :: final_netcdf_file_name
    CHARACTER(len = 32)   :: final_variable_name
    INTEGER(kind = iprec) :: nb_final_netcdf_file_name
    INTEGER(kind = iprec) :: is_err

    INTEGER(kind = iprec) :: dim1
    INTEGER(kind = iprec) :: dim2

    INTEGER(kind = iprec) :: nc_id
    INTEGER(kind = iprec) :: var_id
    INTEGER(kind = iprec) :: dim1_id
    INTEGER(kind = iprec) :: dim2_id

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(*,*)' -db- Enter in sub_netcdf_write_fast_r2D'

    !======================================!
    !- SECTION 1: test optional arguments -!
    !======================================!
    IF (PRESENT(variable_name)) THEN
       final_variable_name = TRIM(variable_name)
    ELSE
       final_variable_name = 'array_r2D'
    END IF

   IF (PRESENT(netcdf_file_name)) THEN
       final_netcdf_file_name = TRIM(netcdf_file_name)
    ELSE
       final_netcdf_file_name = 'netcdf_file.nc'
    END IF

    !- Test if the end of the netcdf file is ".nc".   -!
    !- If not stop the program with an error message. -!
    size_string = LEN_TRIM(final_netcdf_file_name)
    IF (final_netcdf_file_name(size_string-iTwo:size_string) /= '.nc') THEN
       WRITE(lun_error,*)'Error: mod_netcdf_write_fast'
       WRITE(lun_error,*)'NetCDF file name has to finish with .nc'
       WRITE(lun_error,*)' final_netcdf_file_name(size_string-iTwo:size_string) =', &
            final_netcdf_file_name(size_string-iTwo:size_string)
       STOP
    END IF

    IF ( PRESENT(nb_netcdf_file_name)) THEN
       nb_final_netcdf_file_name = nb_netcdf_file_name
    ELSE
       nb_final_netcdf_file_name = -iOne
    END IF

    !- Add a digit number in the NetCDF file name -!
    !- just before ".nc"
    IF (nb_final_netcdf_file_name /= -iOne) THEN
       WRITE(char_format, 1000) size_string-iTwo, nb_digit, nb_digit
       WRITE(final_netcdf_file_name, FMT=TRIM(char_format)) &
            final_netcdf_file_name(1:size_string-iTwo), &
            nb_final_netcdf_file_name,'.nc'
    END IF

1000 FORMAT('(A',I2,',I',I2,'.',I2,',A3)')

    !======================!
    !- CREATE NetCDF FILE -!
    !======================!
    is_err = nf90_create                     ( &
         path  = TRIM(final_netcdf_file_name), & ! NetCDF filename created.
         cmode = NF90_64BIT_OFFSET           , & ! 64_bits record, Large File
         ncid  = nc_id                       )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !------------------!
    !- Set Dimensions -!
    !------------------!
    dim1 = SIZE(array_r2D,1)
    dim2 = SIZE(array_r2D,2)

    is_err=NF90_def_dim ( &
         ncid  = nc_id  , &
         name  = 'dim1' , &
         len   = dim1   , &
         dimid = dim1_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    is_err=NF90_def_dim ( &
         ncid  = nc_id  , &
         name  = 'dim2' , &
         len   = dim2   , &
         dimid = dim2_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-----------------------!
    !- Variable Definition -!
    !-----------------------!
    is_err=NF90_def_var                    ( &
         ncid   = nc_id                    , &
         name   = TRIM(final_variable_name), &
         xtype  = NF90_DOUBLE              , &
         dimids = (/ dim1_id,dim2_id /)    , &
         varid  = var_id                   )

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !------------------------------!
    !- Close the header defintion -!
    !------------------------------!
    is_err =  NF90_enddef(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-------------------------------!
    !- Put data in the NetCDF file -!
    !-------------------------------!
    is_err = NF90_put_var   ( &
         ncid   = nc_id     , &
         varid  = var_id    , &
         values = array_r2D )

   IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !-------------------------!
    !- Close the netcdf file -!
    !-------------------------!
    is_err = nf90_close(ncid = nc_id)

    IF (is_err /= nf90_noerr) THEN
       STOP
    END IF

    !!write(*,*)' -db- Exit sub_netcdf_write_fast_r2D'

  END SUBROUTINE sub_netcdf_write_fast_r2D

  !********************************************************************!
  !- sub_netcdf_write_fast_l2D - (October - 2012)
  !
  !- Author: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Create a NetCDF File and store in it a 2D array of logicals.
  ! Logicals data are transformed into integers
  ! 0 = .FALSE. and 1 = .TRUE.
  !
  !********************************************************************!

  SUBROUTINE sub_netcdf_write_fast_l2D( &
       array_l2D                      , & ! 2D array of logicals.
       variable_name                  , & ! NetCDF variable name (opt).
       netcdf_file_name               , & ! NetCDF file name (opt).
       nb_netcdf_file_name            )   ! Add a number in the NetCDF 
    !                                     ! file name (opt).

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    LOGICAL, DIMENSION(:,:), INTENT(in) :: array_l2D
    CHARACTER(len = *),        OPTIONAL, INTENT(in) :: variable_name
    CHARACTER(len = *),        OPTIONAL, INTENT(in) :: netcdf_file_name
    INTEGER(kind = iprec),     OPTIONAL, INTENT(in) :: nb_netcdf_file_name

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    CHARACTER(len = 32)   :: final_netcdf_file_name
    CHARACTER(len = 32)   :: final_variable_name
    INTEGER(kind = iprec) :: nb_final_netcdf_file_name

    INTEGER(kind = iprec), DIMENSION(:,:), ALLOCATABLE :: array_i2D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!

    !======================================!
    !- SECTION 1: test optional arguments -!
    !======================================!
    IF (PRESENT(variable_name)) THEN
       final_variable_name = TRIM(variable_name)
    ELSE
       final_variable_name = 'array_l2D'
    END IF

    IF (PRESENT(netcdf_file_name)) THEN
       final_netcdf_file_name = TRIM(netcdf_file_name)
    ELSE
       final_netcdf_file_name = 'netcdf_file.nc'
    END IF

    IF ( PRESENT(nb_netcdf_file_name)) THEN
       nb_final_netcdf_file_name = nb_netcdf_file_name
    ELSE
       nb_final_netcdf_file_name = -iOne
    END IF

    !- Dynamic memory allocation -!
    ALLOCATE(array_i2D(Size(array_l2D,dim=1),Size(array_l2D,dim=2)))

    !- transform logicals in intergers -!
    WHERE(array_l2D(:,:))
       array_i2D = iOne   ! 1 = .TRUE.
    ELSEWHERE
       array_i2D = iZero  ! 0 = .TRUE.
    END WHERE

    !- Call the integer 2D subroutine -!
    CALL sub_netcdf_write_fast_i2D          ( &
         array_i2D,TRIM(final_variable_name), &
         TRIM(final_netcdf_file_name)       , &
         nb_final_netcdf_file_name          )

    !- Dynamic memory deallocation -!
    DEALLOCATE(array_i2D)

  END SUBROUTINE sub_netcdf_write_fast_l2D


END MODULE mod_netcdf_write_fast
