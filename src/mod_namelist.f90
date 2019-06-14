!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- mod_namelist - (January - 2013)
!!
!!- Author: Nicolas.Grima@univ-brest.fr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!
!!
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_namelist

  !===================!
  !- USE association -!
  !===================!
  USE mod_precision
  USE mod_cst
  USE mod_lun

  !===============================!
  !- DECLARATIONS of GLOBAL DATA -!
  !===============================!
  IMPLICIT NONE

  !----------!
  ! FSP item !
  !----------!
  !- FSP item pramaters -!
  INTEGER(kind = iprec)       :: ifsp = i_defn
  REAL   (kind = rprec)       :: rfsp = r_def
  LOGICAL                     :: lfsp = l_def
  CHARACTER(len = iSixtyFour) :: cfsp = c_def

  NAMELIST/FSP/ & !
       ifsp   , & ! an example of an  integer namelist parameter
       rfsp   , & ! an example of a      real namelist parameter
       lfsp   , & ! an example of a   logical namelist parameter
       cfsp       ! an example of a character namelist parameter

  !------------!
  ! PARAM item !
  !------------!
  !- PARAM item pramaters -!
  INTEGER(kind = iprec)       :: iparam = i_defn
  REAL   (kind = rprec)       :: rparam = r_def
  LOGICAL                     :: lparam = l_def
  CHARACTER(len = iSixtyFour) :: cparam = c_def

  NAMELIST/PARAM/ & !
       iparam   , & ! an example of an  integer namelist parameter
       rparam   , & ! an example of a      real namelist parameter
       lparam   , & ! an example of a   logical namelist parameter
       cparam       ! an example of a character namelist parameter

  !====================================!
  !- DECLARATIONS of INTERFACE BLOCKS -!
  !====================================!

CONTAINS

  !********************************************************************!
  !- sub_read_namelist - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-best.fr
  !********************************************************************!
  !
  !
  !
  !
  !
  !********************************************************************!

  SUBROUTINE sub_read_namelist( &
       verbose                 ) ! optional argument to select verbose mode or not

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    LOGICAL, OPTIONAL, INTENT(in) :: verbose

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    LOGICAL :: print_info

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_read_namelist'

    !- Test if the optional argument is present -!
    IF (PRESENT(verbose)) THEN
       IF (verbose) THEN
          print_info = .TRUE.
       ELSE
          print_info = .FALSE.
       ENDIF
    ELSE
       print_info = .FALSE.
    ENDIF

    !- print message in lun_standard -!
    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'============'
    WRITE(lun_standard,*)'= NAMELIST ='
    WRITE(lun_standard,*)'============'

    !-----------------------------!
    !- Opening the namelist file -!
    !-----------------------------!
    OPEN(unit=lun_nml, File='namelist', ACTION='READ')
    WRITE(lun_standard,*)' --- Successful Opening ---'

    !----------------------------------!
    !- Reading namelist file item FSP -!
    !----------------------------------!
    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)' - Reading FSP item:'
    REWIND(unit=lun_nml)
    READ(unit=lun_nml, nml=FSP)

    !-------------------------------!
    !- Printing namelist parameter -!
    !-------------------------------!
    IF (print_info) THEN
       WRITE(lun_standard,*)'   - ifsp   =', ifsp
       WRITE(lun_standard,*)'   - rfsp   =', rfsp
       WRITE(lun_standard,*)'   - lfsp   =', lfsp
       WRITE(lun_standard,*)'   - cfsp   =', TRIM(cfsp)
    ENDIF

    !------------------------------------!
    !- Reading namelist file item PARAM -!
    !------------------------------------!
    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)' - Reading PARAM item:'
    REWIND(unit=lun_nml)
    READ(unit=lun_nml, nml=PARAM)

    IF (print_info) THEN
       WRITE(lun_standard,*)'   - iparam =', iparam
       WRITE(lun_standard,*)'   - rparam =', rparam
       WRITE(lun_standard,*)'   - lparam =', lparam
       WRITE(lun_standard,*)'   - cparam =', TRIM(cparam)
    ENDIF

    !!write(lun_dbg,*)' -db- Exit sub_read_namelist'

  END SUBROUTINE sub_read_namelist

END MODULE mod_namelist
