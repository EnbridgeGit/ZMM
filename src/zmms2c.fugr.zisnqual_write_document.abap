FUNCTION zisnqual_write_document.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJECTID) TYPE  CDHDR-OBJECTID
*"     VALUE(TCODE) TYPE  CDHDR-TCODE
*"     VALUE(UTIME) TYPE  CDHDR-UTIME
*"     VALUE(UDATE) TYPE  CDHDR-UDATE
*"     VALUE(USERNAME) TYPE  CDHDR-USERNAME
*"     VALUE(PLANNED_CHANGE_NUMBER) TYPE  CDHDR-PLANCHNGNR DEFAULT
*"       SPACE
*"     VALUE(OBJECT_CHANGE_INDICATOR) TYPE  CDHDR-CHANGE_IND
*"     VALUE(PLANNED_OR_REAL_CHANGES) TYPE  CDHDR-CHANGE_IND DEFAULT
*"       SPACE
*"     VALUE(NO_CHANGE_POINTERS) TYPE  CDHDR-CHANGE_IND DEFAULT SPACE
*"     VALUE(N_ZISNQUAL) TYPE  ZISNQUAL
*"     VALUE(O_ZISNQUAL) TYPE  ZISNQUAL
*"     VALUE(UPD_ZISNQUAL) TYPE  CDPOS-CHNGIND
*"----------------------------------------------------------------------

  CALL FUNCTION 'CHANGEDOCUMENT_OPEN'
    EXPORTING
      objectclass             = 'ZISNQUAL'
      objectid                = objectid
      planned_change_number   = planned_change_number
      planned_or_real_changes = planned_or_real_changes
    EXCEPTIONS
      sequence_invalid        = 1
      OTHERS                  = 2.

  CASE sy-subrc.
    WHEN 0.                                   "OK.
    WHEN 1. MESSAGE a600 WITH 'SEQUENCE INVALID'.
    WHEN 2. MESSAGE a600 WITH 'OPEN ERROR'.
  ENDCASE.

  CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
    EXPORTING
      tablename              = 'ZISNQUAL'
      workarea_old           = o_zisnqual
      workarea_new           = n_zisnqual
      change_indicator       = upd_zisnqual
      docu_delete            = 'X'
    EXCEPTIONS
      nametab_error          = 1
      open_missing           = 2
      position_insert_failed = 3
      OTHERS                 = 4.

  CASE sy-subrc.
    WHEN 0.                                "OK.
    WHEN 1. MESSAGE a600 WITH 'NAMETAB-ERROR'.
    WHEN 2. MESSAGE a600 WITH 'OPEN MISSING'.
    WHEN 3. MESSAGE a600 WITH 'INSERT ERROR'.
    WHEN 4. MESSAGE a600 WITH 'SINGLE ERROR'.
  ENDCASE.


  CALL FUNCTION 'CHANGEDOCUMENT_CLOSE'
    EXPORTING
      objectclass             = 'ZISNQUAL'
      objectid                = objectid
      date_of_change          = udate
      time_of_change          = utime
      tcode                   = tcode
      username                = username
      object_change_indicator = object_change_indicator
      no_change_pointers      = no_change_pointers
    EXCEPTIONS
      header_insert_failed    = 1
      object_invalid          = 2
      open_missing            = 3
      no_position_inserted    = 4
      OTHERS                  = 5.

  CASE sy-subrc.
    WHEN 0.                                   "OK.
    WHEN 1. MESSAGE a600 WITH 'INSERT HEADER FAILED'.
    WHEN 2. MESSAGE a600 WITH 'OBJECT INVALID'.
    WHEN 3. MESSAGE a600 WITH 'OPEN MISSING'.
*    WHEN 4. MESSAGE A600 WITH 'NO_POSITION_INSERTED'.
* do not abort, if positions are not inserted!!!
    WHEN 5. MESSAGE a600 WITH 'CLOSE ERROR'.
  ENDCASE.

ENDFUNCTION.
