FUNCTION ZSRMF_CHECK_ATTACHMENT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJKY) TYPE  OBJKY
*"     VALUE(FILENAME) TYPE  CHAR200
*"  EXPORTING
*"     VALUE(W_EXIST) TYPE  CHAR1
*"----------------------------------------------------------------------

  DATA: LV_DRAD LIKE DRAD,
        LV_DMS LIKE DMS_DOC_FILES.

  W_EXIST = 'N'.

  SELECT SINGLE * FROM DRAD INTO LV_DRAD WHERE
      DOKAR = 'SRM' AND
      DOKOB = 'EBAN' AND
      OBJKY = OBJKY.

  IF SY-SUBRC = 0.
    SELECT SINGLE * FROM DMS_DOC_FILES INTO LV_DMS WHERE
      DOKAR = 'SRM' AND
      DOKNR = LV_DRAD-DOKNR AND
      DOKVR = LV_DRAD-DOKVR AND
      DOKTL = LV_DRAD-DOKTL AND
      FILENAME = FILENAME.
    IF SY-SUBRC = 0.
      W_EXIST = 'Y'.
    ENDIF.
  ENDIF.



ENDFUNCTION.
