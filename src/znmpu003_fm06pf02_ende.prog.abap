* 2000/07/27 mdemeest 4.6B All changes indicated by 'UGL'            UGL
*                          Added PO end of year message              UGL
*eject
*----------------------------------------------------------------------*
* Beenden Formulardruck
*----------------------------------------------------------------------*
FORM ENDE.

* Unterschrift -------------------------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'LAST'
       EXCEPTIONS
            OTHERS  = 01.
  CLEAR SY-SUBRC.

* Folgeseitenzaehler löschen -----------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT  = 'NEXTPAGE'
            WINDOW   = 'NEXTPAGE'
            FUNCTION = 'DELETE'
       EXCEPTIONS
            OTHERS   = 01.
  CLEAR SY-SUBRC.

*-----------------------------  UGL Change -----------------------  UGL
* new message to appear on PO Nov 1 to Dec 15 only                  UGL
  concatenate sy-datum+4(2) sy-datum+6(2) into mthday.             "UGL
  if mthday > '1014' and                                           "UGL
     mthday < '1216'.                                              "UGL
      call function 'WRITE_FORM'                                   "UGL
           exporting element = 'YEAREND_MSG'                       "UGL
           exceptions others = 01.                                 "UGL
      clear sy-subrc.                                              "UGL
  endif.                                                           "UGL
*----------------------------- End of UGL Change -----------------  UGL


* Ende Formulardruck --------------------------------------------------*
  CALL FUNCTION 'CLOSE_FORM'
       IMPORTING
            RESULT = RESULT.
  IF RESULT-TDSPOOLID NE SPACE.
    SPOOLID = RESULT-TDSPOOLID.
    PERFORM PROTOCOL_UPDATE USING '320' SPOOLID SPACE SPACE SPACE.
  ENDIF.

  if result-userexit eq 'C' or
      result-userexit eq 'E'.
    retco = '9'.
  endif.

ENDFORM.

