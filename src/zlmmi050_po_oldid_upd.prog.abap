*&---------------------------------------------------------------------*
*& Report  ZLMMI050_PO_OLDID_UPD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT  ZLMMI050_PO_OLDID_UPD.
tables: EKKO.
data : lv_ebeln type ebeln,
       lv_ZZOLDEALID type ZOLDEALID.

PARAMETERS p_ebeln type EKKO-ebeln.


AT SELECTION-SCREEN .

  select SINGLE ebeln ZZOLDEALID
    from ekko into (lv_ebeln, lv_ZZOLDEALID )  where ebeln = p_ebeln and  ( bstyp =  'L'  or bstyp =  'K' ) and  ekorg = 'GASA'.
  if sy-subrc NE 0.

    message ' Please enter valid Purchasing Document' Type 'E'.

  endif.


START-OF-SELECTION.

  call SCREEN 100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ZUPD'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  case sy-ucomm.

*    when 'BACK'.
*      call SELECTION-SCREEN 1000.
*      leave program.
    when 'SAVE'.
      if EKKO-ZZOLDEALID is not INITIAL.

        PERFORM update_zzoldealid USING ekko-ebeln
                                                  ekko-zzoldealid.



      endif.


    when 'CHANGE'.
      LOOP AT screen.

        IF screen-name = 'EKKO-ZZOLDEALID'.

          screen-input = 1.

          modify screen.

        ENDIF.

      ENDLOOP.

    when 'LEAVE'.
*      leave program.
*leave to screen 1000.
LEAVE TO CURRENT TRANSACTION.
when 'BACK'.
 LEAVE TO CURRENT TRANSACTION.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA OUTPUT.

  if sy-ucomm = 'CHANGE'.
    LOOP AT screen.
      IF screen-name = 'EKKO-ZZOLDEALID'.
        screen-input = 1.
        modify screen.
      ENDIF.
    ENDLOOP.
  endif.

ekko-ebeln = lv_ebeln.
if lv_ZZOLDEALID is not INITIAL.

  ekko-ZZOLDEALID = lv_ZZOLDEALID.
  clear lv_ZZOLDEALID.
endif.
ENDMODULE.                 " GET_DATA  OUTPUT


FORM update_zzoldealid  USING    p_lv_ebeln
                                 p_lwa_header_zzoldealid.

  DATA: lta_oplk_dealid TYPE STANDARD TABLE OF zmms_oplk_dealid,
        lta_oplk_nodeal TYPE STANDARD TABLE OF zmms_oplk_dealid_return,
        lwa_oplk_dealid TYPE zmms_oplk_dealid.

  CALL FUNCTION 'ENQUEUE_EMEKKOE'
    EXPORTING
      mode_ekko      = 'E'
*     MODE_EKPO      = 'E'
      mandt          = sy-mandt
      ebeln          = p_lv_ebeln
      _wait          = '3'
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    CALL FUNCTION 'DEQUEUE_EMEKKOE'
      EXPORTING
        mode_ekko = 'E'
*       MODE_EKPO = 'E'
        mandt     = sy-mandt
        ebeln     = p_lv_ebeln.
  ELSE.
    lwa_oplk_dealid-ebeln = p_lv_ebeln.
    lwa_oplk_dealid-zzoldealid = p_lwa_header_zzoldealid.
    APPEND  lwa_oplk_dealid TO lta_oplk_dealid.

    CALL FUNCTION 'ZLMMI039_OPLK_DEALID'
      TABLES
        tab_dealid = lta_oplk_dealid
        tab_nodeal = lta_oplk_nodeal.
    IF sy-subrc EQ 0.
*    COMMIT WORK.
    ENDIF.
    if lta_oplk_nodeal is not INITIAL.
      Message 'Not a valid deal in SAP' type 'I'.
    else.
      Message 'Updated Successfully' type 'I'.
      LEAVE TO CURRENT TRANSACTION.
    endif.
  ENDIF.
ENDFORM.
