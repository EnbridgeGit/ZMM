*----------------------------------------------------------------------*
***INCLUDE LZMM_SA_WFF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ME38_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM me38_sa .
  DATA: lv_item     TYPE i,
        lv_schedule TYPE i,
        lt_item     TYPE TABLE OF bapimeoutitem,
        lt_schedule TYPE TABLE OF bapimeoutschedule,
        lt_return   TYPE TABLE OF bapiret2.

*  SET PARAMETER ID 'VRT' FIELD gv_ebeln.
  SET PARAMETER ID 'SAG' FIELD gv_ebeln.
  CALL TRANSACTION 'ME38' AND SKIP FIRST SCREEN.

  CALL FUNCTION 'RZL_SLEEP'
    EXPORTING
      seconds        = 4
    EXCEPTIONS
      argument_error = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  break sahmad.


  CALL FUNCTION 'BAPI_SAG_GETDETAIL'
    EXPORTING
      purchasingdocument          = gv_ebeln
     item_data                   = 'X'
*     ACCOUNT_DATA                = ' '
      schedule_data               = 'X'
*     SHIPPING_DATA               = ' '
*     SC_COMPONENT_DATA           = ' '
*     EXPORT_DATA                 = ' '
*     CONDITION_DATA              = ' '
*     TEXT_DATA                   = ' '
*     PARTNER_DATA                = ' '
*   IMPORTING
*     HEADER                      =
*     HEAD_EXPORT_IMPORT          =
   TABLES
     item                        = lt_item
*     ACCOUNT                     =
     schedule                    = lt_schedule
*     DELIVERY_ADDRESS            =
*     SC_COMPONENT                =
*     ITEM_COND_VALIDITY          =
*     ITEM_COND_SCALE_VALUE       =
*     ITEM_COND_SCALE_QUAN        =
*     ITEM_CONDITION              =
*     SHIPPING                    =
*     EXPORT_IMPORT               =
*     ITEM_TEXT                   =
*     HEADER_TEXT                 =
*     HEAD_COND_VALIDITY          =
*     HEAD_CONDITION              =
*     HEAD_COND_SCALE_VAL         =
*     HEAD_COND_SCALE_QUAN        =
*     PARTNER                     =
*     EXTENSIONOUT                =
     return                      = lt_return.
  IF lt_schedule[] IS NOT INITIAL.
    DELETE lt_schedule WHERE delete_ind <> space.
    SORT lt_schedule BY item_no.
    DELETE ADJACENT DUPLICATES FROM lt_schedule COMPARING item_no.
  ENDIF.
  DELETE lt_item WHERE delete_ind <> space.
  DESCRIBE TABLE lt_schedule LINES lv_schedule.
  DESCRIBE TABLE lt_item LINES lv_item.
  IF lt_schedule[] IS INITIAL OR
     lv_schedule < lv_item.    "Schedule lines for all items are updated?
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'Schedule Agreement Delivery Schedule'
        msgid = 'ME'
        msgty = 'E'
        msgno = 303
        msgv1 = 'Delivery Schedule Lines are not updated.'
        msgv2 = ', Work item stays in inbox'
*       MSGV3 =
*       MSGV4 =
      .
    gv_schedule_updated = space.
  ELSE.
    gv_schedule_updated = 'X'.
    PERFORM raise_event USING 'ZDeliverySchedule'.
**
  ENDIF.
  PERFORM set_screen.
ENDFORM.                    " ME38_SA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_ADDR_KEY  text
*----------------------------------------------------------------------*
FORM get_data CHANGING iv_ebeln LIKE ekko-ebeln.

*  SELECT adrnr FROM lfa1
*    INTO cs_addr_key-addrnumber
*   WHERE lifnr = nast-parnr.
*  ENDSELECT.

*  cs_addr_key-addr_type  = '1'.

  SELECT SINGLE * FROM ekko INTO gs_ekko
   WHERE ebeln = iv_ebeln.

  SELECT SINGLE * FROM ekpo INTO gs_ekpo
   WHERE ebeln = gs_ekko-ebeln AND ebelp = 1.

  SELECT SINGLE * FROM lfa1 INTO gs_lfa1
   WHERE lifnr = gs_ekko-lifnr.

  SELECT SINGLE * FROM t001 INTO gs_t001
   WHERE bukrs = gs_ekpo-bukrs.

  SELECT SINGLE * FROM t880 INTO gs_t880
   WHERE rcomp = gs_t001-bukrs.

  SELECT SINGLE * FROM t024 INTO gs_t024
   WHERE ekgrp = gs_ekko-ekgrp.

  SELECT SINGLE * FROM eket INTO gs_eket
   WHERE ebeln = gs_ekko-ebeln AND ebelp = 1.

  SELECT SINGLE * FROM lfm1 INTO gs_lfm1
   WHERE lifnr = gs_lfa1-lifnr.

*Start of change                 COG

    SELECT SINGLE * FROM t001w INTO fs_t001w
     WHERE werks = gs_ekpo-werks.

    SELECT single *  FROM konv INTO fs_konv
         WHERE knumv EQ gs_ekko-knumv
           AND kposn EQ gs_ekpo-ebelp
           AND kschl EQ  'PBXX'.
    IF gs_ekko-zzconprice is INITIAL
      and fs_konv-kbetr is NOT INITIAL.

      SELECT single * FROM T006A into fs_t006a
        WHERE MSEHI = fs_konv-kmein.

      TRANSLATE fs_t006a-mseh6 TO UPPER CASE.
      v_kbetr = fs_konv-kbetr / fs_konv-kpein.
      SHIFT v_kbetr LEFT DELETING LEADING space.
      CONCATENATE fs_konv-waers '/' fs_t006a-mseh6 INTO v_cur.
      CONCATENATE v_kbetr v_cur INTO gs_ekko-zzconprice SEPARATED BY space.

    ENDIF.

*End of change                   COG
*----------------------------------------------------------------------*
* Loading parameters with filler for layout spacing purposes

  PERFORM get_vendor_potext.
  PERFORM get_schagree_hdrtxt.
*
*  CASE nast-ldest.
*    WHEN ' '.
*      MOVE 'EMAIL' TO w_comm_method.
*    WHEN 'ZFAX'.
*      MOVE 'FAX' TO w_comm_method.
*    WHEN OTHERS.
  MOVE 'PRINTER' TO w_comm_method.
*  ENDCASE.

ENDFORM.                    "GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_POTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vendor_potext .
  CONCATENATE gs_lfm1-lifnr gs_lfm1-ekorg INTO w_tdname.

  SELECT tdobject tdname tdid tdspras FROM stxh INTO fs_stxh
   WHERE tdobject = 'LFM1'
     AND tdname LIKE w_tdname
     AND tdid = '0002'
     AND tdspras = 'EN'.
  ENDSELECT.

  IF sy-subrc = 0.

    CLEAR: w_tlinetab.
    REFRESH w_tlinetab.

    CALL FUNCTION 'READ_TEXT'
         EXPORTING
              id                      = fs_stxh-tdid
              language                = fs_stxh-tdspras
              name                    = fs_stxh-tdname
              object                  = fs_stxh-tdobject
*    importing
*         header                  =
         TABLES
              lines                   = w_tlinetab
         EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

    MOVE w_tlinetab[] TO w_potexttab.
  ENDIF.
ENDFORM.                    " GET_VENDOR_POTEXT
*&---------------------------------------------------------------------*
*&      Form  GET_SCHAGREE_HDRTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_schagree_hdrtxt .

  MOVE gs_ekko-ebeln TO w_tdname.

  SELECT tdobject tdname tdid tdspras FROM stxh INTO fs_stxh
   WHERE tdobject = 'EKKO'
     AND tdname LIKE w_tdname
     AND tdid = 'L01'
     AND tdspras = 'EN'.
  ENDSELECT.

  IF sy-subrc = 0.

    CLEAR: w_tlinetab.
    REFRESH w_tlinetab.

    CALL FUNCTION 'READ_TEXT'
         EXPORTING
              id                      = fs_stxh-tdid
              language                = fs_stxh-tdspras
              name                    = fs_stxh-tdname
              object                  = fs_stxh-tdobject
*    importing
*         header                  =
         TABLES
              lines                   = w_tlinetab
         EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

    LOOP AT w_tlinetab.
      CLEAR w_sahdrtexttab.

      IF w_tlinetab-tdline(7) = 'INCLUDE'.
        MOVE 'IN' TO w_sahdrtexttab-msg_type.
        MOVE w_tlinetab-tdformat TO w_sahdrtexttab-tdformat.
        SPLIT w_tlinetab-tdline AT '''' INTO:
              dummy1 w_sahdrtexttab-tdname w_remainder.
        SHIFT w_remainder LEFT DELETING LEADING space.
        SPLIT w_remainder AT space INTO:
              dummy2 w_sahdrtexttab-tdobject dummy3 w_sahdrtexttab-tdid.
        MOVE 'EN' TO w_sahdrtexttab-tdspras.
      ELSE.
        MOVE 'FT' TO w_sahdrtexttab-msg_type.
        MOVE w_tlinetab-tdformat TO w_sahdrtexttab-tdformat.
        MOVE w_tlinetab-tdline TO w_sahdrtexttab-tdline.
      ENDIF.

      APPEND w_sahdrtexttab.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_SCHAGREE_HDRTXT
