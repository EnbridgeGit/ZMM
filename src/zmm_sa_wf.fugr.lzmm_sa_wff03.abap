*----------------------------------------------------------------------*
***INCLUDE LZMM_SA_WFF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_screen USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    "BDC_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  DATA: tval.

*  tval = fval(1).
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  IF bdcdata-fnam(10) = 'EKET-MENGE'.
    SHIFT bdcdata-fval LEFT DELETING LEADING space.
  ENDIF.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DELIVERY_ME38
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_EBELN  text
*----------------------------------------------------------------------*
FORM clear_delivery_me38  USING iv_ebeln TYPE ekko-ebeln.

  DATA: lv_group TYPE apqi-groupid,
        lv_keep  TYPE apqi-qerase VALUE 'X',
        lv_user  TYPE apqi-userid,
        ls_msg TYPE bapiret2,
        lv_error TYPE xfeld,
        lv_tcode TYPE tstc-tcode VALUE 'ME38',
        lv_group1 TYPE d0100-mapn.
  DATA: ls_text TYPE tline,
        lt_text TYPE TABLE OF tline.
  DATA: lt_seltab  TYPE TABLE OF rsparams,
        ls_seltab  LIKE LINE OF lt_seltab,
        lt_abaplist TYPE TABLE OF abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table.
  DATA: lv_tabix    TYPE sy-tabix,
        lv_item_no  TYPE ebelp,
        lv_qty      TYPE eket-menge VALUE 0,
        lv_qfield   TYPE char15,
        lv_cnt      TYPE n LENGTH 2,
        lt_item     TYPE TABLE OF bapimeoutitem,
        ls_item     TYPE bapimeoutitem,
        lt_schedule TYPE TABLE OF bapimeoutschedule,
        ls_schedule TYPE bapimeoutschedule,
        lt_return   TYPE TABLE OF bapiret2,
        lt_apqi     TYPE TABLE OF apqi,
        ls_apqi     TYPE apqi.

  ls_text-tdline = text-001.
  APPEND ls_text TO gt_bdc_rel_msg.
  CLEAR ls_text.
**********************Get Schedule Data
  break sahmad.
  CALL FUNCTION 'BAPI_SAG_GETDETAIL'
    EXPORTING
      purchasingdocument = iv_ebeln
      item_data          = 'X'
      schedule_data      = 'X'
    TABLES
      item               = lt_item
      schedule           = lt_schedule
      return             = lt_return.
  SORT lt_item BY item_no.
  SORT lt_schedule BY item_no sched_line.
******
  CLEAR: bdcdata.
  REFRESH: bdcdata.
  PERFORM bdc_screen USING 'SAPMM06E' '0205'.
  PERFORM bdc_field  USING 'RM06E-EVRTN' iv_ebeln.
  PERFORM bdc_field USING 'BDC_OKCODE' '/00'.
*
  PERFORM bdc_screen USING 'SAPMM06E' '0222'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'MALL'.  "select all
*
  PERFORM bdc_screen USING 'SAPMM06E' '0222'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'ET'.
*
  lv_cnt = 0.
  LOOP AT lt_schedule INTO ls_schedule.
    IF lv_item_no <> ls_schedule-item_no.
      lv_item_no = ls_schedule-item_no.
      lv_cnt = 0.
    ENDIF.
    lv_cnt = lv_cnt + 1.
    CONCATENATE 'EKET-MENGE(' lv_cnt ')' INTO lv_qfield.
    PERFORM bdc_screen USING 'SAPMM06E' '1117'.
    PERFORM bdc_field  USING lv_qfield 0.
    PERFORM bdc_field USING 'BDC_OKCODE' '/00'.
*
    PERFORM bdc_screen USING 'SAPMM06E' '1117'.
    PERFORM bdc_field USING 'BDC_OKCODE' 'NEXP'.
  ENDLOOP.
  PERFORM bdc_screen USING 'SAPMM06E' '0222'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'BU'.
**********************
  lv_user = sy-uname.
  CONCATENATE 'DL' iv_ebeln INTO lv_group.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = lv_group
*     HOLDDATE          =
      keep              = lv_keep
      user              = lv_user
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.
  IF sy-subrc <> 0.
    lv_error = 'X'.
    ls_msg-type = 'E'.
    CASE sy-subrc.
      WHEN 1.
        ls_msg-message = 'BDC_OPEN_GROUP - Invalid Group(ME38).'.
      WHEN 2.
        ls_msg-message = 'BDC_OPEN_GROUP - Group is locked(ME38).'.
      WHEN 3.
        ls_msg-message = 'BDC_OPEN_GROUP - Invalid Hold Date(ME38).'.
      WHEN 4.
        ls_msg-message = 'BDC_OPEN_GROUP - Internal Error(ME38).'.
      WHEN 5.
        ls_msg-message = 'BDC_OPEN_GROUP - Queue Error(ME38)'.
      WHEN 6.
        ls_msg-message = 'BDC_OPEN_GROUP - Running.(ME38)'.
      WHEN 7.
        ls_msg-message = 'BDC_OPEN_GROUP - System Lock Error(ME38)'.
      WHEN 8.
        ls_msg-message = 'BDC_OPEN_GROUP - Invalid User.(ME38)'.
    ENDCASE.
    IF ls_msg-message IS NOT INITIAL.
      APPEND ls_msg TO gt_bdc_error_msg.
    ENDIF.
  ENDIF.
**************
**INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = lv_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    lv_error = 'X'.
    ls_msg-type = 'E'.
    CASE sy-subrc.
      WHEN 1.
        ls_msg-message = 'BDC_INSERT - Internal Error(ME38)'.
      WHEN 2.
        ls_msg-message = 'BDC_INSERT - Not Open(ME38)'.
      WHEN 3.
        ls_msg-message = 'BDC_INSERT - Queue Error(ME38)'.
      WHEN 4.
        ls_msg-message = 'BDC_INSERT - Invalid T-code'.
    ENDCASE.
    IF ls_msg-message IS NOT INITIAL.
      APPEND ls_msg TO gt_bdc_error_msg.
    ENDIF.
  ENDIF.
**End of Insert Session
**CLOSE_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc <> 0.
    lv_error = 'X'.
    ls_msg-type = 'E'.
    ls_msg-message = 'Error while closing BDC Session(ME38)..'.
    APPEND ls_msg TO gt_bdc_error_msg.
  ENDIF.
**end of Close session
**Release BDC sesscion if No error
  IF lv_error IS INITIAL.
    lv_group1 = lv_group.
    SUBMIT rsbdcsub WITH mappe EQ lv_group1
    WITH von EQ sy-datum
    WITH bis EQ sy-datum
    WITH fehler EQ 'X'
    WITH z_verarb EQ 'X'
*  WITH LOGALL EQ 'X'
    EXPORTING LIST TO MEMORY
    AND RETURN.
***Capture message from BDC
    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_abaplist
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = lt_list
      TABLES
        listobject         = lt_abaplist
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT lt_list INTO ls_list.
      ls_text-tdline = ls_list.
      APPEND ls_text TO lt_text.
    ENDLOOP.
    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = lt_abaplist.
*****Get BDC status from table
    WAIT UP TO 10 SECONDS.
******
    SELECT * FROM apqi INTO TABLE lt_apqi
      WHERE groupid = lv_group1
        AND creator = sy-uname
        AND credate = sy-datum.
    SORT lt_apqi BY credate cretime DESCENDING.
    IF lt_apqi[] IS NOT INITIAL.
      READ TABLE lt_apqi INTO ls_apqi INDEX 1.
      IF ls_apqi-qstate = 'E'.
        CONCATENATE '******SESSION' lv_group1 text-003 INTO ls_text-tdline
                                              SEPARATED BY space.
        APPEND ls_text TO gt_bdc_rel_msg.
        CONCATENATE '******SESSION' lv_group1 text-003 INTO ls_msg-message
                                              SEPARATED BY space.
        APPEND ls_msg TO gt_bdc_error_msg.
      ENDIF.
    ENDIF.
    APPEND LINES OF lt_text TO gt_bdc_rel_msg.
****Wait so that ME38 complete its process
      WAIT UP TO 10 SECONDS.
    ENDIF.
**************************
    IF gt_bdc_rel_msg[] IS INITIAL.
      CLEAR lt_text.
      ls_text-tdline = 'No BDC Release message...'.
      APPEND ls_text TO lt_text.
      APPEND LINES OF lt_text TO gt_bdc_rel_msg.
    ENDIF.

  ENDFORM.                    " CLEAR_DELIVERY_ME38
*&---------------------------------------------------------------------*
*&      Form  DELETE_ME32L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_EBELN  text
*----------------------------------------------------------------------*
FORM delete_me32l  USING iv_ebeln TYPE ekko-ebeln.
  DATA: lv_group TYPE apqi-groupid,
        lv_keep  TYPE apqi-qerase VALUE 'X',
        lv_user  TYPE apqi-userid,
        ls_msg TYPE bapiret2,
        lv_error TYPE xfeld,
        lv_tcode TYPE tstc-tcode VALUE 'ME32L',
        lv_group1 TYPE d0100-mapn.
  DATA: ls_text TYPE tline,
        lt_text TYPE TABLE OF tline.
  DATA: lt_seltab  TYPE TABLE OF rsparams,
        ls_seltab  LIKE LINE OF lt_seltab,
        lt_abaplist TYPE TABLE OF abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table,
        lt_apqi TYPE TABLE OF apqi,
        ls_apqi TYPE apqi.

  ls_text-tdline = space.   "Blank line
  APPEND ls_text TO gt_bdc_rel_msg.

  ls_text-tdline = text-002.
  APPEND ls_text TO gt_bdc_rel_msg.
  CLEAR ls_text.

  lv_user = sy-uname.
  CONCATENATE 'SA' iv_ebeln INTO lv_group.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = lv_group
*     HOLDDATE          =
      keep              = lv_keep
      user              = lv_user
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.
  IF sy-subrc <> 0.
    lv_error = 'X'.
    ls_msg-type = 'E'.
    CASE sy-subrc.
      WHEN 1.
        ls_msg-message = 'BDC_OPEN_GROUP - Invalid Group(ME32L).'.
      WHEN 2.
        ls_msg-message = 'BDC_OPEN_GROUP - Group is locked(ME32L).'.
      WHEN 3.
        ls_msg-message = 'BDC_OPEN_GROUP - Invalid Hold Date(ME32L).'.
      WHEN 4.
        ls_msg-message = 'BDC_OPEN_GROUP - Internal Error(ME32L).'.
      WHEN 5.
        ls_msg-message = 'BDC_OPEN_GROUP - Queue Error(ME32L)'.
      WHEN 6.
        ls_msg-message = 'BDC_OPEN_GROUP - Running.(ME32L)'.
      WHEN 7.
        ls_msg-message = 'BDC_OPEN_GROUP - System Lock Error(ME32L)'.
      WHEN 8.
        ls_msg-message = 'BDC_OPEN_GROUP - Invalid User.(ME32L)'.
    ENDCASE.
    IF ls_msg-message IS NOT INITIAL.
      APPEND ls_msg TO gt_bdc_error_msg.
    ENDIF.
  ENDIF.
**************
  CLEAR: bdcdata.
  REFRESH: bdcdata.

  PERFORM bdc_screen USING 'SAPMM06E' '0205'.
  PERFORM bdc_field  USING 'RM06E-EVRTN' iv_ebeln.
  PERFORM bdc_field USING 'BDC_OKCODE' '/00'.
*
  PERFORM bdc_screen USING 'SAPMM06E' '0220'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'MALL'.

  PERFORM bdc_screen USING 'SAPMM06E' '0220'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'DL'.

  PERFORM bdc_screen USING 'SAPMM06E' '0220'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'BU'.

**INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = lv_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.

  IF sy-subrc <> 0.
    lv_error = 'X'.
    ls_msg-type = 'E'.
    CASE sy-subrc.
      WHEN 1.
        ls_msg-message = 'BDC_INSERT - Internal Error(ME32L)'.
      WHEN 2.
        ls_msg-message = 'BDC_INSERT - Not Open(ME32L)'.
      WHEN 3.
        ls_msg-message = 'BDC_INSERT - Queue Error(ME32L)'.
      WHEN 4.
        ls_msg-message = 'BDC_INSERT - Invalid T-code(ME32L)'.
    ENDCASE.
    IF ls_msg-message IS NOT INITIAL.
      APPEND ls_msg TO gt_bdc_error_msg.
    ENDIF.
  ENDIF.
**End of Insert Session
**CLOSE_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc <> 0.
    lv_error = 'X'.
    ls_msg-type = 'E'.
    ls_msg-message = 'Error while closing BDC Session..(ME32L)'.
    APPEND ls_msg TO gt_bdc_error_msg.
  ENDIF.
**end of Close session
**Release BDC sesscion if No error
  IF lv_error IS INITIAL.
    lv_group1 = lv_group.
    SUBMIT rsbdcsub WITH mappe EQ lv_group1
    WITH von EQ sy-datum
    WITH bis EQ sy-datum
    WITH fehler EQ 'X'
    WITH z_verarb EQ 'X'
*  WITH LOGALL EQ 'X'
    EXPORTING LIST TO MEMORY
    AND RETURN.
***Capture message from BDC
    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_abaplist
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = lt_list
      TABLES
        listobject         = lt_abaplist
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT lt_list INTO ls_list.
      ls_text-tdline = ls_list.
      APPEND ls_text TO lt_text.
    ENDLOOP.
    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = lt_abaplist.
*****Get BDC status from table
    WAIT UP TO 10 SECONDS.
***
    SELECT * FROM apqi INTO TABLE lt_apqi
      WHERE groupid = lv_group1
        AND creator = sy-uname
        AND credate = sy-datum.
    SORT lt_apqi BY credate cretime DESCENDING.
    IF lt_apqi[] IS NOT INITIAL.
      READ TABLE lt_apqi INTO ls_apqi INDEX 1.
      IF ls_apqi-qstate = 'E'.
        CONCATENATE '******SESSION' lv_group1 text-003 INTO ls_text-tdline
                                                        SEPARATED BY space.
        APPEND ls_text TO gt_bdc_rel_msg.
      ENDIF.
    ENDIF.
**************************
    IF lt_text[] IS INITIAL.
      ls_text-tdline = 'No BDC Release message for ME32L (Delete).'.
      APPEND ls_text TO lt_text.
    ENDIF.
    APPEND LINES OF lt_text[] TO gt_bdc_rel_msg[].
***End of Capture message
  ENDIF.
ENDFORM.                    " DELETE_ME32L
