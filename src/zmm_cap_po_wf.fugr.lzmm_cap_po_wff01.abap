*----------------------------------------------------------------------*
***INCLUDE LZMM_CAP_PO_WFF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ME22N_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM me22n_po .

  SET PARAMETER ID 'BES' FIELD gv_ebeln.
  CALL TRANSACTION 'ME22N' AND SKIP FIRST SCREEN.

ENDFORM.                    " ME22N_PO
*&---------------------------------------------------------------------*
*&      Form  SEND_FOR_RELEASE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_for_release_po .

  PERFORM raise_event USING 'ZSendForRelease'.

ENDFORM.                    " SEND_FOR_RELEASE_PO
*&---------------------------------------------------------------------*
*&      Form  SET_FOR_DELETE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_for_delete_po .
  DATA: ls_poheader TYPE bapimepoheader,
        ls_poheaderx TYPE bapimepoheaderx,
        lt_poitem TYPE TABLE OF bapimepoitem,
        ls_poitem TYPE bapimepoitem,
        lt_poitemx TYPE TABLE OF bapimepoitemx,
        ls_poitemx TYPE bapimepoitemx,
        lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2.
  DATA: lv_error TYPE xfeld,
        lv_msg TYPE char0256.

  CALL FUNCTION 'BAPI_PO_GETDETAIL1'
    EXPORTING
      purchaseorder = gv_ebeln
    IMPORTING
      poheader      = ls_poheader
    TABLES
      return        = lt_return
      poitem        = lt_poitem.

  LOOP AT lt_return INTO ls_return WHERE type = 'E'
                                      OR type = 'A'.
    CONCATENATE 'Error: ' ls_return-message INTO lv_msg
                                    SEPARATED BY space.
    lv_error = 'X'.
    EXIT.
  ENDLOOP.
  IF lv_error IS INITIAL.
    ls_poheader-delete_ind = 'L'.
    ls_poheaderx-po_number = ls_poheader-po_number.
    ls_poheaderx-delete_ind = 'L'.
    LOOP AT lt_poitem INTO ls_poitem.
      ls_poitem-delete_ind = 'L'.
      ls_poitemx-po_item = ls_poitem-po_item.
      ls_poitemx-po_itemx = 'X'.
      ls_poitemx-delete_ind = 'X'.
      APPEND ls_poitemx TO lt_poitemx.
      MODIFY lt_poitem FROM ls_poitem TRANSPORTING delete_ind.
    ENDLOOP.
    CLEAR lt_return.
    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = gv_ebeln
        poheader      = ls_poheader
        poheaderx     = ls_poheaderx
      TABLES
        return        = lt_return
        poitem        = lt_poitem
        poitemx       = lt_poitemx.
    LOOP AT lt_return INTO ls_return WHERE type = 'E'
                                        OR type = 'A'.
      CONCATENATE 'Error: ' ls_return-message INTO lv_msg
                                      SEPARATED BY space.
      lv_error = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.
  "if no error
  IF lv_error IS INITIAL.
    lv_msg = 'PO is flagged as deleted.'.
    PERFORM raise_event USING 'ZSetForDelete'.
    PERFORM set_screen.
  ENDIF.
  IF lv_msg IS NOT INITIAL.
    MESSAGE i017(zfi01) WITH lv_msg.
  ENDIF.
ENDFORM.                    " SET_FOR_DELETE_PO
*&---------------------------------------------------------------------*
*&      Form  ME2DP_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM me2dp_po .

  DATA:
        submit_message TYPE tline OCCURS 0,
        ls_text TYPE tline,
        lt_text TYPE TABLE OF tline,
        ivebeln TYPE ekko-ebeln.

  DATA: lt_seltab  TYPE TABLE OF rsparams,
        ls_seltab  LIKE LINE OF lt_seltab,
        lt_abaplist TYPE TABLE OF abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table.

*Set parmeter was not working for ME2DP for PO
*SET PARAMETER ID 'BES' FIELD gv_ebeln.
*CALL TRANSACTION 'ME2DP'. " AND SKIP FIRST SCREEN.

  break sahmad.
  ls_seltab-selname = 'SO_EBELN'.   "Name of parameter on submitted program
  ls_seltab-kind    = 'S'.  "'P' for parameter
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = gv_ebeln.
  APPEND ls_seltab TO lt_seltab.

********************
  SUBMIT rm06dpmonitor VIA SELECTION-SCREEN
      WITH SELECTION-TABLE lt_seltab
*      EXPORTING LIST TO MEMORY
      AND RETURN.
********************
*  CALL FUNCTION 'LIST_FROM_MEMORY'
*    TABLES
*      listobject       = lt_abaplist
*   EXCEPTIONS
*     NOT_FOUND        = 1
*     OTHERS           = 2.
*  IF sy-subrc <> 0.
**   Implement suitable error handling here
*  ENDIF.
*  CALL FUNCTION 'LIST_TO_ASCI'
*      IMPORTING
*        list_string_ascii  = lt_list
*      TABLES
*        listobject         = lt_abaplist
*      EXCEPTIONS
*        empty_list         = 1
*        list_index_invalid = 2
*        OTHERS             = 3.
*    IF sy-subrc <> 0.
*    ENDIF.
*    LOOP AT lt_list INTO ls_list.
*         ls_text-tdline = LS_LIST.
*         append ls_text to lt_text.
*    ENDLOOP.
*    CALL FUNCTION 'LIST_FREE_MEMORY'
*      TABLES
*        listobject = lt_abaplist.
*  submit_message[] = lt_text[].
***************************
*  IF submit_message[] is INITIAL.
*     clear lt_text.
*     ls_text-tdline = 'No Return message...'.
*     append ls_text to lt_text.
*     submit_message[] = lt_text[].
*  ENDIF.
******************

ENDFORM.                    " ME2DP_PO
*&---------------------------------------------------------------------*
*&      Form  REJECT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM reject_po .

  PERFORM raise_event USING 'ZReleaseReject'.

ENDFORM.                    " REJECT_PO
*&---------------------------------------------------------------------*
*&      Form  RAISE_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM raise_event  USING iv_event TYPE swr_struct-event.
  DATA: lt_container TYPE TABLE OF swcont.
  DATA: lv_objtype TYPE swr_struct-object_typ VALUE 'BUS2012',
        lv_objkey  TYPE swr_struct-object_key,
        ls_event_cont TYPE swr_cont,
        lt_event_cont TYPE TABLE OF swr_cont,
        lt_message_lines TYPE TABLE OF swr_messag,
        lt_message_struct TYPE TABLE OF swr_mstruc.

  IF iv_event = 'ZReleaseReject'.
    "Event container (parameters)
    ls_event_cont-element = 'RejectComments'.
    ls_event_cont-value   = gv_comments.
    APPEND ls_event_cont TO lt_event_cont.
  ENDIF.
  lv_objkey = gv_ebeln.
* Raise Event
  CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
    EXPORTING
      object_type             = lv_objtype
      object_key              = lv_objkey
      event                   = iv_event
*     COMMIT_WORK             = 'X'
*     EVENT_LANGUAGE          = SY-LANGU
*     LANGUAGE                = SY-LANGU
*     USER                    = SY-UNAME
*     IFS_XML_CONTAINER       =
*   IMPORTING
*     RETURN_CODE             =
*     EVENT_ID                =
   TABLES
     input_container         = lt_event_cont
     message_lines           = lt_message_lines
     message_struct          = lt_message_struct.

*  CALL FUNCTION 'SWE_EVENT_CREATE'
*    EXPORTING
*      objtype           = lv_objtype
*      objkey            = lv_objkey
*      event             = iv_event
*    TABLES
*      event_container   = lt_container
*    EXCEPTIONS
*      objtype_not_found = 1
*      OTHERS            = 2.
*
*  IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*          RAISING objtype_not_found.
*  ENDIF.
ENDFORM.                    " RAISE_EVENT
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_screen .

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.                    " SET_SCREEN
*&---------------------------------------------------------------------*
*&      Form  RELEASE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM release_po USING lv_error TYPE xfeld
                      lv_text TYPE char0256.

  CONSTANTS: lc_releasecode TYPE frgco VALUE 'GS'.
  DATA: lv_msg TYPE char0256,
        lt_return TYPE TABLE OF bapireturn,
        ls_return TYPE bapireturn,
        ls_return1 TYPE bapiret2.

  CALL FUNCTION 'BAPI_PO_RELEASE'
    EXPORTING
      purchaseorder                = gv_ebeln
      po_rel_code                  = lc_releasecode
*   USE_EXCEPTIONS               = 'X'
*   NO_COMMIT                    = ' '
* IMPORTING
*   REL_STATUS_NEW               =
*   REL_INDICATOR_NEW            =
*   RET_CODE                     =
   TABLES
     return                       = lt_return
   EXCEPTIONS
     authority_check_fail         = 1
     document_not_found           = 2
     enqueue_fail                 = 3
     prerequisite_fail            = 4
     release_already_posted       = 5
     responsibility_fail          = 6
     OTHERS                       = 7.

  IF sy-subrc <> 0.
    lv_error = 'X'.
    IF sy-msgty IS NOT INITIAL AND
       sy-msgid IS NOT INITIAL AND
       sy-msgno IS NOT INITIAL.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type       = sy-msgty
          cl         = sy-msgid
          number     = sy-msgno
*         PAR1       = ' '
*         PAR2       = ' '
*         PAR3       = ' '
*         PAR4       = ' '
*         LOG_NO     = ' '
*         LOG_MSG_NO = ' '
*         PARAMETER  = ' '
*         ROW        = 0
*         FIELD      = ' '
        IMPORTING
          return     = ls_return1.

      CONCATENATE 'Error: ' ls_return1-message INTO lv_msg
                                       SEPARATED BY space.
    ENDIF.
  ENDIF.
  IF lv_msg IS INITIAL.
    LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
      CONCATENATE 'Error: ' ls_return-message INTO lv_msg
                                      SEPARATED BY space.
      lv_error = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.
  IF lv_error IS NOT INITIAL AND
     lv_msg IS INITIAL.
    lv_msg = 'Unable to Release the PO, unknown error.'.
  ENDIF.
  IF lv_error IS INITIAL.
    lv_msg = 'Release has been completed.'.
  ENDIF.
  lv_text = lv_msg.
  IF lv_msg IS NOT INITIAL.
    MESSAGE i017(zfi01) WITH lv_msg.
  ENDIF.

ENDFORM.                    " RELEASE_PO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_po .

  SET PARAMETER ID 'BES' FIELD gv_ebeln.
  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_PO
*&---------------------------------------------------------------------*
*&      Form  GET_DOWNPAY_INFO
*&---------------------------------------------------------------------*
*       Is downpayment process run for this po
*----------------------------------------------------------------------*
FORM get_downpay_info  CHANGING  p_dncmp TYPE xfeld.
  DATA: ls_ekbe TYPE ekbe,
        lv_bewtp TYPE ekbe-bewtp VALUE 'A'.

  SELECT SINGLE * FROM ekbe INTO ls_ekbe
    WHERE ebeln = gv_ebeln
      AND bewtp = lv_bewtp. "down-payment
  IF sy-subrc = 0.
    p_dncmp = 'X'.
  ELSE.
    p_dncmp = space.
  ENDIF.
ENDFORM.                    " GET_DOWNPAY_INFO
