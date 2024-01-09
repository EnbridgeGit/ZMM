*----------------------------------------------------------------------*
***INCLUDE LZMM_SA_WFF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  REJECT_SA
*&---------------------------------------------------------------------*
*In case of Reject, Raise Custom Event to trigger workflow task
*----------------------------------------------------------------------*
FORM reject_sa .
*ZRELEASEREJECT
  PERFORM raise_event USING 'ZReleaseReject'.
**
  PERFORM set_screen.

ENDFORM.                    " REJECT_SA
*&---------------------------------------------------------------------*
*&      Form  RELEASE_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM release_sa .

  SET PARAMETER ID 'SAG' FIELD gv_ebeln.
  SET PARAMETER ID 'FAB' FIELD gv_releasecode.
  CALL TRANSACTION 'ME35L' AND SKIP FIRST SCREEN.
******
  PERFORM set_screen.
ENDFORM.                    " RELEASE_SA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_sa .

  SET PARAMETER ID 'SAG' FIELD gv_ebeln.
  CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
***
ENDFORM.                    " DISPLAY_SA
*&---------------------------------------------------------------------*
*&      Form  RAISE_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM raise_event  USING iv_event TYPE swr_struct-event. "swetypecou-event.

*  DATA: lv_objtype TYPE swetypecou-objtype VALUE 'BUS2013',
*        lv_objkey  TYPE sweinstcou-objkey,
  DATA: lt_container TYPE TABLE OF swcont.
  DATA: lv_objtype TYPE swr_struct-object_typ VALUE 'BUS2013',
        lv_objkey  TYPE swr_struct-object_key,
        ls_event_cont TYPE swr_cont,
        lt_event_cont TYPE TABLE OF swr_cont,
        lt_message_lines TYPE TABLE OF swr_messag,
        lt_message_struct TYPE TABLE OF swr_mstruc.

  IF iv_event = 'ZReleaseReject'.
    "Event container (parameters)
    ls_event_cont-element = 'RejectComments'.
    ls_event_cont-value   = gv_comments.
    APPEND ls_Event_cont to lt_event_cont.
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
*LEAVE TO SCREEN 0.
ENDFORM.                    " SET_SCREEN
