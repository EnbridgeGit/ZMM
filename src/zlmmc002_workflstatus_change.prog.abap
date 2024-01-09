*&---------------------------------------------------------------------*
*& Report  ZLMMI0_WORKSTATUS_CHANGE
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& program name       :  ZLMMI0_WORKSTATUS_CHANGE                      *
*& author             :  Jaydep Waychal/ Durgaprakash                  *
*& creation date      :  Jan 17, 2022                                  *
*& object id          :                                                *
*& application area   :  NA-General                                    *
*& description        :  Program to change Workitem status to completed *
*&                       using Function module                         *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           modification log                           *
*----------------------------------------------------------------------*
* version no    : 1.0                                                  *
* date          :                                                      *
* modified by   :                                                      *
* correction no :                                                      *
* description   : initial program development                          *
*----------------------------------------------------------------------*

REPORT  zlmmc002_workflstatus_change MESSAGE-ID zs NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: sww_wi2obj.

*Type Declaration*.
TYPES : BEGIN OF ty_sww_wi2obj,
            wi_id      TYPE sww_wiid ,
            top_wi_id  TYPE swfrtwiid,
            wi_rh_task TYPE sww_task,
            catid      TYPE sibfcatid,
            instid     TYPE sibfboriid,
            typeid     TYPE sibftypeid,
    END OF ty_sww_wi2obj.

*Internal table declarations*.
DATA: ita_workstatus TYPE STANDARD TABLE OF ty_sww_wi2obj.

**Work area declarations*.
DATA: iwa_workstatus TYPE ty_sww_wi2obj.


************************************************************************
* Selection screen *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_istid FOR sww_wi2obj-instid.
SELECT-OPTIONS : s_task   FOR sww_wi2obj-wi_rh_task." DEFAULT 'WS*'.
PARAMETER: p_catid TYPE sww_wi2obj-catid DEFAULT 'BO' OBLIGATORY.
PARAMETER: p_typid TYPE sww_wi2obj-typeid DEFAULT 'BUS2013' OBLIGATORY.
PARAMETER: p_stats(10) TYPE c DEFAULT 'COMPLETED' OBLIGATORY.
PARAMETER: p_usern(10) TYPE c DEFAULT 'DM_P2P'.
SELECTION-SCREEN END OF BLOCK b1.


************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.
  IF s_istid IS NOT INITIAL.
    PERFORM get_workflowid.
    PERFORM change_status.
  ELSE.
    MESSAGE i019 WITH text-002.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_WORKFLOWID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_workflowid .

***Get details from SWW_WI2OBJ table
  SELECT wi_id top_wi_id wi_rh_task catid instid typeid
    FROM sww_wi2obj
    INTO TABLE ita_workstatus
    WHERE catid  EQ p_catid
      AND typeid EQ p_typid
      AND instid IN s_istid
    AND wi_rh_task IN s_task .
  IF sy-subrc EQ 0.
    SORT ita_workstatus BY wi_id.
    DELETE ita_workstatus WHERE wi_rh_task+0(2) EQ 'TS'.
  ELSE.
*    WRITE://msg
    MESSAGE text-003 TYPE 'I'.
  ENDIF.
ENDFORM.                    " GET_WORKFLOWID
*&---------------------------------------------------------------------*
*&      Form  CHANGE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_status .

  DATA: lv_newstatus TYPE swr_wistat,
*        lv_num TYPE sy-subrc,
        lv_finalmsg TYPE string.

  DATA: ls_swr_struct TYPE swr_struct,
        ls_swr_wistat TYPE swr_wistat.

  TYPES: BEGIN OF ty_msg,
        msgtype TYPE swr_messag,
        msgline TYPE swr_mstruc,
    END OF ty_msg.

  DATA: lta_msg TYPE STANDARD TABLE OF ty_msg,
*        lwa_msgstruc TYPE STANDARD TABLE OF swr_mstruc,
        lwa_msg TYPE ty_msg.


    LOOP AT ita_workstatus INTO iwa_workstatus.
      ls_swr_struct-workitemid = iwa_workstatus-top_wi_id.
      ls_swr_wistat-status  = p_stats.
      CALL FUNCTION 'SAP_WAPI_SET_WORKITEM_STATUS'
         EXPORTING
          workitem_id          = ls_swr_struct-workitemid
          status               = ls_swr_wistat-status                     "'COMPLETED'
*          user                 = p_usern
*          language             = sy-langu
*          do_commit            = 'X'
     IMPORTING
      NEW_STATUS           =  lv_newstatus
**      RETURN_CODE          =
      TABLES
      message_lines        = lta_msg
*      MESSAGE_STRUCT       = lwa_msgstruc
        .
      IF lta_msg IS NOT INITIAL.
        READ TABLE lta_msg INTO lwa_msg INDEX 1.
        CONCATENATE iwa_workstatus-instid lwa_msg '->>>New Status:-' lv_newstatus-status INTO lv_finalmsg SEPARATED BY space.
        WRITE: / lv_finalmsg.
      ENDIF.
      CLEAR: iwa_workstatus, ls_swr_struct-workitemid, ls_swr_wistat-status , lwa_msg.
      REFRESH: lta_msg.
    ENDLOOP.


ENDFORM.                    " CHANGE_STATUS
