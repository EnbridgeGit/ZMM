*****           Implementation of object type ZBUS2013             *****
INCLUDE <object>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      PURCHASINGDOCUMENT LIKE EKKO-EBELN,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated

begin_method zmmschdagrdecision changing container.
DATA:
      ivebeln TYPE ekko-ebeln,
      releasecode LIKE t16fc-frgco,
      rejectcomments TYPE wfsyst-result.

ivebeln = object-key-purchasingdocument.
swc_get_element container 'ReleaseCode' releasecode.
CALL FUNCTION 'ZMM_SCHDAGR_DECISION'
  EXPORTING
    iv_ebeln       = ivebeln
    iv_releasecode = releasecode
  IMPORTING
    ev_comments    = rejectcomments
  EXCEPTIONS
    OTHERS         = 01.
CASE sy-subrc.
  WHEN 0.            " OK
  WHEN OTHERS.       " to be implemented
ENDCASE.
swc_set_element container 'RejectComments' rejectcomments.
end_method.

BEGIN_METHOD ZMMUSEREMAIL CHANGING CONTAINER.

DATA:
      USERID TYPE WFSYST-INITIATOR,
      USEREMAIL TYPE ADR6-SMTP_ADDR.

  SWC_GET_ELEMENT CONTAINER 'Userid' USERID.

  CALL FUNCTION 'ZMM_USER_EMAIL'
    EXPORTING
      USERID = USERID
    IMPORTING
      USER_EMAIL = USEREMAIL
    EXCEPTIONS
     NO_EMAIL         = 1
     OTHERS           = 2.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented

  ENDCASE.
  SWC_SET_ELEMENT CONTAINER 'UserEmail' USEREMAIL.

END_METHOD.

BEGIN_METHOD ZMMUSERID CHANGING CONTAINER.
DATA:
      USERID TYPE WFSYST-INITIATOR,
      BNAME TYPE USR21-BNAME.
  SWC_GET_ELEMENT CONTAINER 'Userid' USERID.
  CALL FUNCTION 'ZMM_USERID'
    EXPORTING
      USERID = USERID
    IMPORTING
      BNAME = BNAME.

  SWC_SET_ELEMENT CONTAINER 'Bname' BNAME.
END_METHOD.

BEGIN_METHOD ZLMMI002 CHANGING CONTAINER.
DATA:
      SUBMIT_MESSAGE TYPE TLINE OCCURS 0,
      ls_text TYPE TLINE,
      lt_text type TABLE OF TLINE,
      ivebeln TYPE ekko-ebeln,
      lv_datum type sy-datum,
      lv_aedat type ekko-aedat.
DATA: lt_seltab  TYPE TABLE OF rsparams,
      ls_seltab  LIKE LINE OF lt_seltab,
      lt_abaplist TYPE TABLE OF abaplist,
      lt_list TYPE list_string_table,
      ls_list TYPE LINE OF list_string_table.

ivebeln = object-key-purchasingdocument.
lv_datum = sy-datum.
*********************
select SINGLE aedat INTO lv_aedat FROM ekko
  WHERE ebeln = ivebeln.
*********************
 break sahmad.
 ls_seltab-selname = 'P_PRDOC'.  " Name of parameter on submitted program
 ls_seltab-kind    = 'S'.
 ls_seltab-sign    = 'I'.
 ls_seltab-option  = 'EQ'.
 ls_seltab-low     = ivebeln.
 APPEND ls_seltab TO lt_seltab.

 ls_seltab-selname = 'P_RELDAT'.
 ls_seltab-kind    = 'S'.
 ls_seltab-sign    = 'I'.
 ls_seltab-option  = 'EQ'.
 ls_seltab-low     = lv_aedat.
 APPEND ls_seltab TO lt_seltab.

 ls_seltab-selname = 'P_RELDAT'.
 ls_seltab-kind    = SPACE. "'S'.
 ls_seltab-sign    = SPACE. "'I'.
 ls_seltab-option  = SPACE. "'EQ'.
 ls_seltab-low     = SPACE. "ivebeln.
 APPEND ls_seltab TO lt_seltab.

 CLEAR ls_seltab.
 ls_seltab-selname = 'P_EKORG'.
 ls_seltab-kind    = 'P'.
 ls_seltab-sign    = 'I'.
 ls_seltab-option  = 'EQ'.
 ls_seltab-low     = 'GASA'.
 APPEND ls_seltab TO lt_seltab.

 CLEAR ls_seltab.
 ls_seltab-selname = 'P_TESTRN'. "p_testrn
 ls_seltab-kind    = 'P'.
 ls_seltab-sign    = 'I'.
 ls_seltab-option  = 'EQ'.
 ls_seltab-low     = SPACE.
 APPEND ls_seltab TO lt_seltab.

 CLEAR ls_seltab.
 ls_seltab-selname = 'P_TRANS'.
 ls_seltab-kind    = 'P'.
 ls_seltab-sign    = 'I'.
 ls_seltab-option  = 'EQ'.
 ls_seltab-low     = 'Y'.
 APPEND ls_seltab TO lt_seltab.

 CLEAR ls_seltab.
 ls_seltab-selname = 'P_BYPASS'.
 ls_seltab-kind    = 'P'.
 ls_seltab-sign    = 'I'.
 ls_seltab-option  = 'EQ'.
 ls_seltab-low     = 'N'.
 APPEND ls_seltab TO lt_seltab.
********************
  SUBMIT ZLMMI002_SCHEDAGREE
      WITH SELECTION-TABLE lt_seltab
      EXPORTING LIST TO MEMORY
      AND RETURN.
********************
  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject       = lt_abaplist
   EXCEPTIONS
     NOT_FOUND        = 1
     OTHERS           = 2.
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
         ls_text-tdline = LS_LIST.
         append ls_text to lt_text.
    ENDLOOP.
    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = lt_abaplist.
  submit_message[] = lt_text[].
**************************
  IF submit_message[] is INITIAL.
     clear lt_text.
     ls_text-tdline = 'No Return message...'.
     append ls_text to lt_text.
     submit_message[] = lt_text[].
  ENDIF.
******************
  SWC_SET_TABLE CONTAINER 'submit_message' SUBMIT_MESSAGE.

END_METHOD.

BEGIN_METHOD ZMMSCHDAGRSMARTFORM CHANGING CONTAINER.
DATA:
      IVEBELN TYPE EKKO-EBELN,
      IVWID TYPE SWR_STRUCT-WORKITEMID,
      EVDOCID LIKE SWR_ATT_ID,
      LTMLINES LIKE TLINE OCCURS 0,
      LTMSTRUCT LIKE TLINE OCCURS 0.

  ivebeln = object-key-purchasingdocument.
  SWC_GET_ELEMENT CONTAINER 'IvWid' IVWID.
  CALL FUNCTION 'ZMM_SCHDAGR_SMARTFORM'
    EXPORTING
      IV_EBELN = IVEBELN
      IV_WID = IVWID
    IMPORTING
      EV_DOCID = EVDOCID
    TABLES
      LT_MLINES = LTMLINES
      LT_MSTRUCT = LTMSTRUCT
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
  SWC_SET_ELEMENT CONTAINER 'EvDocid' EVDOCID.
  SWC_SET_TABLE CONTAINER 'LtMlines' LTMLINES.
  SWC_SET_TABLE CONTAINER 'LtMstruct' LTMSTRUCT.
END_METHOD.

BEGIN_METHOD ZMMME38 CHANGING CONTAINER.
DATA:
      IVEBELN TYPE EKKO-EBELN,
      EVSCHEDULEUPDATED TYPE EKKO-LOEKZ.

  ivebeln = object-key-purchasingdocument.
  CALL FUNCTION 'ZMM_SCHDAGR_ME38'
    EXPORTING
      IV_EBELN = IVEBELN
    IMPORTING
      EV_SCHEDULE_UPDATED = EVSCHEDULEUPDATED
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
  SWC_SET_ELEMENT CONTAINER 'EvScheduleUpdated' EVSCHEDULEUPDATED.
END_METHOD.

BEGIN_METHOD ZMMME32LBDCDELETE CHANGING CONTAINER.
DATA:
      IVEBELN TYPE EKPO-EBELN,
      BDCRELEASEMSG_DEL LIKE TLINE OCCURS 0,
      BDCERRORMSG_DEL LIKE BAPIRET2 OCCURS 0.

  ivebeln = object-key-purchasingdocument.

  CALL FUNCTION 'ZMM_ME32L_BDC_DELETE'
    EXPORTING
      IV_EBELN = IVEBELN
    TABLES
      BDC_RELEASE_MSG = BDCRELEASEMSG_DEL
      ERROR_MSG = BDCERRORMSG_DEL
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
  SWC_SET_TABLE CONTAINER 'BdcReleaseMsg_del' BDCRELEASEMSG_DEL.
  SWC_SET_TABLE CONTAINER 'BdcErrorMsg_del' BDCERRORMSG_DEL.
END_METHOD.

BEGIN_METHOD ZMMSCHDAGRCHANGE CHANGING CONTAINER.
DATA:
       IVEBELN TYPE EKKO-EBELN.

  ivebeln = object-key-purchasingdocument.
  CALL FUNCTION 'ZMM_SCHDAGR_CHANGE'
    EXPORTING
      IV_EBELN = IVEBELN
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
END_METHOD.
