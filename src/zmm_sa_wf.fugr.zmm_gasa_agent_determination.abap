FUNCTION ZMM_GASA_AGENT_DETERMINATION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      AC_CONTAINER STRUCTURE  SWCONT
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"  EXCEPTIONS
*"      AGENT_NOT_FOUND
*"----------------------------------------------------------------------

 INCLUDE <cntn01>.
  CONSTANTS: c_0001 TYPE pa0105-subty VALUE '0001',
             c_otype  TYPE  objec-otype VALUE 'C',
             c_objid1 TYPE  char10 VALUE '20001925', "Sr Buyer
             c_objid2 TYPE  char10 VALUE '20002100', "Buyer
             c_wegid  TYPE  gdstr-wegid VALUE 'C_S_S_P'.
  DATA: lv_pernr TYPE pa0105-pernr,
        lv_usrid TYPE pa0105-usrid,
        ls_actor TYPE swhactor,
        ls_tab   TYPE swhactor,
        ls_objec TYPE objec,
        ls_struc TYPE struc,
        lt_tab   TYPE TABLE OF swhactor,
        lt_mtab  TYPE TABLE OF swhactor,
        lt_objec TYPE TABLE OF objec,
        lt_struc TYPE TABLE OF struc.

  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype            = c_otype
      act_objid            = c_objid1
      act_wegid            = c_wegid
*   ACT_INT_FLAG           =
*   ACT_PLVAR              = ' '
*   ACT_BEGDA              = SY-DATUM
*   ACT_ENDDA              = SY-DATUM
*   ACT_TDEPTH             = 0
*   ACT_TFLAG              = 'X'
*   ACT_VFLAG              = 'X'
*   AUTHORITY_CHECK        = 'X'
*   TEXT_BUFFER_FILL       =
*   BUFFER_MODE            =
* IMPORTING
*   ACT_PLVAR              =
   TABLES
     result_tab             = lt_tab
     result_objec           = lt_objec
     result_struc           = lt_struc
   EXCEPTIONS
     no_plvar_found         = 1
     no_entry_found         = 2
     OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
***Delete other than employee
  DELETE lt_tab WHERE otype <> 'P'.
  APPEND LINES OF lt_tab TO lt_mtab.
*******
  CLEAR: lt_tab,
         lt_objec,
         lt_struc.
  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = c_otype
      act_objid      = c_objid2
      act_wegid      = c_wegid
    TABLES
      result_tab     = lt_tab
      result_objec   = lt_objec
      result_struc   = lt_struc
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
***Delete other than employee
  DELETE lt_tab WHERE otype <> 'P'.
  APPEND LINES OF lt_tab TO lt_mtab.
*************************
  LOOP AT lt_mtab INTO ls_tab.
    CLEAR lv_usrid.
    lv_pernr = ls_tab-objid.
    SELECT SINGLE usrid INTO lv_usrid FROM pa0105
      WHERE pernr = lv_pernr
        AND subty = c_0001
        AND endda >= sy-datum.
    ls_actor-otype = 'US'.   "user
    ls_actor-objid = lv_usrid.
    APPEND ls_actor TO actor_tab.
  ENDLOOP.
  IF actor_tab[] IS INITIAL.
    RAISE agent_not_found.
  ENDIF.

ENDFUNCTION.
