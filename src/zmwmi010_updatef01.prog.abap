*----------------------------------------------------------------------*
***INCLUDE MZMM_SLOC_UPDATEF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data from database based on screen 100 selection screen.
*----------------------------------------------------------------------*
FORM GET_DATA.

  Data: TEMP_LGPBE like MARD-LGPBE.

* Basic mandatory selection
  If         P_MATNR is initial
  and        P_LGPBE is initial.
    Select
          WERKS                "Plant
          MATNR                "Material Number
          LGORT                "Storage location
          LGPBE                "Storage bin location
*          from MARD into table ITAB "(-)PANUSURI Ticket 47969
          from MARD into corresponding fields of table ITAB "(+)PANUSURI Ticket 47969
          where WERKS = P_WERKS
             and LGORT = P_LGORT
             and LVORM = space.

* Basic mandatory selection with Bin location
  ElseIf     P_MATNR is initial
  and NOT    P_LGPBE is initial.
    If P_LGPBE CA '*'. "Handle wildcards for bin location
      TEMP_LGPBE = P_LGPBE.
      Translate TEMP_LGPBE using '*%'.
      Select
            WERKS                "Plant
            MATNR                "Material Number
            LGORT                "Storage location
            LGPBE                "Storage bin location
*            from MARD into table ITAB  "(-)PANUSURI Ticket 47969
             from MARD into corresponding fields of table ITAB  "(+)PANUSURI Ticket 47969
             where WERKS = P_WERKS
               and LGORT = P_LGORT
               and LGPBE like TEMP_LGPBE
               and LVORM = space.


    Else.
      Select
            WERKS                "Plant
            MATNR                "Material Number
            LGORT                "Storage location
            LGPBE                "Storage bin location
*            from MARD into table ITAB   "(-)PANUSURI Ticket 47969
             from MARD into corresponding fields of table ITAB  "(+)PANUSURI Ticket 47969
             where WERKS = P_WERKS
               and LGORT = P_LGORT
               and LGPBE = P_LGPBE
               and LVORM = space.

    Endif.

* Basic mandatory selection with Material number
  ElseIf NOT P_MATNR is initial
  and        P_LGPBE is initial.

    shift P_MATNR right deleting trailing space.
    translate P_MATNR using ' 0'.
    Select
          WERKS                "Plant
          MATNR                "Material Number
          LGORT                "Storage location
          LGPBE                "Storage bin location
*          from MARD into table ITAB   "(-)PANUSURI Ticket 47969
           from MARD into corresponding fields of table ITAB  "(+)PANUSURI Ticket 47969
           where WERKS = P_WERKS
             and LGORT = P_LGORT
             and MATNR = P_MATNR
             and LVORM = space.

* Basic mandatory selection with Material number and bin
  ElseIf NOT P_MATNR is initial
  and    NOT P_LGPBE is initial.

    shift P_MATNR right deleting trailing space.
    translate P_MATNR using ' 0'.

    If P_LGPBE CA '*'. "Handle wildcards for bin location
      TEMP_LGPBE = P_LGPBE.
      Translate TEMP_LGPBE using '*%'.
      Select
            WERKS                "Plant
            MATNR                "Material Number
            LGORT                "Storage location
            LGPBE                "Storage bin location
*            from MARD into table ITAB  "(-)PANUSURI Ticket 47969
             from MARD into corresponding fields of table ITAB  "(+)PANUSURI Ticket 47969
             where WERKS = P_WERKS
               and LGORT = P_LGORT
               and LGPBE like TEMP_LGPBE
               and MATNR = P_MATNR
               and LVORM = space.


    Else.
      Select
            WERKS                "Plant
            MATNR                "Material Number
            LGORT                "Storage location
            LGPBE                "Storage bin location
*            from MARD into table ITAB   "(-)PANUSURI Ticket 47969
             from MARD into corresponding fields of table ITAB   "(+)PANUSURI Ticket 47969
             where WERKS = P_WERKS
               and LGORT = P_LGORT
               and LGPBE = P_LGPBE
               and MATNR = P_MATNR
               and LVORM = space.
    Endif.

  EndIf.

  loop at itab.
    select maktx from MAKT into (itab-maktx)
      where matnr = itab-matnr.
      modify itab.
    endselect.

  endloop.

  "end UGL code

  describe table itab lines tc_storagebin-lines.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_ITAB
*&---------------------------------------------------------------------*
*       This routine loops through the ITAB and modifies the storage
*       bin location based on the new entries.
*----------------------------------------------------------------------*
FORM SAVE_ITAB.
*BOI by PANUSURI Ticket 47969
  data : lwa_headdata LIKE  BAPIMATHEAD,
         lwa_STORAGELOCATIONDATA like BAPI_MARD,
         lwa_STORAGELOCATIONDATAX like BAPI_MARDX,
         lwa_returnmessages like BAPI_MATRETURN2,
         lit_returnmessages type standard table of BAPI_MATRETURN2,
         lv_flag type c.
*EOI by PANUSURI Ticket 47969
  Loop at ITAB where NOT NEW_LGPBE is initial.

    check itab-message <> 'No Authorization'.

*Due to WEI's requirement to prevent users of this program to use
* transaction MM02, the lack of any other interface for updating
* storage bin location and the isolated quality of this simple field,
* it was decided to update the table field directly.

*
** Create BDC DATA
*
** Initial Screen for MM02 - enter material
*        PERFORM DYNPRO USING: 'X' 'SAPLMGMM'        '0060',
*                              ' ' 'RMMG1-MATNR'     ITAB-MATNR, "Mat #
*                              ' ' 'BDC_OKCODE'      '/00'. "ENTER
*
** View selection screen -  select all
*        PERFORM DYNPRO USING: 'X' 'SAPLMGMM'        '0070',
*                              ' ' 'BDC_OKCODE'      '=SELA'."Select All
*
** View selection screen -   continue
*        PERFORM DYNPRO USING: 'X' 'SAPLMGMM'        '0070',
*                              ' ' 'BDC_OKCODE'      '=ENTR'. "Enter
*
** Plant/Sloc screen  - enter Plant and Storage location
*        PERFORM DYNPRO USING: 'X' 'SAPLMGMM'        '0080',
*                              ' ' 'RMMG1-WERKS'     P_WERKS, "Plant
*                              ' ' 'RMMG1-LGORT'     P_LGORT, "Sloc
*                              ' ' 'BDC_OKCODE'      'ENTR'. "ENTER
*
** Main data entry screen -   Choose Plant Data/Storage
*        PERFORM DYNPRO USING: 'X' 'SAPLMGMM'        '4004',
*                              ' ' 'BDC_OKCODE'      '=SP19'. "Plnt data
*
** Plant/Sloc screen  - enter Plant and Storage location
*        PERFORM DYNPRO USING: 'X' 'SAPLMGMM'        '4000',
*                              ' ' 'MARD-LGPBE'     ITAB-NEW_LGPBE, "Bin
*                              ' ' 'BDC_OKCODE'      'BU'. "SAVE
*
*    call transaction 'MM02'  using  bdc_data
*                             mode   'N'  "Change to A to test
*                             update 'S'
*                             .
**                             messages into messtab.
*BOC by PANUSURI Ticket 47969
** Lock record for update
*    CALL FUNCTION 'ENQUEUE_EMMARCE'
*      EXPORTING
*        MODE_MARC      = 'E'
*        MANDT          = SY-MANDT
*        MATNR          = ITAB-MATNR
*        WERKS          = p_werks
*      EXCEPTIONS
*        FOREIGN_LOCK   = 1
*        SYSTEM_FAILURE = 2
*        OTHERS         = 3.
*    IF SY-SUBRC <> 0.
*      Move 'Record currently LOCKED, please try later' to itab-message.
*    else.
*
** Direct table update:
*      UPDATE MARD   SET:  LGPBE  = ITAB-NEW_LGPBE
*                    WHERE MATNR  = ITAB-MATNR
*                    AND WERKS    = P_WERKS
*                    AND LGORT    = P_LGORT.
*
** if an error occurs in the call transaction modify the itab to display
** result in table control.
*      if sy-subrc <> 0.
*        Move 'Update-Error, please inform System Admin'
*             to itab-message.
*      else.
*        Move 'Updated Successfully' to itab-message.
*      endif.
** Unlock record
*      CALL FUNCTION 'DEQUEUE_EMMARCE'
*        EXPORTING
*          MODE_MARC = 'E'
*          MANDT     = SY-MANDT
*          MATNR     = ITAB-MATNR
*          WERKS     = p_werks.
*      commit work.
*    endif.
*EOC by PANUSURI Ticket 47969

*BOI by PANUSURI Ticket 47969
*   Record the change history of the Material master
*   Fill Header data
    lwa_headdata-material = ITAB-MATNR.
    lwa_headdata-STORAGE_VIEW = 'X'.
*   Fill Storage location data
    lwa_STORAGELOCATIONDATA-plant = P_WERKS.
    lwa_STORAGELOCATIONDATA-STGE_LOC  = P_LGORT.
    lwa_STORAGELOCATIONDATA-STGE_BIN = ITAB-NEW_LGPBE.

    lwa_STORAGELOCATIONDATAX-plant = P_WERKS.
    lwa_STORAGELOCATIONDATAX-STGE_LOC  = P_LGORT.
    lwa_STORAGELOCATIONDATAX-STGE_BIN = 'X'.

*   Update storage bin for the material
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA             = lwa_headdata
        STORAGELOCATIONDATA  = lwa_STORAGELOCATIONDATA
        STORAGELOCATIONDATAX = lwa_STORAGELOCATIONDATAX
      TABLES
        RETURNMESSAGES       = lit_returnmessages.

    if lit_returnmessages is not initial.
      clear lv_flag.
      read table lit_returnmessages into lwa_returnmessages with key type = 'S' ID = 'M3' number = '801'.
      if sy-subrc = 0.
        itab-message = 'Updated Successfully'.
      else.
        read table lit_returnmessages into lwa_returnmessages with key type = 'E' ID ='M3' number = '022'.
        if sy-subrc = 0.
          itab-message = 'Record currently LOCKED, please try later'.
          lv_flag = 'X'.
        else.
          read table lit_returnmessages into lwa_returnmessages with key type = 'E'.
          if sy-subrc = 0.
            itab-message = 'Update- Error, please inform System Admin'.
            lv_flag = 'X'.
          endif.
        endif.
      endif.
    endif.

    itab-return_message[] = lit_returnmessages[].

    if lv_flag = 'X'. "Error record
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
                .
    else.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
         WAIT          = 'X'
*     IMPORTING
*       RETURN        =
       .
    endif.
*EOI by PANUSURI Ticket 47969

    modify itab.
*BOI by PANUSURI Ticket 47969
    clear : lv_flag,
            lwa_headdata,
            lwa_STORAGELOCATIONDATA,
            lwa_STORAGELOCATIONDATAX,
            lwa_returnmessages.
    refresh : lit_returnmessages.
*EOI by PANUSURI Ticket 47969
    clear bdc_data.
    refresh bdc_data.
  Endloop.

*If update_error is initial.
* Set screen '0100'.



ENDFORM.                    " SAVE_ITAB
*&---------------------------------------------------------------------*
*&      Form  PAGING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0071   text
*----------------------------------------------------------------------*
FORM PAGING USING    CODE.
  DATA: I TYPE I,
        J TYPE I.

  CASE CODE.
    WHEN 'P--'. TC_STORAGEBIN-TOP_LINE = 1.
    WHEN 'P-'.
      TC_STORAGEBIN-TOP_LINE =
        TC_STORAGEBIN-TOP_LINE - LINE_COUNT.
      IF TC_STORAGEBIN-TOP_LINE LE 0.
        TC_STORAGEBIN-TOP_LINE = 1.
      ENDIF.
    WHEN 'P+'.
      I = TC_STORAGEBIN-TOP_LINE + LINE_COUNT.
      J = TC_STORAGEBIN-LINES - LINE_COUNT + 1.
      IF J LE 0.
        J = 1.
      ENDIF.
      IF I LE J.
        TC_STORAGEBIN-TOP_LINE = I.
      ELSE.
        TC_STORAGEBIN-TOP_LINE = J.
      ENDIF.
    WHEN 'P++'.
      TC_STORAGEBIN-TOP_LINE =
         TC_STORAGEBIN-LINES - LINE_COUNT + 1.
      IF TC_STORAGEBIN-TOP_LINE LE 0.
        TC_STORAGEBIN-TOP_LINE = 1.
      ENDIF.
  ENDCASE.

ENDFORM.                    " PAGING
*&---------------------------------------------------------------------*
*&      Form  FIND_POSITION
*&---------------------------------------------------------------------*
*       Finds line item corresponding to search term (material number)
*----------------------------------------------------------------------*
FORM FIND_POSITION.
  if      not mard-matnr is initial
  and         mard-LGPBE is initial.
    Read table itab with key matnr = MARD-MATNR.

    if sy-subrc = 0.
      TC_STORAGEBIN-TOP_LINE = sy-tabix.
    endif.

  elseif      mard-matnr is initial
  and     not mard-LGPBE is initial.

    Read table itab with key LGPBE = MARD-LGPBE.
    if sy-subrc = 0.
      TC_STORAGEBIN-TOP_LINE = sy-tabix.
    endif.

  elseif  not mard-matnr is initial
  and     not mard-LGPBE is initial.

    Read table itab with key matnr = MARD-MATNR
                             LGPBE = MARD-LGPBE.
    if sy-subrc = 0.
      TC_STORAGEBIN-TOP_LINE = sy-tabix.
    endif.

  elseif mard-matnr is initial
    and  mard-lgpbe is initial.
    TC_STORAGEBIN-TOP_LINE = 1.


  endif.
ENDFORM.                    " FIND_POSITION
*&---------------------------------------------------------------------*
*&      Form  remove_lines
*&---------------------------------------------------------------------*
*       Removes all flagged lines in one operation.
*----------------------------------------------------------------------*
FORM remove_lines.
  delete itab where message = 'DELETELINE'.
ENDFORM.                    " remove_lines
