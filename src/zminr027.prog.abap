REPORT ZMINR027 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  zminr027
*    Programmer  :  Nancy Gilligan - OmniLogic
*    Date        :  November 12, 1998
*    Modified    :  January 21, 1999 by: raarssen
*
* This ABAP will display a management summary reporting of inventory
* differences by plant, storage location type (or plant/storage location
* if summarization button is not used) and standardized grouping of
* material groups.
*
* 99/08/31 mdemeest #--- Results have the incorrect sign and add
*                        storage location id's to report
*
* This report was requested by Dave Lambert under DRMM0258, issue log
* number 551.
************************************************************************

************************************************************************
*****************************  DATA  ***********************************
TABLES: MARA, MARD, MSEG, MKPF,
*       makt,                              "Material Description
        T023T,                             "Material Group Description
        T001W,                             "Plant description
        T001L.                             "Storage location description

DATA   : BEGIN OF IT_INVENT OCCURS 10000,
              WERKS       LIKE MARD-WERKS,           "Plant
              LGORT       LIKE MSEG-LGORT,           "Storage Location
              PIPE        LIKE MSEG-DMBTR,
              CONS_MAT    LIKE MSEG-DMBTR,
              APPL_PART   LIKE MSEG-DMBTR,
              GEN_SUP     LIKE MSEG-DMBTR,
              COMP_PART   LIKE MSEG-DMBTR,
              METR_PART   LIKE MSEG-DMBTR,
              TOTAL_COL   LIKE MSEG-DMBTR,
          END OF IT_INVENT.

DATA:   STORAGE(18),                           "Storage group name
        NO_DATA TYPE I VALUE 0.

************************************************************************
***********************  SELECTION SCREEN  *****************************
SELECTION-SCREEN SKIP.
*SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:     S_WERKS         FOR   MARD-WERKS.
SELECT-OPTIONS:     S_LGORT         FOR   MARD-LGORT.
SELECT-OPTIONS:     S_MATKL         FOR   MARA-MATKL.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(30) TEXT-011.
PARAMETERS:     P_DATE1        LIKE   MKPF-BUDAT OBLIGATORY,
                P_DATE2        LIKE   MKPF-BUDAT OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:     P_SUMM         AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 5(35) TEXT-010.
SELECTION-SCREEN END OF BLOCK BOX1.

************************************************************************
*************************  TOP OF PAGE  ********************************
TOP-OF-PAGE.
* standard page header
FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,
        144 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID, SY-SYSID.
  WRITE: 54 TEXT-TTL.
  WRITE: P_DATE1, TEXT-DSH, P_DATE2,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
       SKIP 1.
  ULINE.

IF SY-PAGNO = 1.
  PERFORM WRITE_HEADER_PAGE.
ENDIF.

* report titles
SKIP.
*  top line
WRITE: 51 TEXT-021, 72 TEXT-023, 93 TEXT-025, 114 TEXT-027,
       135 TEXT-028.
*  second line
WRITE: /30 TEXT-020,
           TEXT-022 UNDER TEXT-021,
           TEXT-024 UNDER TEXT-023,
           TEXT-026 UNDER TEXT-025,
           TEXT-024 UNDER TEXT-027,
           TEXT-024 UNDER TEXT-028,
       156 TEXT-029.
PERFORM WRITE_ULINES.
SKIP.
FORMAT INTENSIFIED OFF.
************************************************************************
*******************************  MAIN  *********************************

START-OF-SELECTION.
* clear: it_invent.
REFRESH: IT_INVENT.

PERFORM GET_DATA.
PERFORM WRITE_DATA.

IF NO_DATA = 0.
  SKIP 3.
  WRITE: /60 TEXT-050.
ENDIF.


************************************************************************
**************************  SUBROUTINES  *******************************

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

SELECT * FROM MARA WHERE MATKL IN S_MATKL
                     AND ( MATKL BETWEEN '0100' AND '0199'       "ignore
                        OR MATKL BETWEEN '0200' AND '0299' "other groups
                        OR MATKL BETWEEN '0300' AND '0499'  "even if set
                        OR MATKL BETWEEN '0500' AND '0599'      "by user
                        OR MATKL BETWEEN '0600' AND '0699'
                        OR MATKL BETWEEN '0700' AND '0799' )
                     AND LVORM NE 'X'.

* select single * from makt where spras = sy-langu
*                             and matnr = mara-matnr.

    SELECT * FROM MARD WHERE MATNR = MARA-MATNR
                         AND WERKS IN S_WERKS
                         AND LGORT IN S_LGORT
                         AND LVORM NE 'X'.

      SELECT * FROM MSEG WHERE MATNR = MARA-MATNR
                           AND WERKS = MARD-WERKS
                           AND LGORT = MARD-LGORT
                           AND ( BWART = '701'
                              OR BWART = '702' ).

        SELECT SINGLE * FROM MKPF WHERE MBLNR = MSEG-MBLNR
                                    AND MJAHR = MSEG-MJAHR
                                    AND BUDAT BETWEEN P_DATE1
                                                  AND P_DATE2.

        IF SY-SUBRC = 0.
          PERFORM SAVE_DATA.
        ENDIF.

      ENDSELECT.   "mseg
    ENDSELECT.  "mard
ENDSELECT.  "mara

* if p_summ ne 'X'.
*    perform get_all_storage_locations.
* endif.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
FORM WRITE_DATA.

SORT IT_INVENT BY WERKS LGORT.

LOOP AT IT_INVENT.

* write name of plant
AT NEW WERKS.
  SELECT SINGLE NAME1 FROM T001W INTO T001W-NAME1
                     WHERE WERKS = IT_INVENT-WERKS.
  FORMAT INTENSIFIED ON.
  WRITE: / T001W-NAME1.
  FORMAT INTENSIFIED OFF.
ENDAT.

* write each detail line if requested
IF P_SUMM NE 'X'.
  SELECT SINGLE LGOBE FROM T001L INTO T001L-LGOBE
                       WHERE WERKS = IT_INVENT-WERKS
                         AND LGORT = IT_INVENT-LGORT.
  WRITE: /5 IT_INVENT-LGORT, T001L-LGOBE.
  PERFORM WRITE_AMOUNTS.
ENDIF.

* write storage location summary if requested.
AT END OF LGORT.
  IF P_SUMM = 'X'.
    SUM.
* Assign storage location groups for summary
       CASE IT_INVENT-LGORT(1).
         WHEN 'A'.
*           storage = text-031.                             "99/08/31
            CONCATENATE: TEXT-031 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'B'.
*           storage = text-032.                             "99/08/31
            CONCATENATE: TEXT-032 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'C'.
*           storage = text-033.                             "99/08/31
            CONCATENATE: TEXT-033 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'D'.
*           storage = text-034.                             "99/08/31
            CONCATENATE: TEXT-034 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'E'.
*           storage = text-035.                              "99/08/31
            CONCATENATE: TEXT-035 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'F'.
*           storage = text-036.                              "99/08/31
            CONCATENATE: TEXT-036 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'G'.
*           storage = text-037.                              "99/08/31
            CONCATENATE: TEXT-037 '-' IT_INVENT-LGORT INTO STORAGE.
         WHEN 'Z'.
*           storage = text-038.                              "99/08/31
            CONCATENATE: TEXT-038 '-' IT_INVENT-LGORT INTO STORAGE.
       ENDCASE.
    FORMAT INTENSIFIED ON.
    WRITE: /5 STORAGE.
    FORMAT INTENSIFIED OFF.
    PERFORM WRITE_AMOUNTS.
  ENDIF.
ENDAT.

* total up each plant
AT END OF WERKS.
  SUM.
  FORMAT INTENSIFIED ON.
  PERFORM WRITE_ULINES.
  WRITE: /10 TEXT-039.
  PERFORM WRITE_AMOUNTS.
  FORMAT INTENSIFIED OFF.
  NO_DATA = 1.
  SKIP.
ENDAT.

ENDLOOP.

ENDFORM.                    " WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_AMOUNTS
*&---------------------------------------------------------------------*
FORM WRITE_AMOUNTS.

WRITE:     IT_INVENT-PIPE       UNDER TEXT-020,
           IT_INVENT-CONS_MAT   UNDER TEXT-021,
           IT_INVENT-APPL_PART  UNDER TEXT-023,
           IT_INVENT-GEN_SUP    UNDER TEXT-025,
           IT_INVENT-COMP_PART  UNDER TEXT-027,
           IT_INVENT-METR_PART  UNDER TEXT-028,
           IT_INVENT-TOTAL_COL  UNDER TEXT-029.

ENDFORM.                    " WRITE_AMOUNTS

*&---------------------------------------------------------------------*
*&      Form  WRITE_ULINES
*&---------------------------------------------------------------------*
FORM WRITE_ULINES.
WRITE: /   TEXT-030 UNDER TEXT-020,
           TEXT-030 UNDER TEXT-021,
           TEXT-030 UNDER TEXT-023,
           TEXT-030 UNDER TEXT-025,
           TEXT-030 UNDER TEXT-027,
           TEXT-030 UNDER TEXT-028,
           TEXT-030 UNDER TEXT-029.

ENDFORM.                    " WRITE_ULINES
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
* load up table
       IT_INVENT-WERKS = MARD-WERKS.
       IT_INVENT-LGORT = MARD-LGORT.

* determine value
*      if mseg-shkzg = 'S'.                               "99/08/31
       IF MSEG-SHKZG = 'H'.                               "99/08/31
          MSEG-DMBTR = MSEG-DMBTR * -1.
       ENDIF.
* Sort out values
       IF MARA-MATKL BETWEEN '0100' AND '0199'.
*      pipes
         IT_INVENT-PIPE  = MSEG-DMBTR.
       ELSEIF MARA-MATKL BETWEEN '0200' AND '0299'.
*      construction material
         IT_INVENT-CONS_MAT = MSEG-DMBTR.
       ELSEIF MARA-MATKL BETWEEN '0300' AND '0499'.
*      appliance parts
         IT_INVENT-APPL_PART = MSEG-DMBTR.
       ELSEIF MARA-MATKL BETWEEN '0500' AND '0599'.
*      compressor parts
         IT_INVENT-COMP_PART = MSEG-DMBTR.
       ELSEIF MARA-MATKL BETWEEN '0600' AND '0699'.
*      meter parts
         IT_INVENT-METR_PART = MSEG-DMBTR.
       ELSEIF MARA-MATKL BETWEEN '0700' AND '0799'.
*      general supplies
         IT_INVENT-GEN_SUP = MSEG-DMBTR.
       ENDIF.

*      total column
       IT_INVENT-TOTAL_COL = MSEG-DMBTR.

       COLLECT IT_INVENT.
       CLEAR IT_INVENT.
       CLEAR: MSEG, MKPF.
*      clear: mara, mard.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER_PAGE
*&---------------------------------------------------------------------*
FORM WRITE_HEADER_PAGE.
SKIP 2.
WRITE: / TEXT-040.
SKIP.
* plant selection
WRITE: / TEXT-041.
LOOP AT S_WERKS.
  WRITE: S_WERKS-LOW.
ENDLOOP.
IF NOT S_WERKS-HIGH IS INITIAL.
  WRITE: TEXT-045, S_WERKS-HIGH.
ENDIF.
IF S_WERKS-LOW IS INITIAL.
  WRITE: TEXT-048.
ENDIF.

* storage location selection
WRITE: / TEXT-042.
LOOP AT S_LGORT.
  WRITE: S_LGORT-LOW.
ENDLOOP.
IF NOT S_LGORT-HIGH IS INITIAL.
  WRITE: TEXT-045, S_LGORT-HIGH.
ENDIF.
IF S_LGORT-LOW IS INITIAL.
  WRITE: TEXT-048.
ENDIF.

* material group selection
WRITE: / TEXT-043.
LOOP AT S_MATKL.
  WRITE: S_MATKL-LOW.
ENDLOOP.
IF NOT S_MATKL-HIGH IS INITIAL.
  WRITE: TEXT-045, S_MATKL-HIGH.
ENDIF.
IF S_MATKL-LOW IS INITIAL.
  WRITE: TEXT-049.
ENDIF.


WRITE: / TEXT-044, P_DATE1, TEXT-045, P_DATE2.

IF P_SUMM = 'X'.
  WRITE: / TEXT-046.
ELSE.
  WRITE: / TEXT-047.
ENDIF.

SKIP 2.
ENDFORM.                    " WRITE_HEADER_PAGE
*&---------------------------------------------------------------------*
*&      Form  GET_ALL_STORAGE_LOCATIONS
*&---------------------------------------------------------------------*
*form get_all_storage_locations.

*select * from mara where matkl in s_matkl
*                    and ( matkl between '0100' and '0199'       "ignore
*                       or matkl between '0200' and '0299' "other groups
*                       or matkl between '0400' and '0499'  "even if set
*                       or matkl between '0500' and '0599'      "by user
*                       or matkl between '0600' and '0699'
*                       or matkl between '0700' and '0799' )
*                    and lvorm ne 'X'.
*
*    select * from mard where werks in s_werks
*                         and lgort in s_lgort
*                         and lvorm ne 'X'.
*
*    perform save_data.
*
*    endselect.
* endselect.
* endform.                    " GET_ALL_STORAGE_LOCATIONS
