REPORT ZMINR005 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMINR005
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 5 1996
*
*  This ABAP will create a report that will display the information for
*  any material that has an outstanding transfer.
*-----------------------------------------------------------------------
* 2006/12/21 Mohammad #277 Provide plant code as range in the variant
*                          screen and program changes accordingly.
* 2003/01/08 mdemeest #--- Allow "flagged for deletion" materials
*                          to be reported as well as all active material
************************************************************************
TABLES  : MARA, MARC, MARD, RESB, MSEG, MBEW, MAKT, MKPF.
DATA    : TEMPMINUS(1)    TYPE C,
          TEMPMONTH       TYPE I,
          TEMPDATE        LIKE SY-DATUM,
          TEMPYEAR        TYPE I,
          temp_verpr      like mbew-verpr.


DATA    : BEGIN OF TABLE1 OCCURS 5000,
              WERKS     LIKE MARC-WERKS,           "TR277
              MATNR     LIKE MARA-MATNR,
              MEINS     LIKE MARA-MEINS,
              MAKTX     LIKE MAKT-MAKTX,
*              WERKS     LIKE MARC-WERKS,          "TR277
              UMLMC     LIKE MARC-UMLMC,
              LGORT     LIKE MARD-LGORT,
              BLDAT     LIKE MKPF-BLDAT,
              CPUTM     LIKE MKPF-CPUTM,
              lvorm     like mara-lvorm,                     "2003/01/08
              XBLNR     LIKE MKPF-XBLNR,
              MBLNR     LIKE MSEG-MBLNR,
              BWART     LIKE MSEG-BWART,
              MENGE     LIKE MSEG-MENGE,
              UMWRK     LIKE MSEG-UMWRK,
              UMLGO     LIKE MSEG-UMLGO,
              MINUS(1)  TYPE C,
              verpr     like mbew-verpr,                    "2003/01/30
          END OF TABLE1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_MATNR    FOR   MARA-MATNR,
     S_LGORT    FOR   MARD-LGORT,
*     S_WERKS    FOR   MARC-WERKS OBLIGATORY, " NO INTERVALS,
     S_CPUDT    FOR   MKPF-CPUDT.
SELECTION-SCREEN END OF BLOCK BOX1.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
*    PARAMETERS:  P_WERKS    LIKE   MARC-WERKS OBLIGATORY.  "TR277
SELECT-OPTIONS:  S_WERKS    FOR   MARC-WERKS OBLIGATORY.    "TR277
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) TEXT-044.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX2.
INCLUDE <ICON>.
***************************** INITIALIZATION ***************************
INITIALIZATION.
S_CPUDT-SIGN = 'I'.
S_CPUDT-OPTION = 'BT'.
S_CPUDT-HIGH = SY-DATUM.
TEMPDATE     = S_CPUDT-HIGH.
TEMPDATE = ( TEMPDATE + 1 ).
TEMPYEAR = S_CPUDT-HIGH(4).
TEMPYEAR = ( TEMPYEAR - 1 ).

IF TEMPDATE+4(2) EQ '03'.
     MOVE '12' TO TEMPDATE+4(2).
     MOVE TEMPYEAR TO TEMPDATE(4).
ELSEIF TEMPDATE+4(2) EQ '02'.
     MOVE '11' TO TEMPDATE+4(2).
     MOVE TEMPYEAR TO TEMPDATE(4).
ELSEIF TEMPDATE+4(2) EQ '01'.
     MOVE '10' TO TEMPDATE+4(2).
     MOVE TEMPYEAR TO TEMPDATE(4).
ELSE.
     TEMPDATE+4(2) = TEMPDATE+4(2) - 3.
ENDIF.

S_CPUDT-LOW      = TEMPDATE.
APPEND S_CPUDT.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
Write: /1 text-rpt, sy-repid color col_negative,
       60 text-900 color col_heading,
      140 text-dte, sy-datum color col_group intensified off inverse on,
          text-amp, sy-uzeit color col_group intensified off inverse on.
write: / text-clt under text-rpt, sy-mandt, sy-sysid,
         text-pge under text-dte, sy-pagno,
      68 s_cpudt-low  color col_group inverse on,
      80 text-030     color col_group inverse on,
      84 s_cpudt-high color col_group inverse on.
skip.
uline.



*FORMAT COLOR COL_NORMAL.

WRITE: /61 text-036, 93 TEXT-028, 149 text-040.

WRITE: /1 TEXT-004, 15 TEXT-004, text-038 under text-036,
       70 TEXT-010,
       87 text-028,
       93 TEXT-031, 102 text-004,
      113 text-035,
      127 TEXT-033, 138 TEXT-010, 166 TEXT-025,
          text-041 under text-040.

WRITE: /1 TEXT-005, 15 TEXT-027, text-037 under text-036,
       70 text-002,
       83 text-012,
       87 text-029, text-032 under text-031,
      102 text-033,
      127 TEXT-009, 138 TEXT-034,
          TEXT-006 under text-025,
          text-042 under text-041.
ULINE.
*WRITE: /.                           "TR277
*WRITE: /1 TEXT-029, P_WERKS.
*ULINE: /1(10).

************************************************************************
START-OF-SELECTION.
FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
CLEAR TABLE1.
REFRESH TABLE1.

SELECT * FROM MARA WHERE MATNR IN S_MATNR
                     AND ( MTART EQ 'HAWA' OR MTART EQ 'HIBE' ).
*                     AND LVORM NE 'X'.                      "2003/01/08

SELECT * FROM MARC WHERE MATNR = MARA-MATNR
                     AND WERKS IN S_WERKS.                   "TR277
*                     AND WERKS = P_WERKS.
*                    AND LVORM NE 'X'.                       "2003/01/08
IF MARC-UMLMC > 0.

   SELECT * FROM MSEG
            WHERE MATNR = MARA-MATNR
              AND WERKS = MARC-WERKS
*              AND ( LGORT = SPACE OR BWART EQ '305' )
              AND ( BWART EQ '303' OR BWART EQ '304' OR
                    BWART EQ '305' OR BWART EQ '306' ).
*-----------------------------------------------------------------------
* GET AVERAGE UNIT PRICE from MBEW table                     "2003/01/30
*-----------------------------------------------------------------------
             clear temp_verpr.
             select single * from mbew
                     where matnr = mara-matnr
                       and bwkey = mseg-umwrk.
             if sy-subrc = '0'.
                 move mbew-verpr   to temp_verpr.
             endif.


      SELECT SINGLE * FROM MKPF WHERE MBLNR = MSEG-MBLNR.
              IF MKPF-CPUDT IN S_CPUDT.
                    SELECT SINGLE * FROM MAKT WHERE MATNR = MARA-MATNR
                                                AND SPRAS = SY-LANGU.

                     MOVE MARA-MATNR TO TABLE1-MATNR.
                     MOVE MARA-MEINS TO TABLE1-MEINS.
                     MOVE MARC-UMLMC TO TABLE1-UMLMC.
                     MOVE MARC-WERKS TO TABLE1-WERKS.      "TR277
                     MOVE MAKT-MAKTX TO TABLE1-MAKTX.
                     MOVE MSEG-MBLNR TO TABLE1-MBLNR.
                     MOVE MSEG-BWART TO TABLE1-BWART.
                     MOVE MSEG-MENGE TO TABLE1-MENGE.
                     MOVE MSEG-UMWRK TO TABLE1-UMWRK.
                     MOVE MSEG-UMLGO TO TABLE1-UMLGO.
                     MOVE MKPF-BLDAT TO TABLE1-BLDAT.
                     MOVE MKPF-CPUTM TO TABLE1-CPUTM.
                     MOVE MKPF-XBLNR TO TABLE1-XBLNR.
                     move mara-lvorm to table1-lvorm.
                     move temp_verpr to table1-verpr.

                     IF ( MSEG-BWART EQ '304' OR MSEG-BWART EQ '305' ).
                          MOVE '-'   TO TEMPMINUS.
                     ELSE.
                          MOVE SPACE TO TEMPMINUS.
                     ENDIF.
                     MOVE TEMPMINUS  TO TABLE1-MINUS.

                     APPEND TABLE1.
                     CLEAR TABLE1.
               ENDIF.
ENDSELECT.
ENDIF.
ENDSELECT.
ENDSELECT.

SORT TABLE1 BY WERKS ASCENDING
               MATNR ASCENDING
               BLDAT ASCENDING
               CPUTM ASCENDING
               BWART ASCENDING.

LOOP AT TABLE1.

ON CHANGE OF TABLE1-WERKS.                      "TR277
   NEW-PAGE.
   WRITE: /.
   WRITE: /1 TEXT-029, TABLE1-WERKS.
   ULINE: /1(10).
ENDON.

ON CHANGE OF TABLE1-MATNR.
   WRITE: /.
   WRITE: /1 TABLE1-MATNR COLOR COL_NEGATIVE INTENSIFIED OFF.
   WRITE: 15 TABLE1-MAKTX,
                  table1-lvorm under text-036,
             (12) TABLE1-UMLMC under text-002 COLOR COL_KEY,
                  TABLE1-MEINS under text-012,
               87 TABLE1-UMWRK,
                  TABLE1-UMLGO under text-031,
              102 TABLE1-MBLNR COLOR COL_TOTAL,
                  table1-xblnr under text-035,
              126 TABLE1-BLDAT,
             (10) TABLE1-MENGE under text-034, TABLE1-MINUS,
                  TABLE1-BWART under text-006 COLOR COL_GROUP,
                  table1-verpr under text-040.
ELSE.
   WRITE: / table1-lvorm under text-036,
            87 TABLE1-UMWRK,
               TABLE1-UMLGO under text-031,
           102 TABLE1-MBLNR COLOR COL_TOTAL,
               TABLE1-XBLNR under text-035, 126 TABLE1-BLDAT,
          (10) TABLE1-MENGE under text-034, TABLE1-MINUS,
               tABLE1-BWART under text-006 COLOR COL_GROUP,
               table1-verpr under text-040.
ENDON.
ENDLOOP.
