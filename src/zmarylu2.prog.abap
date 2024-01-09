REPORT ZMINR013 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR013
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  November 15, 1996
*
* This ABAP will display a Physical Inventory Adjustment Summary for a
* plant/storage location from a specified date.
*
************************************************************************
* 98/12/01 md7140 #--- changes to improve performance
* 97/08/20 md7140 add date range in variant, new page for plant/storloc,
*                 add material group breaks, total for # of SKU's
* 97/05/07 md7140 eliminate duplicate storage location titles
* 97/04/25 md7140 add value columns and totals
************************************************************************
TABLES: ENT1027,                                             "98/12/01
        MARD, MSEG, MKPF,
*       makt, mara,                        "Material Description
        T023T.                             "Material Group Description
DATA  : ISSUE           TYPE I,            "LINE LOCATION FOR ISSUES
        TRANSFER        LIKE ISSUE,        "LINE LOCATION FOR TRANSFERS
        TOTALINE        LIKE ISSUE,        "LINE LOCATION FOR NEW PLANT
        FLAG_WERKS(1)   TYPE C VALUE 'X',  "Plant Print Flag
        FLAG_LGORT(1)   TYPE C VALUE 'X',  "Stloc Print Flag
        FLAG_MATNR(1)   TYPE C VALUE 'X',  "Material Print Flag
        FLAG_MATKL(1)   TYPE C VALUE 'X',  "Material Print Flag
        FLAG_TITLE(1)   TYPE C VALUE 'Y',  "Title flag
*       flag2(1)        type c value 'Y',  "INITIAL PRINT OF PLANT/MATNR
*       flag3(1)        type c value 'N',  "FLAG
*       flag4(1)        type c value 'X',  "MATERIAL NUMBER FLAG
* Material Number Registers
        QTYISSUE        LIKE MSEG-MENGE,   "Issue qty
        VALISSUE        LIKE MSEG-DMBTR,   "Issue value
        QTYTRANSFER     LIKE QTYISSUE,     "Transfer qty
        VALTRANSFER     LIKE MSEG-DMBTR,   "Transfer value
        DIFFER          LIKE QTYISSUE,     "Qty difference issue-trans
        VALDIFFER       LIKE MSEG-DMBTR,   "Value difference issue-trans
* Plant Registers
        QTYISSPLT       LIKE QTYISSUE,     "Issue qty
        VALISSPLT       LIKE MSEG-DMBTR,   "Issue value
        QTYTRNPLT       LIKE QTYISSUE,     "Transfer qty
        VALTRNPLT       LIKE MSEG-DMBTR,   "Transfer value
        QTYDIFFPLT      LIKE QTYISSUE,     "Qty difference issue-trans
        VALDIFFPLT      LIKE MSEG-DMBTR,   "Value difference issue-trans
        SKUPLT(5)       TYPE I,
* Storage Location Registers
        QTYISSSL        LIKE QTYISSUE,     "Issue qty
        VALISSSL        LIKE MSEG-DMBTR,   "Issue value
        QTYTRNSL        LIKE QTYISSUE,     "Transfer qty
        VALTRNSL        LIKE MSEG-DMBTR,   "Transfer value
        QTYDIFFSL       LIKE QTYISSUE,     "Qty difference issue-trans
        VALDIFFSL       LIKE MSEG-DMBTR,   "Value difference issue-trans
        SKUSL(5)        TYPE I,
* Material Group Registers
        QTYISSMG        LIKE QTYISSUE,     "Issue qty
        VALISSMG        LIKE MSEG-DMBTR,   "Issue value
        QTYTRNMG        LIKE QTYISSUE,     "Transfer qty
        VALTRNMG        LIKE MSEG-DMBTR,   "Transfer value
        QTYDIFFMG       LIKE QTYISSUE,     "Qty difference issue-trans
        VALDIFFMG       LIKE MSEG-DMBTR,   "Value difference issue-trans
        SKUMG(5)        TYPE I,
* Report Registers
        QTYISSRPT       LIKE QTYISSUE,     "Issue qty
        VALISSRPT       LIKE MSEG-DMBTR,   "Issue value
        QTYTRNRPT       LIKE QTYISSUE,     "Transfer qty
        VALTRNRPT       LIKE MSEG-DMBTR,   "Transfer value
        QTYDIFFRPT      LIKE QTYISSUE,     "Qty difference issue-trans
        VALDIFFRPT      LIKE MSEG-DMBTR,   "Value difference issue-trans
        SKURPT(5)       TYPE I,

        TEMPWERKS       LIKE MARC-WERKS,   "Temporary Fields
        TEMPLGORT       LIKE MARD-LGORT,
        TEMPMATKL       LIKE MARA-MATKL.

DATA   : BEGIN OF TABLE1 OCCURS 10000,
              WERKS       LIKE MARD-WERKS,           "Plant
              LGORT       LIKE MSEG-LGORT,           "Storage Location
              MATKL       LIKE MARA-MATKL,           "Material Group
              MATNR       LIKE MARA-MATNR,           "Material
              BWART       LIKE MSEG-BWART,
              MAKTX       LIKE MAKT-MAKTX,
              MBLNR       LIKE MSEG-MBLNR,
              ZEILE       LIKE MSEG-ZEILE,
              MENGE       LIKE MSEG-MENGE,
              MEINS       LIKE MSEG-MEINS,
              BUDAT       LIKE MKPF-BUDAT,
              SHKZG       LIKE MSEG-SHKZG,
              LGOBE       LIKE T001L-LGOBE,
              DMBTR       LIKE MSEG-DMBTR,
          END OF TABLE1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_PLANT         FOR   MARD-WERKS OBLIGATORY,
     S_STORLO        FOR   MSEG-LGORT OBLIGATORY,
     S_DATE          FOR   MKPF-BUDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS:
     S_MATNUM        FOR   ENT1027-MATNR,
     S_MATGRP        FOR   ENT1027-MATKL.
SELECTION-SCREEN END OF BLOCK BOX2.
INCLUDE <ICON>.
*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_NEGATIVE,
      110 TEXT-002 COLOR COL_HEADING,
      220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.

IF S_DATE+11(8) = 0.                                   "Period Requested
 WRITE: TEXT-015 UNDER TEXT-002,
        S_DATE+3(8).
ELSE.
  WRITE: TEXT-016 UNDER TEXT-002, S_DATE+3(8), TEXT-017, S_DATE+11(8).
ENDIF.

WRITE: /.
ULINE:  108(41).
WRITE: /255 SY-VLINE.
ULINE: 68(187).
WRITE: /105 TEXT-019,                          "GAIN
        175 TEXT-018,                          "LOSS
        233 TEXT-014.                          "DIFFERENCE
PERFORM VERTICAL_LINES.
ULINE.

*write: /1 text-499, 125 text-498.                           "Column No.

FORMAT COLOR COL_NORMAL.
WRITE: /13 TEXT-006, 23 TEXT-036, 69 TEXT-053,              "Base Titles
        77 TEXT-009, 85 TEXT-011, 102 TEXT-045,              "Gain Title
       121 TEXT-021, 134 TEXT-026,
       147 TEXT-037, 155 TEXT-042, 172 TEXT-046,             "Loss Title
       191 TEXT-038, 204 TEXT-039,
       218 TEXT-043,                                         "Diff Title
       236 TEXT-044.
PERFORM VERTICAL_LINES.

WRITE: /  TEXT-007 UNDER TEXT-006,                          "BASE
          TEXT-013 UNDER TEXT-036,
          TEXT-054 UNDER TEXT-053,
          TEXT-010 UNDER TEXT-009,                          "GAIN
          TEXT-022 UNDER TEXT-021,
          TEXT-007 UNDER TEXT-026,
          TEXT-010 UNDER TEXT-037,                          "LOSS
          TEXT-022 UNDER TEXT-038,
          TEXT-007 UNDER TEXT-039.
PERFORM VERTICAL_LINES.
ULINE.
PERFORM VERTICAL_LINES.

MOVE 11 TO ISSUE.
MOVE 11 TO TRANSFER.

FORMAT COLOR OFF.
************************************************************************

START-OF-SELECTION.
CLEAR: TABLE1.
REFRESH: TABLE1.
*-----------------------------------------------------------------------
*select * from mara where matnr in s_matnum                    "98/12/01
*                    and matkl in s_matgrp
*                    and lvorm ne 'X'.

*select single * from makt where spras = sy-langu
*                            and matnr = mara-matnr.
*  only concerned about deletion at the MARD level
*  select * from marc where matnr = mara-matnr
*                       and werks in s_plant
*                       and lvorm ne 'X'.

*        select * from mard where matnr = mara-matnr
*                             and werks in s_plant
*                             and lgort in s_storlo
*                             and lvorm ne 'X'.

*              select * from mseg where matnr = mara-matnr
*                                   and werks = mard-werks
*                                   and lgort = mard-lgort
*                                   and ( bwart = '701'
*                                      or bwart = '702' ).

*                    select single * from mkpf where mblnr = mseg-mblnr
*                                         and mjahr = mseg-mjahr.
*                   check mkpf-budat in s_date.

*                    move mseg-lgort  to table1-lgort.
*                    move mseg-mblnr  to table1-mblnr.
*                    move mseg-zeile  to table1-zeile.
*                    move mseg-menge  to table1-menge.
*                    move mseg-meins  to table1-meins.
*                    move mseg-bwart  to table1-bwart.
*                    move mseg-werks  to table1-werks.
*                    move mara-matnr  to table1-matnr.
*                    move mara-matkl  to table1-matkl.
*                    move makt-maktx  to table1-maktx.
*                    move mkpf-budat  to table1-budat.
*                    move mseg-shkzg  to table1-shkzg.
*                    move mseg-dmbtr  to table1-dmbtr.
*                    append table1.
*                    clear table1.
*              endselect.
*        endselect.
*endselect.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
 SELECT * FROM MKPF
     WHERE BUDAT IN S_DATE.
  SELECT * FROM MSEG
     WHERE MBLNR = MKPF-MBLNR
       AND MJAHR = MKPF-MJAHR
       AND MATNR IN S_MATNUM
       AND WERKS IN S_PLANT
       AND LGORT IN S_STORLO
       AND ( BWART = '701' OR BWART = '702' ).
     SELECT SINGLE * FROM ENT1027
        WHERE MATNR = MSEG-MATNR                       "Material #
          AND MATKL IN S_MATGRP                        "Material Group
          AND LVORM =  ' '                             "Not deleted
          AND SPRAS = SY-LANGU.                        "Language
     IF SY-SUBRC = '0'.
          SELECT SINGLE * FROM MARD       "only concerned about deletion
              WHERE MATNR = MSEG-MATNR    "at MARD level
                AND WERKS = MSEG-WERKS
                AND LGORT = MSEG-LGORT
                AND LVORM = ' '.
             IF SY-SUBRC = '0'.
                PERFORM ADD_TO_TABLE.
             ENDIF.
     ENDIF.                                      "MARA - single sy-subrc
  ENDSELECT.                                     "End of MSEG
 ENDSELECT.                                      "End of MKPF


PERFORM SORT.

************************************************************************
*  This part of the program processes the internal table 'TABLE1' for
*  output.
*  The first time the loop is executed, the ON CHANGE OF for STORAGE
*  LOCATION(lgort), PLANT(werks) and MATERIAL NUMBER(matnr) is by-passed
*  Everything between the IF STATEMENTS for each ON CHANGE OF is ignored
*  for the first record only.  After the first record has been
*  processed, everthing BETWEEN the first IF STATEMENTS will be
*  executed.
*-----------------------------------------------------------------------
LOOP AT TABLE1.
* write: / table1-werks, table1-lgort, table1-matkl, table1-matnr.

ON CHANGE OF TABLE1-WERKS.
   IF FLAG_WERKS <> 'X'.
      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         MOVE ISSUE TO TOTALINE.
      ELSE.
         MOVE TRANSFER TO TOTALINE.
      ENDIF.

      PERFORM MATERIAL_TOTAL.
      PERFORM MATERIAL_GROUP_TOTAL.
      PERFORM STORAGE_LOCATION_TOTAL.
      PERFORM PLANT_TOTALS.
      TEMPWERKS = TABLE1-WERKS.


      MOVE 'X' TO FLAG_WERKS.
      MOVE 'X' TO FLAG_LGORT.
      MOVE 'X' TO FLAG_MATKL. "97/09/25
      MOVE 'X' TO FLAG_MATNR.
      MOVE 'Y' TO FLAG_TITLE.
      NEW-PAGE.
   ENDIF.
ENDON.

ON CHANGE OF TABLE1-LGORT.
   IF FLAG_LGORT <> 'X'.
      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         MOVE ISSUE TO TOTALINE.
      ELSE.
         MOVE TRANSFER TO TOTALINE.
      ENDIF.

      PERFORM MATERIAL_TOTAL.
      PERFORM MATERIAL_GROUP_TOTAL.
      PERFORM STORAGE_LOCATION_TOTAL.
      TEMPLGORT = TABLE1-LGORT.


      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         ISSUE = ISSUE + 4.
         TRANSFER = ISSUE.
         SKIP TO LINE ISSUE.
*        write: / table1-werks under text-040,            "97/05/07
*                 text-030     under text-030,
*                 table1-lgort under text-041,
*                 table1-matnr under text-006,
*                 table1-maktx under text-036.
*        perform vertical_lines.
      ELSE.
         TRANSFER = TRANSFER + 4.
         ISSUE = TRANSFER.
         SKIP TO LINE ISSUE.
*        write: / table1-werks under text-040,             "97/05/07
*                 text-030     under text-030,
*                 table1-lgort under text-041,
*                 table1-matnr under text-006,
*                 table1-maktx under text-036.
*        perform vertical_lines.
      ENDIF.

      MOVE 'X' TO FLAG_WERKS.
      MOVE 'X' TO FLAG_LGORT.
      MOVE 'X' TO FLAG_MATNR.
      MOVE 'X' TO FLAG_MATKL.  "97/09/25
      MOVE 'Y' TO FLAG_TITLE.
      NEW-PAGE.
   ENDIF.
ENDON.

ON CHANGE OF TABLE1-MATKL.
   IF FLAG_MATKL <> 'X'.
      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         MOVE ISSUE TO TOTALINE.
      ELSE.
         MOVE TRANSFER TO TOTALINE.
      ENDIF.

      PERFORM MATERIAL_TOTAL.
      PERFORM MATERIAL_GROUP_TOTAL.
   TEMPMATKL = TABLE1-MATKL.

      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         ISSUE = ISSUE + 4.
         TRANSFER = ISSUE.
         SKIP TO LINE ISSUE.
*        write: / table1-werks under text-040,            "97/05/07
*                 text-030     under text-030,
*                 table1-lgort under text-041,
*                 table1-matnr under text-006,
*                 table1-maktx under text-036.
*        perform vertical_lines.
      ELSE.
         TRANSFER = TRANSFER + 4.
         ISSUE = TRANSFER.
         SKIP TO LINE ISSUE.
*        write: / table1-werks under text-040,             "97/05/07
*                 text-030     under text-030,
*                 table1-lgort under text-041,
*                 table1-matnr under text-006,
*                 table1-maktx under text-036.
*        perform vertical_lines.
      ENDIF.

      MOVE 'X' TO FLAG_WERKS.
      MOVE 'X' TO FLAG_LGORT.
      MOVE 'X' TO FLAG_MATNR.
      MOVE 'Y' TO FLAG_MATKL.  "97/09/25
      MOVE 'Y' TO FLAG_TITLE.
   ENDIF.
ENDON.

ON CHANGE OF TABLE1-MATNR.
   IF FLAG_MATNR <> 'X'.
      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         MOVE ISSUE TO TOTALINE.
      ELSE.
         MOVE TRANSFER TO TOTALINE.
      ENDIF.

      PERFORM MATERIAL_TOTAL.

      IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
         ISSUE = ISSUE + 2.
         TRANSFER = ISSUE.
         SKIP TO LINE ISSUE.
         WRITE: / TABLE1-MATNR UNDER TEXT-006,        "Material #
                  TABLE1-MAKTX UNDER TEXT-036.        "Material Desc
         PERFORM VERTICAL_LINES.
      ELSE.
         TRANSFER = TRANSFER + 2.
         ISSUE = TRANSFER.
         SKIP TO LINE ISSUE.
         WRITE: / TABLE1-MATNR UNDER TEXT-006,
                  TABLE1-MAKTX UNDER TEXT-036.
         PERFORM VERTICAL_LINES.
      ENDIF.
   ENDIF.
ENDON.
   MOVE 'Y' TO FLAG_MATNR.

IF FLAG_TITLE EQ 'Y'.
   FORMAT COLOR COL_POSITIVE.
   WRITE: /1 TEXT-040, 17 TABLE1-WERKS, TEXT-SLS, TABLE1-LGORT.
   PERFORM VERTICAL_LINES.
   SELECT SINGLE * FROM T023T
      WHERE MATKL = TABLE1-MATKL
        AND SPRAS = SY-LANGU.
   FORMAT COLOR COL_TOTAL.
   WRITE: / TEXT-051, TABLE1-MATKL(4) UNDER TABLE1-WERKS,
            TEXT-DSH, T023T-WGBEZ.
   FORMAT COLOR COL_BACKGROUND.
   PERFORM VERTICAL_LINES.

   WRITE: /1 ' '.
   PERFORM VERTICAL_LINES.

   WRITE: / TABLE1-MATNR UNDER TEXT-006,
            TABLE1-MAKTX UNDER TEXT-036.
   PERFORM VERTICAL_LINES.
   MOVE SY-LINNO TO ISSUE.
   MOVE SY-LINNO TO TRANSFER.
   MOVE 'Y' TO FLAG_WERKS.
   MOVE 'Y' TO FLAG_LGORT.
   MOVE 'Y' TO FLAG_MATNR.
   MOVE 'Y' TO FLAG_MATKL. "97/09/25
   MOVE 'X' TO FLAG_TITLE.
   TEMPWERKS = TABLE1-WERKS.
   TEMPLGORT = TABLE1-LGORT.
   TEMPMATKL = TABLE1-MATKL.
ENDIF.

IF TABLE1-SHKZG = 'S'.
   SKIP TO LINE ISSUE.
   WRITE: TABLE1-BWART UNDER TEXT-009,
          TABLE1-MENGE UNDER TEXT-011,
          TABLE1-BUDAT UNDER TEXT-021,
          TABLE1-MBLNR UNDER TEXT-026,
          TABLE1-DMBTR UNDER TEXT-045.
   PERFORM VERTICAL_LINES.
   QTYISSUE = QTYISSUE + TABLE1-MENGE.
   VALISSUE = VALISSUE + TABLE1-DMBTR.
   ISSUE = ISSUE + 1.
ELSEIF TABLE1-SHKZG = 'H'.
   SKIP TO LINE TRANSFER.
   WRITE: TABLE1-BWART UNDER TEXT-037,
          TABLE1-MENGE UNDER TEXT-042,
          TABLE1-BUDAT UNDER TEXT-038,
          TABLE1-MBLNR UNDER TEXT-039,
          TABLE1-DMBTR UNDER TEXT-046.
   PERFORM VERTICAL_LINES.
   QTYTRANSFER = QTYTRANSFER + TABLE1-MENGE.
   VALTRANSFER = VALTRANSFER + TABLE1-DMBTR.
   TRANSFER = TRANSFER + 1.
ENDIF.

AT LAST.
   IF ISSUE > TRANSFER OR ISSUE = TRANSFER.
*     issue = issue + 1.
      MOVE ISSUE TO TOTALINE.
   ELSE.
*     transfer = transfer + 1.
      MOVE TRANSFER TO TOTALINE.
   ENDIF.
*  totaline = totaline + 1.

   PERFORM MATERIAL_TOTAL.
   PERFORM MATERIAL_GROUP_TOTAL.
   PERFORM STORAGE_LOCATION_TOTAL.
   perform plant_totals.
   PERFORM REPORT_TOTALS.
ENDAT.
ENDLOOP.
WRITE: /, /, / TEXT-END UNDER TEXT-019.


FORM ADD_TO_TABLE.
  TABLE1-LGORT = MSEG-LGORT.
  TABLE1-MBLNR = MSEG-MBLNR.
  TABLE1-ZEILE = MSEG-ZEILE.
  TABLE1-MENGE = MSEG-MENGE.
  TABLE1-MEINS = MSEG-MEINS.
  TABLE1-BWART = MSEG-BWART.
  TABLE1-WERKS = MSEG-WERKS.
  TABLE1-MATNR = ENT1027-MATNR.
  TABLE1-MATKL = ENT1027-MATKL.
  TABLE1-MAKTX = ENT1027-MAKTX.
  TABLE1-BUDAT = MKPF-BUDAT.
  TABLE1-SHKZG = MSEG-SHKZG.
  TABLE1-DMBTR = MSEG-DMBTR.
  APPEND TABLE1.
  CLEAR TABLE1.
ENDFORM.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*    sub-routine SORT
*-----------------------------------------------------------------------
*  This sub-routine will sort the internal table before the data is
*  processed for output.
*-----------------------------------------------------------------------
FORM SORT.
SORT TABLE1 BY WERKS  ASCENDING
               LGORT  ASCENDING
               MATKL  ASCENDING
               MATNR  ASCENDING
               BWART  ASCENDING.
ENDFORM.

*-----------------------------------------------------------------------
*    routine UNDERLINE_QTY_TOTALS
*-----------------------------------------------------------------------
*  This routine underlines the columns to be totalled.
*-----------------------------------------------------------------------
FORM UNDERLINE_QTY_TOTALS.
   ULINE: 92(10), 162(10), 225(10).
   PERFORM VERTICAL_LINES.
ENDFORM.
*-----------------------------------------------------------------------
*    routine UNDERLINE_VALUE_TOTALS
*-----------------------------------------------------------------------
*  This routine underlines the columns to be totalled.
*-----------------------------------------------------------------------
FORM UNDERLINE_VALUE_TOTALS.
   ULINE: 104(13), 174(13),  237(14).
   PERFORM VERTICAL_LINES.
ENDFORM.

*-----------------------------------------------------------------------
*    routine VERTICAL_LINES
*-----------------------------------------------------------------------
*  This routine writes the vertical lines between sections of report
*-----------------------------------------------------------------------
FORM VERTICAL_LINES.
   WRITE:  68 SY-VLINE,  75 SY-VLINE,
          145 SY-VLINE, 215 SY-VLINE, 255 SY-VLINE.
ENDFORM.
*-----------------------------------------------------------------------
*    routine MATERIAL_TOTAL
*-----------------------------------------------------------------------
*  This routine writes the material total
*-----------------------------------------------------------------------
FORM MATERIAL_TOTAL.
      SKIP TO LINE TOTALINE.
      PERFORM UNDERLINE_QTY_TOTALS.
      PERFORM UNDERLINE_VALUE_TOTALS.
      DIFFER = QTYISSUE - QTYTRANSFER.
      VALDIFFER = VALISSUE - VALTRANSFER.
      WRITE: / QTYISSUE UNDER TEXT-011,
               VALISSUE UNDER TEXT-045,
               QTYTRANSFER UNDER TEXT-042,
               VALTRANSFER UNDER TEXT-046,
               DIFFER UNDER TEXT-043,
               VALDIFFER UNDER TEXT-044.
      PERFORM VERTICAL_LINES.
*                                          Calculations for Stloc Totals
      QTYISSMG = QTYISSMG + QTYISSUE.
      QTYTRNMG = QTYTRNMG + QTYTRANSFER.
      VALISSMG = VALISSMG + VALISSUE.
      VALTRNMG = VALTRNMG + VALTRANSFER.
      SKUMG = SKUMG + 1.
      CLEAR:  QTYISSUE, QTYTRANSFER, DIFFER,
              VALISSUE, VALTRANSFER, VALDIFFER.

      ADD 2 TO TOTALINE.
*     move: totaline to sy-linno,
      MOVE: TOTALINE TO TRANSFER,
            TOTALINE TO ISSUE.
ENDFORM.
*-------------------  MATERIAL_GROUP_TOTAL  ---------------------------
*  This routine writes the storage location totals
*-----------------------------------------------------------------------
FORM MATERIAL_GROUP_TOTAL.                              "md7140 DRMM0187
      SKIP TO LINE TOTALINE.
      PERFORM UNDERLINE_VALUE_TOTALS.
      QTYDIFFMG = QTYISSMG - QTYTRNMG.
      VALDIFFMG = VALISSMG - VALTRNMG.
      FORMAT COLOR COL_TOTAL.
      WRITE: / TEXT-050 UNDER TEXT-036,           "Total Material Group
               TEMPMATKL(4),
               TEXT-052,                          "for
               TEMPWERKS NO-GAP, TEXT-SLS NO-GAP, TEMPLGORT NO-GAP,
               62 SKUMG,
               VALISSMG UNDER TEXT-045,
               VALTRNMG UNDER TEXT-046,
               VALDIFFMG UNDER TEXT-044.
      FORMAT COLOR COL_BACKGROUND.
      PERFORM VERTICAL_LINES.
*                                           Calulation for Plant Totals
      QTYISSSL = QTYISSSL + QTYISSMG.
      QTYTRNSL = QTYTRNSL + QTYTRNMG.
      VALISSSL = VALISSSL + VALISSMG.
      VALTRNSL = VALTRNSL + VALTRNMG.
      SKUSL = SKUSL + SKUMG.

      CLEAR: QTYISSMG, QTYTRNMG, QTYDIFFMG,
             VALISSMG, VALTRNMG, VALDIFFMG, SKUMG.

      ADD 2 TO TOTALINE.
      MOVE: TOTALINE TO TRANSFER,
            TOTALINE TO ISSUE.

ENDFORM.
*-----------------------------------------------------------------------
*    routine STORAGE_LOCATION_TOTAL
*-----------------------------------------------------------------------
*  This routine writes the storage location totals
*-----------------------------------------------------------------------
FORM STORAGE_LOCATION_TOTAL.
      SKIP TO LINE TOTALINE.
      PERFORM UNDERLINE_VALUE_TOTALS.
      QTYDIFFSL = QTYISSSL - QTYTRNSL.
      VALDIFFSL = VALISSSL - VALTRNSL.
      FORMAT COLOR COL_POSITIVE.
      WRITE: / TEXT-049 UNDER TEXT-036, TEMPWERKS NO-GAP,
               TEXT-SLS NO-GAP, TEMPLGORT,
               62 SKUSL,
               VALISSSL UNDER TEXT-045,
               VALTRNSL UNDER TEXT-046,
               VALDIFFSL UNDER TEXT-044.
      FORMAT COLOR COL_BACKGROUND.
      PERFORM VERTICAL_LINES.
*                                           Calulation for Plant Totals
      QTYISSPLT = QTYISSPLT + QTYISSSL.
      QTYTRNPLT = QTYTRNPLT + QTYTRNSL.
      VALISSPLT = VALISSPLT + VALISSSL.
      VALTRNPLT = VALTRNPLT + VALTRNSL.
      SKUPLT = SKUPLT + SKUSL.

      CLEAR: QTYISSSL, QTYTRNSL, QTYDIFFSL,
             VALISSSL, VALTRNSL, VALDIFFSL, SKUSL.

      ADD 2 TO TOTALINE.
*     move: totaline to sy-linno,
      MOVE: TOTALINE TO TRANSFER,
            TOTALINE TO ISSUE.

ENDFORM.
*--------------------------  PLANT_TOTAL  ------------------------------
*  This routine writes the plant totals
*-----------------------------------------------------------------------
form plant_totals.
      SKIP TO LINE TOTALINE.
      PERFORM UNDERLINE_VALUE_TOTALS.
      QTYDIFFPLT = QTYISSPLT - QTYTRNPLT.
      VALDIFFPLT = VALISSPLT - VALTRNPLT.
      WRITE: / TEXT-047 UNDER TEXT-036, TEMPWERKS,
               VALISSPLT UNDER TEXT-045,
               VALTRNPLT UNDER TEXT-046,
               VALDIFFPLT UNDER TEXT-044,
              62 SKUPLT.
      PERFORM VERTICAL_LINES.
      WRITE: /, ' '.
      PERFORM VERTICAL_LINES.
*                                         Calculations for Report Totals
      QTYISSRPT = QTYISSRPT + QTYISSPLT.
      QTYTRNRPT = QTYTRNRPT + QTYTRNPLT.
      VALISSRPT = VALISSRPT + VALISSPLT.
      VALTRNRPT = VALTRNRPT + VALTRNPLT.
      SKURPT = SKURPT + SKUPLT.

      CLEAR: QTYISSPLT, QTYTRNPLT, QTYDIFFPLT,
             VALISSPLT, VALTRNPLT, VALDIFFPLT, SKUPLT.

      ADD 2 TO TOTALINE.
*     move: totaline to sy-linno,
      MOVE: TOTALINE TO TRANSFER,
            TOTALINE TO ISSUE.

ENDFORM.
*-----------------------------------------------------------------------
*    routine REPORT_TOTALS
*-----------------------------------------------------------------------
*  This routine writes the report totals
*-----------------------------------------------------------------------
FORM REPORT_TOTALS.
      SKIP TO LINE TOTALINE.
      PERFORM UNDERLINE_VALUE_TOTALS.
      QTYDIFFRPT = QTYISSRPT - QTYTRNRPT.
      VALDIFFRPT = VALISSRPT - VALTRNRPT.
      WRITE: / TEXT-048 UNDER TEXT-036,
               VALISSRPT UNDER TEXT-045,
               VALTRNRPT UNDER TEXT-046,
               VALDIFFRPT UNDER TEXT-044,
               62 SKURPT.
      PERFORM VERTICAL_LINES.
ENDFORM.
