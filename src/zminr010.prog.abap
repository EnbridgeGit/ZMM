REPORT ZMINR010 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMINR010
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  September 11, 1996
*    Modified    :  November  15, 1996
*                   July 23,1997 -added movement types 561/562  raarssen
*
* This ABAP will display the Goods Issued and Transfer to a Contractor
* from a specified date.
*
* 98/08/10 md7140      If line is first filled with issues, followed by
*                      receipts, WBS element will wipe out first digit
*                      of "Movement Type".
* 98/06/18 md7140 #539 Rewrite how the report is displayed.
*
************************************************************************
TABLES: MARA, MARC, MARD, MSEG, MKPF, MAKT,
        T001W,      "Plant Description
        T001L,      "Storage Location Description
        T023T.      "Material Group Description

DATA:  WERKS            LIKE MARD-WERKS,
       LGORT            LIKE MARD-LGORT,
       MATKL            LIKE MARA-MATKL,
       MATNR            LIKE MARA-MATNR,
       LABST            LIKE MARD-LABST,   "Current QOH
       QTYISSUE         TYPE P DECIMALS 3, "TOTAL OF GOODS ISSUE
       QTYTRANSFER      LIKE QTYISSUE,     "TOTAL OF TRANSFERS
       DIFFER           LIKE QTYISSUE,     "DIFFERENCE OF QTYISSUE &
                                           "              QTYTRANSFER
       BEGQOH           LIKE QTYISSUE,     "Beginning QOH
       DIFFER2          LIKE QTYISSUE.     "Difference

DATA  : ISSUE           TYPE I,            "LINE LOCATION FOR ISSUES
        RECEIPT         LIKE ISSUE,        "LINE LOCATION FOR TRANSFERS
        TOTALINE        LIKE ISSUE,        "LINE LOCATION FOR NEW PLANT
        FLAG(1)         TYPE C VALUE 'X',  "FLAG
        FLAG2(1)        TYPE C VALUE 'Y',  "INITIAL PRINT OF PLANT/MATNR
        FLAG3(1)        TYPE C VALUE 'N',  "FLAG
        FLAG4(1)        TYPE C VALUE 'X'.  "MATERIAL NUMBER FLAG

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_PRIMARY   LIKE CABN-ATINN,       "Primary Characteristic
       G_ATINN_SECONDARY LIKE CABN-ATINN,       "Secondary Character
       PRI_LGTH          LIKE AUSP-ATWRT,       "Primary Desc
       SEC_LGTH          LIKE AUSP-ATWRT.       "Secondary Desc

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

DATA   : BEGIN OF TABLE1 OCCURS 10000,
              WERKS       LIKE MARD-WERKS,           "Plant
              LGORT       LIKE MSEG-LGORT,           "S/loc
              MATKL       LIKE MARA-MATKL,           "Material Group
              MATNR       LIKE MARA-MATNR,           "Material
              BUDAT       LIKE MKPF-BUDAT,
              BWART       LIKE MSEG-BWART,
              LABST       LIKE MARD-LABST,
*             maktx       like makt-maktx,
              MBLNR       LIKE MSEG-MBLNR,
              ZEILE       LIKE MSEG-ZEILE,
              MENGE       LIKE MSEG-MENGE,
              MEINS       LIKE MSEG-MEINS,
              SHKZG       LIKE MSEG-SHKZG,
              LGOBE       LIKE T001L-LGOBE,
              FROMTO(16)  TYPE C,
              FROMTOP          LIKE MSEG-PS_PSP_PNR,
*             umwrk       like mseg-umwrk,           "Receiving Plant
*             umlgo       like mseg-umlgo,           "Receiving S/Loc
          END OF TABLE1.

DATA:  BEGIN OF TABLE2 OCCURS 10000.
            INCLUDE STRUCTURE TABLE1.
DATA:  END OF TABLE2.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
           S_PLANT    FOR   MARC-WERKS OBLIGATORY,
           S_STORLO   FOR   MSEG-LGORT OBLIGATORY,
           S_DATE     FOR   MKPF-BUDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS:
           S_MATNUM    FOR   MARA-MATNR,
           S_MATGRP    FOR   MARA-MATKL.
SELECTION-SCREEN END OF BLOCK BOX2.
INCLUDE <ICON>.

INITIALIZATION.
   MOVE 'IBT' TO S_DATE.
   MOVE SY-DATUM TO S_DATE+11(8).
   APPEND S_DATE.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID, 70 TEXT-TTL, 142 TEXT-DTE, SY-DATUM,
          TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

IF S_DATE+1(2) = 'BT'.                                      "Time Period
   WRITE: 74 S_DATE+3(4) NO-GAP, TEXT-SLS NO-GAP, S_DATE+7(2) NO-GAP,
             TEXT-SLS NO-GAP, S_DATE+9(2), TEXT-DSH,
             S_DATE+11(4) NO-GAP, TEXT-SLS NO-GAP, S_DATE+15(2) NO-GAP,
             TEXT-SLS NO-GAP, S_DATE+17(2).
ELSE.
   WRITE: 80 S_DATE+3(4) NO-GAP, TEXT-SLS NO-GAP, S_DATE+7(2) NO-GAP,
             TEXT-SLS NO-GAP, S_DATE+9(2).
ENDIF.

WRITE: /.
WRITE: /1 TEXT-100, 19 WERKS, TEXT-DSH, T001W-NAME1.             "Plant
WRITE: / TEXT-101 UNDER TEXT-100, LGORT UNDER WERKS,
         TEXT-DSH UNDER TEXT-DSH, T001L-LGOBE UNDER T001W-NAME1. "S/loc
WRITE: / TEXT-102 UNDER TEXT-100, MATKL UNDER WERKS,
         TEXT-DSH UNDER TEXT-DSH, T023T-WGBEZ UNDER T001W-NAME1. "MatGp
ULINE.

WRITE: /.
MOVE 10 TO ISSUE.
MOVE 10 TO RECEIPT.

FORMAT COLOR OFF.
************************************************************************

START-OF-SELECTION.
MOVE 'PRIMARY_DESCRIPTION'       TO CHARIC.   "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_PRIMARY.

MOVE 'SECONDARY_DESCRIPTION'     TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_SECONDARY.

CLEAR: TABLE1.
REFRESH: TABLE1.

SELECT * FROM MARA WHERE MATNR IN S_MATNUM
                     AND MATKL IN S_MATGRP
                     AND LVORM NE 'X'.

SELECT SINGLE * FROM MAKT WHERE SPRAS = SY-LANGU
                            AND MATNR = MARA-MATNR.

   SELECT * FROM MARC WHERE MATNR = MARA-MATNR
                        AND WERKS IN S_PLANT
                        AND LVORM NE 'X'.

        SELECT * FROM MARD WHERE MATNR = MARA-MATNR
                             AND WERKS = MARC-WERKS
                             AND LGORT IN S_STORLO
                             AND LVORM NE 'X'.

              SELECT * FROM MSEG WHERE MATNR = MARA-MATNR
                                   AND WERKS = MARC-WERKS
                                   AND LGORT = MARD-LGORT.
*                                   and ( bwart = '311'
*                                      or bwart = '312'
*                                      or bwart = '301'
*                                      or bwart = '302'
*                                      or bwart = '561'
*                                      or bwart = '562'
*                                      or bwart = '701'
*                                      or bwart = '702'
*                                      or bwart = '291'
*                                      or bwart = '292' ).

                    SELECT SINGLE * FROM MKPF WHERE MBLNR = MSEG-MBLNR
                                                AND MJAHR = MSEG-MJAHR
                                                AND BUDAT IN S_DATE.

                      IF SY-SUBRC = '0'.
                         MOVE MARD-WERKS  TO TABLE1-WERKS.
                         MOVE MARD-LABST  TO TABLE1-LABST.
                         MOVE MSEG-LGORT  TO TABLE1-LGORT.
                         MOVE MARA-MATKL  TO TABLE1-MATKL.
                         MOVE MARA-MATNR  TO TABLE1-MATNR.
                         MOVE MSEG-MBLNR  TO TABLE1-MBLNR.
                         MOVE MSEG-ZEILE  TO TABLE1-ZEILE.
                         MOVE MSEG-MENGE  TO TABLE1-MENGE.
                         MOVE MSEG-MEINS  TO TABLE1-MEINS.
                         MOVE MSEG-BWART  TO TABLE1-BWART.
*                        move makt-maktx  to table1-maktx.
                         MOVE MKPF-BUDAT  TO TABLE1-BUDAT.
                         MOVE MSEG-SHKZG  TO TABLE1-SHKZG.
                         IF MSEG-UMWRK <> SPACE.
                            MOVE MSEG-UMWRK TO TABLE1-FROMTO.
                            MOVE '/'        TO TABLE1-FROMTO+4(1).
                            MOVE MSEG-UMLGO TO TABLE1-FROMTO+5(4).
                         ELSEIF MSEG-AUFNR <> SPACE.
                            MOVE MSEG-AUFNR TO TABLE1-FROMTO.
                         ELSEIF MSEG-PS_PSP_PNR <> SPACE.
                           MOVE MSEG-PS_PSP_PNR TO TABLE1-FROMTOP.
                         ELSE.
                            MOVE MSEG-KOSTL TO TABLE1-FROMTO.
                         ENDIF.
                         APPEND TABLE1.
                         CLEAR TABLE1.
                     ENDIF.
              ENDSELECT.
        ENDSELECT.
   ENDSELECT.
ENDSELECT.

SORT TABLE1 BY LGORT  ASCENDING
               WERKS  ASCENDING
               MATKL  ASCENDING
               MATNR  ASCENDING
               BUDAT  DESCENDING.

LOOP AT TABLE1.
  MOVE TABLE1 TO TABLE2.
  APPEND TABLE2.
ENDLOOP.
************************************************************************
*  This part of the program processes the internal table 'TABLE1' for
*  output.
*  The first time the loop is executed, the ON CHANGE OF for STORAGE
*  LOCATION(werks), PLANT(lgort) and MATERIAL NUMBER(matnr) is by-passed
*  Everything between the IF STATEMENTS for each ON CHANGE OF is ignored
*  for the first record only.  After the first record has been
*  processed, everthing BETWEEN the first IF STATEMENTS will be
*  executed.
*-----------------------------------------------------------------------

LOOP AT TABLE1.
   MOVE TABLE1-WERKS TO WERKS.                   "Used to print header
   MOVE TABLE1-LGORT TO LGORT.
   MOVE TABLE1-MATKL TO MATKL.
   MOVE TABLE1-MATNR TO MATNR.
   MOVE TABLE1-LABST TO LABST.



   AT NEW WERKS.                                 "New Plant
      PERFORM CALCULATE_DIFFERENCE.
      SELECT SINGLE * FROM T001W
         WHERE WERKS = TABLE1-WERKS
           AND SPRAS = SY-LANGU.
      NEW-PAGE.
   ENDAT.

   AT NEW LGORT.                                   "New Storage Location
      PERFORM CALCULATE_DIFFERENCE.
      SELECT SINGLE * FROM T001L
         WHERE WERKS = TABLE1-WERKS
           AND LGORT = TABLE1-LGORT.
      NEW-PAGE.
   ENDAT.

   AT NEW MATKL.                                   "New Material Group
      PERFORM CALCULATE_DIFFERENCE.
      SELECT SINGLE * FROM T023T
         WHERE MATKL = TABLE1-MATKL
           AND SPRAS = SY-LANGU.
      NEW-PAGE.
      ULINE.
   ENDAT.

   AT NEW MATNR.
      PERFORM CALCULATE_DIFFERENCE.
      OBJECT = TABLE1-MATNR.                          "Material Headings
      PERFORM FIND_CHARACTERISTIC.
      WRITE: /2 TEXT-006,  11 TEXT-013, 100 TEXT-011,
            120 TEXT-012, 140 TEXT-014,
              1 SY-VLINE, 170 SY-VLINE.
      WRITE: / TABLE1-MATNR UNDER TEXT-006,
               PRI_LGTH UNDER TEXT-013 NO-GAP,
               SEC_LGTH,
               LABST   DECIMALS 0 UNDER TEXT-011,
               BEGQOH  DECIMALS 0 UNDER TEXT-012,
               DIFFER2 DECIMALS 0 UNDER TEXT-014,
              1 SY-VLINE, 170 SY-VLINE.
      ULINE.

      ADD 3 TO: RECEIPT, ISSUE.                    "Receipt/Issue Title
      SKIP TO LINE RECEIPT.
      WRITE: 2 TEXT-017,  77 TEXT-018, 75 SY-VLINE, 150 SY-VLINE,
             1 SY-VLINE, 170 SY-VLINE.

      PERFORM PRINT_VERT.
      WRITE: 2 TEXT-039, 77 TEXT-040, 75 SY-VLINE, 150 SY-VLINE,
             1 SY-VLINE, 170 SY-VLINE.

      PERFORM PRINT_VERT.                       "Movement Heading
      WRITE: 4 TEXT-010,                        "RECEIPTS - Type
            10 TEXT-031,                        "Quantity
            30 TEXT-033,                        "Trans Date
            43 TEXT-035,                        "Document
            56 TEXT-037,                        "To/From
            79 TEXT-029,                        "ISSUES   - Type
            85 TEXT-032,                        "Quantity
           105 TEXT-034,                        "Trans Date
           118 TEXT-036,                        "Document
           131 TEXT-038,                        "To/From
           153 TEXT-015,                        "Difference
            75 SY-VLINE, 150 SY-VLINE, 1 SY-VLINE, 170 SY-VLINE.
      PERFORM PRINT_VERT.
      WRITE: 2 SY-ULINE(8),  14 SY-ULINE(8),  30 SY-ULINE(10),
            43 SY-ULINE(8),  56 SY-ULINE(7),
            77 SY-ULINE(8),  89 SY-ULINE(8), 105 SY-ULINE(10),
           118 SY-ULINE(8), 131 SY-ULINE(7), 153 SY-ULINE(10),
            75 SY-VLINE, 150 SY-VLINE, 1 SY-VLINE, 170 SY-VLINE.
   ENDAT.


* Print details.
  IF TABLE1-SHKZG = 'S'.
     ADD TABLE1-MENGE TO QTYTRANSFER.
     ADD 1 TO RECEIPT.
     SKIP TO LINE RECEIPT.
     WRITE: 4 TABLE1-BWART,
           10 TABLE1-MENGE DECIMALS 0,
           30 TABLE1-BUDAT,
           43 TABLE1-MBLNR,
           56(16) TABLE1-FROMTO.
           IF TABLE1-FROMTO = SPACE.
               WRITE: 56(16) TABLE1-FROMTOP.
             ENDIF.
     WRITE: 75 SY-VLINE, 150 SY-VLINE, 1 SY-VLINE, 170 SY-VLINE.
*    write: 1 sy-vline, 170 sy-vline.
  ELSE.
     ADD TABLE1-MENGE TO QTYISSUE.
     ADD 1 TO ISSUE.
     SKIP TO LINE ISSUE.
     WRITE: 79 TABLE1-BWART,
            85 TABLE1-MENGE DECIMALS 0,
           105 TABLE1-BUDAT,
           118 TABLE1-MBLNR,
           131 TABLE1-FROMTO.
            IF TABLE1-FROMTO = SPACE.
               WRITE: 131 TABLE1-FROMTOP.
            ENDIF.
     WRITE: 1 SY-VLINE, 170 SY-VLINE.
     WRITE:  75 SY-VLINE, 150 SY-VLINE.
  ENDIF.

  AT END OF MATNR.
     IF RECEIPT > ISSUE.
        MOVE RECEIPT TO: SY-LINNO, ISSUE.
        SKIP TO LINE RECEIPT.
     ELSE.
        MOVE ISSUE   TO: SY-LINNO, RECEIPT.
        SKIP TO LINE ISSUE.
     ENDIF.
     PERFORM PRINT_VERT.
     WRITE: 10 SY-ULINE(16), 85 SY-ULINE(16), 75 SY-VLINE, 150 SY-VLINE,
             1 SY-VLINE,    170 SY-VLINE.

     DIFFER = QTYTRANSFER - QTYISSUE.
     PERFORM PRINT_VERT.
     WRITE: 10 QTYTRANSFER DECIMALS 0,
            85 QTYISSUE    DECIMALS 0,
           153 DIFFER      DECIMALS 0,
            75 SY-VLINE, 150 SY-VLINE, 1 SY-VLINE, 170 SY-VLINE.
     PERFORM PRINT_VERT.
     ULINE.
     CLEAR: QTYTRANSFER, QTYISSUE, DIFFER.
     MOVE SY-LINNO TO: RECEIPT, ISSUE.
  ENDAT.

  IF SY-LINNO > 52.
     NEW-PAGE.
  ENDIF.
ENDLOOP.
WRITE: /50 TEXT-END.

FORM CALCULATE_DIFFERENCE.
  CLEAR DIFFER2.
  LOOP AT TABLE2.
  IF TABLE2-WERKS = TABLE1-WERKS AND
     TABLE2-LGORT = TABLE1-LGORT AND
     TABLE2-MATKL = TABLE1-MATKL AND
     TABLE2-MATNR = TABLE1-MATNR.
     IF TABLE2-SHKZG = 'S'.
       ADD TABLE2-MENGE TO DIFFER2.
     ELSE.
       SUBTRACT TABLE2-MENGE FROM DIFFER2.
     ENDIF.
  ENDIF.
 ENDLOOP.
 BEGQOH = LABST - DIFFER2.

ENDFORM.


*----------------------------  GET_ATINN -------------------------------
* Routine used to get the internal character number for material class
*-----------------------------------------------------------------------
FORM GET_ATINN.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
           CLASS_TYPE                   = '001'
           FEATURE_NEUTRAL_NAME         = CHARIC
       IMPORTING
           FEATURE_ID                   = G_ATINN
       EXCEPTIONS
           INVALID_CLASS_TYPE           = 1
           MISSING_FEATURE_INFORMATION  = 2
           NO_FEATURE_FOUND             = 3
           NO_FEATURE_VALID             = 4
           NO_LANGUAGE                  = 5
           OTHERS                       = 6.
 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF', CHARIC.
 ENDIF.
ENDFORM.

*------------------------  FIND_CHARACTERISTIC  ------------------------

FORM FIND_CHARACTERISTIC.
    REFRESH CHAR_TAB.
    CALL FUNCTION 'CLFM_SELECT_AUSP'
        EXPORTING
            MAFID           = 'O'
            CLASSTYPE       = '001'
            OBJECT          = OBJECT
        TABLES
            EXP_AUSP        = CHAR_TAB
        EXCEPTIONS
            NO_VALUES       = 1
            OTHERS          = 2.
* Character values are in "ATWRT", (numeric values would be in "ATFLV")
 CLEAR: PRI_LGTH, SEC_LGTH.
*refresh: char_tab.
 IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
 ENDIF.
* primary description                                   (only one)
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PRIMARY BINARY SEARCH.
    IF SY-SUBRC EQ 0.
       MOVE CHAR_TAB-ATWRT TO PRI_LGTH.
    ENDIF.
* secondary description                                 (only one)
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_SECONDARY BINARY SEARCH.
    IF SY-SUBRC EQ 0 AND CHAR_TAB-ATWRT <> 'N/A'.
       MOVE CHAR_TAB-ATWRT TO SEC_LGTH.
    ENDIF.
 CLEAR: OBJECT.
ENDFORM.

*----------------------  PRINT_VERT  -----------------------------------

FORM PRINT_VERT.
      ADD 1 TO: RECEIPT, ISSUE.
      SKIP TO LINE RECEIPT.
ENDFORM.
