REPORT ZMMMI016 LINE-SIZE 120 LINE-COUNT 90 NO STANDARD PAGE HEADING
************************************************************************
* 2003/03/25 mdemeest Adjusted transfer lengths for BMM00 & BMMH1 to
*                     reflect longer table definitions for 4.6B
* 2000/06/21 gymana   4.6B Upgrade Changed table DD03P to DD03L
************************************************************************
MESSAGE-ID ZM.

TABLES: EINA,
        MARA,
        T001W,
        EINE,
        MAKT,
        MARC.

DATA: BEGIN OF BDCDATA OCCURS 10.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: BEGIN OF ITAB OCCURS 100,
      LIFNR    LIKE EINA-LIFNR,        " Vendor
      MATNR    LIKE EINA-MATNR,        " Material
      MAKTX    LIKE MAKT-MAKTX,        " Description
      WERKS    LIKE MARC-WERKS,        " Plant
      APLFZOLD LIKE EINE-APLFZ,        " Old Delivery time
      APLFZNEW LIKE EINE-APLFZ,        " New Delivery time
      MATKL    LIKE EINA-MATKL,        " Material Group
      EKORG    LIKE EINE-EKORG,        " Purchasing Organization
      END OF   ITAB.

DATA:   BEGIN OF MESSTAB OCCURS 10.    "Transaction return
        INCLUDE STRUCTURE BDCMSGCOLL.  "messages
DATA:   END OF MESSTAB.

SELECT-OPTIONS: SLIFNR FOR EINA-LIFNR OBLIGATORY,  "Vendor
                SMATNR FOR EINA-MATNR, "MAterial
                SMATKL FOR EINA-MATKL, "Material group
                SEKORG FOR EINE-EKORG OBLIGATORY,  "Purchasing org. 'x'.
                SWERKS FOR MARC-WERKS. "Plant

PARAMETERS:     PAPLFZ LIKE EINE-APLFZ OBLIGATORY.  " Delivery time
DATA:  VARMOD TYPE C VALUE 'N',        " Can be 'A' 'E' or 'N'
       APLFZC(3) TYPE C.

DATA: BEGIN OF ITAB1 OCCURS 100,
      MATNR    LIKE EINA-MATNR,        "Material
      WERKS    LIKE EINE-WERKS,        "Werks
      APLFZOLD LIKE EINE-APLFZ,        "Old Delivery Time
      APLFZNEW LIKE EINE-APLFZ,        "New Delivery Time
      END OF ITAB1.

************************************************************************
* This part of data of the program is connected to RMMMBIM0
************************************************************************
TABLES  :  DD03L.

FIELD-SYMBOLS: <F1> .
DATA      : T_CODE       LIKE BMM00-TCODE VALUE 'MM02',
            CHAR(21)     TYPE C,
            NODATA(40) VALUE '////////////////////////////////////////'.

DATA      : PHYFILE LIKE FILENAMECI-FILEEXTERN.
DATA:       INDEX LIKE SY-INDEX.

SELECTION-SCREEN ULINE.
PARAMETERS: CORRFILE LIKE FILENAME-FILEINTERN
            DEFAULT 'ZMMMI016' NO-DISPLAY.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECTION-SCREEN COMMENT 1(70) TEXT-CO1.
SELECTION-SCREEN COMMENT /1(70) TEXT-CO2.
SELECTION-SCREEN COMMENT /1(70) TEXT-CO3 MODIF ID A.
SELECTION-SCREEN END OF BLOCK B1.

DATA    : BEGIN OF Z_BGR00.
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BMM00.
        INCLUDE STRUCTURE BMM00.
DATA    : END OF Z_BMM00.

DATA    : BEGIN OF Z_BMMH1.
        INCLUDE STRUCTURE BMMH1.
DATA    : END OF Z_BMMH1.

INCLUDE <ICON>.
* Check user entry.
IF ( ( NOT  SMATKL-LOW IS INITIAL ) AND ( NOT SMATNR-LOW IS INITIAL ) ).
    MESSAGE E018.
  ENDIF.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'A'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

************************************************************************

START-OF-SELECTION.
*Purchasing Info Record - General Data
  SELECT * FROM EINA WHERE
                           MATNR IN SMATNR AND
                           LIFNR IN SLIFNR.


* EINA-MATKL is not populated, so we are checking material master


    SELECT SINGLE * FROM MARA WHERE
                             MATNR EQ EINA-MATNR AND
                             MATKL IN SMATKL.
    CHECK SY-SUBRC = 0.



*Purchasing Info Record - Purchasing Organization Data
    SELECT * FROM EINE WHERE
                             INFNR EQ EINA-INFNR AND
                             EKORG IN SEKORG.
      CHECK SY-SUBRC = 0.

      SELECT SINGLE * FROM MAKT WHERE
                                     MATNR EQ EINA-MATNR AND
                                     SPRAS EQ SY-LANGU.
      MOVE :
            EINA-LIFNR TO ITAB-LIFNR,  " Vendor
            EINA-MATNR TO ITAB-MATNR,  " Material
            MAKT-MAKTX TO ITAB-MAKTX,  " Description
            EINE-WERKS TO ITAB-WERKS,  " Plant
            EINE-APLFZ TO ITAB-APLFZOLD,      " Old Lead time
            PAPLFZ TO ITAB-APLFZNEW,   " New Lead Time
            MARA-MATKL TO ITAB-MATKL,  " Material Group
            EINE-EKORG TO ITAB-EKORG.  " Purchasing Organization

      APPEND ITAB.
*break-point.
    ENDSELECT.
    CHECK SY-SUBRC = 0.
  ENDSELECT.
  CHECK SY-SUBRC = 0.


  SORT ITAB BY LIFNR MATNR EKORG.
  LOOP AT ITAB.
    MOVE SY-TABIX TO INDEX.

    PERFORM BDC_SCREEN USING 'SAPMM06I' '100'.
    PERFORM BDC_FIELD USING  'EINA-LIFNR' ITAB-LIFNR.    " VENDOR
    PERFORM BDC_FIELD USING  'EINA-MATNR' ITAB-MATNR.    " MATERIAL
    PERFORM BDC_FIELD USING  'EINE-EKORG' ITAB-EKORG.    " PURCH. ORG.

    PERFORM BDC_SCREEN USING 'SAPMM06I' '101'.
    PERFORM BDC_FIELD USING  'BDC_OKCODE' '/7'.  " Push but.

    WRITE ITAB-APLFZNEW TO APLFZC.
    PERFORM BDC_SCREEN USING 'SAPMM06I' '102'.
    PERFORM BDC_FIELD USING  'EINE-APLFZ' APLFZC.
    PERFORM BDC_FIELD USING  'BDC_OKCODE' '/11'.
* break-point.
    CALL TRANSACTION 'ME12' USING BDCDATA MODE VARMOD
                            MESSAGES INTO MESSTAB
                            UPDATE 'S'."Synchronous
*break-point.
    REFRESH BDCDATA.
    IF SY-SUBRC = 0.
      FORMAT COLOR 2.
      WRITE: / ITAB-LIFNR UNDER TEXT-001, ITAB-MATNR UNDER TEXT-003,
               ITAB-MAKTX UNDER TEXT-004, ITAB-WERKS UNDER TEXT-005,
             ITAB-APLFZOLD UNDER TEXT-007, ITAB-APLFZNEW UNDER TEXT-008,
               ITAB-MATKL UNDER TEXT-010, ITAB-EKORG UNDER TEXT-011.
* Find the info rec number
      CLEAR ITAB.
      READ TABLE MESSTAB INDEX INDEX.
      IF SY-SUBRC = 0.
        WRITE: MESSTAB-MSGV1 UNDER TEXT-012.
      ENDIF.
      FORMAT RESET.
    ELSE.
* ERROR OUTPUT
      CLEAR ITAB-APLFZNEW.
      FORMAT COLOR 6 INTENSIFIED ON.
      WRITE: / ITAB-LIFNR UNDER TEXT-001, ITAB-MATNR UNDER TEXT-003,
               ITAB-MAKTX UNDER TEXT-004, ITAB-WERKS UNDER TEXT-005,
             ITAB-APLFZOLD UNDER TEXT-007, ITAB-APLFZNEW UNDER TEXT-008,
               ITAB-MATKL UNDER TEXT-010, ITAB-EKORG UNDER TEXT-011.
      FORMAT RESET.
      CONTINUE.
    ENDIF.

  ENDLOOP.
*break-point.

  WRITE: / 'END OF REPORT1'.
  SKIP.
  ULINE.
******************************* Second Part of The Program**************
* Fill the table for another BDC.
  LOOP AT ITAB.
    SELECT * FROM MARC WHERE
                            MATNR EQ ITAB-MATNR AND
                            WERKS IN SWERKS.
      MOVE:
           ITAB-MATNR    TO ITAB1-MATNR,
           MARC-WERKS    TO ITAB1-WERKS,
           MARC-PLIFZ TO ITAB1-APLFZOLD, " Old delivery time
           ITAB-APLFZNEW TO ITAB1-APLFZNEW. "New delivery time
      APPEND ITAB1.
    ENDSELECT.
    CHECK SY-SUBRC = 0.
  ENDLOOP.
*break-point.

  PERFORM OPEN_OUTPUT_FILE.            " Get Physical name and open file
  PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BGR00.

* Fill the struct. BGR00
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZMM Delitime' TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167. "Another file is that size

* Fill the struct. BMM00.
  LOOP AT ITAB1.

    MOVE '1'            TO Z_BMM00-STYPE.  "Obligatory
    MOVE T_CODE         TO Z_BMM00-TCODE.  " T-code
    MOVE ITAB1-MATNR    TO Z_BMM00-MATNR.  " Material
    MOVE ITAB1-WERKS    TO Z_BMM00-WERKS.  "Plant
    MOVE 'X'            TO Z_BMM00-XEID1.  " MRP View
    TRANSFER Z_BMM00  TO PHYFILE LENGTH 199.

* Fill the struct BMMH1
    MOVE '2'              TO Z_BMMH1-STYPE.    ""Obligatory
    MOVE ITAB1-APLFZNEW   TO Z_BMMH1-PLIFZ.    " Planned delivery time
    TRANSFER Z_BMMH1      TO PHYFILE LENGTH 2744.

    PERFORM INIT_STRUCTURES USING 'BMM00'. " I_BGR00.
    PERFORM INIT_STRUCTURES USING 'BMMH1'. " I_BGR00.

  ENDLOOP.
  CLOSE DATASET PHYFILE.
* Output report
  SKIP 2.
  WRITE:/ TEXT-COM, 'New delivery time is expected time.'.
  SKIP.
  LOOP AT ITAB1.
    FORMAT COLOR 2.

    WRITE: / ITAB1-MATNR UNDER TEXT-003,
             ITAB1-WERKS UNDER TEXT-005,
             ITAB1-APLFZNEW UNDER TEXT-008,
             ITAB1-APLFZOLD UNDER TEXT-007.
    AT END OF MATNR.
      WRITE: /8 SY-ULINE(6).
    ENDAT.

    AT LAST.
      WRITE: /60 TEXT-END.
    ENDAT.
  ENDLOOP.
  FORMAT RESET.
******************************* Second Part of The Program**************

TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         20 SY-TITLE COLOR 4 INTENSIFIED ON,
        86 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.


  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  SKIP.
  WRITE: /1 TEXT-001, 8 TEXT-003, 20 TEXT-004, 64 TEXT-005, 72 TEXT-006.
  WRITE: 83 TEXT-010, 96 TEXT-011, 109 TEXT-012.

  WRITE: /72 TEXT-007, 76 SY-VLINE, 78 TEXT-008.

END-OF-PAGE.
  PERFORM BDC_SCREEN USING SY-DATUM SY-DATUM.
  PERFORM BDC_FIELD USING SY-DATUM SY-DATUM.

*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                               " BDC_SCREEN

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                               " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  OPEN_OUTPUT_FILE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = CORRFILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH CORRFILE.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.

ENDFORM.                               " OPEN_OUTPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  INIT_STRUCTURES
*&---------------------------------------------------------------------*
*       Obligatory                                                 *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_STRUCTURES USING TABNAME.    "tab.
  SELECT * FROM DD03L WHERE TABNAME = TABNAME.
    CLEAR CHAR.
    CHAR(2)   = 'Z_'.
    CHAR+2(5) = TABNAME.
    CHAR+7(1) = '-'.
    CHAR+8    = DD03L-FIELDNAME.
    ASSIGN (CHAR) TO <F1>.
    <F1> = NODATA.
  ENDSELECT.
ENDFORM.                               " INIT_STRUCTURES
