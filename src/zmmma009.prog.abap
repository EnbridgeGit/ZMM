report zmmma009 no standard page heading line-count 65 message-id zm.
************************************************************************
*    Program     :  zmmma009
*    Programmer  :  M De Meester
*    Client      :  Centra Union Gas Limited
*    Date        :  June 1997
*
* This program will look at all materials in a plant and create a file.
* Program RMMMBIM0 is executed with IFMM001/ZMMMA009_01 as input.
* The BDC session is then posted.  This program will select any record
* matching the plant record matching the material group and change its
* purchasing group to the one entered in the variant.
*
************************************************************************
TABLES: MARA, MARC, DD03L.

FIELD-SYMBOLS: <F1>.
DATA: T_CODE       LIKE BMM00-TCODE VALUE 'MM02',  "Transaction Code
      CHAR(21)     TYPE C,
      NODATA(40) VALUE '////////////////////////////////////////'.

DATA    : BEGIN OF Z_BGR00.
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BMM00.
        INCLUDE STRUCTURE BMM00.
DATA    : END OF Z_BMM00.

DATA    : BEGIN OF Z_BMMH1.
        INCLUDE STRUCTURE BMMH1.
DATA    : END OF Z_BMMH1.
DATA:
     BEGIN OF REC_TABLE OCCURS 10000,      "Temporary table to store all
        MATNR      LIKE MARC-MATNR,        "records that need to be
        WERKS      LIKE MARC-WERKS,        "processed.
     END OF REC_TABLE.

DATA      : PHYFILE LIKE FILENAMECI-FILEEXTERN.

DATA  : BEGIN OF BDCDATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA  : G_TRANSCODE    LIKE TSTC-TCODE     VALUE   'MM02'.
DATA  : BDCFLAG(3)     TYPE C              VALUE 'NO '.
*        blank(4)       type c value '    ',
*        counter        type i value 0,
*        searchkey(22)  type c.          "Used to find duplicate records

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS: CORRFILE LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA009_01'.
SELECTION-SCREEN END OF BLOCK BOX1. SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS:
     S_WERKS         FOR MARC-WERKS   OBLIGATORY,   "Plant
     S_MATKL         FOR MARA-MATKL   OBLIGATORY.   "Material Group
PARAMETER
     P_EKGRP         LIKE MARC-EKGRP  OBLIGATORY.   "Purchasing group
SELECT-OPTIONS:
     S_MATNR         FOR MARA-MATNR.                "Material Number
SELECTION-SCREEN END OF BLOCK BOX2.

*******************************  MAIN  *********************************
START-OF-SELECTION.

PERFORM OPEN_OUTPUT_FILE.                 "Open File for processing
SELECT * FROM MARA
    WHERE MATNR IN S_MATNR               "Material Number
      AND MATKL IN S_MATKL                "Material Group Selection
      ORDER BY MATNR.

    SELECT * FROM MARC
       WHERE WERKS IN S_WERKS              "Plant Selection
         AND MATNR = MARA-MATNR
         AND EKGRP <> P_EKGRP.
       CLEAR REC_TABLE.
       REC_TABLE-WERKS = MARC-WERKS.
       REC_TABLE-MATNR = MARC-MATNR.
       APPEND REC_TABLE.

    ENDSELECT.                            "End of Plant Select

ENDSELECT.                                "End of Material Group Select

IF SY-TABIX > 1.
   PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
   PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BMM00.
   PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BMMH1.
   PERFORM BATCH_HEADER.
   LOOP AT REC_TABLE.
      PERFORM MATERIAL_HEADER.
      PERFORM PLANT_HEADER.
      PERFORM INIT_STRUCTURES USING 'BMM00'. " I_BGR00.
      PERFORM INIT_STRUCTURES USING 'BMMH1'. " I_BGR00.
   ENDLOOP.
ENDIF.

WRITE: / SY-REPID, TEXT-007.
WRITE: /.
IF  SY-TABIX = 1.
    WRITE: / TEXT-002.           "NO BDC session
ELSE.
    WRITE: / TEXT-003.           "Continue with RMMMBIM0
    WRITE: /.
    WRITE: / TEXT-004.           "Continue with RMMMBIM0
    WRITE: /.
    WRITE: / TEXT-005.
ENDIF.
CLOSE DATASET PHYFILE.


**************************** SUB ROUTINES ******************************
*------------------------  OPEN_OUTPUT_FILE  ---------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
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
ENDFORM.

*---------------------  INIT_STRUCTURES  -------------------------------
*  -->  TABNAME - NAME OF THE STRUCTURE TABLE PASSED
*-----------------------------------------------------------------------
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
ENDFORM.

*---------------------  BATCH_HEADER  ----------------------------------
*  -->  TABNAME - NAME OF THE STRUCTURE TABLE PASSED
*-----------------------------------------------------------------------
FORM BATCH_HEADER.
 MOVE '0'               TO  Z_BGR00-STYPE.       "Batch Header Record
 MOVE 'ZMM-BUYER   '    TO  Z_BGR00-GROUP.
 MOVE SY-MANDT          TO  Z_BGR00-MANDT.
 MOVE SY-UNAME          TO  Z_BGR00-USNAM.
 TRANSFER Z_BGR00       TO  PHYFILE LENGTH 167.
ENDFORM.
*---------------------  MATERIAL_HEADER  -------------------------------
*  -->  TABNAME - NAME OF THE STRUCTURE TABLE PASSED
*-----------------------------------------------------------------------
FORM MATERIAL_HEADER.
  MOVE '1'             TO Z_BMM00-STYPE.     "Material Header Record
  MOVE T_CODE          TO Z_BMM00-TCODE.
  MOVE REC_TABLE-MATNR TO Z_BMM00-MATNR.
  MOVE REC_TABLE-WERKS TO Z_BMM00-WERKS.
  MOVE 'X'             TO Z_BMM00-XEIE1.     "Purchasing View
  TRANSFER Z_BMM00     TO PHYFILE LENGTH 167.
ENDFORM.
*---------------------  PLANT_HEADER  ----------------------------------
*  -->  TABNAME - NAME OF THE STRUCTURE TABLE PASSED
*-----------------------------------------------------------------------
FORM PLANT_HEADER.
  MOVE '2'             TO Z_BMMH1-STYPE.        "Plant Record
  MOVE P_EKGRP         TO Z_BMMH1-EKGRP.
  TRANSFER Z_BMMH1     TO PHYFILE LENGTH 2241.

ENDFORM.
