REPORT ZMMMA011 NO STANDARD PAGE HEADING LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*    Program     :  zmmma011
*    Programmer  :  M De Meester
*    Client      :  Centra Union Gas Limited
*    Date        :  Sept 1997
*
* This program will look at all materials with forecast model (PRMOD)
* = 'S' and change the number of months (PERIN) to '60' and the
* initalization indicator (KZINI) to 'X' and create the flat file
* IFMM001/ZMMMA011_01.
* RMMMBIM0 is then executed with IFMM001/ZMMMA011_01 as input and
* produces a BDC session ZMM_FRCSTINT.
* The BDC session is then posted.
* structure "MPOP"
* 2004/05/21 - mdemeest - 4.6C BMMH1 length changed to 2746 and
*                              BMM00 length changed to 199.

* 2000/06/21 - gymana - 4.6B Upgrade
*   - Changed table DD03P to DD03L
************************************************************************
TABLES: MARA, MAPR, MARC, PROP, DD03L.

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
        PRMOD      LIKE PROP-PRMOD,
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
PARAMETERS: CORRFILE LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA011_01'.
SELECTION-SCREEN END OF BLOCK BOX1. SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS:
     S_WERKS         FOR MARC-WERKS,         "Plant
     S_MATNR         FOR MARC-MATNR.         "Material
SELECTION-SCREEN END OF BLOCK BOX2.

*******************************  MAIN  *********************************
START-OF-SELECTION.

SELECT * FROM MARC
   WHERE WERKS IN S_WERKS
     AND MATNR IN S_MATNR.

   CLEAR BDCFLAG.

   SELECT * FROM MAPR
      WHERE WERKS = MARC-WERKS
        AND MATNR = MARC-MATNR.

      SELECT * FROM PROP                   "Material with forecast model
         WHERE HSNUM = '00'
           AND PNUM1 = MAPR-PNUM1
           AND PRMOD = 'S'.                "PRMOD must be 'S'
         MOVE 'YES'     TO BDCFLAG.
*        write: / 'PROP', mapr-werks, mapr-matnr, prop-prmod.
      ENDSELECT.                                               "end PROP
   ENDSELECT.                                                  "end MAPR

   IF BDCFLAG = 'YES'.
      MOVE MARC-MATNR     TO REC_TABLE-MATNR.
      MOVE MARC-WERKS     TO REC_TABLE-WERKS.

      MOVE PROP-PRMOD     TO REC_TABLE-PRMOD.

*     write: / rec_table-matnr, rec_table-werks, rec_table-prmod,
*              rec_table-autru.
      APPEND REC_TABLE.
      CLEAR  REC_TABLE.
   ENDIF.
ENDSELECT.                                                     "end MARC


PERFORM OPEN_OUTPUT_FILE.                 "Open File for processing
PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BMM00.
PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BMMH1.

MOVE '0'               TO  Z_BGR00-STYPE.       "Batch Header Record
MOVE 'ZMM_FRCSTINT'    TO  Z_BGR00-GROUP.
MOVE SY-MANDT          TO  Z_BGR00-MANDT.
MOVE SY-UNAME          TO  Z_BGR00-USNAM.
TRANSFER Z_BGR00       TO  PHYFILE LENGTH 167.

LOOP AT REC_TABLE.                           "Generate Required Records

  MOVE '1'             TO Z_BMM00-STYPE.     "Material Header Record
  MOVE T_CODE          TO Z_BMM00-TCODE.
  MOVE REC_TABLE-MATNR TO Z_BMM00-MATNR.
  MOVE REC_TABLE-WERKS TO Z_BMM00-WERKS.
  MOVE 'X'             TO Z_BMM00-XEIP1.
*  TRANSFER Z_BMM00     TO PHYFILE LENGTH 167.
  TRANSFER Z_BMM00     TO PHYFILE LENGTH 199.        "4.6C Upgrade

  MOVE '2'             TO Z_BMMH1-STYPE.        "Plant Record
  MOVE '60'            TO Z_BMMH1-PERIN.
  MOVE 'X'             TO Z_BMMH1-KZINI.
  TRANSFER Z_BMMH1     TO PHYFILE LENGTH 2746.

  PERFORM INIT_STRUCTURES USING 'BMM00'. " I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMMH1'. " I_BGR00.
ENDLOOP.


WRITE: / ' ZMMMA011 IS COMPLETE'.
CLOSE DATASET PHYFILE.


**************************** SUB ROUTINES ******************************
*-----------------------------------------------------------------------
*   FORM OPEN_OUTPUT_FILE.
*-----------------------------------------------------------------------
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

*---------------------------------------------------------------------*
*       FORM INIT_STRUCTURES                                          *
*---------------------------------------------------------------------*
*  -->  TABNAME - NAME OF THE STRUCTURE TABLE PASSED                  *
*---------------------------------------------------------------------*
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
