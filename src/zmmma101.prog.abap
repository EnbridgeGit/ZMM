REPORT ZMMMA101 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  zmmma101
*    Programmer  :  Ric Aarssen
*    Client      :  Union Gas Limited
*    Date        :  March 23, 1998
*
* This ABAP will setup the necessary structure used by the load program
* RMMMBIM0.
* It updates records from the Material Master table (MARC). Based on
* the entered MM/PP status for plant using a BDC - MM02 transaction.
*
* This will initally be used for the Centra MM Merger.
*
***** ***** ***** ***** ****** ***** ***** ***** ***** ***** ***** *****
*  MODIFIED BY NANCY GILLIGAN, OMNILOGIC - NOV 24/98  D30K906417       *
*  - SPED UP PROCESSING
*
* 2000/08/29 - gymana - 4.6B Upgrade
*   - Changed table DD03P to DD03L
************************************************************************

***************************  TABLES  ***********************************
TABLES: mara,                            "Material Master: General Data
        marc,                            "Material Master: Plant Data
        dd03l.

************************  DATA ELEMENTS  *******************************

FIELD-SYMBOLS: <f1> .
data      : t_code       like bmm00-tcode value 'MM02',
            char(21)     type C,
            nodata(40) value '////////////////////////////////////////'.

data      : phyfile like filenameci-fileextern.
PARAMETERS: CORRFILE LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA101_01'.

data    : begin of z_bgr00.
        include structure bgr00.
data    : end of z_bgr00.

data    : begin of z_bmm00.
        include structure bmm00.
data    : end of z_bmm00.

data    : begin of z_bmmh1.
        include structure bmmh1.
data    : end of z_bmmh1.

include <ICON>.

DATA:   G_TRANSCODE LIKE TSTC-TCODE.     "Transaction Code

DATA:   BEGIN OF BDCDATA OCCURS 100.     "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA:   END OF BDCDATA.

*-------------------- Work Area ----------------------------------------
*data:  none
***********************  SELECTION SCREEN  *****************************

*--------- Entering a range of Plant Locations
*               to select from Purchasing Source List ------------------
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  S_WERKS FOR MARC-WERKS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

*---  Entering a material type, or range of Material Groups, or
*         Material Numbers to select from Material Master  -------------
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_MTART FOR MARA-MTART NO INTERVALS.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: S_MATKL FOR MARA-MATKL.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: S_MATNR FOR MARC-MATNR.
SELECTION-SCREEN END OF BLOCK BOX2.

*--------  Entering a the new MM/PP status  ----------------------------
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETER: P_MMSTA LIKE MARC-MMSTA OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX3.

*--------  Text describing what this program is going to do  -----------
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-901.

*--------  Check to make sure an entry has been made  ------------------
AT SELECTION-SCREEN.
  IF ( S_MTART-LOW IS INITIAL )     AND
     ( S_MATKL-LOW IS INITIAL ) AND ( S_MATNR-LOW IS INITIAL ).
     MESSAGE E100 WITH 'Must enter material type, group or number'.
  ENDIF.

**************************** MAIN  PROGRAM *****************************
START-OF-SELECTION.

*-------------  initialize batch header record  ------------------------
PERFORM OPEN_OUTPUT_FILE.

PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BMM00.
PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BMMH1.

MOVE '0'            TO  Z_BGR00-STYPE.
MOVE 'ZMM_MMPP'     TO  Z_BGR00-GROUP.
MOVE SY-MANDT       TO  Z_BGR00-MANDT.
MOVE SY-UNAME       TO  Z_BGR00-USNAM.
TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167.

*-------------  select records to be updated  --------------------------
SELECT * FROM MARA WHERE MTART IN S_MTART
                   AND   MATKL IN S_MATKL
                   AND   MATNR IN S_MATNR.

   SELECT * FROM MARC WHERE WERKS IN S_WERKS
                      AND   MATNR = MARA-MATNR.
      MOVE '1'          TO Z_BMM00-STYPE.
      MOVE T_CODE       TO Z_BMM00-TCODE.
      MOVE MARC-MATNR   TO Z_BMM00-MATNR.
      MOVE MARC-WERKS   TO Z_BMM00-WERKS.
      MOVE 'X'          TO Z_BMM00-XEIE1.
      TRANSFER Z_BMM00  TO PHYFILE LENGTH 167.

      MOVE '2'          TO Z_BMMH1-STYPE.
      MOVE P_MMSTA      TO Z_BMMH1-MMSTA.
      TRANSFER Z_BMMH1  TO PHYFILE LENGTH 2241.

PERFORM RESET_VALUES.                                            "omning
*     perform init_structures using 'BMM00'. " I_BMM00.          "omning
*     perform init_structures using 'BMMH1'. " I_BMMH1.          "omning
   ENDSELECT.

ENDSELECT.

CLOSE DATASET PHYFILE.

END-OF-SELECTION.

******************** 1st LEVEL SUBROUTINES *****************************
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
*&---------------------------------------------------------------------*
*&      Form  RESET_VALUES
*&---------------------------------------------------------------------*
FORM RESET_VALUES.                                               "omning
      MOVE '/'                   TO Z_BMM00-STYPE.               "omning
      MOVE '////'                TO Z_BMM00-TCODE.               "omning
      MOVE '//////////////////'  TO Z_BMM00-MATNR.               "omning
      MOVE '////'                TO Z_BMM00-WERKS.               "omning
      MOVE '/'                   TO Z_BMM00-XEIE1.               "omning

      MOVE '/'          TO Z_BMMH1-STYPE.                        "omning
      MOVE '//'         TO Z_BMMH1-MMSTA.                        "omning

ENDFORM.                    " RESET_VALUES
