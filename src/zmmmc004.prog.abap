REPORT ZMMMC004 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMc004
*    PROGRAMMER  :  M DeMeester
*    Client      :  Union Gas Limited
*    Date        :  February 18, 1998
*
* This ABAP will setup the necessary structure used by the load program
* RMMMBIM0.
* Select all active marc records and create the proper layouts
* to populate LGFSB with main storage location of 'A001'
* This program should be only used once, any further use of this program
* after the initial use is DANGEROUS!
************************************************************************
TABLES: MARA,      "Material Master
        MARC.      "Plant Record
TABLES:  DD03L.
*-------------------  selection-screen  --------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.
PARAMETERS:     L_FILE LIKE FILENAME-FILEEXTERN      "dataset for output
                DEFAULT 'ZMMMC004_01'.
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME.
SELECT-OPTIONS: S_WERKS FOR MARC-WERKS,              "Plant
                S_MATNR FOR MARA-MATNR,              "Material
                S_MATKL FOR MARA-MATKL.              "Material Group
SELECTION-SCREEN END OF BLOCK BLK2.
*-----------------------------------------------------------------------

FIELD-SYMBOLS: <F1> .
DATA: T_CODE       LIKE BMM00-TCODE VALUE 'MM02',  "MM02 -> Change
      CHAR(21)     TYPE C,
      NODATA(40) VALUE '////////////////////////////////////////'.

DATA: PHYFILE LIKE FILENAMECI-FILEEXTERN.
DATA: MARD_FLAG(3)       TYPE C,
      MARC_FLAG(3)       TYPE C,                             "98/03/17
      MBEW_FLAG(3)       TYPE C,
      MAPR_FLAG(3)       TYPE C.

DATA    : BEGIN OF Z_BGR00.
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BMM00.
        INCLUDE STRUCTURE BMM00.
DATA    : END OF Z_BMM00.

DATA    : BEGIN OF Z_BMMH1.
        INCLUDE STRUCTURE BMMH1.
DATA    : END OF Z_BMMH1.

DATA:   G_TRANSCODE LIKE TSTC-TCODE     VALUE 'MM06'.  "Transaction Code

DATA  : BEGIN OF BDCDATA OCCURS 100.                   "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA: WERKS LIKE MARC-WERKS.

INCLUDE <ICON>.

*******************************  MAIN  *********************************
START-OF-SELECTION.

PERFORM OPEN_OUTPUT_FILE.                       "Material create file
PERFORM OPEN_BATCH_SESSION.                     "BDC for deleted records

PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BGR00.
PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BGR00.

PERFORM STRUCTURE_BGR00.               "Writes header structure

SELECT * FROM MARC
  WHERE MATNR IN S_MATNR
    AND WERKS IN S_WERKS
    AND LVORM = ' '
    AND dismm <> '  '.                 " ==> MRP
*  SELECT * FROM MARA
*    WHERE MATNR = MARC-MATNR
*      AND MATKL IN S_MATKL.
*  ENDSELECT.

  IF SY-SUBRC EQ '0'.                       "No storage records found
     PERFORM STRUCTURE_BMM00.
     PERFORM STRUCTURE_BMMH1.
  ENDIF.

ENDSELECT.

CLOSE DATASET PHYFILE.
PERFORM CLOSE_BATCH_SESSION.
*-----------------------------------------------------------------------
*   FORM OPEN_OUTPUT_FILE.
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = L_FILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH L_FILE.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.
ENDFORM.
*-----------------  structure_bgr00  -----------------------------------
* set up initial record for batch
*-----------------------------------------------------------------------
FORM STRUCTURE_BGR00.
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZMM_LGFSB'    TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167.
ENDFORM.

*-----------------  structure_bmm00  -----------------------------------
* set up plant/storage location
*-----------------------------------------------------------------------
FORM STRUCTURE_BMM00.
    MOVE '1'          TO Z_BMM00-STYPE.
    MOVE T_CODE       TO Z_BMM00-TCODE.
    MOVE MARc-MATNR   TO Z_BMM00-MATNR.
    MOVE MARC-WERKS   TO Z_BMM00-WERKS.
    move 'X'          to Z_BMM00-XEID1.          "MRP1 view
    move 'X'          to Z_BMM00-XEID2.
    move 'X'          to z_bmm00-Xeid3.

   TRANSFER Z_BMM00  TO PHYFILE LENGTH 199.
   PERFORM INIT_STRUCTURES USING 'BMM00'.       " I_BGR00.
ENDFORM.

*-----------------  structure_bmmh1  -----------------------------------
* various views
*-----------------------------------------------------------------------
FORM STRUCTURE_BMMH1.
  MOVE '2'               TO Z_BMMH1-STYPE.
  MOVE 'A001'            to Z_BMMH1-lgfsb.
  TRANSFER Z_BMMH1 to phyfile length 2744.
  perform init_structures using 'BMMh1'.
ENDFORM.

*---------------------- init_structures ------------------------------*
*  -->  tabname - name of the structire table passed                  *
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
************************************************************************
*-------------------------  BDC_SCREEN  --------------------------------
* This routine adds an entry to the table BDCDATA with screen
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  PROGRAM - Program name of the screen
*      DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
    CLEAR BDCDATA.
    BDCDATA-PROGRAM = PROGRAM.
    BDCDATA-DYNPRO = DYNPRO.
    BDCDATA-DYNBEGIN = 'X'.
    APPEND BDCDATA.
ENDFORM.

*-------------------------  BDC_FIELD  ---------------------------------
* This routine adds an entry to the table BDCDATA with field
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  fnam - name of the field on the screen
*      fval - value to be entered for that field on the screen.
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.


*------------------------  OPEN_BATCH_SESSION --------------------------
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
CALL FUNCTION 'BDC_OPEN_GROUP'
     EXPORTING
         CLIENT              = SY-MANDT
         GROUP               = 'ZMM_LGFSB'
*         HOLDDATE            =
         KEEP                = 'X'
         USER                = SY-UNAME
     EXCEPTIONS
          CLIENT_INVALID      = 1
          DESTINATION_INVALID = 2
          GROUP_INVALID       = 3
          GROUP_IS_LOCKED     = 4
          HOLDDATE_INVALID    = 5
          INTERNAL_ERROR      = 6
          QUEUE_ERROR         = 7
          RUNNING             = 8
          SYSTEM_LOCK_ERROR   = 9
          USER_INVALID        = 10
          OTHERS              = 11.
      IF SY-SUBRC NE 0.
           MESSAGE E001 WITH 'ZMM_LGFSB'.
      ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    CLOSE_BATCH_SESSION
*-----------------------------------------------------------------------
*  - This closes the batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_BATCH_SESSION.
CALL FUNCTION 'BDC_CLOSE_GROUP'
     EXCEPTIONS
          NOT_OPEN    = 1
          QUEUE_ERROR = 2
          OTHERS      = 3.
     IF SY-SUBRC NE 0.
          MESSAGE I003 WITH 'ZMM_DELETE'.
     ENDIF.
ENDFORM.












