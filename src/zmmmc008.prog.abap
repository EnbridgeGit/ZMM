REPORT ZMMMC008 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMC008
*    PROGRAMMER  :  E G YMANA
*    Client      :  Union Gas Limited
*    Date        :  October 3, 2000
*
* This ABAP will setup the necessary structure used by the load program
* RMMMBIM0.
************************************************************************

TABLES:  DD03L,
         MARC,            "Plant Record
         MARD.            "Storage Location Record
*-------------------  selection-screen  --------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.
PARAMETERS:     IN_FILE(70) TYPE C DEFAULT '/usr/sap/interfaces/'
                                           LOWER CASE,
                OUT_FILE(70) TYPE C DEFAULT '/usr/sap/interfaces/'
                                           LOWER CASE,
                P_WERKS LIKE  MARC-WERKS,     "Plant
                P_LGORT LIKE  MARD-LGORT.     "Storage Location
SELECTION-SCREEN END OF BLOCK BLK1.

*-----------------------------------------------------------------------

FIELD-SYMBOLS: <F1> .
DATA: T_CODE       LIKE BMM00-TCODE VALUE 'MM02',
      CHAR(21)     TYPE C,
      NODATA(40) VALUE '////////////////////////////////////////'.

DATA    : INREC(12).

DATA    : BEGIN OF Z_BGR00.
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BMM00.
        INCLUDE STRUCTURE BMM00.
DATA    : END OF Z_BMM00.

DATA    : BEGIN OF Z_BMMH1.
        INCLUDE STRUCTURE BMMH1.
DATA    : END OF Z_BMMH1.


*******************************  MAIN  *********************************
INITIALIZATION.
MOVE SY-SYSID TO IN_FILE+20(3).
MOVE '/CFMM001/ZMMMC001_01.SAP' TO IN_FILE+23(24).
MOVE SY-SYSID TO OUT_FILE+20(3).
MOVE '/CFMM001/ZMMMC001_03.SAP' TO OUT_FILE+23(24).

START-OF-SELECTION.

PERFORM OPEN_FILES.                             "Material create file

PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
PERFORM INIT_STRUCTURES USING 'BMM00'.
PERFORM INIT_STRUCTURES USING 'BMMH1'.

PERFORM STRUCTURE_BGR00.               "Writes structure

READ DATASET IN_FILE INTO INREC.

DO.
  IF SY-SUBRC NE '0'.
     EXIT.
  ENDIF.
                                            "No storage records found
  PERFORM STRUCTURE_BMM00.
  PERFORM STRUCTURE_BMMH1.
  READ DATASET IN_FILE INTO INREC.
ENDDO.

CLOSE DATASET IN_FILE.
CLOSE DATASET OUT_FILE.
*-----------------------------------------------------------------------
*   FORM OPEN_FILES.
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_FILES.
    OPEN DATASET IN_FILE FOR INPUT IN TEXT MODE.
    OPEN DATASET OUT_FILE FOR OUTPUT IN TEXT MODE.
ENDFORM.
*-----------------  structure_bgr00  -----------------------------------
* set up initial record for batch
*-----------------------------------------------------------------------
FORM STRUCTURE_BGR00.
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZMM_MATL'     TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  OUT_FILE LENGTH 167.
ENDFORM.

*-----------------  structure_bmm00  -----------------------------------
* set up plant/storage location
*-----------------------------------------------------------------------
FORM STRUCTURE_BMM00.
    MOVE '1'          TO Z_BMM00-STYPE.
    MOVE T_CODE       TO Z_BMM00-TCODE.        "TRANSACTION-CODE
    MOVE '000000000000' TO Z_BMM00-MATNR(12).
    MOVE INREC(6)     TO Z_BMM00-MATNR+12(6).  "MATL. NUMBER
    MOVE P_WERKS      TO Z_BMM00-WERKS.        "PLANT
    MOVE P_LGORT      TO Z_BMM00-LGORT.        "STORAGE LOCATION
    MOVE 'X'          TO Z_BMM00-XEIL1.
    TRANSFER Z_BMM00  TO OUT_FILE LENGTH 199.
    PERFORM INIT_STRUCTURES USING 'BMM00'. " I_BGR00.
ENDFORM.

*-----------------  structure_bmmh1  -----------------------------------
* various views
*-----------------------------------------------------------------------
FORM STRUCTURE_BMMH1.
  MOVE '2'               TO Z_BMMH1-STYPE.
  MOVE INREC+6(6)        TO Z_BMMH1-LGPBE.     "BIN LOCATION
  TRANSFER Z_BMMH1  TO OUT_FILE LENGTH 2744.
  PERFORM INIT_STRUCTURES USING 'BMMH1'.       "I_BGR00.
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
