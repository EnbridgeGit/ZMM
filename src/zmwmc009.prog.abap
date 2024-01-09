REPORT ZMWMC009 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMWMC009
*    Programmer  :  Ric Aarssen
*    Copy of     :  Marv Radsma - OMNILOGIC SYSTEMS GROUP
*    Created on  :  September 28. 1998
*----------------------------------------------------------------------
* (1) This program extracts data for Union Energy separation:
*               - years consumption for each period
*     It is an extracted copy of ZMMMC009 create BDC for Centra Gas
*        merger - consumption data
*
* (2) Output file:
*        Logical File  - ZMWMC009_01
*        Physical File - /usr/sap/interfaces/P01/UEC/ZMWMC009.SAP
*
*----------------------------------------------------------------------
*    TABLE DECLARATIONS
*----------------------------------------------------------------------
TABLES:   MARA,                         " material master
          MVER.                         " material consumtion

*----------------------------------------------------------------------
*    OUTPUT RECORD LAYOUT
*----------------------------------------------------------------------
DATA:    OUTFILE(100),
*--> consumption record to be downloaded to Union Energy
         BEGIN OF IMVER OCCURS 0,
           MATNR     LIKE MVER-MATNR,                 " material number
           WERKS     LIKE MVER-WERKS,                 " plant code
           GJAHR     LIKE MVER-GJAHR,                 " fiscal year
           MGV01(15) TYPE C,                          " period 01 amount
           MGV02(15) TYPE C,                          " period 02 amount
           MGV03(15) TYPE C,                          " period 03 amount
           MGV04(15) TYPE C,                          " period 04 amount
           MGV05(15) TYPE C,                          " period 05 amount
           MGV06(15) TYPE C,                          " period 06 amount
           MGV07(15) TYPE C,                          " period 07 amount
           MGV08(15) TYPE C,                          " period 08 amount
           MGV09(15) TYPE C,                          " period 09 amount
           MGV10(15) TYPE C,                          " period 10 amount
           MGV11(15) TYPE C,                          " period 11 amount
           MGV12(15) TYPE C,                          " period 12 amount
         END OF IMVER.

*----------------------------------------------------------------------
*    WORKING STORAGE DATA DECLARATIONS
*----------------------------------------------------------------------
*data:

*---------------------------------------------------------------------
*    SELECTION CRITERIA EVENT
*---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 10(60) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    PARAMETERS:     P_WERKS   LIKE MVER-WERKS OBLIGATORY
                               DEFAULT 'P105'.
    SELECT-OPTIONS: S_MATNR    FOR MARA-MATNR.
*   select-options: s_matkl    for mara-matkl.  "if narrowing selection
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
    SELECT-OPTIONS: S_GJAHR    FOR MVER-GJAHR OBLIGATORY
                               DEFAULT SY-DATUM(4) TO SY-DATUM(4).
  SELECTION-SCREEN END OF BLOCK BOX3.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    PARAMETERS: P_OUTFIL       LIKE FILENAME-FILEINTERN OBLIGATORY
                               DEFAULT 'ZMWMC009_01'.
  SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.
*   End of selection screen.

*---------------------------------------------------------------------
*    MAIN PROCESSING EVENT
*---------------------------------------------------------------------
START-OF-SELECTION.

* open the output file
  PERFORM OPEN_OUTPUT_FILE.

* display progress message
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Building the output file'
       EXCEPTIONS
            OTHERS = 1.

* get the material consumption data
  SELECT *
*        matnr  werks  gjahr  mgv01
*        mgv02  mgv03  mgv04  mgv05
*        mgv06  mgv07  mgv08  mgv09
*        mgv10  mgv11  mgv12
  FROM   MVER
* into   table imver
  WHERE  MATNR IN S_MATNR
  AND    WERKS = P_WERKS
  and    gjahr in s_gjahr.

  MOVE-CORRESPONDING MVER TO IMVER.

* loop at imver.
    TRANSFER IMVER TO OUTFILE LENGTH 206.
*   would need to add a select single on mara if wanting to
*   narrow down the selection criteria.  This would also involve
*   filling this internal table first and then looping through before
*   writing out a record
* endloop.
  ENDSELECT.
* close the dataset
  CLOSE DATASET OUTFILE.

  MESSAGE I100 WITH 'Processing has completed'.
END-OF-SELECTION.

*-----------------------------------------------------------------------
*   FORM OPEN_OUTPUT_FILE.
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            CLIENT           = SY-MANDT
            LOGICAL_FILENAME = P_OUTFIL
            OPERATING_SYSTEM = SY-OPSYS
       IMPORTING
            FILE_NAME        = OUTFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH P_OUTFIL.
  ELSE.
    OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       THIS IS THE END                                               *
*---------------------------------------------------------------------*
