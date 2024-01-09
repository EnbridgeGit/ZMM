REPORT ZMWMC002 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMWMC002
*    Programmer  :  Ric Aarssen
*    Created on  :  October 4, 1998
*----------------------------------------------------------------------
* (1) This program extracts data for Union Energy separation:
*               - Vendor Master (general info)
*     All Purchasing Vendors will be extracted.  Cambridge Technologies
*       will have to decide if they wish to eliminate some Vendors based
*       on possibly Material Number and Info Records.
*
* (2) Selection Criteria - only logical file name required
*
* (3) Output file:
*        Logical File  - ZMWMC002_01
*        Physical File - /usr/sap/interfaces/P01/UEC/ZMWMC002.SAP
*
*----------------------------------------------------------------------
*    TABLE DECLARATIONS
*----------------------------------------------------------------------
tables:   lfm1.                         " vendor master (general)

*----------------------------------------------------------------------
*    OUTPUT RECORD LAYOUT
*----------------------------------------------------------------------
data:    outfile(100),
*--> consumption record to be downloaded to Union Energy
         begin of ilfm1 occurs 0,
           LOEVM     LIKE LFM1-LOEVM,          " deletion flag
           LIFNR     LIKE LFM1-LIFNR,          " vendor number
           EKORG     LIKE LFM1-EKORG,          " purchase organization
           MINBW(13) TYPE C,                   " minimum order levels
           PLIFZ(4)  TYPE C,                   " planned delivery days
           VERKF     LIKE LFM1-VERKF,          " responsible salesperson
           TELF1     LIKE LFM1-TELF1,          " telephone number
           WAERS     LIKE LFM1-WAERS,          " purchase order currency
           ZTERM     LIKE LFM1-ZTERM,          " terms of payment
         end of ilfm1.

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

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    PARAMETERS: P_OUTFIL       LIKE FILENAME-FILEINTERN OBLIGATORY
                               DEFAULT 'ZMWMC002_01'.
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

* get the vendor master general data
  select * from lfm1.

  move-corresponding lfm1 to ilfm1.

* loop at ilfm1.
    TRANSFER ILFM1 TO OUTFILE LENGTH 87.

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
