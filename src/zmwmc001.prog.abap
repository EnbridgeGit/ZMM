REPORT ZMWMC001 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMWMC001
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
*        Logical File  - ZMWMC001_01
*        Physical File - /usr/sap/interfaces/P01/UEC/ZMWMC001.SAP
*
*----------------------------------------------------------------------
*    TABLE DECLARATIONS
*----------------------------------------------------------------------
TABLES:   LFA1.                         " vendor master (general)

*----------------------------------------------------------------------
*    OUTPUT RECORD LAYOUT
*----------------------------------------------------------------------
data:    outfile(100),
*--> consumption record to be downloaded to Union Energy
         BEGIN OF ILFA1 OCCURS 0,
           LOEVM     LIKE LFA1-LOEVM,                " deletion flag
           lifnr     like lfa1-lifnr,                " vendor number
           land1     like lfa1-land1,                " country key
           name1     like lfa1-name1,                " 1st name field
           name2     like lfa1-name2,                " 2nd name field
           name3     like lfa1-name3,                " 3rd name field
           name4     like lfa1-name4,                " 4th name field
           stras     like lfa1-stras,                " Street
           ort01     like lfa1-ort01,                " City
           pstlz     like lfa1-pstlz,                " Postal Code
           regio     like lfa1-regio,                " Region Code
           sortl     like lfa1-sortl,                " Short Description
           stcd1     like lfa1-stcd1,                " Tax Number
           telf1     like lfa1-telf1,                " 1st Telephone no
           telf2     like lfa1-telf2,                " 2nd Telephone no
           telfx     like lfa1-telfx,                " Fax Telephone no
           teltx     like lfa1-teltx,                " Teletex Number
         end of ilfa1.

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
                               DEFAULT 'ZMWMC001_01'.
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
  select * from lfa1.

  move-corresponding lfa1 to ilfa1.

* loop at ilfa1.
    transfer ilfa1 to outfile length 356.

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
