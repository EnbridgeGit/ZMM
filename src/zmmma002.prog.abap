REPORT ZMMMA002 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 200
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMA002
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  August 23 1996
*    Modified    :  November 20 1996
*
* This ABAP is an interface to load records from an Excel file, to
* maintain the Source List for plant / vendor relationship.  This
* program will be used for the initial data load of vendor and
* materials.
*
************************************************************************
TABLES: MARA, MARC, LFB1.

DATA: DATAFILE LIKE FILENAMECI-FILEEXTERN,
      VENDDATA LIKE FILENAMECI-FILEEXTERN.

PARAMETERS: VENFILE  LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA002_01',
            SOURCE   LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA002_02'.

DATA:   BEGIN OF IN_RECORD,                         "Input structure
          BISMT(8),      "LIKE MARA-BISMT, Old Material Number
          DUMY1(2),
          ALTKN(5),      "LIKE LFB1-ALTKN, Previous vendor number
          DUMY2(2),
          WERKS(4),      "LIKE MARC-WERKS, Plant
          DUMY3(2),
          FLIFN(1),      "LIKE EORD-FLIFN, Fixed vendor indicator
          DUMY5(2),
          AUTET(1),      "LIKE EORD-AUTET, MRP indicator
          DUMY6(2),
          NOTKZ(1),      "LIKE EORD-NOTKZ, Block Source Supply
          DUMY7(2),
          VDATU(8),      "LIKE EORD-VDATU, Source list record Valid from
          DUMY8(2),
          BDATU(8),      "LIKE EORD-BDATU, Source list record Valid to
          MATNR     LIKE MARA-MATNR,            "Current Material Number
          LIFNR     LIKE EORD-LIFNR,            "Current Vendor Number
          SEQNO     LIKE LFB1-ALTKN,
        END OF IN_RECORD.

DATA:   BEGIN OF TEMPVENDOR OCCURS 25000,
          SOURCE(4)        TYPE C,
          OLDACCT(10)       TYPE C,
*          DUMMY1(5)        TYPE C,
          VENNAME(35)      TYPE C,
          LOADSW(1)        TYPE C,
          DUPLDW(1)        TYPE C,
          SEQNO(5)         TYPE C,
        END OF TEMPVENDOR.

DATA:   G_TRANSCODE LIKE TSTC-TCODE,                 "Transaction Code
        SEARCHKEY(14)   TYPE C.
DATA  : BEGIN OF BDCDATA OCCURS 100.                 "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

PARAMETERS:  STARTDAT LIKE SY-DATUM.
PARAMETERS:  ENDDAT   LIKE SY-DATUM.

INCLUDE <ICON>.
AT SELECTION-SCREEN.
PERFORM GET_FILE_SOURCE.
PERFORM GET_FILE_VENDOR.
************************************************************************
TOP-OF-PAGE.

*WRITE: / TEXT-499, 131 TEXT-498.
*WRITE: / TEXT-500, 131 TEXT-501.
WRITE: / ICON_DATE AS ICON.
WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 85 TEXT-002 COLOR COL_HEADING.
WRITE: 192 SY-REPID COLOR COL_NEGATIVE.

WRITE: / ICON_TIME AS ICON.
WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE: 83(44).
WRITE: 192 'PAGE:' INTENSIFIED OFF.
WRITE: 198(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: /.
FORMAT COLOR COL_NORMAL.
ULINE.
WRITE: /1  TEXT-007, 25 TEXT-007.
WRITE: /1  TEXT-004, 25 TEXT-006, 40 TEXT-013, 50 TEXT-008.
WRITE: 60  TEXT-009, 70 TEXT-010, 85 TEXT-011, 100 TEXT-012.
WRITE: 120 TEXT-016.
WRITE: /1  TEXT-005, 25 TEXT-005, 85 TEXT-014, 101 TEXT-015.
WRITE: 119 TEXT-017.
ULINE.

**************************** MAIN  PROGRAM *****************************
START-OF-SELECTION.
PERFORM OPEN_BATCH_SESSION.
PERFORM CREATE_BATCH_INPUT.
PERFORM CLOSE_BATCH_SESSION.

****************************** SUBROUTINES *****************************
*-----------------------------------------------------------------------
*  ADD_TRANSAC
*-----------------------------------------------------------------------
*  -  This routine provides the information on the screens and fields
*     necessary for the batch input session.
*-----------------------------------------------------------------------
FORM ADD_TRANSAC.
DATA: TEMPMATNR(18) TYPE C.
      TEMPMATNR = IN_RECORD-MATNR+12(6).
      PERFORM BDC_SCREEN USING 'SAPDM06I' '0200'.
      PERFORM BDC_FIELD  USING 'EORD-MATNR' TEMPMATNR.
      PERFORM BDC_FIELD  USING 'EORD-WERKS' IN_RECORD-WERKS.
      PERFORM BDC_SCREEN USING 'SAPDM06I' '0205'.
      PERFORM BDC_FIELD  USING 'EORD-VDATU(1)' IN_RECORD-VDATU.
      PERFORM BDC_FIELD  USING 'EORD-BDATU(1)' IN_RECORD-BDATU.
      PERFORM BDC_FIELD  USING 'EORD-LIFNR(1)' IN_RECORD-LIFNR.
      PERFORM BDC_FIELD  USING 'EORD-EKORG(1)' 'MATL'.
      PERFORM BDC_FIELD  USING 'RM06W-FESKZ(1)' IN_RECORD-FLIFN.
      PERFORM BDC_FIELD  USING 'EORD-AUTET(1)' IN_RECORD-AUTET.
      PERFORM BDC_FIELD  USING 'EORD-NOTKZ(1)' IN_RECORD-NOTKZ.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM INSERT_SESSION
*-----------------------------------------------------------------------
*    Description:
*    - This routine inserts the BDC data for one transaction into the
*      batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION USING G_TRANSCODE.
CALL FUNCTION 'BDC_INSERT'
     EXPORTING
          TCODE          = G_TRANSCODE
     TABLES
          DYNPROTAB      = BDCDATA
     EXCEPTIONS
          INTERNAL_ERROR = 1
          NOT_OPEN       = 2
          QUEUE_ERROR    = 3
          TCODE_INVALID  = 4
          OTHERS         = 5.
     IF SY-SUBRC NE 0.
           MESSAGE I002 WITH G_TRANSCODE.
           PERFORM LINE.
           WRITE: 'COULD NOT ENTER INTO BDC SESSSION'.
     ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM BDC_SCREEN
*-----------------------------------------------------------------------
*    Description:
*    - This routine adds an entry to the table BDCDATA with screen
*      information from a particular transaction.  This is used as part
*      of the process for creating data for batch input.
*    Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
    CLEAR BDCDATA.
    BDCDATA-PROGRAM = PROGRAM.
    BDCDATA-DYNPRO = DYNPRO.
    BDCDATA-DYNBEGIN = 'X'.
    APPEND BDCDATA.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM BDC_FIELD
*-----------------------------------------------------------------------
*    Description:
*    - This routine adds an entry to the table BDCDATA with field
*      information from a particular transaction.  This is used as part
*      of the process for creating data for batch input.
*    Parameters:
*      -->  fnam - name of the field on the screen
*           fval - value to be entered for that field on the screen.
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.

*-----------------------------------------------------------------------
*   FORM GET_FILE_SOURCE
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM GET_FILE_SOURCE.
    CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
           CLIENT              = SY-MANDT
           LOGICAL_FILENAME    = SOURCE
           OPERATING_SYSTEM    = SY-OPSYS
       IMPORTING
           FILE_NAME           = DATAFILE
       EXCEPTIONS
           FILE_NOT_FOUND      = 1

DATA:  ERROR_MESSAGE(100).

OPEN DATASET DATAFILE FOR INPUT IN TEXT MODE.
IF SY-SUBRC NE 0.
    MESSAGE E006 WITH DATAFILE.
ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*   FORM GET_FILE_VENDOR
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM GET_FILE_VENDOR.
    CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
           CLIENT              = SY-MANDT
           LOGICAL_FILENAME    = VENFILE
           OPERATING_SYSTEM    = SY-OPSYS
       IMPORTING
           FILE_NAME           = VENDDATA
       EXCEPTIONS
           FILE_NOT_FOUND      = 1

DATA:  ERROR_MESSAGE2(100).

OPEN DATASET VENDDATA FOR INPUT IN TEXT MODE.
IF SY-SUBRC NE 0.
    MESSAGE E006 WITH VENDDATA.
ENDIF.

* This part will move all the files from the unix box to an internal
* table
 IF SY-SUBRC EQ 0.
  DO.
     READ DATASET VENDDATA INTO TEMPVENDOR.
        IF SY-SUBRC NE 0.
            EXIT.
        ENDIF.

        IF TEMPVENDOR-SOURCE = 'IMMS'.
           APPEND TEMPVENDOR.
        ENDIF.

        CLEAR TEMPVENDOR.
  ENDDO.
ENDIF.

PERFORM SORTTEMPVENDOR.
ENDFORM.

*-----------------------------------------------------------------------
*  -  CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
*  This is the main routine in the program which reads each record from
*  the input file and creates the batch input data.
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.
REFRESH BDCDATA.
G_TRANSCODE = 'ME01'.
DO.
REFRESH BDCDATA.
     READ DATASET DATAFILE INTO IN_RECORD.
        IF SY-SUBRC NE 0.
            EXIT.
        ENDIF.
     PERFORM CONVERSION.
     IF IN_RECORD-MATNR NE SPACE.
        IF IN_RECORD-LIFNR NE SPACE.
           PERFORM CHECK_DATE.
             IF IN_RECORD-VDATU < IN_RECORD-BDATU.
                IF IN_RECORD-WERKS EQ SPACE.
                     SELECT * FROM MARC WHERE MATNR = IN_RECORD-MATNR
                                          AND LVORM NE 'X'.
                        IF MARC-WERKS(1) EQ 'P'.
                           MOVE MARC-WERKS TO IN_RECORD-WERKS.
                           PERFORM ADD_TRANSAC.
                           PERFORM INSERT_SESSION USING G_TRANSCODE.
                           REFRESH BDCDATA.
                        ENDIF.
                     ENDSELECT.
                ELSE.
                        PERFORM ADD_TRANSAC.
                        PERFORM INSERT_SESSION USING G_TRANSCODE.
                ENDIF.
             ELSE.
                PERFORM LINE.
                WRITE: 119 'VALID FROM date MUST be EARLIER than'.
                WRITE: 'VALID TO date'.
             ENDIF.
        ELSE.
          PERFORM LINE.
          WRITE: 119 'Could not find NEW VENDOR NUMBER for OLD'.
          WRITE: 'VENDOR NUMBER'.
        ENDIF.
     ELSE.
        PERFORM LINE.
        WRITE: 119 'Could not find NEW MATERIAL NUMBER for OLD '.
        WRITE: 'MATERIAL NUMBER'.
     ENDIF.
ENDDO.
ENDFORM.

*-----------------------------------------------------------------------
*   OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
CALL FUNCTION 'BDC_OPEN_GROUP'
     EXPORTING
         CLIENT              = SY-MANDT
         GROUP               = 'SOURCE LIST'
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
           MESSAGE E001 WITH 'SOURCE LIST'.
           WRITE: /'ERROR OPENNING BATCH SESSION'.
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
          MESSAGE I003 WITH 'SOURCE LIST'.
          WRITE: /'ERROR CLOSING BATCH SESSION'.
     ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*  CONVERSION
*-----------------------------------------------------------------------
*  -  This routine takes the OLD MATERIAL NUMBER entered in Excel, looks
*     for it in the Material Master and fetches the new material number.
*     The new Material Number is then moved to a record structure.
*  -  The second part of this routine does the same in Vendor Master,
*     but takes the OLD VENDOR NUMBER and fetches the new vendor number.
*-----------------------------------------------------------------------
FORM CONVERSION.
CLEAR: SEARCHKEY.
SELECT * FROM MARA WHERE LVORM NE 'X'
                     AND BISMT NE SPACE.

     IF MARA-BISMT(8) CP IN_RECORD-BISMT.
         MOVE MARA-MATNR  TO IN_RECORD-MATNR.
         EXIT.
     ENDIF.
ENDSELECT.

*Retrieve NEW VENDOR number from old Vendor number.

MOVE 'IMMS' TO SEARCHKEY(4).
MOVE IN_RECORD-ALTKN TO SEARCHKEY+4(10).

READ TABLE TEMPVENDOR WITH KEY SEARCHKEY
                           BINARY SEARCH.
IF SY-SUBRC EQ 0.
    MOVE TEMPVENDOR-SEQNO TO IN_RECORD-SEQNO(5).
    MOVE 'IM'             TO IN_RECORD-SEQNO+8(2).

SELECT SINGLE * FROM LFB1 WHERE ALTKN EQ IN_RECORD-SEQNO.
      MOVE LFB1-LIFNR  TO IN_RECORD-LIFNR.
ENDIF.

ENDFORM.

*-----------------------------------------------------------------------
*  CHECK DATE
*-----------------------------------------------------------------------
*  -  This routine check to see if the user entered the date values
*     in the EXCEL spreadsheet.  If the user didn't enter any values
*     then the values entered for VALID TO and VALID FROM at the
*     SELECTION-SCREEN will be defaulted for the absent values in the
*     EXCEL spreadsheet.
*-----------------------------------------------------------------------
FORM CHECK_DATE.
      IF IN_RECORD-VDATU EQ SPACE.
           MOVE STARTDAT TO IN_RECORD-VDATU.
      ENDIF.
      IF IN_RECORD-BDATU EQ SPACE.
           MOVE ENDDAT TO IN_RECORD-BDATU.
      ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     LINE
*-----------------------------------------------------------------------
*  -  This routine prints out each line on the report before the
*     error message is displayed.
*-----------------------------------------------------------------------
FORM LINE.
WRITE: / IN_RECORD-BISMT, 26 IN_RECORD-ALTKN, 41 IN_RECORD-WERKS.
WRITE: 51 IN_RECORD-FLIFN, 61 IN_RECORD-AUTET, 73 IN_RECORD-NOTKZ.
WRITE: 84 IN_RECORD-VDATU, 99 IN_RECORD-BDATU.
ENDFORM.

FORM SORTTEMPVENDOR.
     SORT TEMPVENDOR BY OLDACCT ASCENDING
                        SEQNO   ASCENDING.
ENDFORM.
