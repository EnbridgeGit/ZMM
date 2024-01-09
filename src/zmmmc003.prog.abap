REPORT ZMMMC003  MESSAGE-ID ZM.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - This program creates price records for pricing condition ZR00     *
*    The program reads input data from the mainframe and creates a BDC *
*    session                                                           *
*----------------------------------------------------------------------*
TABLES: MARA.

DATA: PHYFILE LIKE FILENAMECI-FILEEXTERN.

DATA: BEGIN OF BDCDATA OCCURS 100.     "BDC data structure
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: BEGIN OF ITAB    OCCURS 100,
        MATNR(18),                     "KOMG-MATNR   "Material #
        KBETR(10),                   "KONP-KBETR   "Rate (Selling Price)
        DATAB(8),                    "RV13A-DATAB  "Validity Start Date
        DATBI(8),                    "RV13A-DATBI  "Validity End   Date
        KMEIN LIKE KONP-KMEIN,         "Unit of measure (eg.EA)
        PROBLEM(1),                  "Indicator of a problem
      END OF ITAB.

DATA: TEMPDATEC(8).                     "To write dates
DATA: TEMPDATED   LIKE SY-DATUM.        "To write dates

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.

* logical  file  name
PARAMETERS: LFILE   LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMC003_02',
            P_GROUP LIKE APQI-GROUPID        DEFAULT 'PRICE_LOAD'.
* test  run
PARAMETERS: TESTRUN AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK BOX1.


AT SELECTION-SCREEN.
  PERFORM OPEN_INPUT_FILE.

START-OF-SELECTION.
  PERFORM POPULATE_ITAB.

* Write out a message if nothing to process
  READ TABLE ITAB INDEX 1.
  IF SY-SUBRC <> 0.
    WRITE: / 'No data to process'.
    STOP.
  ENDIF.

  IF TESTRUN <> 'X'.
    PERFORM OPEN_SESSION.
    PERFORM CREATE_BATCH_INPUT.
    PERFORM CLOSE_SESSION.
  ENDIF.

*---------------------------------------------------------------------*
*       FORM OPEN_INPUT_FILE                                          *
*---------------------------------------------------------------------*
* Routine to convert the logicl file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
*---------------------------------------------------------------------*
FORM OPEN_INPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = LFILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006.
  ELSE.
    OPEN DATASET PHYFILE FOR INPUT IN TEXT MODE.
  ENDIF.
ENDFORM.



*-----------------------------------------------------------------------
*     FORM POPULATE_ITAB
*-----------------------------------------------------------------------
* - This routine populates internal table ITAB with data required for
*   the bdc session
*-----------------------------------------------------------------------
FORM POPULATE_ITAB.
  IF TESTRUN = 'X'.
    WRITE: /'Test Run. Data display only. No Updates will be performed'.
    ULINE.
  ENDIF.

  CLEAR ITAB.
  REFRESH ITAB.

  DO.
    CLEAR ITAB.
    READ DATASET PHYFILE INTO ITAB.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM MARA WHERE MATNR = ITAB-MATNR.     "to get unit
    IF SY-SUBRC = 0.
      MOVE MARA-MEINS TO ITAB-KMEIN.
      ITAB-PROBLEM = ' '.
    ELSE.
      ITAB-PROBLEM = 'M'.
    ENDIF.
    IF ITAB-DATAB(2) <> '19'.
      ITAB-PROBLEM = 'D'.
    ENDIF.
    APPEND ITAB.
    WRITE: / ITAB-MATNR,
             ITAB-KBETR,
             ITAB-DATAB,
             ITAB-DATBI,
             ITAB-KMEIN.
    IF ITAB-PROBLEM = 'M'.
      WRITE: 'Material Not found'.
    ENDIF.
    IF ITAB-PROBLEM = 'D'.
      WRITE: 'Starting date incorrect'.
    ENDIF.
  ENDDO.



ENDFORM.


*-----------------------------------------------------------------------
*     FORM CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
* - This is the main routine of the program which reads each record
*   from the input file and creates the batch input data.
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.

  LOOP AT ITAB.
    CLEAR BDCDATA.
    REFRESH BDCDATA.
    CHECK ITAB-PROBLEM IS INITIAL.            "Check for problem records
    PERFORM BDC_SCREEN USING 'SAPMV13A'    '100'.
    PERFORM BDC_FIELD  USING 'RV13A-KSCHL' 'ZR00'.

    PERFORM BDC_SCREEN USING 'SAPMV13A'    '1904'.
    WRITE ITAB-DATAB TO TEMPDATED DD/MM/YYYY.
    WRITE TEMPDATED TO TEMPDATEC.
    PERFORM BDC_FIELD  USING 'RV13A-DATAB' TEMPDATEC.        "Valid from
    WRITE ITAB-DATBI TO TEMPDATED DD/MM/YYYY.
    WRITE TEMPDATED TO TEMPDATEC.
    PERFORM BDC_FIELD  USING 'RV13A-DATBI' TEMPDATEC.        "Valid to

    PERFORM BDC_FIELD  USING 'KOMG-MATNR(1)' ITAB-MATNR.     "Material#
    PERFORM BDC_FIELD  USING 'KONP-KBETR(1)' ITAB-KBETR.     "Rate(Price
    PERFORM BDC_FIELD  USING 'KONP-KONWA(1)' 'CAD'.          "Rate Unit
    PERFORM BDC_FIELD  USING 'KONP-KPEIN(1)' '1'.            "Price Unit
    PERFORM BDC_FIELD  USING 'KONP-KMEIN(1)' ITAB-KMEIN.     "Unit of Me
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.             "Save
    PERFORM INSERT_SESSION.
  ENDLOOP.
ENDFORM.


*-----------------------------------------------------------------------
*   FORM OPEN_SESSION
*-----------------------------------------------------------------------
* - This routine opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = P_GROUP
*           HOLDDATE          =
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERRORID     = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALIDD     = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E004.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine closes a batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_SESSION.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  IF SY-SUBRC <> 0.
    WRITE: / 'BDC Close Group Error. rc=', SY-SUBRC.
    EXIT.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'VK11'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error inserting data into session.'.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM BDC_SCREEN
*-----------------------------------------------------------------------
*   Description:
*   - This routine adds an entry to the table BDCDATA with screen
*     information from a particular transaction.  This is used as part
*     of the process for creating data for batch input.
*
*   Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.


*-----------------------------------------------------------------------
*  FORM BDC_FIELD
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
