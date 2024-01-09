REPORT ZMMMC002 LINE-COUNT 65 LINE-SIZE 132 MESSAGE-ID ZM NO STANDARD
       PAGE HEADING.

************************************************************************
*  Author: Dorothy Bialkowska
*  Brief Description:
*     -  This program will be used to load characteristics and their
*        values for a given material.
************************************************************************

TABLES: CABN.

DATA:   BEGIN OF RECORD,
              NUMBER(18)     TYPE C,
              DUMY1(1)       TYPE C,
              CLASS(18)      TYPE C,
              DUMY2(1)       TYPE C,
              CHAR(30)       TYPE C,
              DUMY3(1)       TYPE C,
              VALUE(30)      TYPE C,
        END OF RECORD,

        BEGIN OF BDC_TAB OCCURS 200.
              INCLUDE STRUCTURE BDCDATA.
DATA:   END OF BDC_TAB.

DATA:   TRANSCODE      LIKE     TSTC-TCODE,
        COUNTER        TYPE     I,
        BLOCK          LIKE     COUNTER,
        STRING1(15)    TYPE     C,
        STRING2(15)    TYPE     C,
        NUMVAL(30)     TYPE     N,
        CURRENT(18)    TYPE     C,
        NEXT(18)       TYPE     C,
        DATAFILE       LIKE     FILENAME-FILEEXTERN.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
   SELECTION-SCREEN SKIP 1.
   PARAMETERS:  LOGICFLE     LIKE     FILENAME-FILEINTERN    DEFAULT
                'ZMMMC002'.
SELECTION-SCREEN END OF BLOCK BOX.

AT SELECTION-SCREEN.
   PERFORM GET_FILE.

TOP-OF-PAGE.
  DATA: WRK_REPID LIKE SY-REPID.                "UPGRAD 4.7 CHANGES

   FORMAT INTENSIFIED OFF.
   WRITE: / TEXT-001, SY-DATUM, 117 TEXT-002, SY-PAGNO.
   MOVE SY-REPID TO WRK_REPID.                  "UPGRAD 4.7 CHANGES
   WRK_REPID+0(1) = 'Z'.                        "UPGRAD 4.7 CHANGES
*   WRITE : / SY-UZEIT UNDER SY-DATUM, SY-REPID UNDER TEXT-002. "UPGR4.7
   WRITE : / SY-UZEIT UNDER SY-DATUM, WRK_REPID UNDER TEXT-002. "UPGR4.7
   WRITE: /43 TEXT-000.
   ULINE.
   FORMAT INTENSIFIED ON.

START-OF-SELECTION.
   PERFORM OPEN_BATCH_SESSION.
   PERFORM CREATE_BATCH_INPUT.
   PERFORM CLOSE_BATCH_SESSION.



************************************************************************
*                          SUBROUTINES                                 *
*-----------------------------------------------------------------------
*     FORM GET_FILE.
*   Description:
*   - This subroutine attempts to the physical file with data for a
*     batch file.  If file can not be accessed, the eror message is
*     displayed.
*-----------------------------------------------------------------------
FORM GET_FILE.
     CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
              CLIENT                  = SY-MANDT
              LOGICAL_FILENAME        = LOGICFLE
              OPERATING_SYSTEM        = SY-OPSYS
          IMPORTING
              FILE_NAME               = DATAFILE
          EXCEPTIONS
              FILE_NOT_FOUND          = 1

     DATA: ERROR_MESSAGE(100).

     OPEN DATASET DATAFILE FOR INPUT IN TEXT MODE.
     IF SY-SUBRC NE 0.
        MESSAGE E006 WITH DATAFILE.
     ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
*   Description:
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
CALL FUNCTION 'BDC_OPEN_GROUP'
     EXPORTING
         CLIENT              = SY-MANDT
         GROUP               = 'CHAR_VALUES'
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
        MESSAGE E001 WITH 'CHAR_VALUES'.
        WRITE: / 'Error openning batch session'.
     ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM CREATE_BATCH_INPUT.
*-----------------------------------------------------------------------
*   Desription:
*   - This is a main routine in the program.  It's purpose is to read
*     each record in the flat file and create the batch input data.
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.

TRANSCODE = 'CL20'.
BLOCK = 0.
REFRESH BDC_TAB.
CLEAR: CURRENT, NEXT.

DO.
   READ DATASET DATAFILE INTO RECORD.
   IF SY-SUBRC NE 0.
      PERFORM LAST_STEP.
      PERFORM INSERT_SESSION USING TRANSCODE.
      EXIT.
   ENDIF.

   MOVE RECORD-NUMBER TO CURRENT.

   IF CURRENT <> NEXT.
      COUNTER = 0.
      BLOCK = BLOCK + 1.

      IF BLOCK NE 1.
         PERFORM LAST_STEP.
         PERFORM INSERT_SESSION USING TRANSCODE.
         REFRESH BDC_TAB.
      ENDIF.

      PERFORM GET_SCREEN USING 'SAPMMCLF' '0100' 'X'.
      PERFORM GET_FIELD USING 'RMCBC-OBJEK' RECORD-NUMBER.
      PERFORM GET_FIELD USING 'RMCLF-KLART' '001'.

      PERFORM GET_SCREEN USING 'SAPLCLFM' '0500' 'X'.
      PERFORM GET_FIELD USING 'RMCLF-CLASS(1)' RECORD-CLASS.
      PERFORM GET_FIELD USING 'RMCLF-STATU(1)' '3'.
      PERFORM GET_FIELD USING 'BDC_OKCODE' '/2'.

      PERFORM GET_SCREEN USING 'SAPLCTMS' '0109' 'X'.
      COUNTER = COUNTER + 1.
      PERFORM ASSIGN_INDEX.
      PERFORM ADD_CHAR.
   ENDIF.

   IF CURRENT EQ NEXT.
      COUNTER = COUNTER + 1.
      PERFORM ASSIGN_INDEX.
      IF COUNTER LE 10.
         PERFORM ADD_CHAR.
      ENDIF.

      IF COUNTER = 11.
         PERFORM GET_FIELD USING 'BDC_OKCODE' '/3'.
         PERFORM GET_SCREEN USING 'SAPLCLFM' '0500' 'X'.
         PERFORM GET_FIELD USING 'BDC_OKCODE' '/2'.
         PERFORM GET_SCREEN USING 'SAPLCTMS' '0109' 'X'.

         COUNTER = 1.
         PERFORM ASSIGN_INDEX.
         PERFORM ADD_CHAR.
      ENDIF.
   ENDIF.
   MOVE RECORD-NUMBER TO NEXT.
ENDDO.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM CLOSE_BATCH_SESSION.
*-----------------------------------------------------------------------
*   Description:
*    - This closes the batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_BATCH_SESSION.
CALL FUNCTION 'BDC_CLOSE_GROUP'
     EXCEPTIONS
          NOT_OPEN    = 1
          QUEUE_ERROR = 2
          OTHERS      = 3.

     IF SY-SUBRC NE 0.
        MESSAGE I003 WITH 'CHAR_VALUES'.
        WRITE: / 'Error closing batch session'.
     ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
*   Description:
*   - For each transaction, after the BDC data is created, this routine
*     inserts data into the batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION USING TRANSCODE.
CALL FUNCTION 'BDC_INSERT'
     EXPORTING
         TCODE           = TRANSCODE
     TABLES
          DYNPROTAB      = BDC_TAB
     EXCEPTIONS
          INTERNAL_ERROR = 1
          NOT_OPEN       = 2
          QUEUE_ERROR    = 3
          TCODE_INVALID  = 4
          OTHERS         = 5.

      IF SY-SUBRC NE 0.
         MESSAGE I002 WITH TRANSCODE.
         WRITE: / 'Could not enter', RECORD-NUMBER, 'into BDC session'.
      ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_SCREEN
*-----------------------------------------------------------------------
*   Description:
*   - For a particular transaction, this routine adds information about
*     program name and screen number into BDC_TAB. This is used as a
*     part of the process for creating data for batch input.
*   Parameters:
*   --> PROGRAM - Program name of the screen
*       DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM GET_SCREEN USING PROGRAM DYNPRO START.
     CLEAR BDC_TAB.
     BDC_TAB-PROGRAM = PROGRAM.
     BDC_TAB-DYNPRO = DYNPRO.
     BDC_TAB-DYNBEGIN = START.
     APPEND BDC_TAB.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_FIELD
*-----------------------------------------------------------------------
*   Description:
*   - For a particular transaction, this routine adds information about
*     field name and its value into BDC_TAB.  This is used as a part
*     of the process for creating data for batch input.
*   Parameters:
*   --> FNAME - field name on the screen
*       FVAL - value of the field to be entered on this screen
*-----------------------------------------------------------------------
FORM GET_FIELD USING FNAME FVAL.
     CLEAR BDC_TAB.
     BDC_TAB-FNAM = FNAME.
     BDC_TAB-FVAL = FVAL.
     APPEND BDC_TAB.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM LAST_STEP
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine encompasses final steps before entering
*     transaction into batch session.
*-----------------------------------------------------------------------
FORM LAST_STEP.
     PERFORM GET_FIELD USING 'BDC_OKCODE' '/3'.
     PERFORM GET_SCREEN USING 'SAPLCLFM' '0500' 'X'.
     PERFORM GET_FIELD USING 'RMCLF-STATU(1)' '1'.
     PERFORM GET_FIELD USING 'BDC_OKCODE' '/11'.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM ASSIGN_INDEX.
*-----------------------------------------------------------------------
*   Description:
*   - This is used to placed characteristic name and value in the next
*     available position.
*-----------------------------------------------------------------------
FORM ASSIGN_INDEX.
     STRING1 = 'RCTMS-MNAME(  )'.
     STRING1+12(2) = COUNTER.
     STRING2 = 'RCTMS-MWERT(  )'.
     STRING2+12(2) = COUNTER.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM ADD_CHAR.
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine is used to put on the screen name and
*     corresponding value of characteristic, one set at the time.
*-----------------------------------------------------------------------
FORM ADD_CHAR.
     SELECT SINGLE * FROM CABN
            WHERE ATNAM = RECORD-CHAR.
     IF CABN-ATFOR = 'NUM' AND ( RECORD-VALUE = 'N/A' OR RECORD-VALUE =
                     'UNK' ).
        RECORD-VALUE = 0.
     ENDIF.
     PERFORM GET_FIELD USING STRING1 RECORD-CHAR.
     PERFORM GET_FIELD USING STRING2 RECORD-VALUE.
ENDFORM.
