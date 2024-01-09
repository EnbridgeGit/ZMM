REPORT ZMMMC005 NO STANDARD PAGE HEADING LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMC005
*    Programmer  :  Ric Aarssen
*    Client      :  Union Gas Limited
*    Date        :  February 1998
*
* This ABAP loads records from the Purchasing Source List table (EORD).
* It moves the Source List for plant / vendor relationship of Centra
* plants to the new Plants in UGL.  This program will be used for the
* initial data load of vendor and materials from Centra Plants (P2*) to
* new Union Plants (P3*).
*
* 1998/03/16 added a check for MM/PP status field.  If it has a value
*            then do NOT create a new source list record.
************************************************************************

***************************  TABLES  ***********************************
TABLES: MARA,                            "Material Master: General Data
        MARC,                            "Material Master: Plant Data
        EORD,                            "Purchasing Source List
        LFB1.                            "Vendor master (Company Code)

************************  DATA ELEMENTS  *******************************
DATA:   G_TRANSCODE LIKE TSTC-TCODE.     "Transaction Code

DATA:   BEGIN OF BDCDATA OCCURS 100.     "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA:   END OF BDCDATA.
*-------------------- Work Area ----------------------------------------
DATA:   SUB(2)      TYPE N,              "Subcript for multiple lines
        W_MATNR     LIKE EORD-MATNR,     "Material Number
        W_WERKS     LIKE EORD-WERKS,     "Plant Location
        NEW_WERKS   LIKE EORD-WERKS,     "New Plant Location
        W_INSERT(1) TYPE C VALUE 'n',    "Insert Transaction Check
        STRING1(14)    TYPE     C,       "eord-vdatu (sub)
        STRING2(14)    TYPE     C,       "eord-bdatu (sub)
        STRING3(14)    TYPE     C,       "eord-lifnr (sub)
        STRING4(14)    TYPE     C,       "eord-ekorg (sub)
        STRING5(15)    TYPE     C,       "rm06w-feskz (sub)
        STRING6(14)    TYPE     C.       "eord-autet (sub)
***********************  SELECTION SCREEN  *****************************

*--------- Entering a range of Plant Locations
*               to select from Purchasing Source List ------------------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS: S_WERKS FOR EORD-WERKS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX2.

*-------------- Entering a range of Material Numbers
*                 to select from Purchasing Source List ----------------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS: S_MATNR FOR EORD-MATNR.
SELECTION-SCREEN END OF BLOCK BOX1.

*--------- Entering a range of Material Groups
*               to select from Material Master: General Data -----------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-003.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
SELECT-OPTIONS: S_MATKL FOR MARA-MATKL.
SELECTION-SCREEN END OF BLOCK BOX3.
*--------- Text describing what this program is going to do ------------
SELECTION-SCREEN SKIP 3.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-901.

* ************************** MAIN  PROGRAM *****************************

START-OF-SELECTION.

PERFORM OPEN_BATCH_SESSION.

PERFORM CREATE_BATCH_INPUT.

PERFORM CLOSE_BATCH_SESSION.

END-OF-SELECTION.

******************** 1st LEVEL SUBROUTINES *****************************

*-----------------------------------------------------------------------
*   OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
CALL FUNCTION 'BDC_OPEN_GROUP'
     EXPORTING
         CLIENT              = SY-MANDT
         GROUP               = 'ZMM_SRCLST'
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
           MESSAGE E001 WITH 'ZMM_SRCLST'.
      ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*  -  CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
*  This is the main routine in the program which reads each record from
*  the input file and creates the batch input data.
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.
G_TRANSCODE = 'ME01'.
REFRESH BDCDATA.

*--- read Purchasing Source List table for all materials & plants ------
SELECT * FROM EORD
   WHERE MATNR IN S_MATNR AND WERKS IN S_WERKS AND NOTKZ <> 'X'
   ORDER BY MATNR.        "group all the plants for a material together
   IF EORD-MATNR = W_MATNR AND EORD-WERKS = W_WERKS.
     ADD 1 TO SUB.
     PERFORM ADD_TO_SCREEN_0205.
   ELSE.
     SUB = 1.
     IF W_INSERT = 'y'.
       PERFORM SAVE_SCREENS.
       REFRESH BDCDATA.
       MOVE 'n' TO W_INSERT.
     ENDIF.

*--- read Material Master: General Data (once) for matl group check ----
     SELECT * FROM MARA
        WHERE MATNR = EORD-MATNR AND MATKL IN S_MATKL.
        IF SY-SUBRC = 0.

*--- read Material Master: Plant Data (once) for MM/PP status check ----
        SELECT SINGLE *  FROM MARC
           WHERE MATNR = EORD-MATNR
           AND WERKS   = EORD-WERKS
           AND MMSTA <> SPACE.
           IF SY-SUBRC <> 0.
             PERFORM ADD_SCREEN_0200.
             PERFORM ADD_TO_SCREEN_0205.
           ENDIF.
        ENDIF.
     ENDSELECT.
     MOVE EORD-MATNR TO W_MATNR.
     MOVE EORD-WERKS TO W_WERKS.
   ENDIF.
ENDSELECT.
*--- add last record to BDC --------------------------------------------
     IF W_INSERT = 'y'.
       PERFORM SAVE_SCREENS.
       REFRESH BDCDATA.
       MOVE 'n' TO W_INSERT.
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
          MESSAGE I003 WITH 'ZMM_SRCLST'.
     ENDIF.
ENDFORM.

******************** 2nd LEVEL SUBROUTINES *****************************

*-----------------------------------------------------------------------
*  ADD_SCREEN_0200 subroutine
*-----------------------------------------------------------------------
*  -  This routine provides the information on the screen 0200
*     necessary for the batch input session.
*-----------------------------------------------------------------------
FORM ADD_SCREEN_0200.
DATA: TEMPMATNR(18) TYPE C.
*      TEMPMATNR = EORD-MATNR+12(6).
      PERFORM BDC_SCREEN USING 'SAPDM06I' '0200'.
      PERFORM BDC_FIELD  USING 'EORD-MATNR'  EORD-MATNR.
      MOVE EORD-WERKS TO NEW_WERKS.
      MOVE '3' TO NEW_WERKS+1(1).
      PERFORM BDC_FIELD  USING 'EORD-WERKS'  NEW_WERKS.
      MOVE 'y' TO W_INSERT.
ENDFORM.

*-----------------------------------------------------------------------
*  ADD_TO_SCREEN_0205 subroutine
*-----------------------------------------------------------------------
*  -  This routine provides the information on the screen 0205
*     necessary for the batch input session.
*-----------------------------------------------------------------------
FORM ADD_TO_SCREEN_0205.
      PERFORM ASSIGN_INDEX.
      PERFORM BDC_SCREEN USING 'SAPDM06I'      '0205'.
      PERFORM BDC_FIELD  USING STRING1  EORD-VDATU.
      PERFORM BDC_FIELD  USING STRING2  EORD-BDATU.
      PERFORM BDC_FIELD  USING STRING3  EORD-LIFNR.
      PERFORM BDC_FIELD  USING STRING4  EORD-EKORG.
      PERFORM BDC_FIELD  USING STRING5  EORD-FLIFN.
      PERFORM BDC_FIELD  USING STRING6  EORD-AUTET.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM ASSIGN_INDEX.
*-----------------------------------------------------------------------
*   Description:
*   - This is used to place the source list records in the next
*     available position.
*-----------------------------------------------------------------------
FORM ASSIGN_INDEX.
     STRING1 = 'EORD-VDATU(  )'.
     STRING1+11(2) = SUB.
     STRING2 = 'EORD-BDATU(  )'.
     STRING2+11(2) = SUB.
     STRING3 = 'EORD-LIFNR(  )'.
     STRING3+11(2) = SUB.
     STRING4 = 'EORD-EKORG(  )'.
     STRING4+11(2) = SUB.
     STRING5 = 'RM06W-FESKZ(  )'.
     STRING5+12(2) = SUB.
     STRING6 = 'EORD-AUTET(  )'.
     STRING6+11(2) = SUB.
ENDFORM.

*-----------------------------------------------------------------------
*  SAVE_SCREENS subroutine
*-----------------------------------------------------------------------
*  -  This routine adds the information previously collected to
*     the batch input session.
*-----------------------------------------------------------------------
FORM SAVE_SCREENS.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
      PERFORM INSERT_SESSION USING G_TRANSCODE.
ENDFORM.

*-----------------------------------------------------------------------
*  INSERT_SESSION subroutine
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
     ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*  BDC_SCREEN subroutine
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
*  BDC_FIELD subroutine
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
