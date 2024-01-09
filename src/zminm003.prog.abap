REPORT ZMINM003.
************************************************************************
*  Programmer: Lee Haire
*              OmniLogic Systems Group
*  Brief Description:
*    - This program is used for mass reversal/cancellation of material
*  documents.  Obviously, the user must know all the material document
*  numbers which are to be reversed.
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
*
*
************************************************************************

TABLES: MKPF,                              "material doc. header
        MSEG,                              "material doc. line items
        T100.                              "message table
* internal table of material documents to reverse
DATA: BEGIN OF DOCTAB OCCURS 100,
        MBLNR LIKE MKPF-MBLNR,
      END OF DOCTAB.
* table of material documents that were succesfully reversed and the
* corresponding reversal document.
DATA: BEGIN OF POSTTAB OCCURS 100,
        MBLNR LIKE MKPF-MBLNR,                           "mat. doc.
        RBLNR LIKE MKPF-MBLNR,                           "rev. doc.
      END OF POSTTAB.
* table of material docs. that did not successfully reverse and the
* error message returned from the transaction
DATA: BEGIN OF ERRTAB OCCURS 10,
        MBLNR LIKE MKPF-MBLNR,
        MSG(150),
      END OF ERRTAB.
* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.
* defaults for call transaction
CONSTANTS: G_TCODE LIKE SY-TCODE VALUE 'MBST',            "trans. code
           G_MODE(1) VALUE 'N',                           "background
           G_UPDATE(1) VALUE 'S'.                         "update mode
*=======================================================================
* SELECTION SCREEN
*=======================================================================
SELECT-OPTIONS: S_MBLNR FOR MKPF-MBLNR OBLIGATORY.

*=======================================================================
*  MAIN PROCESSING
*=======================================================================
START-OF-SELECTION.
* build table of mat. docs. to reverse...
  SELECT MBLNR INTO DOCTAB-MBLNR FROM MKPF WHERE MBLNR IN S_MBLNR.
    APPEND DOCTAB.
  ENDSELECT.
  PERFORM PROCESS_BDC.
  PERFORM REPORT.

*-----------------------------------------------------------------------
*     FORM PROCESS_BDC
*-----------------------------------------------------------------------
* - The BDC mapping for reversing/cancelling a material document.
*-----------------------------------------------------------------------
FORM PROCESS_BDC.

* number of line items in a mat. document
  DATA: N TYPE I.

  SELECT COUNT(*) INTO N FROM MSEG WHERE MBLNR = DOCTAB-MBLNR.

  LOOP AT DOCTAB.
    REFRESH: BDCDATA.
    PERFORM BDC_SCREEN USING 'SAPMM07M' '0460'.
    PERFORM BDC_FIELD  USING 'RM07M-MBLNR' DOCTAB-MBLNR.
    PERFORM BDC_SCREEN USING 'SAPMM07M' '0421'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
    PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
*   coding block appears as many times as there are line items
    DO N TIMES.
      PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
    ENDDO.
    PERFORM BDC_SCREEN USING 'SAPMM07M' '0460'.
    PERFORM GET_RESULTS.
  ENDLOOP.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM GET_RESULTS
*-----------------------------------------------------------------------
* - Calls the transaction and stores the results
*-----------------------------------------------------------------------
FORM GET_RESULTS.

* messages collected from call transaction
  DATA: BEGIN OF MESSTAB OCCURS 5.
          INCLUDE STRUCTURE BDCMSGCOLL.
  DATA: END OF MESSTAB.
  DATA: MESS_STR(150),                              "message string
        N TYPE I,                                   "# messages
        RC LIKE SY-SUBRC.                           "return code


  CALL TRANSACTION G_TCODE USING BDCDATA MODE G_MODE UPDATE G_UPDATE
       MESSAGES INTO MESSTAB.
  RC = SY-SUBRC.
* read last message returned from transaction
  DESCRIBE TABLE MESSTAB LINES N.
  READ TABLE MESSTAB INDEX N.
* successul reversals
  IF RC = 0.
    CLEAR POSTTAB.
    POSTTAB-MBLNR = DOCTAB-MBLNR.
    POSTTAB-RBLNR = MESSTAB-MSGV1.
    APPEND POSTTAB.
* unsuccessul reversals
  ELSE.
    PERFORM CONVERT_MESS_TO_STRING USING MESSTAB MESS_STR.
    CLEAR ERRTAB.
    ERRTAB-MBLNR = DOCTAB-MBLNR.
    ERRTAB-MSG = MESS_STR.
    APPEND ERRTAB.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM CONVERT_MESS_TO_STRING
*-----------------------------------------------------------------------
* - Converts the message returned from call transaction into a
* "readable" format.  All occurrences of the symbol '&' are replaced
* by the message variables to get an appropriate error message.
*
* Parameters:
*      -->  MSTRUCT - message structure returned by call transaction
*      <--  MSTRING - resulting error message
*-----------------------------------------------------------------------
FORM CONVERT_MESS_TO_STRING USING MSTRUCT LIKE BDCMSGCOLL
                                  MSTRING.

  DATA: RC LIKE SY-SUBRC.                           "return code

  SELECT SINGLE * FROM T100 WHERE SPRSL = MSTRUCT-MSGSPRA
                              AND ARBGB = MSTRUCT-MSGID
                              AND MSGNR = MSTRUCT-MSGNR.
  CHECK SY-SUBRC = 0.
  MSTRING = T100-TEXT.

  REPLACE '&' WITH MSTRUCT-MSGV1 INTO MSTRING.
  RC = SY-SUBRC.
  CONDENSE MSTRING.
  CHECK RC = 0.

  REPLACE '&' WITH MSTRUCT-MSGV2 INTO MSTRING.
  RC = SY-SUBRC.
  CONDENSE MSTRING.
  CHECK RC = 0.

  REPLACE '&' WITH MSTRUCT-MSGV3 INTO MSTRING.
  RC = SY-SUBRC.
  CONDENSE MSTRING.
  CHECK RC = 0.

  REPLACE '&' WITH MSTRUCT-MSGV4 INTO MSTRING.
  RC = SY-SUBRC.
  CONDENSE MSTRING.
  CHECK RC = 0.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM REPORT
*-----------------------------------------------------------------------
* - Report generated after all reversals have been attempted.  The
* report lists all documents that were successfully reversed and what
* the reversal document numbers are.  As well, for any failed reversals
* the mat. doc. number and the corresponding error message are given.
*-----------------------------------------------------------------------
FORM REPORT.

* number of lines in internal tables
  DATA: N1 TYPE I,
        N2 TYPE I.
* report for successful reversals
  DESCRIBE TABLE POSTTAB LINES N1.
  IF N1 > 0.
    FORMAT INTENSIFIED ON.
    WRITE: / 'The following documents were successfully reversed:'.
    WRITE: / 'Mat. Doc.', 25 'Rev. Doc.'.
    FORMAT INTENSIFIED OFF.
    LOOP AT POSTTAB.
      WRITE: / POSTTAB-MBLNR, 25 POSTTAB-RBLNR.
    ENDLOOP.
  ENDIF.

* report for unsuccessful reversals
  DESCRIBE TABLE ERRTAB LINES N2.
  CHECK N2 > 0.
  FORMAT INTENSIFIED ON.
  WRITE: / 'The following documents could not be reversed:'.
  WRITE: / 'Mat. Doc.', 25 'Error Message'.
  FORMAT INTENSIFIED OFF.
  LOOP AT ERRTAB.
    WRITE: / ERRTAB-MBLNR, 25 ERRTAB-MSG.
  ENDLOOP.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
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
*     FORM BDC_FIELD
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

  DATA: TVAL.

  TVAL = FVAL(1).
* if field is not BDC OKCODE, only add to BDC table if not initialized
* with ' ' or '/'.
  IF FNAM <> 'BDC_OKCODE'.
    CHECK FVAL <> SPACE AND TVAL <> '/'.
  ENDIF.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
