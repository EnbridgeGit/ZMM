REPORT ZMMMR098 MESSAGE-ID 00.

*_-________________________________________________________________
* Program      :  Zmmmr098
* Created ON   :  jANUARY 19, 2001.
* Created By   :  Mark Dufault
*__________________________________________________________________
* This program READS bseg DATA TO DETERMINE the records that need
* to be placed in the email file.  The records are used to send EFT
* emails to all people who received an EFT expense cheque deposit.
*__________________________________________________________________
TABLES: BSEG,
        LFB1.  " Vendor file

DATA:  COUNT(6)     TYPE N,                "records read
       WORK_DATE   LIKE BSEG-ZFBDT,
       ANY_DATE    LIKE BSEG-ZFBDT,
       MESS(100),                           " store error message
       DATA_LIN(76) .                      "line of data

DATA:   BEGIN OF OUT_FILE,
         EFTVENDOR    LIKE LFB1-LIFNR,     "EMPLOYEE # L= 10
         EFTAMOUNT(13) TYPE C,   "AMOUNT OF DEPOSIT L= 13
         EFTEMAIL     LIKE LFB1-INTAD,     "EMAIL ADDRESS L= 130
         EFTDATE      LIKE BSEG-ZFBDT,     "TRANSACTION DATE L= 8
         FILLIT(9),                       "FILLER TO 170 BYTES
       END OF OUT_FILE.


*___________________________________________________________________
* parameters
*__________________________________________________________________
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.
PARAMETERS:      P_FILE LIKE FILENAME-FILEEXTERN OBLIGATORY DEFAULT
                 '/usr/sap/interfaces/P01/EDI/outbound/EXPENEFT.dat'.
PARAMETERS:      X_FILE LIKE FILENAME-FILEEXTERN OBLIGATORY DEFAULT
                 '/usr/sap/interfaces/P01/EDI/inbound/expendate.dat'.

SELECTION-SCREEN END OF BLOCK BLK1.

*___________________________________________________________________
* Event : start-of-selection.... open file to OUTPUT
*____________________________________________________________________
START-OF-SELECTION.

**    For reruns could add the reading of a date card from a file
**    This would allow you run any day you wish.

    "open file for output
      OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MESS.
      IF SY-SUBRC <> 0.
         MESSAGE E368 WITH MESS.
      ENDIF.

*_________________________________________________________________
* Event:  start-of-selection  . . . main program
*_________________________________________________________________

    "re-initialization
    CLEAR OUT_FILE.
*    COMPUTE ANY_DATE = SY-DATUM - 1.
      COMPUTE ANY_DATE = SY-DATUM.

    SELECT * FROM BSEG
*      WHERE ZLSCH = 'F'.
      WHERE ZFBDT = ANY_DATE AND
            ZLSCH = 'F'.
       SELECT * FROM LFB1
         WHERE LIFNR = BSEG-LIFNR.
           OUT_FILE-EFTVENDOR = BSEG-LIFNR.
           OUT_FILE-EFTEMAIL = LFB1-INTAD.
       ENDSELECT.
      OUT_FILE-EFTAMOUNT = BSEG-WRBTR.
      OUT_FILE-EFTDATE = BSEG-ZFBDT.
      IF OUT_FILE-EFTEMAIL = SPACE.
         MOVE 'X' TO OUT_FILE-EFTEMAIL.
      ENDIF.
     IF OUT_FILE-EFTVENDOR(1) = 'E'.
       TRANSFER OUT_FILE TO P_FILE.
     ENDIF.
    ENDSELECT.


END-OF-SELECTION.

FORM GET_DATE_CARD.
    "read the input file to itab
    CLEAR DATA_LIN.
    OPEN DATASET X_FILE FOR INPUT IN TEXT MODE MESSAGE MESS.
    DO.
       READ DATASET X_FILE INTO DATA_LIN.
       IF SY-SUBRC <> 0.
          EXIT.
       ENDIF.
       IF DATA_LIN <> SPACE.
          COUNT = COUNT + 1.
          MOVE DATA_LIN+00(08) TO WORK_DATE.
       ENDIF.
   ENDDO.
*** This is the end.....
ENDFORM.
