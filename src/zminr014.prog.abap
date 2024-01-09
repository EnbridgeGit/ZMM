REPORT ZMINR014 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 255
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMINR014
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  November 22 1996
*
* This ABAP will allow the user to enter a new vendor number and
* then create a report displaying New Vendor Number, Old Vendor Number
* and Vendor Name.
*
************************************************************************
TABLES: MARA, MARC, LFA1, LFB1.

DATA: VENDDATA LIKE FILENAMECI-FILEEXTERN.
DATA: TEMPSRCE(4)   TYPE C,
      TEMPSEQNO(5),
      TEMPTYPE(2).
PARAMETERS: VENFILE  LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA002_01'.

DATA:   BEGIN OF TEMPVENDOR OCCURS 25000,
          SOURCE(4)        TYPE C,
          OLDACCT(10)      TYPE C,
          VENNAME(35)      TYPE C,
          LOADSW(1)        TYPE C,
          DUPLDW(1)        TYPE C,
          SEQNO(5)         TYPE C,
        END OF TEMPVENDOR.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
       S_LIFNR          FOR LFB1-LIFNR.
SELECTION-SCREEN END OF BLOCK BOX1.

INCLUDE <ICON>.
AT SELECTION-SCREEN.
PERFORM GET_FILE_VENDOR.
************************************************************************
TOP-OF-PAGE.

WRITE: / ICON_DATE AS ICON.
WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 100 TEXT-002 COLOR COL_HEADING.
WRITE: 246 SY-REPID COLOR COL_NEGATIVE.

WRITE: / ICON_TIME AS ICON.
WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE: 99(15).
WRITE: 246 'PAGE:' INTENSIFIED OFF.
WRITE: 251(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: /.
FORMAT COLOR COL_NORMAL.
ULINE.
WRITE: /1  TEXT-008, 30 TEXT-007.

WRITE: /1  TEXT-006, 30 TEXT-006, 60 TEXT-006.

WRITE: /1  TEXT-005, 30 TEXT-005, 60 TEXT-009.
ULINE.

**************************** MAIN  PROGRAM *****************************
START-OF-SELECTION.
SELECT * FROM LFB1 WHERE LIFNR IN S_LIFNR.
CLEAR: TEMPSEQNO, TEMPTYPE, TEMPSRCE.
MOVE LFB1-ALTKN(5)    TO TEMPSEQNO.
MOVE LFB1-ALTKN+8(2)  TO TEMPTYPE.

IF TEMPTYPE EQ 'IM'.
     MOVE 'IMMS' TO TEMPSRCE.
ELSEIF TEMPTYPE EQ 'AP'.
     MOVE 'AP  ' TO TEMPSRCE.
ELSEIF TEMPTYPE EQ 'CN'.
     MOVE 'INGT' TO TEMPSRCE.
ELSEIF TEMPTYPE EQ 'R2'.
     MOVE 'R2  ' TO TEMPSRCE.
ENDIF.

   LOOP AT TEMPVENDOR.
* This IF statement will be executed if the Old Vendor Number field has
* been entered with the first five spots and the last two spots.
     IF TEMPVENDOR-SEQNO EQ TEMPSEQNO AND TEMPVENDOR-SOURCE EQ TEMPSRCE.
       SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
       WRITE: /1 LFB1-LIFNR, 30 TEMPVENDOR-OLDACCT, 60 LFA1-NAME1.
       WRITE: 96 LFA1-NAME2, 132 LFA1-NAME3, 168 LFA1-NAME4.
       EXIT.
     ENDIF.
   ENDLOOP.
ENDSELECT.

****************************** SUBROUTINES *****************************
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

           APPEND TEMPVENDOR.

        CLEAR TEMPVENDOR.
  ENDDO.
ENDIF.

SORT TEMPVENDOR BY SEQNO   ASCENDING
                   OLDACCT ASCENDING.
ENDFORM.
