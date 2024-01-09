REPORT ZMINR015 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 255
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMINR015
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  November 26 1996
*
* This ABAP will allow the user to enter a new vendor number and
* then create a report or file displaying New Vendor Number,
* Old Vendor Number and Vendor Name.
*
************************************************************************
TABLES: MARA, MARC, LFA1, LFB1.

DATA: VENDDATA LIKE FILENAMECI-FILEEXTERN,
      PHYFILE  LIKE FILENAMECI-FILEEXTERN.

DATA: TEMPSRCE(4)   TYPE C,
      TEMPSEQNO(5),
      TEMPTYPE(2).

PARAMETERS: VENFILE  LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA002_01'.
PARAMETERS: EVANFILE LIKE FILENAME-FILEINTERN DEFAULT 'ZMINR015'.

DATA:   BEGIN OF TEMPVENDOR OCCURS 25000,
          SOURCE(4)        TYPE C,
          OLDACCT(10)      TYPE C,
          VENNAME(35)      TYPE C,
          LOADSW(1)        TYPE C,
          DUPLDW(1)        TYPE C,
          SEQNO(5)         TYPE C,
        END OF TEMPVENDOR.

DATA   : BEGIN OF TEMPFILE OCCURS 20,
          SOURCE    LIKE TEMPVENDOR-SOURCE,
          OLDACCT   LIKE TEMPVENDOR-OLDACCT,
          LIFNR     LIKE LFB1-LIFNR,
          NAME1     LIKE LFA1-NAME1,
          NAME2     LIKE LFA1-NAME2,
          NAME3     LIKE LFA1-NAME3,
          NAME4     LIKE LFA1-NAME4,
        END OF TEMPFILE.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
       S_LIFNR          FOR LFB1-LIFNR.
SELECTION-SCREEN END OF BLOCK BOX1.
PARAMETERS: CHECK1   AS CHECKBOX.

INCLUDE <ICON>.
AT SELECTION-SCREEN.
PERFORM GET_FILE_VENDOR.

IF CHECK1 NE 'X'.
    PERFORM OPEN_OUTPUT_FILE.
ENDIF.
************************************************************************
TOP-OF-PAGE.
IF CHECK1 EQ 'X'.
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

WRITE: /1  TEXT-006, 20 TEXT-010, 30 TEXT-006, 60 TEXT-006.

WRITE: /1  TEXT-005, 30 TEXT-005, 60 TEXT-009.
ULINE.
ENDIF.
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
*ELSEIF TEMPTYPE EQ 'CN'.
*     MOVE 'INGT' TO TEMPSRCE.
*ELSEIF TEMPTYPE EQ 'R2'.
*     MOVE 'R2  ' TO TEMPSRCE.
ENDIF.

* This statement will be executed if the first 5 spots have a value and
* the last 2 spots in the Old Vendor Number field are blank.  This
* indicates that the Old Vendor Number has been manual entered.
   IF LFB1-ALTKN+8(2) EQ SPACE AND LFB1-ALTKN(5) NE SPACE.
       IF CHECK1 EQ 'X'.
          SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
          WRITE: /1 LFB1-LIFNR.
          WRITE: 30 LFB1-ALTKN, 60 LFA1-NAME1.
          WRITE: 96 LFA1-NAME2, 132 LFA1-NAME3, 168 LFA1-NAME4.
       ELSEIF CHECK1 EQ SPACE.
          SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
*          MOVE TEMPVENDOR-SOURCE  TO  TEMPFILE-SOURCE.
          MOVE LFB1-ALTKN         TO  TEMPFILE-OLDACCT.
          MOVE LFB1-LIFNR         TO  TEMPFILE-LIFNR.
          MOVE LFA1-NAME1         TO  TEMPFILE-NAME1.
          MOVE LFA1-NAME2         TO  TEMPFILE-NAME2.
          MOVE LFA1-NAME3         TO  TEMPFILE-NAME3.
          MOVE LFA1-NAME4         TO  TEMPFILE-NAME4.
          TRANSFER TEMPFILE TO PHYFILE LENGTH 164.
          CLEAR TEMPFILE.
       ENDIF.
   ENDIF.

IF TEMPTYPE EQ 'AP' OR TEMPTYPE EQ 'IM'.
   LOOP AT TEMPVENDOR.
* This IF statement will be executed if the Old Vendor Number field has
* been entered with the first five spots and the last two spots.
     IF TEMPVENDOR-SEQNO EQ TEMPSEQNO AND TEMPVENDOR-SOURCE EQ TEMPSRCE.
       IF CHECK1 EQ 'X'.
          SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
          WRITE: /1 LFB1-LIFNR, 20 TEMPVENDOR-SOURCE.
          WRITE: 30 TEMPVENDOR-OLDACCT, 60 LFA1-NAME1.
          WRITE: 96 LFA1-NAME2, 132 LFA1-NAME3, 168 LFA1-NAME4.
          EXIT.
       ELSEIF CHECK1 EQ SPACE.
          SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
          MOVE TEMPVENDOR-SOURCE  TO  TEMPFILE-SOURCE.
          MOVE TEMPVENDOR-OLDACCT TO  TEMPFILE-OLDACCT.
          MOVE LFB1-LIFNR         TO  TEMPFILE-LIFNR.
          MOVE LFA1-NAME1         TO  TEMPFILE-NAME1.
          MOVE LFA1-NAME2         TO  TEMPFILE-NAME2.
          MOVE LFA1-NAME3         TO  TEMPFILE-NAME3.
          MOVE LFA1-NAME4         TO  TEMPFILE-NAME4.
          TRANSFER TEMPFILE TO PHYFILE LENGTH 164.
          CLEAR TEMPFILE.
          EXIT.
       ENDIF.
     ENDIF.
   ENDLOOP.
ENDIF.
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
    MESSAGE E006 WITH VENFILE.
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

*-----------------------------------------------------------------------
*   FORM OPEN_OUTPUT_FILE.
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = EVANFILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH EVANFILE.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.
ENDFORM.
