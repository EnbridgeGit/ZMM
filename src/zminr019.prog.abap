REPORT ZMINR019 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 255
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMINR019
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 10 1996.
*
* This ABAP will allow the user to enter a new vendor number and
* then create a report or file displaying OLD Vendor Number,
* NEW Vendor Number and Vendor Name.
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
SELECTION-SCREEN COMMENT 1(79) TEXT-200.
SELECT-OPTIONS:
       S_LIFNR          FOR LFB1-LIFNR,    " VENDOR NUMEBR
       S_KONZS          FOR LFA1-KONZS.    " MATERIAL GROUP
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
WRITE: /1  TEXT-007, 30 TEXT-008.

WRITE: /1  TEXT-006, 12 TEXT-010, 30 TEXT-006, 60 TEXT-006.

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

* If the Old Vendor Number is blank then the following if statement will
* be executed.
   IF LFB1-ALTKN EQ SPACE.
   SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
          IF LFA1-KONZS IN S_KONZS.
             MOVE '         '  TO TEMPFILE-OLDACCT.
             MOVE LFB1-LIFNR   TO TEMPFILE-LIFNR.
             MOVE LFA1-NAME1   TO TEMPFILE-NAME1.
             MOVE LFA1-NAME2   TO TEMPFILE-NAME2.
             MOVE LFA1-NAME3   TO TEMPFILE-NAME3.
             MOVE LFA1-NAME4   TO TEMPFILE-NAME4.
             APPEND TEMPFILE.
             CLEAR  TEMPFILE.
          ENDIF.
   ENDIF.

* This statement will be executed if the first 5 spots have a value and
* the last 2 spots in the Old Vendor Number field are blank.  This
* indicates that the Old Vendor Number has been manual entered.
   IF LFB1-ALTKN+8(2) EQ SPACE AND LFB1-ALTKN(5) NE SPACE.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
        IF LFA1-KONZS IN S_KONZS.
            MOVE LFB1-ALTKN   TO TEMPFILE-OLDACCT.
*           MOVE '**********' TO TEMPFILE-OLDACCT.
            MOVE LFB1-LIFNR   TO TEMPFILE-LIFNR.
            MOVE LFA1-NAME1   TO TEMPFILE-NAME1.
            MOVE LFA1-NAME2   TO TEMPFILE-NAME2.
            MOVE LFA1-NAME3   TO TEMPFILE-NAME3.
            MOVE LFA1-NAME4   TO TEMPFILE-NAME4.
            APPEND TEMPFILE.
            CLEAR  TEMPFILE.
         ENDIF.
   ENDIF.

* This IF statement will be executed if the Old Vendor Number field has
* been entered with the first five spots and the last two spots.
IF TEMPTYPE EQ 'AP' OR TEMPTYPE EQ 'IM' OR TEMPTYPE EQ 'CN'
                    OR TEMPTYPE EQ 'R2'.
   LOOP AT TEMPVENDOR.
     IF TEMPVENDOR-SEQNO EQ TEMPSEQNO AND TEMPVENDOR-SOURCE EQ TEMPSRCE.
          SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFB1-LIFNR.
          IF LFA1-KONZS IN S_KONZS.
             MOVE TEMPVENDOR-OLDACCT TO TEMPFILE-OLDACCT.
             MOVE TEMPVENDOR-SOURCE  TO TEMPFILE-SOURCE.
             MOVE LFB1-LIFNR   TO TEMPFILE-LIFNR.
             MOVE LFA1-NAME1   TO TEMPFILE-NAME1.
             MOVE LFA1-NAME2   TO TEMPFILE-NAME2.
             MOVE LFA1-NAME3   TO TEMPFILE-NAME3.
             MOVE LFA1-NAME4   TO TEMPFILE-NAME4.

             APPEND TEMPFILE.
             CLEAR  TEMPFILE.
             EXIT.
          ENDIF.
          EXIT.
     ENDIF.
   ENDLOOP.
ENDIF.
ENDSELECT.

SORT TEMPFILE BY OLDACCT ASCENDING
                 SOURCE  ASCENDING
                 LIFNR   ASCENDING.

LOOP AT TEMPFILE.
WRITE: /1 TEMPFILE-OLDACCT, 12 TEMPFILE-SOURCE, 30 TEMPFILE-LIFNR.
WRITE: 60 TEMPFILE-NAME1, 96 TEMPFILE-NAME2, 132 TEMPFILE-NAME3.
WRITE: 168 TEMPFILE-NAME4.
ENDLOOP.

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
