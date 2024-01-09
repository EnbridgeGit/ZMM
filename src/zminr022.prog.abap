REPORT ZMINR022 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 120
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMINR022
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 23 1996
*
*  This report is used for an audit of the Source List Creation Load.
*
************************************************************************
TABLES: MARA, MARC, LFB1, EORD.

DATA      : VENDDATA    LIKE FILENAMECI-FILEEXTERN,
            TEMPALTKN   LIKE LFB1-ALTKN,
            TEMPTYPE(2) TYPE C,
            TEMPSRCE(4) TYPE C.

PARAMETERS: VENFILE  LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMA002_01'.

DATA:   BEGIN OF TABLE1 OCCURS 100000,
          BISMT(8),      "LIKE MARA-BISMT, Old Material Number
          ALTKN(5),      "LIKE LFB1-ALTKN, Previous vendor number
          OLDACCT(10)     TYPE C,
          WERKS(4),      "LIKE MARC-WERKS, Plant
          NOTKZ(1),      "LIKE EORD-NOTKZ, Block Source Supply
          MATNR      LIKE MARA-MATNR,     "Current Material Number
          LIFNR      LIKE EORD-LIFNR,     "Current Vendor Number
        END OF TABLE1.

DATA:   BEGIN OF TEMPVENDOR OCCURS 25000,
          SOURCE(4)        TYPE C,
          OLDACCT(10)       TYPE C,
          VENNAME(35)      TYPE C,
          LOADSW(1)        TYPE C,
          DUPLDW(1)        TYPE C,
          SEQNO(5)         TYPE C,
        END OF TEMPVENDOR.

INCLUDE <ICON>.
AT SELECTION-SCREEN.
PERFORM GET_FILE_VENDOR.
************************************************************************
TOP-OF-PAGE.

WRITE: / ICON_DATE AS ICON.
WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 85 TEXT-002 COLOR COL_HEADING.
WRITE: 112 SY-REPID COLOR COL_NEGATIVE.

WRITE: / ICON_TIME AS ICON.
WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE: 83(44).
WRITE: 112 'PAGE:' INTENSIFIED OFF.
WRITE: 118(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: /.
FORMAT COLOR COL_NORMAL.
ULINE.
WRITE: /1  TEXT-005, 15 TEXT-008, 40 TEXT-005, 55 TEXT-008.

WRITE: /1  TEXT-006, 15 TEXT-006, 40 TEXT-009, 55 TEXT-009, 80 TEXT-010.

WRITE: /1  TEXT-007, 15 TEXT-007, 40 TEXT-007, 55 TEXT-007.
ULINE.

**************************** MAIN  PROGRAM *****************************
START-OF-SELECTION.

SELECT * FROM EORD WHERE NOTKZ EQ 'X'.
   SELECT * FROM MARA WHERE MATNR EQ EORD-MATNR
                        AND LVORM NE 'X'.
        MOVE MARA-BISMT(8)  TO TABLE1-BISMT.
   ENDSELECT.

   SELECT * FROM LFB1 WHERE LIFNR EQ EORD-LIFNR.
      MOVE LFB1-ALTKN(5)      TO TEMPALTKN.
      MOVE LFB1-ALTKN+8(2)    TO TEMPTYPE.

        IF TEMPTYPE EQ 'IM'.
           MOVE 'IMMS' TO TEMPSRCE.
        ELSEIF TEMPTYPE EQ 'AP'.
           MOVE 'AP  ' TO TEMPSRCE.
        ELSEIF TEMPTYPE EQ 'CN'.
           MOVE 'INGT' TO TEMPSRCE.
        ELSEIF TEMPTYPE EQ 'R2'.
           MOVE 'R2  ' TO TEMPSRCE.
        ENDIF.

         LOOP AT TEMPVENDOR WHERE SEQNO EQ TEMPALTKN
                              AND SOURCE EQ TEMPSRCE.
            MOVE TEMPVENDOR-OLDACCT  TO  TABLE1-OLDACCT.
         ENDLOOP.
   ENDSELECT.

        MOVE EORD-LIFNR     TO TABLE1-LIFNR.
        MOVE EORD-MATNR     TO TABLE1-MATNR.
        MOVE EORD-WERKS     TO TABLE1-WERKS.

APPEND TABLE1.
CLEAR  TABLE1.
CLEAR  TEMPALTKN.
ENDSELECT.

LOOP AT TABLE1.
  WRITE: /1 TABLE1-MATNR, 15 TABLE1-BISMT, 40 TABLE1-LIFNR.
  WRITE: 55 TABLE1-OLDACCT, 80 TABLE1-WERKS.
ENDLOOP.
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

SORT TEMPVENDOR BY SEQNO ASCENDING.
ENDFORM.
