REPORT ZMMMA004 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMA004
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 19 1996

*    This ABAP will mark the selected info records entered at the
*    selection screen for deletion.  This program should be only
*    used once, any further use of this program after the
*    initial use is DANGEROUS.
* 98/06/24 Janet Reid-renaming bdc session
************************************************************************
TABLES: MARA, MAKT, EINA, EINE. ", EORD, KONP, A018.

DATA  : G_TRANSCODE    LIKE TSTC-TCODE VALUE 'ME15'.

DATA  : BEGIN OF BDCDATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA   : BEGIN OF TABLE1 OCCURS 100000,
          INFNR     LIKE EINA-INFNR,
          MATNR     LIKE MARA-MATNR,
          MATKL     LIKE MARA-MATKL,
          LIFNR     LIKE EINA-LIFNR,
        END OF TABLE1.

DATA    : BEGIN OF TABLE2 OCCURS 100000.
                INCLUDE STRUCTURE TABLE1.
DATA    : END OF TABLE2.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
       S_LIFNR          FOR EINA-LIFNR,
       S_MATKL          FOR EINA-MATKL.
*       S_EKORG          FOR EORD-EKORG.

SELECTION-SCREEN END OF BLOCK BOX1.

INCLUDE <ICON>.
************************************************************************
TOP-OF-PAGE.
WRITE: / ICON_DATE AS ICON.
WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 40 TEXT-002 COLOR COL_HEADING.
WRITE: 71 'PAGE:' INTENSIFIED OFF.
WRITE: 76(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE.

**************************** MAIN  PROGRAM *****************************
START-OF-SELECTION.
SELECT * FROM EINA WHERE LIFNR IN S_LIFNR.

      SELECT SINGLE * FROM MARA WHERE MATNR = EINA-MATNR
                                  AND LVORM NE 'X'
                                  AND MATKL IN S_MATKL.
      IF SY-SUBRC EQ 0.
          SELECT SINGLE * FROM EINE WHERE INFNR = EINA-INFNR
                                      AND EKORG = 'MATL'
                                      AND LOEKZ NE 'X'.
          IF SY-SUBRC EQ 0.
               MOVE EINA-INFNR       TO TABLE1-INFNR.
               MOVE EINA-MATNR       TO TABLE1-MATNR.
               MOVE MARA-MATKL       TO TABLE1-MATKL.
               MOVE EINA-LIFNR       TO TABLE1-LIFNR.

               APPEND TABLE1.
               CLEAR  TABLE1.
*           ELSE.
*               WRITE: /1 EINA-INFNR, 30 EINA-MATNR, 50 MARA-MATKL.
*               WRITE: 70 EINA-LIFNR, 90 'DID NOT FIND MATCH'.
           ENDIF.
       ENDIF.
ENDSELECT.

SORT TABLE1 BY LIFNR ASCENDING
               MATNR ASCENDING
               MATKL ASCENDING.

LOOP AT TABLE1.
SELECT * FROM EINA WHERE LIFNR EQ TABLE1-LIFNR
                     AND MATNR EQ TABLE1-MATNR.

    SELECT SINGLE * FROM EINE WHERE INFNR = EINA-INFNR
                                 AND EKORG = 'MDSE'
                                 AND LOEKZ NE 'X'.
          IF SY-SUBRC EQ 0.
               MOVE TABLE1-INFNR       TO TABLE2-INFNR.
               MOVE TABLE1-MATNR       TO TABLE2-MATNR.
               MOVE TABLE1-MATKL       TO TABLE2-MATKL.
               MOVE TABLE1-LIFNR       TO TABLE2-LIFNR.

               APPEND TABLE2.
               CLEAR  TABLE2.
           ELSE.
               WRITE: /1 TABLE1-INFNR, 30 TABLE1-MATNR, 50 TABLE1-MATKL.
               WRITE: 70 TABLE1-LIFNR, 90 'DID NOT FIND MATCH'.
           ENDIF.
ENDSELECT.
ENDLOOP.


WRITE: /.
WRITE: / 'THE SECTION BELOW WILL BE PROCESSED, THE ABOVE HAVE NO MATCH'.
WRITE: /.
WRITE: /.
LOOP AT TABLE2.
    WRITE: /1 TABLE2-INFNR, 30 TABLE2-MATNR, 50 TABLE2-MATKL.
    WRITE: 70 TABLE2-LIFNR.
ENDLOOP.

************************************************************************
* This part of the program will process the record in TABLE1 for
* deletion through a BDC session.

PERFORM OPEN_BDC.

LOOP AT TABLE2.
  REFRESH BDCDATA.
  DATA: TEMPMATNR(18) TYPE C.
  TEMPMATNR = TABLE2-MATNR+12(6).
  PERFORM BDC_SCREEN USING 'SAPMM06I' '0100'.
  PERFORM BDC_FIELD  USING 'EINA-LIFNR' TABLE2-LIFNR.
  PERFORM BDC_FIELD  USING 'EINA-MATNR' TEMPMATNR.
  PERFORM BDC_FIELD  USING 'EINE-EKORG' 'MATL'.

  PERFORM BDC_SCREEN USING 'SAPMM06I' '0104'.
  PERFORM BDC_FIELD  USING 'EINE-LOEKZ' 'X'.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

  PERFORM INSERT_SESSION USING G_TRANSCODE.

CLEAR TABLE1.
ENDLOOP.

PERFORM CLOSE_BDC.

*---------------------------------------------------------------------*
*       FORM OPEN_BDC                                                 *
*---------------------------------------------------------------------*
*       This routine will attempt to open the BDC session             *
*---------------------------------------------------------------------*
FORM OPEN_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
           CLIENT              = SY-MANDT
           GROUP               = 'ZPU_ZMMMA004'
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
    MESSAGE E001 WITH 'ZMMMA004'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CLOSE_BDC                                                *
*---------------------------------------------------------------------*
*   - This routine will attempt to close the BDC session              *
*---------------------------------------------------------------------*
FORM CLOSE_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
  IF SY-SUBRC NE 0.
    MESSAGE I003 WITH 'ZMMMA004'.
  ENDIF.
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
    MESSAGE I002 WITH TABLE2-MATNR.
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
