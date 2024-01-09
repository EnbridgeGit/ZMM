REPORT ZMMMI021 NO STANDARD PAGE HEADING LINE-SIZE 100 LINE-COUNT 60.

*----------------------------------------------------------------------*
*       REPORT ZMMMI021.                                               *
*       AUTHOR M. Khan                                                 *
*       DATE   March, 2003.                                            *
*----------------------------------------------------------------------*
*
*   The purpose of this program is to release the blocked invoices
*
*----------------------------------------------------------------------*

TABLES: RM08X.

DATA: PBUKRS LIKE RM08X-BUKRS.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

**------------------------ Selection Screen  ---------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: SBUKRS  FOR RM08X-BUKRS OBLIGATORY
                         NO INTERVALS DEFAULT 'UGL'.
*PARAMETERS: PBUKRS LIKE RM08X-BUKRS DEFAULT 'UGL' OBLIGATORY,
PARAMETERS: PGJAHR LIKE RM08X-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.

SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
     PERFORM OPEN_BDC.
     LOOP AT SBUKRS.
          MOVE SBUKRS-LOW TO PBUKRS.
          PERFORM BUILD_BDC_TABLE.
          CLEAR PBUKRS.
     ENDLOOP.
     PERFORM CLOSE_BDC.

************************************************************************
*                      FORM   BUILD_BDC_TABLE                          *
************************************************************************
FORM BUILD_BDC_TABLE.
    REFRESH BDCDATA.

    PERFORM BDC_SCREEN USING 'SAPMM08A'    '0100'.
    PERFORM BDC_FIELD  USING 'RM08X-BUKRS'  PBUKRS.
    PERFORM BDC_FIELD  USING 'RM08X-GJAHR'  PGJAHR.
    PERFORM BDC_FIELD  USING 'RM08X-ZLSPR' 'R'.
    PERFORM BDC_FIELD  USING 'RM08X-XASPE' 'X'.
    PERFORM BDC_FIELD  USING 'RM08X-XFREI' 'X'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/00'.        "Press Enter

*    PERFORM BDC_SCREEN USING 'SAPMM08A'    '0120'.
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'PRI'.       "Print
*    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/3'.
*    PERFORM BDC_SCREEN USING 'SAPMM08A'    '0100'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/3'.
    PERFORM INSERT_BDC.

ENDFORM.
*
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

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM Open_BDC                                                 *
*---------------------------------------------------------------------*
FORM Open_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = sy-mandt
            GROUP               = 'ZMM_INV_RELS'
            KEEP                = 'X'
            USER                = sy-uname
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
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Insert_BDC                                               *
*---------------------------------------------------------------------*
FORM Insert_BDC.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'MR02'
       TABLES
            DYNPROTAB      = BDCData
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Close_BDC                                                *
*---------------------------------------------------------------------*
FORM Close_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
ENDFORM.
*---------------------END OF PROGRAM ----------------------------------*

