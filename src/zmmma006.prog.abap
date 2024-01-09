REPORT ZMMMA006 NO STANDARD PAGE HEADING LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMA006
*    Programmer  :  M De Meester
*    Client      :  Centra Union Gas Limited
*    Date        :  June 1997
*
* This program will look at all materials in a plant and create a BDC
* session to flag all plant records for deletion that do not have an
* MRP TYPE. (ie. VB,PD,VM,ND). This program should be used with care.
* 98/06/24 Janet Reid-renaming bdc session
************************************************************************
TABLES: MARA, MARC.
DATA:
     BEGIN OF REC_TABLE OCCURS 10000,      "Temporary table to store all
        MATNR      LIKE MARC-MATNR,        "records that need to be
        WERKS      LIKE MARC-WERKS,        "processed.
     END OF REC_TABLE.

DATA  : BEGIN OF BDCDATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA  : G_TRANSCODE    LIKE TSTC-TCODE     VALUE   'MM06'.
*        blank(4)       type c value '    ',
*        counter        type i value 0,
*        searchkey(22)  type c.          "Used to find duplicate records

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:
     P_WERKS         LIKE MARC-WERKS,         "Plant
     P_MATKL         LIKE MARA-MATKL.         "Material Group
SELECTION-SCREEN END OF BLOCK BOX1.

*******************************  MAIN  *********************************
START-OF-SELECTION.
PERFORM OPEN_BDC.

SELECT * FROM MARA                      "Material with material group
   WHERE MATKL = P_MATKL
     AND LVORM NE 'X'.

   SELECT * FROM MARC                   "Select Plant Records
      WHERE MATNR = MARA-MATNR
        AND WERKS = P_WERKS
        AND DISMM = SPACE
        AND LVORM NE 'X'.


    MOVE MARA-MATNR  TO  REC_TABLE-MATNR.
    MOVE MARC-WERKS  TO  REC_TABLE-WERKS.
    APPEND REC_TABLE.
    CLEAR  REC_TABLE.
*   write: marc-werks, marc-matnr, marc-dismm, mara-matkl.

    ENDSELECT.
ENDSELECT.

LOOP AT REC_TABLE.
    REFRESH BDCDATA.
    G_TRANSCODE = 'MM06'.
    PERFORM UPDATE_SES USING REC_TABLE-MATNR REC_TABLE-WERKS.
    PERFORM INSERT_SESSION USING G_TRANSCODE.
 ENDLOOP.

PERFORM CLOSE_BDC.

**************************** SUB ROUTINES ******************************
*---------------------------------------------------------------------*
*       FORM OPEN_BDC                                                 *
*---------------------------------------------------------------------*
*       This routine will attempt to open the BDC session             *
*---------------------------------------------------------------------*
FORM OPEN_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
           CLIENT              = SY-MANDT
           GROUP               = 'ZMM_ZMMMA006'
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
    MESSAGE E001 WITH 'ZMMMA006'.
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
    MESSAGE I003 WITH 'ZMMMA006'.
  ENDIF.
ENDFORM.
****************************** SUBROUTINES *****************************
FORM UPDATE_SES USING MATNR WERKS.
   DATA: TEMPMATNR(18) TYPE C.
   TEMPMATNR = MATNR+12(6).

   PERFORM BDC_SCREEN USING 'SAPMM03G' '100'.
   PERFORM BDC_FIELD  USING 'RM03G-MATNR' TEMPMATNR.
   PERFORM BDC_FIELD  USING 'RM03G-werks' WERKS.
*   perform bdc_field  using 'BDC_OKCODE' '/5'.               "Enter

   PERFORM BDC_SCREEN USING 'SAPMM03G' '111'.
   PERFORM BDC_FIELD  USING 'RM03G-LVOWK' 'X'.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.              "Save
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
     MESSAGE I002 WITH REC_TABLE-MATNR.
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
