REPORT ZMMMA010 NO STANDARD PAGE HEADING LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMA010
*    Programmer  :  M De Meester
*    Client      :  Centra Union Gas Limited
*    Date        :  June 1997
*
* This program will select all info records in a particular material
* group and change the purchasing group to the one entered on the
* variant.  Development Request DRMM0190
************************************************************************
TABLES: MARA, EINA, EINE.

DATA:
     BEGIN OF REC_TABLE OCCURS 10000,      "Temporary table to store all
        MATNR      LIKE EINA-MATNR,
        LIFNR      LIKE EINA-LIFNR,
        EKORG      LIKE EINE-EKORG,
     END OF REC_TABLE.

DATA  : BEGIN OF BDCDATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA  : G_TRANSCODE    LIKE TSTC-TCODE     VALUE   'ME12'.
*        blank(4)       type c value '    ',
*        counter        type i value 0,
*        searchkey(22)  type c.          "Used to find duplicate records

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:
*    p_werks  like marc-werks obligatory,              "Plant
     P_MATKL  LIKE MARA-MATKL OBLIGATORY,              "Material Group
     P_EKGRP  LIKE EINE-EKGRP OBLIGATORY.              "Purchasing Group
SELECT-OPTIONS:
     S_MATNR  FOR MARA-MATNR.                          "Material Number
SELECTION-SCREEN END OF BLOCK BOX1.

*******************************  MAIN  *********************************
START-OF-SELECTION.
PERFORM OPEN_BDC.

SELECT * FROM MARA                     "Select all active materials that
   WHERE MATKL = P_MATKL               "match entered material group
     AND MATNR IN S_MATNR
     AND LVORM NE 'X'.

   SELECT * FROM EINA                  "Select info records
       WHERE MATNR = MARA-MATNR.
       SELECT * FROM EINE
          WHERE INFNR = EINA-INFNR.
            REC_TABLE-MATNR = EINA-MATNR.
            REC_TABLE-LIFNR = EINA-LIFNR.
            REC_TABLE-EKORG = EINE-EKORG.
*           write: / rec_table-matnr, rec_table-lifnr, rec_table-ekorg.
            APPEND REC_TABLE.
            CLEAR REC_TABLE.
       ENDSELECT.
   ENDSELECT.
ENDSELECT.


LOOP AT REC_TABLE.
   REFRESH BDCDATA.
   G_TRANSCODE = 'ME12'.
   PERFORM UPDATE_SES USING REC_TABLE-MATNR
                            REC_TABLE-LIFNR
                            REC_TABLE-EKORG.
   PERFORM INSERT_SESSION USING G_TRANSCODE.
 ENDLOOP.

PERFORM CLOSE_BDC.
WRITE: / SY-REPID, TEXT-007.

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
           GROUP               = 'ZMM_INFOPUR'
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
    MESSAGE E001 WITH 'ZMM_INFOPUR'.
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
    MESSAGE I003 WITH 'ZMM_INFOPUR'.
  ENDIF.
ENDFORM.
****************************** SUBROUTINES *****************************
FORM UPDATE_SES USING MATNR LIFNR EKORG.
   DATA: TEMPMATNR(18) TYPE C.
   TEMPMATNR = MATNR+12(6).

   PERFORM BDC_SCREEN USING 'SAPMM06I' '100'.
   PERFORM BDC_FIELD  USING 'EINA-LIFNR' LIFNR.
   PERFORM BDC_FIELD  USING 'EINA-MATNR' TEMPMATNR.
   PERFORM BDC_FIELD  USING 'EINE-EKORG' EKORG.
*   perform bdc_field  using 'BDC_OKCODE' '/5'.         "Enter

   PERFORM BDC_SCREEN USING 'SAPMM06I' '101'.           "No Data Changes

   PERFORM BDC_SCREEN USING 'SAPMM06I' '102'.
   PERFORM BDC_FIELD  USING 'EINE-EKGRP' P_EKGRP.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.         "Save
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
