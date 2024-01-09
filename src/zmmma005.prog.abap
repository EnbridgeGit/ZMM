REPORT ZMMMA005 NO STANDARD PAGE HEADING LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMA005
*    Programmer  :  M De Meester
*    Client      :  Centra Union Gas Limited
*    Date        :  May 23 1997
*
* This program will select all open reservations with movement type 311
* and check that the storage location exists.  If the storage location
* does not exist, the MRP3 view and Storage view would be created via
* a BDC session.
************************************************************************
TABLES:  MARA, MARC, MARD, RESB.


DATA  : BEGIN OF REC_TABLE OCCURS 500,     "Temporary table to store all
            MATNR      LIKE MARA-MATNR,    "records that need to be
            UMWRK      LIKE RESB-UMWRK,    "processed.
            UMLGO      LIKE RESB-UMLGO,
            MTART      LIKE MARA-MTART,
            MBRSH      LIKE MARA-MBRSH,
*           pstat      like mard-pstat,
        END OF REC_TABLE.

DATA  : BEGIN OF BDCDATA OCCURS 100.
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA  : G_TRANSCODE LIKE TSTC-TCODE,
        COUNTER TYPE I VALUE 0,
        SEARCHKEY(26) TYPE C.            "Used to find duplicate records

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_MATNR         FOR   RESB-MATNR,             "material number
     S_WERKS         FOR   RESB-WERKS,             "plant
     S_LGORT         FOR   RESB-LGORT,             "storage location
     S_BWART         FOR   RESB-BWART,             "movement type
     S_MTART         FOR   MARA-MTART.             "material type
SELECTION-SCREEN END OF BLOCK BOX1.

*******************************  MAIN  *********************************
START-OF-SELECTION.
SELECT * FROM RESB
    WHERE MATNR IN S_MATNR           "Material match
      AND UMWRK IN S_WERKS           "Receiving plant
      AND UMLGO IN S_LGORT           "Storage location match
      AND UMLGO <> SPACE             "Storage location is entered
      AND BWART IN S_BWART           "Movement type match
      AND XLOEK <> 'X'               "Item not deleted
      AND KZEAR <> 'X'.              "Item not final issued

    SELECT SINGLE * FROM MARC        "Select only plants with MRP Type
       WHERE MATNR = RESB-MATNR
         AND WERKS = RESB-UMWRK
         AND DISMM <> SPACE.

    IF  SY-SUBRC = 0.

        SELECT * FROM MARD              "Does storage location exist?
           WHERE MATNR = RESB-MATNR
             AND WERKS = RESB-UMWRK
             AND LGORT = RESB-UMLGO.
        ENDSELECT.

        IF SY-SUBRC <> 0.                   "Requires storage location
           SELECT SINGLE * FROM MARA             "BDC only if required
              WHERE MATNR = RESB-MATNR
                AND MTART IN S_MTART
                AND LVORM NE 'X'.

          IF SY-SUBRC = 0.
             SEARCHKEY(18) = RESB-MATNR.
             SEARCHKEY+18(4) = RESB-UMWRK.
             SEARCHKEY+22(4) = RESB-UMLGO.
             READ TABLE REC_TABLE WITH KEY SEARCHKEY
                                           BINARY SEARCH.
             IF SY-SUBRC <> 0.
                MOVE MARA-MTART TO REC_TABLE-MTART.
                MOVE MARA-MATNR TO REC_TABLE-MATNR.
                MOVE MARA-MBRSH TO REC_TABLE-MBRSH.
                MOVE RESB-UMWRK TO REC_TABLE-UMWRK.
                MOVE RESB-UMLGO TO REC_TABLE-UMLGO.
                INSERT REC_TABLE INDEX SY-TABIX.
             ENDIF.
             CLEAR REC_TABLE.
          ENDIF.
       ENDIF.
     ENDIF.
ENDSELECT.

CALL FUNCTION 'BDC_OPEN_GROUP'
     EXPORTING
         CLIENT              = SY-MANDT
         GROUP               = 'MRP3-RESV'
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
           MESSAGE E001 WITH 'MRP3-RESV'.
      ENDIF.

LOOP AT REC_TABLE.
    REFRESH BDCDATA.
    G_TRANSCODE = 'MM01'.
    PERFORM HIBE USING REC_TABLE-MATNR REC_TABLE-UMWRK REC_TABLE-UMLGO
                       REC_TABLE-MBRSH REC_TABLE-MTART.
    PERFORM INSERT_SESSION USING G_TRANSCODE.
ENDLOOP.

CALL FUNCTION 'BDC_CLOSE_GROUP'
     EXCEPTIONS
          NOT_OPEN    = 1
          QUEUE_ERROR = 2
          OTHERS      = 3.
     IF SY-SUBRC NE 0.
          MESSAGE I003 WITH 'MRP3-RESV'.
     ENDIF.

****************************** SUBROUTINES *****************************
FORM HIBE USING MATNR WERKS LGORT MBRSH MTART.
DATA: TEMPMATNR(18) TYPE C.
      TEMPMATNR = MATNR+12(6).

*     perform bdc_screen using 'SAPMM03M' '60'.
*     perform bdc_field  using 'RM03M-MATNR' tempmatnr.
*     perform bdc_field  using 'RM03M-MBRSH' mbrsh.
*     perform bdc_field  using 'RM03M-MTART' mtart.
      PERFORM BDC_SCREEN USING 'SAPLMGMM' '60'.
      PERFORM BDC_FIELD  USING 'RMMG1-MATNR' TEMPMATNR.
      PERFORM BDC_FIELD  USING 'RMMG1-MBRSH' MBRSH.
      PERFORM BDC_FIELD  USING 'RMMG1-MTART' MTART.

* Pop-up views requested
      PERFORM BDC_SCREEN USING 'SAPLMGMM' '0070'.
      IF  MTART = 'HAWA'.
*         perform bdc_field  using 'RMMG1-KZAU1(12)' 'X'.  "MRP3
          PERFORM BDC_FIELD  USING 'MSICHTAUSW-KZSEL(11)' 'X'.
                                                           "Storage Loc
      ELSEIF MTART = 'HIBE'.
*         perform bdc_field  using 'RMMG1-KZAU1(8)'  'X'.  "MRP3
          PERFORM BDC_FIELD  USING 'MSICHTAUSW-KZSEL(8)'  'X'.
                                                           "Storage Loc
      ENDIF.

      PERFORM BDC_SCREEN USING 'SAPLMGMM' '0080'.
      PERFORM BDC_FIELD  USING 'RMMG1-WERKS' WERKS.
      PERFORM BDC_FIELD  USING 'RMMG1-LGORT' LGORT.


*      perform bdc_screen using 'SAPMM03M' '250'.

*      perform bdc_screen using 'SAPMM03M' '270'.
       PERFORM BDC_SCREEN USING 'SAPLMGMM' '3000'.

      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
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
