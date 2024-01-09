REPORT ZMPUA001 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 80
                MESSAGE-ID ZP.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        February 1997
*  Description:
*     - This program looks at all outstanding Purchase Requistions,
*       selecting those that were generated by MRP and creates a BDC
*       session to post the Storage Location from the Reservation to
*       the Purchase Requistion.
************************************************************************
* 98/06/24 Janet Reid-renaming bdc session
* 97/11/10 md7140 #282 Eliminate purchase reqs flagged for deletion
* 97/06/27 md7140 eliminate duplicate updates for purch req.           *
* 97/05/08 added MATERIAL GROUP (matkl) selection criteria.  only 0300 *
*          material group should process thru this program             *
************************************************************************

TABLES: EBAN, MARA, MARC, RESB.

DATA:  OUTFILE(60).                                 "Physical File Name
DATA:  DELNR        LIKE MDME-DELNR.
DATA:  DELPS        LIKE MDME-DELPS.

DATA: BEGIN OF ZMDPS   OCCURS 50.
        INCLUDE STRUCTURE MDPS.
DATA: END OF ZMDPS.
DATA:  SESSION      LIKE BMSEG-MAPPE VALUE 'ZIM_RESPUREQ'.

DATA: BEGIN OF ZMDRQX OCCURS 50.                     "Pegging Info
      INCLUDE STRUCTURE MDZU.
DATA: END OF ZMDRQX.

DATA: BEGIN OF BDCDATA OCCURS 100.                   "BDC Structure
      INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: BEGIN OF REC_TABLE OCCURS 0,
      BANFN LIKE EBAN-BANFN,
      BNFPO LIKE EBAN-BNFPO,
      LGORT LIKE RESB-LGORT,
      END OF REC_TABLE.

DATA: SEARCHKEY(15) TYPE C.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS: P_PLANT LIKE MARC-WERKS          DEFAULT 'P106',
            P_MATKL LIKE MARA-MATKL          DEFAULT '0300',
            P_USER  LIKE APQI-USERID         DEFAULT 'BATCH',
            P_XKEEP AS CHECKBOX              DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK BOX.

* End of selection screen
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.

   PERFORM OPEN_BATCH_SESSION.

START-OF-SELECTION.
 SELECT * FROM EBAN                      "Select records from PurReq.
   WHERE WERKS = P_PLANT
     AND STATU = 'N'
     AND ESTKZ = 'B'
     AND LOEKZ NE 'X'                                          "#282
     AND LGORT = SPACE.
     MOVE EBAN-BANFN      TO DELNR.
     MOVE EBAN-BNFPO      TO DELPS.

     SELECT SINGLE * FROM MARA                 "Only 0300 material group
       WHERE MATNR = EBAN-MATNR
         AND MATKL = P_MATKL.

     IF SY-SUBRC = '0'.
        SELECT SINGLE * FROM MARC              "Only 'PD' materials
          WHERE MATNR = EBAN-MATNR
            AND WERKS = EBAN-WERKS.

        IF SY-SUBRC = '0'.
           IF MARC-DISMM = 'PD'.

              CALL FUNCTION 'Z_MD_PEGGING'
                EXPORTING EMATNR = EBAN-MATNR
                          EWERKS = EBAN-WERKS
                          EPLSCN = '000'
                          EDELNR = DELNR
                          EDELPS = DELPS
                          EDELKZ = 'BA'
                          EDELET = '0000'
                 TABLES   ZMDRQX = ZMDRQX
                          EMDPSX = ZMDPS
                 EXCEPTIONS ERROR = 01
                          NO_REQUIREMENTS_FOUND = 02
                          ORDER_NOT_FOUND = 03.

               LOOP AT ZMDRQX WHERE MATNR = EBAN-MATNR.

                 SELECT * FROM RESB
                   WHERE RSNUM = ZMDRQX-DELNR
                     AND MATNR = EBAN-MATNR
                     AND WERKS = EBAN-WERKS
                     AND BDART = 'MR'
                     AND LGORT <> SPACE.                      "97/05/08

                    SEARCHKEY(10) = EBAN-BANFN.
                    SEARCHKEY+10(5) = EBAN-BNFPO.
                    READ TABLE REC_TABLE WITH KEY SEARCHKEY
                                             BINARY SEARCH.
                    IF SY-SUBRC <> 0.
                       MOVE EBAN-BANFN TO REC_TABLE-BANFN.
                       MOVE EBAN-BNFPO TO REC_TABLE-BNFPO.
                       MOVE RESB-LGORT TO REC_TABLE-LGORT.
                       INSERT REC_TABLE INDEX SY-TABIX.
*                     write: /.
*                     write: / ' added to table'.
                    ENDIF.                      "end of if sy-subrc <> 0

*                    write: / eban-banfn, eban-matnr, eban-werks,
*                             resb-lgort, delps, eban-menge,
*                             zmdrqx-delnr, resb-bdmng.
                  ENDSELECT.                    "end of RESB
                ENDLOOP.                        "end of ZMDRQX
          ENDIF.                                "end of IF DISMM = 'PD'
       ENDIF.
   ENDIF.
 ENDSELECT.

LOOP AT REC_TABLE.                                   "Create BDC session
   PERFORM BUILD_BDC_SESSION USING REC_TABLE-BANFN
                                   REC_TABLE-BNFPO
                                   REC_TABLE-LGORT.
ENDLOOP.

PERFORM CLOSE_BATCH_SESSION.

*--------------------------  SUBROUTINES  ------------------------------

*----------------------  BUILD_BDC_SESSION  ----------------------------
* Creates screens that simulate manual data entry
*-----------------------------------------------------------------------

FORM BUILD_BDC_SESSION USING BANFN BNFPO LGORT.
    PERFORM BDC_SCREEN USING 'SAPMM06B'      '105'.
    PERFORM BDC_FIELD  USING 'EBAN-BANFN'    BANFN.         "PurReq#

    PERFORM BDC_SCREEN USING 'SAPMM06B'      '101'.
    PERFORM BDC_FIELD  USING 'RM06B-BNFPO'   BNFPO.

    PERFORM BDC_SCREEN USING 'SAPMM06B'      '101'.
    PERFORM BDC_FIELD  USING 'EBAN-LGORT(1)' LGORT.          "StorageLoc
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/11'.          "Save
    PERFORM INSERT_SESSION.
    REFRESH BDCDATA.                                "Clears entire table
ENDFORM.

*------------------------  BDC_SCREEN  ---------------------------------
* This routine adds an entry to the table BDCDATA with screen info.
* This is used as part of the process for creating data for batch input.
*
* Parameters:
*   PROGRAM - Program name of the screen
*   DYNPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
    CLEAR BDCDATA.
    BDCDATA-PROGRAM = PROGRAM.
    BDCDATA-DYNPRO  = DYNPRO.
    BDCDATA-DYNBEGIN = 'X'.
    APPEND BDCDATA.
ENDFORM.

*----------------------  BDC_FIELD  ------------------------------------
* This routine adds an entry to the table BDCDATA with field information
* This is used as part of the process for creating data for batch input.
*
* Parameters:
*   FNAM - name of the field on the screen
*   FVAL - value to be entered for that field on the screen
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
    CHECK FVAL <> SPACE.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.

*-----------------------  INSERT_SESSION  ------------------------------
* This routine inserts the BDC data for one transaction into the batch
* input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.
    CALL FUNCTION 'BDC_INSERT'
        EXPORTING
            TCODE         = 'ME52'                       "Change PurReq
        TABLES
            DYNPROTAB     = BDCDATA
        EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
    IF  SY-SUBRC <> 0.
        WRITE: / 'Error inserting data into session.'.
    ENDIF.
ENDFORM.

*---------------------  OPEN_BATCH_SESSION  ----------------------------
* This routine simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
    CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
           CLIENT              = SY-MANDT
           GROUP               = SESSION
*          holddate            = p_start
           KEEP                = P_XKEEP
           USER                = P_USER                         "BATCH"
       EXCEPTIONS
           CLIENT_INVALID      = 1
           DESTINATION_INVALID = 2
           GROUP_INVALID       = 3
           GROUP_IS_LOCKED     = 4
           HOLDDATE_INVALID    = 5
           INTERNAL_ERROR      = 6
           QUEUE_ERRORID       = 7
           RUNNING             = 8
           SYSTEM_LOCK_ERROR   = 9
           USER_INVALID        = 10.
    IF  SY-SUBRC <> 0.
        MESSAGE E004(ZS) WITH SESSION  SY-SUBRC.
    ENDIF.
ENDFORM.

*--------------------  CLOSE_BATCH_SESSION  ----------------------------
* This routine simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_BATCH_SESSION.
    CALL FUNCTION 'BDC_CLOSE_GROUP'
        EXCEPTIONS
            NOT_OPEN        = 1
            QUEUE_ERROR     = 2
            OTHERS          = 3.

    IF SY-SUBRC = 0.
       MESSAGE I003(ZS) WITH SESSION.
    ELSE.
       MESSAGE E027(ZS) WITH SY-SUBRC.
    ENDIF.

ENDFORM.