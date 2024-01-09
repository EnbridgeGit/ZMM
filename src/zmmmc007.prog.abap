REPORT ZMMMC007 NO STANDARD PAGE HEADING LINE-SIZE 125 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMC007
*    PROGRAMMER  :  Nancy Gilligan, OmniLogic
*    Client      :  Union Gas Limited
*    Date        :  October 1998
*
* This program creates a materials movement BDC session to facilitate
* the issue of materials from Union Gas to Union Energy where all
* materials on hand in selected material groups for a selected plant/
* storage location will belong to Union Energy after the separation
* is complete.
*
* For more detailed info on this program, refer to the documentation.
* 98/11/11 MDEMEEST Aded index awtyp = 'MKPF' to BKPF select
* 98/11/24 OMNING get value from MSEG instead of calculating it
************************************************************************

TABLES:  MARA,                          " material master
         MARD,                          " material master: Storage Loc
         MSEG,                          " document document segment
         MAKT,                          " material descriptions
         BKPF,
         MBEW,                          " Material valuation
         T001W,                         " plant description
         PRPS.                          " wbs project

DATA:    QUANT(13)   TYPE C,
         GROUP(12)   TYPE C,
         MTYPE(3)    TYPE C,
         IDX(2)      TYPE N,
         XMATNR(14)  VALUE 'MSEG-MATNR(  )',
         XERFMG(14)  VALUE 'MSEG-ERFMG(  )',
         DATE        LIKE SY-DATUM,
         YEAR(4),
         V_AWKEY     LIKE BKPF-AWKEY,
         LGORT_SUM   LIKE MBEW-VERPR,
         MATKL_SUM   LIKE MBEW-VERPR,
         MAT_GROUP   LIKE MARA-MATKL,
         STORAGE_LOC LIKE MARD-LGORT,
         GRAND_TOT   LIKE MBEW-VERPR,
         PLANT_NAME  LIKE T001W-NAME1.


DATA:    BEGIN OF IT_ISSUE OCCURS 5000,
           WERKS     LIKE MARD-WERKS,                 " Union plant code
           MATKL     LIKE MARA-MATKL,                 " material group
           LGORT     LIKE MARD-LGORT,                 " storage location
           MAT_LGO(13),                " combo mat group and storage loc
           AUFNR     LIKE COBL-AUFNR,                 " internal order
           KOSTL     LIKE COBL-KOSTL,                 " cost centre
           PSPNR     LIKE PRPS-POSID,                 " WBS element
           MATNR     LIKE MARD-MATNR,                 " material number
           MAKTX     LIKE MAKT-MAKTX,                 " material desc.
           ZEILE     LIKE MSEG-ZEILE,                 " material line #
           ERFMG     LIKE MSEG-ERFMG,                 " quantity
           VERPR     LIKE MBEW-VERPR,                 " ave moving price
           WAERS     LIKE MSEG-WAERS,                 " local currency
           MBLNR     LIKE MSEG-MBLNR,                 " material doc
           BELNR     LIKE BKPF-BELNR,                 " accounting doc
         END OF IT_ISSUE.

DATA  : BEGIN OF BDCDATA OCCURS 100.                   "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    PARAMETERS: P_BWART LIKE MSEG-BWART OBLIGATORY  DEFAULT '291'.
    SELECTION-SCREEN SKIP.
    PARAMETERS: P_WERKS LIKE MARD-WERKS OBLIGATORY.
    SELECT-OPTIONS: S_LGORT FOR MARD-LGORT OBLIGATORY.
    SELECTION-SCREEN SKIP.
* test for material number - less docs selected
*   select-options: s_matnr for mara-matnr no intervals.
*                                default '101209'.
    SELECT-OPTIONS: S_MATKL FOR MARA-MATKL.
  SELECTION-SCREEN END OF BLOCK BOX2.
  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-000.
    SELECTION-SCREEN SKIP.
    PARAMETERS: P_AUFNR LIKE COBL-AUFNR.
    PARAMETERS: P_KOSTL LIKE COBL-KOSTL.
    PARAMETERS: P_PSPNR LIKE COBL-PS_PSP_PNR.
  SELECTION-SCREEN END OF BLOCK BOX3.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    PARAMETERS: P_KONTO LIKE MSEGK-KONTO.
  SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.
*   End of selection screen.

* Top Of Page
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,
         97 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,
                                        SY-SYSID.
  WRITE: 40 TEXT-002.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
       SKIP 1.
  ULINE.
  SKIP 1.
  WRITE: 1 TEXT-010, P_WERKS, PLANT_NAME.

     IF NOT P_AUFNR IS INITIAL.
       WRITE:  /1 TEXT-012, P_AUFNR.
     ENDIF.
     IF NOT P_KOSTL IS INITIAL.
       WRITE:  /1 TEXT-022, P_KOSTL.
     ENDIF.
     IF NOT P_PSPNR IS INITIAL.
       WRITE:  /1 TEXT-023, IT_ISSUE-PSPNR.
     ENDIF.
  SKIP 1.
  WRITE: /1 TEXT-015, 20 TEXT-016, 69 TEXT-017, 89 TEXT-018,
         100 TEXT-019, 113 TEXT-020.
  SKIP 1.

* At Selection Screen Processing
AT SELECTION-SCREEN.

IF P_BWART EQ '291' OR P_BWART = '292'.
* go ahead with goods issue.
ELSE.
   MESSAGE E060 WITH TEXT-006.
ENDIF.

* make sure one of p_kostl, p_aufnr or p_pspnr has a value
IF P_AUFNR IS INITIAL AND P_KOSTL IS INITIAL AND P_PSPNR IS INITIAL.
  MESSAGE E060 WITH  TEXT-005.
ENDIF.
* make sure only one of the three has a value
IF NOT P_AUFNR IS INITIAL AND NOT P_KOSTL IS INITIAL.
  MESSAGE E060 WITH  TEXT-024.
ELSEIF NOT P_AUFNR IS INITIAL AND NOT P_PSPNR IS INITIAL.
  MESSAGE E060 WITH  TEXT-024.
ELSEIF NOT P_KOSTL IS INITIAL AND NOT P_PSPNR IS INITIAL.
  MESSAGE E060 WITH  TEXT-024.
ENDIF.
************************************************************************
*   START OF MAIN PROGRAM
************************************************************************
START-OF-SELECTION.

* check that the user has the authority to run this program!!
IF NOT SY-UNAME = 'RAARSSEN'.
   MESSAGE E060 WITH TEXT-007 TEXT-008.
   EXIT.
ENDIF.

* get wbs element number if necessary.
IF NOT P_PSPNR IS INITIAL.
  SELECT SINGLE * FROM PRPS WHERE PSPNR = P_PSPNR.
ENDIF.
* get name of plant
  SELECT SINGLE NAME1 FROM T001W INTO PLANT_NAME WHERE WERKS = P_WERKS.

* use the plant/storage locations
  SELECT * FROM MARD WHERE  WERKS = P_WERKS
                       AND    LGORT IN S_LGORT
                       AND    LABST NE SPACE.
*                      and    matnr in s_matnr.  (for testing)

    SELECT SINGLE * FROM MARA WHERE  MATNR EQ MARD-MATNR
                                AND    MATKL IN S_MATKL.

    IF SY-SUBRC          = 0.
     SELECT SINGLE MAKTX FROM MAKT INTO IT_ISSUE-MAKTX
                                    WHERE MATNR = MARA-MATNR
                                      AND SPRAS = SY-LANGU.
       IT_ISSUE-WERKS = MARD-WERKS.
       IT_ISSUE-LGORT = MARD-LGORT.
       IT_ISSUE-MATKL = MARA-MATKL.
       CONCATENATE MARD-LGORT MARA-MATKL INTO IT_ISSUE-MAT_LGO.
       it_issue-aufnr = p_aufnr.
       it_issue-kostl = p_kostl.
       IT_ISSUE-PSPNR = PRPS-POSID.
       IT_ISSUE-MATNR = MARD-MATNR.
       IT_ISSUE-ERFMG = MARD-LABST.
      APPEND IT_ISSUE.
      CLEAR IT_ISSUE.
    ENDIF.

  ENDSELECT.

* sort the table
  SORT IT_ISSUE BY WERKS LGORT MATNR.

*create the BDC sessions with multiple lines
* for materials (5 per page)
  WRITE: SY-DATUM TO DATE.
  WRITE: SY-DATUM(4) TO YEAR.
  GROUP  = 'ZMM_UGL_291'.
  MTYPE = P_BWART.
  IDX    = 1.
  SET UPDATE TASK LOCAL.

  LOOP AT IT_ISSUE.
      perform bdc_init.
      perform bdc_lines.                       " materials in plant
      perform bdc_save.
  ENDLOOP.



* print a report to show where the materials went
IF SY-SUBRC = 0.
   PERFORM GET_DOC_NUMBERS.   "and print
ENDIF.

END-OF-SELECTION.

*  These sections build the data.  It will transfer quantities
*  out of the SPECIFIED plant.

************************************************************************
*    FORM bdc_INIT                                                     *
************************************************************************
form bdc_init.

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0400'.
  PERFORM BDC_FIELD  USING 'MKPF-BLDAT'     DATE.         " document dte
  PERFORM BDC_FIELD  USING 'MKPF-BUDAT'     DATE.         " posting date
  PERFORM BDC_FIELD  USING 'RM07M-BWARTWA'  MTYPE.        " movement typ
  PERFORM BDC_FIELD  USING 'RM07M-WERKS'  IT_ISSUE-WERKS.  " Union plant
  PERFORM BDC_FIELD  USING 'RM07M-LGORT'  IT_ISSUE-LGORT. " storage loc
  PERFORM BDC_FIELD  USING 'RM07M-WVERS3'   'X'.          " collect slip

   PERFORM BDC_SCREEN USING 'SAPMM07M'       '0421'.


ENDFORM.
************************************************************************
*    FORM bdc_LINES                                                    *
************************************************************************
form bdc_lines.

  QUANT(13) = IT_ISSUE-ERFMG.
  XMATNR+11(2) = IDX.
  XERFMG+11(2) = IDX.



  IF NOT P_KONTO IS INITIAL.
    PERFORM BDC_FIELD  USING 'MSEGK-KONTO'   P_KONTO.    " G/L ACCOUNT
  ENDIF.

  PERFORM BDC_FIELD  USING XMATNR        IT_ISSUE-MATNR.  " material nbr
  PERFORM BDC_FIELD  USING XERFMG             QUANT.          " quantiTY

  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/11'.        " <SAVE>

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
   IF NOT P_KOSTL IS INITIAL.                             " COST CENTRE
    PERFORM BDC_FIELD  USING 'COBL-KOSTL'  IT_ISSUE-KOSTL.
   ENDIF.
   IF NOT P_PSPNR IS INITIAL.                            " WBS ELEMENT
     PERFORM BDC_FIELD  USING 'COBL-PS_PSP_PNR' IT_ISSUE-PSPNR.
   ENDIF.
   IF NOT P_AUFNR IS INITIAL.                            " ORDER #
     PERFORM BDC_FIELD  USING 'COBL-AUFNR'    IT_ISSUE-AUFNR.
   ENDIF.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>


*get material line number
IT_ISSUE-ZEILE = IDX.
MODIFY IT_ISSUE.

ENDFORM.


************************************************************************
*    FORM bdc_SAVE                                                     *
************************************************************************
form bdc_save.

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.       " handle msg

   PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

   PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.

   PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>


*run batch session now so we can get the material document number
* that is generated by the system.

CALL TRANSACTION 'MB1A' USING BDCDATA MODE 'E' UPDATE 'S'.
IF SY-SUBRC = 0.
* get material document number
  GET PARAMETER ID 'MBN' FIELD IT_ISSUE-MBLNR.
ELSE.
  IT_ISSUE-MBLNR = 'ERROR'.
ENDIF.
  MODIFY IT_ISSUE.

  REFRESH BDCDATA.
  CLEAR BDCDATA.


ENDFORM.


* This routine adds an entry to the table BDCDATA with screen
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  PROGRAM - Program name of the screen
*      DNYPRO  - Screen number

************************************************************************
*    FORM BDC_SCREEN                                                   *
************************************************************************
FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

* This routine adds an entry to the table BDCDATA with field
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  fnam - name of the field on the screen
*      fval - value to be entered for that field on the screen.

************************************************************************
*    FORM BDC_FIELD                                                    *
************************************************************************
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.


************************************************************************
*      Form  GET_DOC_NUMBERS                                           *
************************************************************************
FORM GET_DOC_NUMBERS.

SORT IT_ISSUE BY MATKL LGORT MATNR.

LOOP AT IT_ISSUE.
  CONCATENATE IT_ISSUE-MBLNR YEAR INTO V_AWKEY.

* get amount in local currency from the material document instead of
* calculating it - Nancy Gilligan  November 25, 1998   D30K906463
  SELECT SINGLE BUKRS DMBTR FROM MSEG INTO (MSEG-BUKRS, IT_ISSUE-VERPR)
                      WHERE MBLNR = IT_ISSUE-MBLNR
                        AND MJAHR = YEAR
                        AND ZEILE = IT_ISSUE-ZEILE.

       SELECT SINGLE BELNR FROM BKPF INTO IT_ISSUE-BELNR
                      WHERE AWTYP = 'MKPF'                     "mdemeest
                        AND AWKEY = V_AWKEY
                        AND BUKRS = MSEG-BUKRS
                        AND GJAHR = YEAR.

  SELECT SINGLE VERPR FROM MBEW INTO MBEW-VERPR
                       WHERE MATNR = IT_ISSUE-MATNR
                         AND BWKEY = IT_ISSUE-WERKS
                         AND BWTAR = SPACE.
*it_issue-verpr = it_issue-erfmg * mbew-verpr.  (see above  D30K906463)
        MODIFY IT_ISSUE.
*** print_report.
  STORAGE_LOC = IT_ISSUE-LGORT.
  AT NEW MATKL.
    SKIP.
    FORMAT INTENSIFIED ON.
    WRITE: /1 TEXT-021, IT_ISSUE-MATKL.
*   write: /1 text-011, storage_loc.
    FORMAT INTENSIFIED OFF.
  ENDAT.
  AT NEW MAT_LGO.
    FORMAT INTENSIFIED ON.
    WRITE: /1 TEXT-011, IT_ISSUE-LGORT.
    FORMAT INTENSIFIED OFF.
  ENDAT.

  FORMAT INTENSIFIED OFF.
  WRITE:/ IT_ISSUE-MATNR, IT_ISSUE-MAKTX, IT_ISSUE-ERFMG,
         IT_ISSUE-VERPR, IT_ISSUE-WAERS, 101 IT_ISSUE-MBLNR,
         114 IT_ISSUE-BELNR.

     LGORT_SUM = LGORT_SUM + IT_ISSUE-VERPR.
     MATKL_SUM = MATKL_SUM + IT_ISSUE-VERPR.
     GRAND_TOT = GRAND_TOT + IT_ISSUE-VERPR.
     MAT_GROUP = IT_ISSUE-MATKL.

  AT END OF MAT_LGO.
     FORMAT INTENSIFIED ON.
     WRITE: /15 TEXT-003, MAT_GROUP, TEXT-014, IT_ISSUE-LGORT,
               LGORT_SUM UNDER IT_ISSUE-VERPR.
     SKIP.
     CLEAR LGORT_SUM.
  ENDAT.

  AT END OF MATKL.
     FORMAT INTENSIFIED ON.
     SKIP.
     WRITE: /15 TEXT-009, MAT_GROUP,
               MATKL_SUM UNDER IT_ISSUE-VERPR.
     SKIP 1.
     CLEAR MATKL_SUM.
      CLEAR LGORT_SUM.
     FORMAT INTENSIFIED OFF.
  ENDAT.

  AT LAST.
     SUM.
     FORMAT INTENSIFIED ON.
       WRITE: /15 TEXT-004, IT_ISSUE-VERPR UNDER IT_ISSUE-VERPR.
  ENDAT.
*lear it_issue.
ENDLOOP.


ENDFORM.                    " GET_DOC_NUMBERS



************************************************************************
*                            END                                       *
************************************************************************
