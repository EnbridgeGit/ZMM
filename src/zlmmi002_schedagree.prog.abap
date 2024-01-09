REPORT  ZLMMI002_SCHEDAGREE LINE-SIZE 132 MESSAGE-ID ZS
        LINE-COUNT 65 NO STANDARD PAGE HEADING.
*&---------------------------------------------------------------------*
*& Author: Glenn Ymana
*& Date  : March, 2011.
*& Description:
*&
*& This program will select purchasing contracts based on the variant
*& (Typically the contracts have been released but not sent to CARE)
*& A function will be executed that will transmit each record to Biztalk
*& and in turn post to CARE. If successful transmission, a return
*& code '1' is sent and the record is marked as sent in table EKKO.
*& If failed, a return code '0' is sent.
*& A detail report is generated to list all contracts sent and the.
*& results of each transmission.
*&---------------------------------------------------------------------*
*& Changes:
*& 2011/07/06 gymana - Add condition amount check (KONV-KPOSN)
*&---------------------------------------------------------------------*
*2021/03/01 SSRIVAS5 COG Changes to include MSA,Locations data in output
*&---------------------------------------------------------------------*

TABLES: EKKO,
        EKPO,
        LFM1,
        KONV,
        T006A.

* Following Line Added D30K932247
TABLES: ZFIT_XPARAM .           " Parameter Master for ABAP Development

* Following Lines Added D30K932245
TYPES: Begin of ty_trnsfr,
        EBELN    type ebeln,      " 10
        BSART    type ESART,      " 4
        KDATB	   type KDATB,      " 8
        KDATE	   type KDATE,      "	8
        EIKTO	   type EIKTO_M,    "	12
        UOM      type MSEH6,      " 6
        KTMNG	   type KTMNG,      "	15
        BUKRS	   type BUKRS,      "	4
        LOEKZ	   type ELOEK,      "	1
        WERKS	   type WERKS_D,    "	4
        ZZTRLOC1 type ZTRLOCID,   "	5
        ZZTRLOC2 type ZTRLOCID,   "	5
        ZZTRLOC3 type ZTRLOCID,   " 5
        ZZTRLOC4 type ZTRLOCID,   "	5
        ZZPARTY	 type ZPARTY,     " 12
      End of ty_trnsfr .

*DATA: gt_trnsfr type STANDARD TABLE OF ty_trnsfr,
DATA:  gw_trnsfr(155) type c .

DATA: gf_ktmng(16) type c .

DATA: gf_filename TYPE localfile,
      gf_file_open type c .
* End of New Lines      D30K932245

DATA: BEGIN OF ITAB_KONV OCCURS 0,
          KNUMV LIKE KONV-KNUMV,
          KPOSN LIKE KONV-KPOSN,
          KMEIN LIKE KONV-KMEIN,
          WAERS LIKE KONV-WAERS,
      END OF ITAB_KONV.

DATA: BEGIN OF ITAB_PDOC OCCURS 0,
          EBELN LIKE EKKO-EBELN,
*         change fsd05 p2p  plant-new fields
          BUKRS TYPE BUKRS,
*         end of new fields ********
          BSART LIKE EKKO-BSART,
          KDATB LIKE EKKO-KDATB,
          KDATE LIKE EKKO-KDATE,
          WAERS LIKE EKKO-WAERS,
          KNUMV LIKE EKKO-KNUMV,
*        change fsd05 p2p  plant- new fields
          ZZTRLOC1  TYPE ZTRLOCID,
          ZZTRLOC2  TYPE ZTRLOCID,
          ZZTRLOC3  TYPE ZTRLOCID,
          ZZTRLOC4  TYPE ZTRLOCID,
          ZZPARTY   TYPE ZPARTY,
          DAILY_QTY TYPE KTMNG,
          WERKS     TYPE WERKS,
*         End of new fields
          MEINS LIKE EKPO-MEINS,
          BPRME LIKE EKPO-BPRME,
          LOEKZ LIKE EKPO-LOEKZ,
          UOM   LIKE T006A-MSEH6,
          EIKTO LIKE LFM1-EIKTO,

      END OF ITAB_PDOC.

DATA: BEGIN OF ITAB_ERR OCCURS 0,
          MSG(50) TYPE C,
      END OF ITAB_ERR.

DATA: W_RETURNCODE  TYPE I,
      W_MSG(50)     TYPE C,
      LN_CNTR       TYPE I VALUE 99,
      I_COUNT       TYPE I VALUE 0,
      W_ERR_FLAG    TYPE C VALUE 'N',
      W_NODATA_FLAG TYPE C VALUE 'N',
      W_KONV_ERR    TYPE C VALUE 'N'.

CONSTANTS:C_2009 TYPE EKPO-MATKL VALUE '2009'. "COG

* Following Lines Added D30K932245
CONSTANTS: c_pipe TYPE c VALUE '|',
           c_error TYPE c VALUE 'E' .
* End of New Lines      D30K932245

*----------------------------------------------------------------------*
* selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  P_PRDOC  FOR EKKO-EBELN,
                 P_DOCCAT FOR EKKO-BSTYP DEFAULT 'L',
                 P_S_TYPE FOR EKKO-BSART.
PARAMETERS:      P_EKORG  LIKE EKKO-EKORG DEFAULT 'GASA'.
SELECT-OPTIONS:  P_RELDAT FOR SY-DATUM DEFAULT SY-DATUM.
PARAMETERS:      P_RELIND LIKE EKKO-ZZSENT_IND DEFAULT ' '.
SELECTION-SCREEN SKIP.
PARAMETERS:      P_TESTRN(1)   TYPE C DEFAULT 'X',
                 P_TRANS(1)    TYPE C DEFAULT 'N',
                 P_BYPASS(1)   TYPE C DEFAULT 'N'.

SELECTION-SCREEN END OF BLOCK BOX1.

* Following Lines Added D30K932245
SELECTION-SCREEN BEGIN OF BLOCK fpt WITH FRAME TITLE text-tf1 .
PARAMETERS: p_path TYPE FILEEXTERN
  DEFAULT '/usr/sap/interfaces/D30/TEST/' .
SELECTION-SCREEN END OF BLOCK fpt .
* End of New Lines      D30K932245
*----------------------------------------------------------------------*
*                  at selection-screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

* Following Lines Added D30K932247
   select * up to 1 rows
     from ZFIT_XPARAM
    where PARAMTYPE = 'ZLMMI002_SCHEDAGREE'
      and SUBTYPE   = 'FILE_PATH'
      and KEY1      = space
      and KEY2      = space
      and KEY3      = space
      and KEY4      = space
      and KEY5      = space .

    endselect .

  p_path = ZFIT_XPARAM-value1 .
* End of New Lines      D30K932247

  REFRESH P_S_TYPE.
  CLEAR P_S_TYPE.
  MOVE 'E' TO P_S_TYPE-SIGN.
  MOVE 'EQ' TO P_S_TYPE-OPTION.
  MOVE 'ZLOC' TO P_S_TYPE-LOW.
  APPEND P_S_TYPE.
  MOVE 'E' TO P_S_TYPE-SIGN.
  MOVE 'EQ' TO P_S_TYPE-OPTION.
  MOVE 'ZDP' TO P_S_TYPE-LOW.
  APPEND P_S_TYPE.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EKORG'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
*                  start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM GENERATE_REPORT_HEADER.
  PERFORM GET_DATA.

  IF W_NODATA_FLAG = 'N'.
    SORT ITAB_PDOC BY EBELN.

* Following Lines Added D30K932245
*   CONCATENATE: p_path    <== Delete D30K932247
   CONCATENATE:          " <== Insert D30K932247
        ZFIT_XPARAM-value1
             'BODS' syst-datum syst-uzeit '.dat'
        INTO gf_filename .
* End of New Lines      D30K932245

    LOOP AT ITAB_PDOC.

      W_ERR_FLAG = 'N'.

      REFRESH ITAB_ERR.
      CLEAR W_MSG.

      IF ITAB_PDOC-EIKTO = ''.
        W_ERR_FLAG = 'Y'.
        ITAB_ERR-MSG = 'Missing Offered By Party ID.'.
        APPEND ITAB_ERR.
      ENDIF.

      IF P_BYPASS = 'N'.
        SELECT * FROM EKPO
         WHERE EBELN = ITAB_PDOC-EBELN
           AND MATKL NE C_2009.        "COG

          IF SY-SUBRC = 0.
            IF EKPO-LOEKZ = ''.
              PERFORM EDIT_CHECKS.
            ENDIF.
          ENDIF.

        ENDSELECT.
      ENDIF.

      IF W_ERR_FLAG ='N'.

       if P_TESTRN is initial .      " <== Insert D30K932247
* Following Lines Added D30K932245
        if gf_file_open is initial .
          gf_file_open = abap_true .

          OPEN DATASET gf_filename
           FOR OUTPUT
            IN TEXT MODE
            ENCODING DEFAULT .

  CONCATENATE TEXT-021        "SA_NUMBER     "Scheduling
              TEXT-022        "DOC_TYPE      "Document
              TEXT-023        "START_DATE    "Document
              TEXT-024        "END_DATE      "Unit of
              TEXT-025        "OFFER_PARTYID "Doc
              TEXT-026        "UOM           "Offered By
              TEXT-027        "DAILY_QUANTITY"Transmit
              TEXT-028        "COMP_CODE     "Agreement
              TEXT-029        "DELE_FLAG     "Start Date
              TEXT-030        "PLANT         "End Date
              TEXT-031        "ZZTRLOC1      "Measure
              TEXT-032        "ZZTRLOC2      "Type
              TEXT-033        "ZZTRLOC3      "Party ID
              TEXT-034        "ZZTRLOC4      "Return Code
              TEXT-035        "ZZPARTY       "Message
         into gw_trnsfr
    SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB .

          TRANSFER gw_trnsfr TO gf_filename .
        endif .

        clear: gf_ktmng .
        gf_ktmng = ITAB_PDOC-DAILY_QTY .

        CONCATENATE ITAB_PDOC-EBELN ITAB_PDOC-BSART ITAB_PDOC-KDATB
                    ITAB_PDOC-KDATE ITAB_PDOC-EIKTO ITAB_PDOC-UOM
                    gf_ktmng ITAB_PDOC-BUKRS ITAB_PDOC-LOEKZ
                    ITAB_PDOC-WERKS ITAB_PDOC-ZZTRLOC1
                    ITAB_PDOC-ZZTRLOC2 ITAB_PDOC-ZZTRLOC3
                    ITAB_PDOC-ZZTRLOC4 ITAB_PDOC-ZZPARTY
               into gw_trnsfr
          SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB .

        TRANSFER gw_trnsfr TO gf_filename .

*   Mark record as sent in EKKO
*    IF P_TESTRN <> 'X' AND W_RETURNCODE = 1.
      SELECT SINGLE * FROM EKKO
       WHERE EBELN = ITAB_PDOC-EBELN.

      IF SY-SUBRC = 0.
        MOVE 'Y' TO EKKO-ZZSENT_IND.
        UPDATE EKKO.
        COMMIT WORK.
      ENDIF.
*    ENDIF.
* End of New Lines      D30K932245

*        PERFORM CALL_FUNCTION.  <== Delete D30K932245
      ENDIF.
    endif .                        " <== Insert D30K932247

      PERFORM WRITE_REPORT_DETAIL.

    ENDLOOP.

* Following Lines Added D30K932245
  if not gf_file_open is initial .
    clear: gf_file_open .
    CLOSE DATASET gf_filename .
  endif .
* End of New Lines      D30K932245

  ELSE.
    SKIP.
    WRITE: /45    '*** No data to process ***'.
    ADD +2 TO LN_CNTR.
  ENDIF.

*----------------------------------------------------------------------*
*       Form  edit_checks
*----------------------------------------------------------------------*
FORM EDIT_CHECKS.

* UOM / Currency check (2)

  IF ITAB_PDOC-WAERS = 'USD'.
    IF EKPO-MEINS = 'DTH' OR
       EKPO-MEINS = 'DT1' OR
       EKPO-MEINS = 'MMB'.
*      w_err_flag = 'N'.
    ELSE.
      W_ERR_FLAG = 'Y'.
      ITAB_ERR-MSG = 'UoM and Document Currency not consistent,  '.
      APPEND ITAB_ERR.
      ITAB_ERR-MSG = 'please check.'.
      APPEND ITAB_ERR.
    ENDIF.
  ELSEIF ITAB_PDOC-WAERS = 'CAD'.
    IF EKPO-MEINS = 'GJ' OR
       EKPO-MEINS = 'GJ1' OR
       EKPO-MEINS = 'KM3'.
*      w_err_flag = 'N'.
    ELSE.
      W_ERR_FLAG = 'Y'.
      ITAB_ERR-MSG = 'UoM and Document Currency not consistent,  '.
      APPEND ITAB_ERR.
      ITAB_ERR-MSG = 'please check.'.
      APPEND ITAB_ERR.
    ENDIF.
  ENDIF.

* Condition Currency check (3)
  CLEAR ITAB_KONV.
  REFRESH ITAB_KONV.

  SELECT * FROM KONV
   WHERE KNUMV = ITAB_PDOC-KNUMV
     AND KPOSN = EKPO-EBELP
     AND KBETR <> 0
     AND KINAK = SPACE.   "Only active conditions

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING KONV TO ITAB_KONV.
      APPEND ITAB_KONV.
    ENDIF.
  ENDSELECT.

  W_KONV_ERR = 'N'.
  LOOP AT ITAB_KONV.
    IF ITAB_KONV-WAERS <> ''.
      IF ITAB_KONV-WAERS <> ITAB_PDOC-WAERS.
        W_KONV_ERR = 'Y'.
      ENDIF.
    ENDIF.

*   Condition Unit check
    IF ITAB_KONV-KMEIN <> ''.
      IF ITAB_KONV-KMEIN <> EKPO-BPRME.
        W_KONV_ERR = 'Y'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF W_KONV_ERR = 'Y'.
    W_ERR_FLAG = 'Y'.
    ITAB_ERR-MSG = 'Pricing Condition Currency and/or Order Price'.
    APPEND ITAB_ERR.
    ITAB_ERR-MSG = 'Unit not consistent, please check.'.
    APPEND ITAB_ERR.
  ENDIF.

ENDFORM.                    "edit_checks

*----------------------------------------------------------------------*
*       Form  call_function
*----------------------------------------------------------------------*
FORM CALL_FUNCTION.
* Call function to transmit data to Biztalk.
  IF P_TRANS = 'Y'.
*    CALL FUNCTION 'ZLMMI002_GASSUP_SCHAGR' DESTINATION 'Z_BIZTALK'
*      EXPORTING
*        SA_NUMBER      = ITAB_PDOC-EBELN
*        DOC_TYPE       = ITAB_PDOC-BSART
*        START_DATE     = ITAB_PDOC-KDATB
*        END_DATE       = ITAB_PDOC-KDATE
*        OFFER_PARTYID  = ITAB_PDOC-EIKTO
*        UOM            = ITAB_PDOC-UOM
*  Begin of change fsd05 p2p
*        DAILY_QUANTITY = ITAB_PDOC-DAILY_QTY
*        COMP_CODE      = ITAB_PDOC-BUKRS
*        DELE_FLAG      = ITAB_PDOC-LOEKZ
*        PLANT          = ITAB_PDOC-WERKS
*        ZZTRLOC1       = ITAB_PDOC-ZZTRLOC1
*        ZZTRLOC2       = ITAB_PDOC-ZZTRLOC2
*        ZZTRLOC3       = ITAB_PDOC-ZZTRLOC3
*        ZZTRLOC4       = ITAB_PDOC-ZZTRLOC4
*        ZZPARTY        = ITAB_PDOC-ZZPARTY
*  End of change fsd05 p2p
*      IMPORTING
*        RETURNCODE     = W_RETURNCODE.
*
*    WAIT UP TO 3 SECONDS.

*   Return Code 1 = Successful, 0 = failed
    IF W_RETURNCODE = 1.
      W_MSG = 'Transmit Successful'.
    ELSEIF W_RETURNCODE = 0.
      W_MSG = 'Transmit Failed'.
    ENDIF.

*   Mark record as sent in EKKO
* Following Lines Deleted D30K932245
*    IF P_TESTRN <> 'X' AND W_RETURNCODE = 1.
*      SELECT SINGLE * FROM EKKO
*       WHERE EBELN = ITAB_PDOC-EBELN.
*
*      IF SY-SUBRC = 0.
*        MOVE 'Y' TO EKKO-ZZSENT_IND.
*        UPDATE EKKO.
*        COMMIT WORK.
*      ENDIF.
*    ENDIF.
* End of Deletion         D30K932245
  ELSE.
    W_MSG = 'N/A - Test mode Only'.
  ENDIF.

ENDFORM.                    "call_function

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM GET_DATA.

  SELECT * FROM EKKO
    WHERE ZZSENT_IND = P_RELIND
      AND EBELN IN P_PRDOC
      AND BSTYP IN P_DOCCAT
      AND BSART IN P_S_TYPE
      AND EKORG = P_EKORG
      AND AEDAT IN P_RELDAT
      AND FRGKE = 'Y'.

    IF SY-SUBRC = 0.
      PERFORM BUILD_TABLEREC.
    ENDIF.

  ENDSELECT.

* Check if there is data to transmit.
  DESCRIBE TABLE ITAB_PDOC LINES I_COUNT.

  IF I_COUNT = 0.
    W_NODATA_FLAG = 'Y'.
  ENDIF.

ENDFORM.                    "collect_data

*----------------------------------------------------------------------
*                         build_tablerec
*----------------------------------------------------------------------
FORM BUILD_TABLEREC.
* Start of Changes                  COG
  DATA: LV_QTY      TYPE KTMNG,
        LV_FRMDATE  TYPE VTBBEWE-DBERVON,
        LV_TODATE   TYPE VTBBEWE-DBERVON,
        LV_DAYS     TYPE VTBBEWE-ATAGE,
        LV_PARTY    TYPE ZPARTY.
  CLEAR: LV_QTY, LV_FRMDATE,LV_TODATE,LV_DAYS,LV_PARTY.
* End of Changes                  COG

  MOVE-CORRESPONDING EKKO TO ITAB_PDOC.
* Start of Changes                  COG
  IF NOT EKKO-ZZTRLOC1 IS INITIAL.
    SELECT SINGLE ZZTRLOCID INTO ITAB_PDOC-ZZTRLOC1
      FROM ZMMT_LOCMAST WHERE ZZTRLOC = EKKO-ZZTRLOC1.
    IF sy-subrc = 0.

    ENDIF.
*    SELECT SINGLE ZZPARTY INTO LV_PARTY
*      FROM ZMMT_LOCMAST WHERE ZZTRLOC = EKKO-ZZTRLOC1.
*    IF SY-SUBRC = 0.
*      ITAB_PDOC-ZZPARTY = LV_PARTY.
*    ENDIF.
  ENDIF.

  IF NOT EKKO-ZZTRLOC2 IS INITIAL.
    SELECT SINGLE ZZTRLOCID INTO ITAB_PDOC-ZZTRLOC2
      FROM ZMMT_LOCMAST WHERE ZZTRLOC = EKKO-ZZTRLOC2.
    IF sy-subrc = 0.

    ENDIF.
  endif.

  IF NOT EKKO-ZZTRLOC3 IS INITIAL.
    SELECT SINGLE ZZTRLOCID INTO ITAB_PDOC-ZZTRLOC3
      FROM ZMMT_LOCMAST WHERE ZZTRLOC = EKKO-ZZTRLOC3.
    IF sy-subrc = 0.

    ENDIF.
  endif.

  IF NOT EKKO-ZZTRLOC4 IS INITIAL.
    SELECT SINGLE ZZTRLOCID INTO ITAB_PDOC-ZZTRLOC4
      FROM ZMMT_LOCMAST WHERE ZZTRLOC = EKKO-ZZTRLOC4.
    IF sy-subrc = 0.

    ENDIF.
  endif.
* End of Changes                  COG
  SELECT SINGLE * FROM EKPO
    WHERE EBELN = EKKO-EBELN
      AND LOEKZ = ''
      AND MATKL NE C_2009. "COG

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING EKPO TO ITAB_PDOC.
    IF ITAB_PDOC-MEINS = 'MMB'.
      MOVE 'DTH' TO ITAB_PDOC-UOM.
    ELSE.
      SELECT MSEH6 FROM T006A INTO ITAB_PDOC-UOM
       WHERE MSEHI = ITAB_PDOC-MEINS.
      ENDSELECT.
    ENDIF.
*   change fsd05 p2p  plant
    ITAB_PDOC-WERKS = EKPO-WERKS.
  else.
    ITAB_PDOC-loekz = 'Y'.
  ENDIF.

  SELECT EIKTO FROM LFM1 INTO ITAB_PDOC-EIKTO
    WHERE EKORG = EKKO-EKORG
      AND LIFNR = EKKO-LIFNR.
  ENDSELECT.
* Total Quantity
  SELECT SUM( KTMNG ) FROM EKPO INTO LV_QTY
                      WHERE EBELN = EKKO-EBELN
                      AND LOEKZ = ''
                      AND MATKL NE C_2009. "COG
* calculate contract days- change fsd05 p2p  plant
  LV_TODATE = EKKO-KDATE.
  LV_FRMDATE = EKKO-KDATB.
  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      I_DATE_FROM = LV_FRMDATE
      I_DATE_TO   = LV_TODATE
    IMPORTING
      E_DAYS      = LV_DAYS.

  LV_DAYS = LV_DAYS + 1   .
* Daily quantity-change fsd05 p2p  plant
  ITAB_PDOC-DAILY_QTY = LV_QTY / LV_DAYS.

  APPEND ITAB_PDOC.

ENDFORM.                    "build_tablerec

*&---------------------------------------------------------------------*
*&      Form  generate_report_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GENERATE_REPORT_HEADER.
  NEW-PAGE.
  CLEAR LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-002, 35 TEXT-003.
  WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: /50 SY-DATUM.
  WRITE: 121 TEXT-PGE, SY-PAGNO.
  SKIP.
  WRITE:  /3 TEXT-004,        "Scheduling
          17 TEXT-006,        "Document
          30 TEXT-008,        "Document
          43 TEXT-010,        "Unit of
          52 TEXT-012,        "Doc
          58 TEXT-014,        "Offered By
          70 TEXT-016.        "Transmit
  WRITE:  /3 TEXT-005,        "Agreement
          17 TEXT-007,        "Start Date
          30 TEXT-009,        "End Date
          43 TEXT-011,        "Measure
          52 TEXT-013,        "Type
          58 TEXT-015,        "Party ID
          70 TEXT-017,        "Return Code
          84 TEXT-018.        "Message

  SKIP 1.
  MOVE '6' TO LN_CNTR.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_report_header
*&---------------------------------------------------------------------*
*&      Form  write_report_detail
*&---------------------------------------------------------------------*
FORM WRITE_REPORT_DETAIL.

  IF LN_CNTR >= 55.
    PERFORM GENERATE_REPORT_HEADER.
  ENDIF.

  WRITE ITAB_PDOC-EBELN UNDER TEXT-005.
  WRITE ITAB_PDOC-KDATB UNDER TEXT-007.
  WRITE ITAB_PDOC-KDATE UNDER TEXT-009.
  WRITE ITAB_PDOC-UOM   UNDER TEXT-011.
  WRITE ITAB_PDOC-BSART UNDER TEXT-013.
  WRITE ITAB_PDOC-EIKTO UNDER TEXT-015.
  WRITE W_RETURNCODE    UNDER TEXT-017.
  IF W_ERR_FLAG = 'Y'.
    MOVE '*** ERROR: Document Not Sent ***' TO W_MSG.
  ENDIF.
  WRITE W_MSG           UNDER TEXT-018.
  ADD +1 TO LN_CNTR.

  IF W_ERR_FLAG = 'Y'.
    SKIP.
    ADD +1 TO LN_CNTR.
    LOOP AT ITAB_ERR.
      NEW-LINE.
      WRITE ITAB_ERR-MSG UNDER TEXT-018.
      ADD +1 TO LN_CNTR.
    ENDLOOP.
  ENDIF.

  SKIP.
  ADD +1 TO LN_CNTR.

ENDFORM.                    "write_report_detail
