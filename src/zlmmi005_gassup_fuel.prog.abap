REPORT  zlmmi005_gassup_fuel MESSAGE-ID zs LINE-SIZE 132
        LINE-COUNT 65 NO STANDARD PAGE HEADING.
************************************************************************
* Author      : Glenn Ymana
* Date Created: Feb 28, 2011
*----------------------------------------------------------------------*
* Description :
* This program is posting Goods Issues related to previously posted
* CARE "FUEL" goods receipts. To be executed monthly and scheduled to
* run after the monthly "Supply" interface has been run on the first
* workday
*
* Program Logic:
* This program will read MKPF & MSEG to select the appropriate Goods
* Receipts based on the selection criteria. The appropriate details are
* extracted and the quantities are summed by plant & storage location.
* A Goods Issue will be created by BAPI_GOODSMVMT_CREATE for each
* Plant / Storage Loc combination. A report is created summarizing
* Goods Issues created and any error that come up.
************************************************************************
* Date         Developer      Request #       Description              *
************************************************************************
* 2011/07/06 gymana     TR804      modified code to include bill_of_
*                                  lading in the gmheader and change
*                                  doc_date to use accounting date.
************************************************************************

TYPE-POOLS:  slis.

TABLES:  tvarvc,
         mkpf,
         mseg,
         ska1,
         csks.

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-hdr.
SELECT-OPTIONS:  s_blart  FOR mkpf-blart OBLIGATORY DEFAULT 'WE',
                 s_mblnr  FOR mkpf-mblnr,
                 s_budat  FOR mkpf-budat,
                 s_bldat  FOR mkpf-bldat,
                 s_cpudt  FOR mkpf-cpudt,
                 s_frbnr  FOR mkpf-frbnr OBLIGATORY DEFAULT 'FUEL',
                 s_mjahr  FOR mkpf-mjahr,
                 s_usnam  FOR mkpf-usnam.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-hd1.
PARAMETERS:     p_noupdt AS CHECKBOX DEFAULT '', "Report only, no update
                p_detrpt AS CHECKBOX DEFAULT ''. "Print detail report
SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-hd2.
PARAMETERS:      p_acdat  TYPE mkpf-budat OBLIGATORY,
                 p_saknr  TYPE saknr OBLIGATORY,
                 p_kostl  TYPE kostl OBLIGATORY,
                 p_dmbtr  LIKE mseg-dmbtr.
SELECTION-SCREEN END OF BLOCK box3.
SELECTION-SCREEN END OF BLOCK box.

************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
* Insert a default date (previous month first and last day) into
* the variant posting date
************************************************************************
INITIALIZATION.

*  move 'I' to s_budat-sign.
  MOVE 'BT' TO s_budat-option.

  SELECT * FROM tvarvc
   WHERE name = 'ZMM_PREV_PER_FIRSTDAY'.

    IF sy-subrc = 0.
      s_budat-low = tvarvc-low.
    ENDIF.
  ENDSELECT.

  SELECT * FROM tvarvc
   WHERE name = 'ZMM_PREV_PER_LASTDAY'.

    IF sy-subrc = 0.
      s_budat-high = tvarvc-low.
    ENDIF.
  ENDSELECT.
  APPEND s_budat.

************************************************************************
*    VARIABLES                                                         *
************************************************************************

* Goods Receipt detail data internal table
  DATA: BEGIN OF itab_grdata OCCURS 0,
          mblnr LIKE mkpf-mblnr,
          mjahr LIKE mkpf-mjahr,
          budat LIKE mkpf-budat,
          bldat LIKE mkpf-bldat,
          cpudt LIKE mkpf-cpudt,
          usnam LIKE mkpf-usnam,
          frbnr LIKE mkpf-frbnr,
          matnr LIKE mseg-matnr,
          werks LIKE mseg-werks,
          lgort LIKE mseg-lgort,
          bwart LIKE mseg-bwart,
          shkzg LIKE mseg-shkzg,
          menge LIKE mseg-menge,
          meins LIKE mseg-meins,
        END OF itab_grdata.

* summarized data internal table
  DATA: BEGIN OF itab_grsum OCCURS 0,
          werks    LIKE mseg-werks,
          lgort    LIKE mseg-lgort,
          menge    LIKE mseg-menge,
          reccnt   TYPE i,
          mjahr    LIKE mkpf-mjahr,
        END OF itab_grsum.

* GR create report internal table
  DATA: BEGIN OF itab_grcreate OCCURS 0,
          werks    LIKE mseg-werks,
          lgort    LIKE mseg-lgort,
          budat    LIKE mkpf-budat,
          bldat    LIKE mkpf-bldat,
          grnum    LIKE bapi2017_gm_head_ret-mat_doc,
          reccnt   TYPE i,
          meins    LIKE mseg-meins,
          menge    LIKE mseg-menge,
        END OF itab_grcreate.

* Goods Receipt alv report table
  DATA: BEGIN OF itab_rpt OCCURS 0,
          werks LIKE mseg-werks,
          lgort LIKE mseg-lgort,
          mblnr LIKE mkpf-mblnr,
          budat LIKE mkpf-budat,
          bldat LIKE mkpf-bldat,
          bwart LIKE mseg-bwart,
          menge LIKE mseg-menge,
          meins LIKE mseg-meins,
        END OF itab_rpt.

* SES error internal table
  DATA: BEGIN OF itab_err_rec OCCURS 0,
          werks      LIKE mseg-werks,
          lgort      LIKE mseg-lgort,
          menge      LIKE mseg-menge,
          reccnt     TYPE i,
          errmsg(70) TYPE c,
        END OF itab_err_rec.

* BAPI tables
  DATA: BEGIN OF tbl_bapi_gmhdr OCCURS 0.
          INCLUDE STRUCTURE bapi2017_gm_head_01.
  DATA: END OF tbl_bapi_gmhdr.

  DATA: BEGIN OF tbl_bapi_gmcode OCCURS 0.
          INCLUDE STRUCTURE bapi2017_gm_code.
  DATA: END OF tbl_bapi_gmcode.

  DATA: BEGIN OF tbl_bapi_gmitem OCCURS 0.
          INCLUDE STRUCTURE bapi2017_gm_item_create.
  DATA: END OF tbl_bapi_gmitem.

  DATA: BEGIN OF tbl_bapi_gmhdret OCCURS 0.
          INCLUDE STRUCTURE bapi2017_gm_head_ret.
  DATA: END OF tbl_bapi_gmhdret.

  DATA: BEGIN OF tbl_bapi_gmreturn OCCURS 0.
          INCLUDE STRUCTURE bapiret2.
  DATA: END OF tbl_bapi_gmreturn.

  DATA: w_testrun     TYPE c VALUE ' ',
        w_count       TYPE i,
        w_mandt(5)    TYPE c,
        ln_cntr       TYPE i VALUE 99,
        w_nodata_flag TYPE c VALUE 'N',
        w_default_uom LIKE mseg-meins VALUE 'GJ1'.

************************************************************************
*    START OF SELECTION                                                *
************************************************************************
START-OF-SELECTION.

  CONCATENATE '(' sy-mandt ')' INTO w_mandt.

  IF p_noupdt <> ''.
    w_testrun = 'X'.
  ENDIF.

  PERFORM validate_gl_variants.

  PERFORM get_gr_data.

  DESCRIBE TABLE itab_grdata LINES w_count.

  IF w_count = 0.
    PERFORM generate_err_report_header.
    WRITE: 20 '*** No data was selected. ',
           46 'Please verify input parameters ***'.
    EXIT.
  ENDIF.

  PERFORM summarize_data.

  IF w_testrun = ' '.
    LOOP AT itab_grsum.
      PERFORM setup_goodsmvmt_create.
      PERFORM call_goodsmvmt_create.

      IF tbl_bapi_gmhdret-mat_doc <> ' '.
        CLEAR itab_grcreate.
        MOVE-CORRESPONDING itab_grsum TO itab_grcreate.
        MOVE p_acdat                  TO itab_grcreate-budat.
        MOVE sy-datum                 TO itab_grcreate-bldat.
        MOVE w_default_uom            TO itab_grcreate-meins.
        MOVE tbl_bapi_gmhdret-mat_doc TO itab_grcreate-grnum.
        APPEND itab_grcreate.
      ELSE.
        CLEAR itab_err_rec.
        MOVE-CORRESPONDING itab_grsum  TO itab_err_rec.
        MOVE tbl_bapi_gmreturn-message TO itab_err_rec-errmsg.
        APPEND itab_err_rec.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM write_summary_rpt.

  IF w_testrun = ' '.
    PERFORM write_grcreate_rpt.
  ENDIF.

  PERFORM write_error_rpt.

* Used for testing error report
*  ln_cntr = 55.
*  perform write_err_report_detail.

  IF p_detrpt <> ''.

*- Load report table for ALV
    LOOP AT itab_grdata.
      READ TABLE itab_grsum
        WITH KEY werks = itab_grdata-werks lgort = itab_grdata-lgort
             BINARY SEARCH.

      MOVE-CORRESPONDING itab_grdata TO itab_rpt.
      APPEND itab_rpt.
    ENDLOOP.

    PERFORM output_alv.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  validate_gl_variants
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validate_gl_variants.

  DATA: msg(150) TYPE c.

  IF s_budat = '' AND
     s_bldat = ''.
    CONCATENATE 'You must input a Posting and/'
                'or Document Date/Range.'
                INTO msg.
    MESSAGE e019 WITH msg.
    STOP.
  ENDIF.

  SELECT * FROM ska1
    WHERE saknr = p_saknr
      AND xspeb <> 'X'.
  ENDSELECT.

  IF sy-subrc <> 0.
    CONCATENATE 'GL Acct: ' p_saknr
                ' not valid or open for posting.'
                INTO msg.
    MESSAGE e019 WITH msg.
    STOP.
  ENDIF.

  IF s_budat-option = 'EQ'.
    MOVE s_budat-low TO s_budat-high.
  ENDIF.

  IF s_bldat-option = 'EQ'.
    MOVE s_bldat-low TO s_bldat-high.
  ENDIF.

  SELECT * FROM csks
    WHERE kostl = p_kostl
      AND datab LE p_acdat
      AND datbi GE p_acdat.
  ENDSELECT.

  IF sy-subrc <> 0.
    CONCATENATE 'Cost center: ' p_kostl
                ' not valid/open for posting.'
           INTO msg.
    MESSAGE e019 WITH msg.
    STOP.
  ENDIF.

ENDFORM.                    "validate_gl_variants

*&---------------------------------------------------------------------*
*&      Form  GET_GR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_gr_data.

  SELECT mkpf~mblnr mkpf~mjahr mkpf~budat mkpf~bldat
         mkpf~cpudt mkpf~usnam mkpf~frbnr mseg~matnr
         mseg~werks mseg~lgort mseg~bwart mseg~shkzg
         mseg~menge mseg~meins
    INTO CORRESPONDING FIELDS OF TABLE itab_grdata
    FROM ( mkpf INNER JOIN mseg
             ON mkpf~mblnr = mseg~mblnr
            AND mkpf~mjahr = mseg~mjahr )
   WHERE mkpf~blart IN s_blart
     AND mkpf~mblnr IN s_mblnr
     AND mkpf~budat IN s_budat
     AND mkpf~bldat IN s_bldat
     AND mkpf~cpudt IN s_cpudt
     AND mkpf~frbnr IN s_frbnr
     AND mkpf~mjahr IN s_mjahr
     AND mkpf~usnam IN s_usnam.

ENDFORM.                    "get_gr_data

*&---------------------------------------------------------------------*
*&      Form  SUMMARIZE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM summarize_data.

  DATA: va_menge LIKE mseg-menge VALUE 0.

*- prepare table summary
  REFRESH itab_grsum.
  CLEAR itab_grsum.

*- Set the sign of the quantity based on the db/cr indicator
  LOOP AT itab_grdata.
    IF itab_grdata-shkzg = 'H'.
      itab_grdata-menge = itab_grdata-menge * -1.
      MODIFY itab_grdata INDEX sy-tabix TRANSPORTING menge.
    ENDIF.
  ENDLOOP.

*- sort data
  SORT itab_grdata BY werks lgort.

*- summarize data
  LOOP AT itab_grdata.
    READ TABLE itab_grsum
         WITH KEY werks = itab_grdata-werks
                  lgort = itab_grdata-lgort.

* If there is a match in table itab_grsum, then add itab_grdata
* quantity and count to isum record (sum up)
* If no match, then a new record is added to itab_grsum.

    IF sy-subrc = 0.
* sum up quantity and add to record count.
      IF itab_grsum-menge <> 0.
        MOVE itab_grsum-menge TO va_menge.
*         if itab_grdata-bwart = '101'.
*           add itab_grdata-menge to va_menge.
*         elseif itab_grdata-bwart = '102'.
*           subtract itab_grdata-menge from va_menge.
*         endif.
        ADD itab_grdata-menge TO va_menge.
        MOVE va_menge TO itab_grsum-menge.
        ADD +1 TO itab_grsum-reccnt.
        MODIFY itab_grsum INDEX sy-tabix TRANSPORTING menge reccnt.
        CLEAR itab_grsum.
      ENDIF.
    ELSE.
*  no match in isum. Add isort to isum.
      IF itab_grdata-menge <> 0.
        MOVE-CORRESPONDING itab_grdata TO itab_grsum.
        MOVE +1 TO itab_grsum-reccnt.
        APPEND itab_grsum. CLEAR itab_grsum.
      ENDIF.
    ENDIF.
  ENDLOOP.

*- sort data
  SORT itab_grsum BY werks lgort.


ENDFORM.                    "summarize_data
*&---------------------------------------------------------------------*
*&      Form  SETUP_GOODMVMT_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM setup_goodsmvmt_create.

* Populate BAPI parameters

  REFRESH: tbl_bapi_gmhdr, tbl_bapi_gmcode, tbl_bapi_gmitem.
  CLEAR: tbl_bapi_gmhdr, tbl_bapi_gmcode, tbl_bapi_gmitem.

  tbl_bapi_gmhdr-pstng_date    = p_acdat.
  tbl_bapi_gmhdr-doc_date      = p_acdat.

  CONCATENATE 'FUEL' '_' itab_grsum-werks itab_grsum-lgort
         INTO tbl_bapi_gmhdr-ref_doc_no.

  tbl_bapi_gmhdr-header_txt     = 'COMPRESSOR FUEL MONTHLY SUM'.
  tbl_bapi_gmhdr-bill_of_lading = 'FUEL'.
  tbl_bapi_gmhdr-gr_gi_slip_no  = 'AS PER CARE'.
  APPEND tbl_bapi_gmhdr.

  tbl_bapi_gmcode-gm_code    = '03'.
  APPEND tbl_bapi_gmcode.

  tbl_bapi_gmitem-material   = 'NATGAS'.
  tbl_bapi_gmitem-plant      = itab_grsum-werks.
  tbl_bapi_gmitem-stge_loc   = itab_grsum-lgort.
  tbl_bapi_gmitem-move_type  = '201'.
  tbl_bapi_gmitem-entry_qnt  = itab_grsum-menge.
  tbl_bapi_gmitem-entry_uom  = w_default_uom.
  tbl_bapi_gmitem-item_text  = 'Monthly Sum of CARE FUEL receipts'.
  tbl_bapi_gmitem-gl_account = p_saknr.
  tbl_bapi_gmitem-costcenter = p_kostl.
  tbl_bapi_gmitem-amount_lc  = p_dmbtr.
  APPEND tbl_bapi_gmitem.

ENDFORM.                    "setup_goodmvmt_create

*&---------------------------------------------------------------------*
*&      Form  CALL_GOODMVMT_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_goodsmvmt_create.
* Create the Goods Receipt Document

  REFRESH: tbl_bapi_gmhdret, tbl_bapi_gmreturn.
  CLEAR: tbl_bapi_gmhdret, tbl_bapi_gmreturn.


  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header                      = tbl_bapi_gmhdr
      goodsmvt_code                        = tbl_bapi_gmcode
      testrun                              = w_testrun
    IMPORTING
      goodsmvt_headret                     = tbl_bapi_gmhdret
*      MATERIALDOCUMENT                     = tbl_bapi_gmhdret-mat_doc
*      MATDOCUMENTYEAR                      = tbl_bapi_gmhdret-doc_year
    TABLES
      goodsmvt_item                        = tbl_bapi_gmitem
      return                               = tbl_bapi_gmreturn.

*  Commit the work or rollback the BAPI call.

  IF tbl_bapi_gmhdret-mat_doc <> ' '.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.                    "call_goodsmvmt_create

*&---------------------------------------------------------------------*
*&      Form  write_summary_rpt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_summary_rpt.
  ln_cntr = 55.
  LOOP AT itab_grsum.
    PERFORM write_summary_rpt_detail.
  ENDLOOP.

ENDFORM.                    "write_summary_rpt

*&---------------------------------------------------------------------*
*&      Form  write_grcreate_rpt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_grcreate_rpt.
  ln_cntr = 55.
  LOOP AT itab_grcreate.
    PERFORM write_grcreate_rpt_detail.
  ENDLOOP.

ENDFORM.                    "write_grcreate_rpt

*&---------------------------------------------------------------------*
*&      Form  write_error_rpt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_error_rpt.

  ln_cntr = 55.
  LOOP AT itab_err_rec.
    PERFORM write_err_report_detail.
  ENDLOOP.

ENDFORM.                    "write_error_rpt

*&---------------------------------------------------------------------*
*&      Form  generate_summary_rpt_header
*&---------------------------------------------------------------------*

FORM generate_summary_rpt_header.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 40 text-002.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /1 sy-sysid, 5 w_mandt, 47 text-02a.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.

  WRITE: /25   text-010,        "No. of
          40   text-08a.        "Total
  WRITE: /3    text-003,        "Plant
          12   text-004,        "Storage
          25   text-011,        "Records
          40   text-008,        "Quantity
          65   text-013.        "Doc. Year
  SKIP 2.
  MOVE '5' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_summary_rpt_header

*&---------------------------------------------------------------------*
*&      Form  write_report_detail
*&---------------------------------------------------------------------*
FORM write_summary_rpt_detail.

  IF ln_cntr >= 55.
    PERFORM generate_summary_rpt_header.
  ENDIF.

  WRITE itab_grsum-werks  UNDER text-003.
  WRITE itab_grsum-lgort  UNDER text-004.
  WRITE itab_grsum-reccnt UNDER text-011.
  WRITE itab_grsum-menge  UNDER text-008.
  WRITE itab_grsum-mjahr  UNDER text-013.
  SKIP.
  ADD +2 TO ln_cntr.

ENDFORM.                    "write_summary_rpt_detail

*&---------------------------------------------------------------------*
*&      Form  generate_grcreate_rpt_header
*&---------------------------------------------------------------------*

FORM generate_grcreate_rpt_header.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 40 text-002.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /1 sy-sysid, 5 w_mandt, 39 text-02c.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.

  WRITE: /78   text-010,        "No. of
          102  text-08a.        "Total
  WRITE: /3    text-003,        "Plant
          12   text-004,        "Storage Loc
          28   text-006,        "Posting Date
          45   text-06a,        "Document Date
          62   text-014,        "GR Document No.
          78   text-011,        "Records
          93   text-009,        "UOM
          102  text-008.        "Quantity
  SKIP 2.
  MOVE '5' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_grcreate_rpt_header

*&---------------------------------------------------------------------*
*&      Form  write_grcreate_rpt_detail
*&---------------------------------------------------------------------*
FORM write_grcreate_rpt_detail.

  IF ln_cntr >= 55.
    PERFORM generate_grcreate_rpt_header.
  ENDIF.

  WRITE itab_grcreate-werks  UNDER text-003.
  WRITE itab_grcreate-lgort  UNDER text-004.
  WRITE itab_grcreate-budat  UNDER text-006.
  WRITE itab_grcreate-bldat  UNDER text-06a.
  WRITE itab_grcreate-grnum  UNDER text-014.
  WRITE itab_grcreate-reccnt UNDER text-011.
  WRITE itab_grcreate-meins  UNDER text-009.
  WRITE itab_grcreate-menge  UNDER text-008.
  SKIP.
  ADD +2 TO ln_cntr.

ENDFORM.                    "write_summary_rpt_detail

*&---------------------------------------------------------------------*
*&      Form  generate_err_report_header
*&---------------------------------------------------------------------*

FORM generate_err_report_header.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 40 text-002.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /1 sy-sysid, 5 w_mandt, 47 text-02b.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /25  text-08a,        "Total
          45  text-010.        "No. Of
  WRITE: /3   text-003,        "Plant
          12  text-004,        "Storage loc
          25  text-008,        "Quantity
          45  text-011,        "Records
          60  text-012.        "Error Message
  SKIP 2.
  MOVE '5' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_err_report_header

*&---------------------------------------------------------------------*
*&      Form  write_report_detail
*&---------------------------------------------------------------------*
FORM write_err_report_detail.

  IF ln_cntr >= 55.
    PERFORM generate_err_report_header.
  ENDIF.

  WRITE itab_err_rec-werks  UNDER text-003.
  WRITE itab_err_rec-lgort  UNDER text-004.
  WRITE itab_err_rec-menge  UNDER text-008.
  WRITE itab_err_rec-reccnt UNDER text-011.
  WRITE itab_err_rec-errmsg UNDER text-012.
  SKIP.
  ADD +2 TO ln_cntr.

ENDFORM.                    "write_report_detail

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_alv.

  DATA: fieldcat TYPE slis_t_fieldcat_alv,
        fc_str   TYPE slis_fieldcat_alv,
        layout   TYPE slis_layout_alv,
        title    TYPE lvc_title,
        repid    LIKE sy-repid,
        variant  LIKE disvariant,
        sort     TYPE slis_t_sortinfo_alv,
        sort_str TYPE slis_sortinfo_alv.

  SORT itab_rpt BY werks lgort mblnr.

  repid = sy-repid.
  layout-colwidth_optimize = 'X'.
  variant-report = repid.

  fc_str-fieldname = 'WERKS'.
  fc_str-key    = ' '.                " Key columns -not first
  fc_str-seltext_l = text-003.        " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'LGORT'.
  fc_str-key    = ' '.                " Key columns -notfirst
  fc_str-seltext_l = text-004.        " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MBLNR'.
  fc_str-key    = ' '.                " Key columns -not first
  fc_str-seltext_l = text-005.        " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'BUDAT'.
  fc_str-seltext_l = text-006.        " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'BLDAT'.
  fc_str-seltext_l = text-06a.        " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'BWART'.
  fc_str-seltext_l = text-007.        " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MENGE'.
  fc_str-seltext_l = text-008.        " Alternative col header
  fc_str-ddictxt = 'L'.               " Use Large system text
  fc_str-do_sum  = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MEINS'.
  fc_str-seltext_l = text-009.        " Alternative col header
  fc_str-ddictxt = 'L'.               " Use Large system text
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  sort_str-fieldname = 'WERKS'.
  sort_str-group = 'UL'.
  sort_str-up = 'X'.
  APPEND sort_str TO sort.

  sort_str-fieldname = 'LGORT'.
  sort_str-subtot = 'X'.
  sort_str-up = 'X'.
  APPEND sort_str TO sort.

* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
         it_fieldcat             = fieldcat
         is_layout               = layout
         i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
         i_callback_program      = repid
         it_sort                 = sort
*        I_SAVE                   = 'A'
*        IS_VARIANT               = variant
*        I_GRID_TITLE             = TITLE
*        I_CALLBACK_USER_COMMAND  = 'OUTPUTALV_DETAILS'
      TABLES
             t_outtab           = itab_rpt
      EXCEPTIONS
             program_error = 1
      OTHERS               = 2.

ENDFORM.                    "OUTPUT_ALV
*************************************************************

FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.
  DATA: t_line LIKE ls_line-info.
  DATA: i_count  TYPE i.
  DATA: i_countc(10) TYPE c.
  DATA: datum1(10).
  DATA: uzeit1(10).

*1- HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

*2- SELECTION LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  WRITE sy-datum TO datum1 DD/MM/YYYY.
  WRITE sy-uzeit TO uzeit1 USING EDIT MASK '__:__:__'.
  CONCATENATE 'CLIENT:' sy-sysid sy-mandt
              '  DATE:' datum1 '@' uzeit1
              INTO ls_line-info
              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

  CLEAR ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  MOVE '---' TO ls_line-info.
  APPEND ls_line TO lt_top_of_page.

* Total No. of Records Selected
  DESCRIBE TABLE itab_rpt LINES i_count.
  i_countc = i_count.
  CONCATENATE 'Total No. of Documents Selected: ' i_countc
                    INTO t_line SEPARATED BY space.
  ls_line-typ  = 'A'.
  ls_line-info = t_line.
  APPEND ls_line TO lt_top_of_page.
  CLEAR: ls_line, t_line.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.

ENDFORM.                               " ALV_TOP_OF_PAGE
