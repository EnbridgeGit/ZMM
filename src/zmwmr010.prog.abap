report zmwmi010 no standard page heading line-size 170 line-count 58.
*----------------------------------------------------------------------*
* abap to List to Transactions by Date                                 *
*----------------------------------------------------------------------*
* 2012/09/06 M Khan   TR995 Change C: drive to H: drive with directory *
*                           file selection using F4 & move             *
*                           hard-coded file path/name to variant.      *
*                                                                      *
* 2010/01/19      lritchie - Add ALV & EXCEL options
*                          - Add PCE column
*                          - Major re-write for performance TR789
* 2002/04/16      mdemeest - Total quantity field (UOM doesn't matter  *
*                            as per D. Bossy's memo 2002/04/16
* 2002/02/25 CARS mdemeest - Original abap
*----------------------------------------------------------------------*

************************************************************************
*    TABLES                                                            *
************************************************************************
tables: ekpo,         "Purchasing item
        mkpf,         "Material Transaction Header
        mseg,         "Material Transaction Detail
        makt,         "Material Description
        prps.         "WBS element

type-pools  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* MKPF - material document header
data: begin of tbl_mkpf occurs 0,
         mblnr          like mkpf-mblnr, "Material document number
         mjahr          like mkpf-mjahr, "Material doc year
         budat          like mkpf-budat, "Posting date
         usnam          like mkpf-usnam, "User name
         xblnr          like mkpf-xblnr, "Reference
      end of tbl_mkpf.

* MSEG - material document item
data: begin of tbl_mseg occurs 0,
         mblnr          like mseg-mblnr, "Material document number
         mjahr          like mseg-mjahr, "Material doc year
         zeile          like mseg-zeile, "Item number
         bwart          like mseg-bwart, "Movement type
         matnr          like mseg-matnr, "Material number
         werks          like mseg-werks, "Plant
         lgort          like mseg-lgort, "Storage location
         shkzg          like mseg-shkzg, "Debit or credit
         erfmg          like mseg-erfmg, "Quantity
         erfme          like mseg-erfme, "Issuing uom
         ebeln          like mseg-ebeln, "PO number
         ebelp          like mseg-ebelp, "PO item
         aufnr          like mseg-aufnr, "Internal Order
         ps_psp_pnr     like mseg-ps_psp_pnr, "WBS
         sakto          like mseg-sakto, "G/L account (or PCE)
      end of tbl_mseg.

*
data tbl_big_mseg like tbl_mseg occurs 0 with header line.

* temporary table to minimize the MKPF lookups
data: begin of tbl_mblnr_mjahr occurs 0,
         mblnr          like mseg-mblnr,
         mjahr          like mseg-mjahr,
      end of tbl_mblnr_mjahr.

* MAKT - material description
data: begin of tbl_makt occurs 0,
         matnr          like makt-matnr, "Material number
         maktx          like makt-maktx, "Material description
      end of tbl_makt.

* temporary table to get the material descriptions.
data: begin of tbl_matnr occurs 0,
         matnr          like makt-matnr,
      end of tbl_matnr.

* PRPS - to print the WBS number
data: begin of tbl_prps occurs 0,
         pspnr          like prps-pspnr, "WBS element
         poski          like prps-poski, "WBS element - display version with dashes
      end of tbl_prps.

* temporary table to get WBS numbers
data: begin of tbl_pspnr occurs 0,
         pspnr          like prps-pspnr,
      end of tbl_pspnr.

* EKPO - purchase order items
data: begin of tbl_ekpo occurs 0,
         ebeln          like ekpo-ebeln, "PO number
         ebelp          like ekpo-ebelp, "PO item
         txz01          like ekpo-txz01, "Material text
      end of tbl_ekpo.

* temporay table to get POs
data: begin of tbl_ebeln occurs 0,
        ebeln           like ekpo-ebeln,
      end of tbl_ebeln.

* data to be printed
data: begin of tbl_report  occurs 0,
         usnam          like mkpf-usnam, "SAP user name
         budat          like mkpf-budat, "Date entry date
         mblnr          like mkpf-mblnr, "Document Number
         bwart          like mseg-bwart, "Movement Type
         werks          like mseg-werks, "Plant
         lgort          like mseg-lgort, "Storage Location
         shkzg          like mseg-shkzg, "Debit or credit
         matnr          like mseg-matnr, "Material Number
         maktx          like makt-maktx, "Description
         erfmg(6)       type p,          "Quantity
         erfme          like mseg-erfme, "Issuing uom
         xblnr          like mkpf-xblnr, "Reference
         sakto          like mseg-sakto, "G/L account (or PCE)
         aufnr          like mseg-aufnr, "Internal Order
         ps_psp_pnr(16) type c,          "WBS element with dashes
      end of tbl_report.

* data for ALV or EXCEL
data: begin of tbl_alv  occurs 0,
         usnam          like mkpf-usnam, "SAP user name
         budat(12)      type c,          "Date entry date
         mblnr          like mkpf-mblnr, "Document Number
         bwart          like mseg-bwart, "Movement Type
         werks          like mseg-werks, "Plant
         lgort          like mseg-lgort, "Storage Location
         matnr          like mseg-matnr, "Material Number
         maktx          like makt-maktx, "Description
         erfmg(12)      type c,           "Quantity
         erfme          like mseg-erfme, "Issuing uom
         xblnr          like mkpf-xblnr, "Reference
         sakto          like mseg-sakto, "G/L account (or PCE)
         aufnr          like mseg-aufnr, "Internal Order
         ps_psp_pnr(16) type c,          "WBS element with dashes
      end of tbl_alv.

* EXCEL table header
data:  begin of tbl_excel_header occurs 1,
         spaltenname(20)  type c,
         ddic_table(5)    type c,
         ddic_field(5)    type c,
         key              type c,
       end of tbl_excel_header.

*  internal table for field catalog.
data : tbl_fieldtab type slis_t_fieldcat_alv with header line,
       tbl_fieldcat type slis_fieldcat_alv.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: ctr type i.                         "Count for documents
data: x(50) type c.
data: v_quantity(7)              type p,
      v_prev_mblnr               like mseg-mblnr,
      v_prev_mjahr               like mseg-mjahr,
      v_error(1)                 type c,
      v_matnr_count(7)           type p,
      v_pspnr_count(7)           type p,
      v_ebeln_count(7)           type p,
      v_unique_document_count(5) type p,
      v_document_item_count(5)   type p,
      v_total_quantity(5)        type p.

* ALV stuff
data: st_layout type slis_layout_alv,
      st_sort  type slis_sortinfo_alv occurs 0,
      st_events type slis_t_event,
      v_repid like sy-repid.

data: st_line type slis_listheader.
data: tbl_top_of_page type slis_t_listheader.
data: v_head01(100) type c,
      v_head02(100) type c.

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************

selection-screen skip 1.
selection-screen  begin of block block1 with frame title text-100.
selection-screen skip 1.
select-options:
            s_werks for mseg-werks,                "Plant
            s_lgort for mseg-lgort,                "Storage Location
            s_budat for mkpf-budat,                "Posting date
            s_bwart for mseg-bwart no intervals.   "Movement Type
parameters:
            p_usnam like mkpf-usnam.               "User name
selection-screen  end of block block1.

selection-screen skip 1.
selection-screen begin of block block2 with frame title text-014.
selection-screen skip 1.
parameter  p_alv   radiobutton group r2.        "Print Report
parameter  p_excel radiobutton group r2.        "Excel Spreadsheet
parameter  p_file like rlgrap-filename default 'H:\SAPTEMP'. "TR995
parameter  p_back  radiobutton group r2.        "Background processing
selection-screen end of block block2.
selection-screen skip 2.
selection-screen comment 5(50) text-018.

select-options s_matnr for makt-matnr no-display.
select-options s_pspnr for prps-pspnr no-display.
select-options s_ebeln for ekpo-ebeln no-display.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILE.
  IF SY-BATCH <> 'X'.
   PERFORM CHECK_FILE_PATH.
  ENDIF.
*End of TR995 changes
************************************************************************
*    TOP OF PAGE                                                       *
************************************************************************

top-of-page.

  if p_back = 'X'.

    write: /1 text-rpt, sy-repid, 40 text-ttl, 110 text-dte,sy-datum,
              text-amp, sy-uzeit.
    write: /  text-clt under text-rpt, sy-mandt under sy-repid, sy-sysid,
              text-pge under text-dte, sy-pagno.
    skip 1.
    uline.
    write: /1 text-001, 10 text-002, 20 text-003, 32 text-009, 36 text-004,
           42 text-005, 47 text-006, 56 text-013, 86 text-007, 96 text-008,
           100 text-011, 118 text-015, 128 text-016, 138 text-017.
    write: / text-010 under text-005.
    uline.
    skip 1.

  endif.

******************************************************************
*                   START OF SELECTION                           *
******************************************************************

start-of-selection.

  clear v_error.
  if s_werks[] is initial and s_lgort[] is initial and
     s_budat[] is initial and s_bwart[] is initial and
     p_usnam = ' '.
    skip 5.
    write:/10 'Please enter something in the DATA SELECTION CRITERIA'.
    v_error = 'X'.
    stop.
  endif.

  perform get_material_documents.

  perform get_material_desc_and_wbs.

  perform create_report_table.

end-of-selection.

  sort tbl_report by usnam budat mblnr matnr.

  if tbl_report[] is initial.
    if v_error = ' '.
      skip 5.
      write:/15 'NO DATA TO REPORT'.
    endif.
  else.
    if p_alv = 'X'.
      perform load_alv_excel_table.
      perform build_fieldcat.
      perform build_events using st_events[].
      perform display_grid.
    endif.
    if p_excel = 'X'.
      perform load_alv_excel_table.
      perform build_excel_table.
    endif.
    if p_back = 'X'.
      perform create_hardcopy_report.
    endif.
  endif.

*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_DOCUMENTS
*&---------------------------------------------------------------------*
*       Table MSEG is terrible for performance, so it needs to be read
*       with different SELECT combinations, depending on what the user
*       enters.
*----------------------------------------------------------------------*
form get_material_documents .


  clear: tbl_mseg, tbl_mkpf, tbl_matnr, tbl_pspnr, tbl_ebeln.
  refresh: tbl_mseg, tbl_mkpf, tbl_matnr, tbl_pspnr, tbl_ebeln.

  concatenate p_usnam '%' into x.

** Option 1 - there is a posting date

  if not s_budat[] is initial.
    select mblnr mjahr budat usnam xblnr
           into table tbl_mkpf
           from mkpf
           where budat in s_budat
             and usnam like x
             %_hints oracle 'INDEX("&TABLE&" "MKPF" "MKPF~BUD")'.

    sort tbl_mkpf by mblnr mjahr.

    if not tbl_mkpf[] is initial.
      select mblnr mjahr zeile bwart matnr werks lgort shkzg erfmg
             erfme ebeln ebelp aufnr ps_psp_pnr sakto
             into table tbl_mseg
             from mseg
             for all entries in tbl_mkpf
             where mblnr = tbl_mkpf-mblnr
               and mjahr = tbl_mkpf-mjahr
               and werks in s_werks
               and lgort in s_lgort
               and bwart in s_bwart
               %_hints oracle 'INDEX("&TABLE&" "MSEG" "MSEG~0")'.
      sort tbl_mseg by mblnr mjahr zeile.
      loop at tbl_mseg.
        perform setup_temporary_data.
      endloop.
    endif.
  endif.

** Option 2 - there is no posting date but a user name

  if s_budat[] is initial and p_usnam <> ' '.
    select mblnr mjahr budat usnam xblnr
           into table tbl_mkpf
           from mkpf
           bypassing buffer
           where usnam like x.

    sort tbl_mkpf by mblnr mjahr.

    if not tbl_mkpf[] is initial.
      select mblnr mjahr zeile bwart matnr werks lgort shkzg erfmg
             erfme ebeln ebelp aufnr ps_psp_pnr sakto
             into table tbl_mseg
             from mseg
             for all entries in tbl_mkpf
             where mblnr = tbl_mkpf-mblnr
               and mjahr = tbl_mkpf-mjahr
               and werks in s_werks
               and lgort in s_lgort
               and bwart in s_bwart
               %_hints oracle 'INDEX("&TABLE&" "MSEG" "MSEG~0")'.
      sort tbl_mseg by mblnr mjahr zeile.
      loop at tbl_mseg.
        perform setup_temporary_data.
      endloop.
    endif.
  endif.

** Option 3 - no posting date & no user name
**          - do not attempt to use any index or the program will timeout

  if s_budat[] is initial and p_usnam = ' '.

    select mblnr mjahr zeile bwart matnr werks lgort shkzg erfmg
              erfme ebeln ebelp aufnr ps_psp_pnr sakto
              into table tbl_big_mseg
              from mseg
              bypassing buffer.

    loop at tbl_big_mseg.
      if tbl_big_mseg-werks in s_werks and
         tbl_big_mseg-lgort in s_lgort and
         tbl_big_mseg-bwart in s_bwart.
        tbl_mseg = tbl_big_mseg.
        append tbl_mseg.
      endif.
    endloop.

    free tbl_big_mseg.

    sort tbl_mseg by mblnr mjahr zeile.

    if not tbl_mseg[] is initial.

      clear tbl_mblnr_mjahr.
      refresh tbl_mblnr_mjahr.

      loop at tbl_mseg.
        if tbl_mseg-mblnr <> tbl_mblnr_mjahr-mblnr or
           tbl_mseg-mjahr <> tbl_mblnr_mjahr-mjahr.
          tbl_mblnr_mjahr-mblnr = tbl_mseg-mblnr.
          tbl_mblnr_mjahr-mjahr = tbl_mseg-mjahr.
          append tbl_mblnr_mjahr.
        endif.
        perform setup_temporary_data.
      endloop.

      select mblnr mjahr budat usnam xblnr
             into table tbl_mkpf
             from mkpf
             for all entries in tbl_mblnr_mjahr
             where mblnr = tbl_mblnr_mjahr-mblnr
               and mjahr = tbl_mblnr_mjahr-mjahr.

      sort tbl_mkpf by mblnr mjahr.

    endif.

    free tbl_mblnr_mjahr.

  endif.

endform.                    " GET_MATERIAL_DOCUMENTS

*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_report_table .

  clear: v_total_quantity, v_unique_document_count, v_document_item_count,
         v_prev_mblnr, v_prev_mjahr.
  clear: tbl_report, tbl_mkpf, tbl_makt.
  refresh: tbl_report.

  loop at tbl_mseg.

    v_document_item_count = v_document_item_count + 1.
    if v_prev_mblnr <> tbl_mseg-mblnr or v_prev_mjahr <> tbl_mseg-mjahr.
      v_unique_document_count = v_unique_document_count + 1.
      v_prev_mblnr = tbl_mseg-mblnr.
      v_prev_mjahr = tbl_mseg-mjahr.
    endif.

* get the material document header
    if tbl_mseg-mblnr <> tbl_mkpf-mblnr or
       tbl_mseg-mjahr <> tbl_mkpf-mjahr.
      read table tbl_mkpf with key mblnr = tbl_mseg-mblnr
                                   mjahr = tbl_mseg-mjahr
                                   binary search.
    endif.

    tbl_report-mblnr = tbl_mkpf-mblnr.
    tbl_report-budat = tbl_mkpf-budat.
    tbl_report-usnam = tbl_mkpf-usnam.
    tbl_report-xblnr = tbl_mkpf-xblnr.

    tbl_report-bwart = tbl_mseg-bwart.
    tbl_report-matnr = tbl_mseg-matnr.
    tbl_report-werks = tbl_mseg-werks.
    tbl_report-lgort = tbl_mseg-lgort.
    tbl_report-erfmg = tbl_mseg-erfmg.
    if tbl_mseg-shkzg = 'S' and tbl_mseg-erfmg <> 0.
      tbl_report-erfmg = tbl_mseg-erfmg * -1.
    endif.
    tbl_report-erfme = tbl_mseg-erfme.
    tbl_report-aufnr = tbl_mseg-aufnr.

    if not tbl_mseg-ps_psp_pnr is initial.
      if tbl_mseg-ps_psp_pnr <> tbl_prps-pspnr.
        read table tbl_prps with key pspnr = tbl_mseg-ps_psp_pnr
                                     binary search.
      endif.
      tbl_report-ps_psp_pnr = tbl_prps-poski.
    endif. .

    tbl_report-sakto = tbl_mseg-sakto.

    v_total_quantity = v_total_quantity + tbl_report-erfmg.

* get the material description
    if not tbl_mseg-matnr is initial.
      if tbl_mseg-matnr <> tbl_makt-matnr.
        read table tbl_makt with key matnr = tbl_mseg-matnr
                                     binary search.
      endif.
      tbl_report-maktx = tbl_makt-maktx.
    else.
      read table tbl_ekpo with key ebeln = tbl_mseg-ebeln
                                   ebelp = tbl_mseg-ebelp
                                   binary search.
      if sy-subrc = 0.
        tbl_report-maktx = tbl_ekpo-txz01.
      endif.
    endif.

    append tbl_report.

  endloop.

  free: tbl_mseg, tbl_mkpf, tbl_makt, tbl_prps, tbl_ekpo.

endform.                    " CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*&      Form  CREATE_HARDCOPY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_hardcopy_report .

  loop at tbl_report.
    at new usnam.
      write: / tbl_report-usnam under text-001.
    endat.

    at new budat.
      write: / tbl_report-budat under text-002.
    endat.

    write: / tbl_report-mblnr      under text-003,
             tbl_report-werks      under text-004,
             tbl_report-lgort      under text-005,
             tbl_report-matnr      under text-006,
         (9) tbl_report-erfmg      under text-007 decimals 0, "4.6C Upgrade
             tbl_report-erfme      under text-008,
             tbl_report-bwart      under text-009,
             tbl_report-xblnr      under text-011,
             tbl_report-maktx(28)  under text-013,
             tbl_report-sakto      under text-015,
             tbl_report-aufnr      under text-016,
             tbl_report-ps_psp_pnr under text-017.

    at end of mblnr.
      add 1 to ctr.
    endat.

    at end of budat.
      skip.
      sum.
      write: /   '---------'   under text-007.
      write: /(9) tbl_report-erfmg  under text-007 decimals 0.
      skip.
      write: /1 'Number of Documents:', ctr under text-003.
      clear ctr.
      skip.
    endat.

  endloop.
  skip 2.
  write:/ v_total_quantity under text-007.
  write:/ text-t01, v_document_item_count under text-003.
  write:/ text-t02, v_unique_document_count under text-003.

  skip 2.
  write: text-end under text-ttl.


endform.                    " CREATE_HARDCOPY_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form build_fieldcat .

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'USNAM'.
  tbl_fieldcat-seltext_s = 'User ID'.
  tbl_fieldcat-seltext_m = 'User ID'.
  tbl_fieldcat-seltext_l = 'User ID'.
  tbl_fieldcat-reptext_ddic = 'User ID'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'BUDAT'.
  tbl_fieldcat-seltext_s = 'Post Date'.
  tbl_fieldcat-seltext_m = 'Posting Date'.
  tbl_fieldcat-seltext_l = 'Document Posting Date'.
  tbl_fieldcat-reptext_ddic = 'Document Posting Date'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'MBLNR'.
  tbl_fieldcat-seltext_s = 'Document #'.
  tbl_fieldcat-seltext_m = 'Document Number'.
  tbl_fieldcat-seltext_l = 'Document Number'.
  tbl_fieldcat-reptext_ddic = 'Document Number'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'BWART'.
  tbl_fieldcat-seltext_s = 'MvT'.
  tbl_fieldcat-seltext_m = 'Movement Type'.
  tbl_fieldcat-seltext_l = 'Movement Type'.
  tbl_fieldcat-reptext_ddic = 'Movement Type'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'WERKS'.
  tbl_fieldcat-seltext_s = 'Plnt'.
  tbl_fieldcat-seltext_m = 'Plnt'.
  tbl_fieldcat-seltext_l = 'Plnt'.
  tbl_fieldcat-reptext_ddic = 'Plnt'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'LGORT'.
  tbl_fieldcat-seltext_s = 'SLoc'.
  tbl_fieldcat-seltext_m = 'Storage Location'.
  tbl_fieldcat-seltext_l = 'Storage Location'.
  tbl_fieldcat-reptext_ddic = 'Storage Location'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'MATNR'.
  tbl_fieldcat-seltext_s = 'Mtl#'.
  tbl_fieldcat-seltext_m = 'Material Number'.
  tbl_fieldcat-seltext_l = 'Material Number'.
  tbl_fieldcat-reptext_ddic = 'Material Number'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'MAKTX'.
  tbl_fieldcat-seltext_s = 'Matl Desc'.
  tbl_fieldcat-seltext_m = 'Material Description'.
  tbl_fieldcat-seltext_l = 'Material Description'.
  tbl_fieldcat-reptext_ddic = 'Material Description'.
  tbl_fieldcat-just = 'L'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'ERFMG'.
  tbl_fieldcat-seltext_s = 'Qty'.
  tbl_fieldcat-seltext_m = 'Quantity'.
  tbl_fieldcat-seltext_l = 'Quantity'.
  tbl_fieldcat-reptext_ddic = 'Quantity'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-do_sum = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'ERFME'.
  tbl_fieldcat-seltext_s = 'UOM'.
  tbl_fieldcat-seltext_m = 'Unit of Measure'.
  tbl_fieldcat-seltext_l = 'Unit of Measure'.
  tbl_fieldcat-reptext_ddic = 'Unit of Measure'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'XBLNR'.
  tbl_fieldcat-seltext_s = 'Reference'.
  tbl_fieldcat-seltext_m = 'Reference'.
  tbl_fieldcat-seltext_l = 'Reference'.
  tbl_fieldcat-reptext_ddic = 'Reference'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'SAKTO'.
  tbl_fieldcat-seltext_s = 'PCE'.
  tbl_fieldcat-seltext_m = 'Cost Element'.
  tbl_fieldcat-seltext_l = 'Cost Element'.
  tbl_fieldcat-reptext_ddic = 'Cost Element'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'AUFNR'.
  tbl_fieldcat-seltext_s = 'Order'.
  tbl_fieldcat-seltext_m = 'Internal Order'.
  tbl_fieldcat-seltext_l = 'Internal Order'.
  tbl_fieldcat-reptext_ddic = 'Internal Order'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_ALV'.
  tbl_fieldcat-fieldname = 'PS_PSP_PNR'.
  tbl_fieldcat-seltext_s = 'WBS'.
  tbl_fieldcat-seltext_m = 'WBS Element'.
  tbl_fieldcat-seltext_l = 'WBS Element'.
  tbl_fieldcat-reptext_ddic = 'WBS Element'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

endform.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_EVENTS[]  text
*----------------------------------------------------------------------*
form build_events using rt_events type slis_t_event.

* Get all the events into itab rt_events
  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type = 0
    importing
      et_events   = rt_events.

endform.                    " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_grid .

* Populate the layout
  v_repid = sy-repid.
  st_layout-colwidth_optimize = 'X'.
  st_layout-detail_popup = 'X'.
  st_layout-no_keyfix = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY'
   exporting
     i_callback_program     = v_repid
*      i_callback_top_of_page = gv_top_of_page
     i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
     i_grid_title           = ''
     is_layout              = st_layout
     it_fieldcat            = tbl_fieldtab[]
     it_sort                = st_sort
     i_save                 = 'A'
     i_default              = 'X'
   tables
     t_outtab               = tbl_alv[]
   exceptions
     program_error          = 1
     others                 = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " DISPLAY_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form build_excel_table .

  perform setup_excel_header.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
    exporting
*     file_name                 = 'C:\SAPTEMP' "TR995
      file_name                 = P_FILE       "TR995
      create_pivot              = 0
    tables
      data_tab                  = tbl_alv
      fieldnames                = tbl_excel_header
    exceptions
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_pivot_fields      = 6
      download_problem          = 7
      others                    = 8.

  if sy-subrc <> 0.
    write: /1 'table download unsuccessful - reason = ', sy-subrc.
  endif.

endform.                    " BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*&      Form  SETUP_EXCEL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_excel_header .

  move 'User ID'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Date'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Document#'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MvT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Plnt'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'SLoc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Mtl#'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Material Description'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Qty'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'UOM'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Reference'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'PCE'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Order'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'WBS'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.

endform.                    " SETUP_EXCEL_HEADER

*&---------------------------------------------------------------------*
*&      Form  alv_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form alv_top_of_page.

* if you are printing the ALV, do not setup the header again
  if tbl_top_of_page[] is initial.

    move text-dte to v_head01+0(7).
    write sy-datum to v_head01+8(10).
    move text-amp  to v_head01+19(5).
    write sy-uzeit to v_head01+25(10).

    move text-clt  to v_head02+0(7).
    move sy-mandt  to v_head02+8(4).
    move sy-sysid  to v_head02+14(5).

*1- HEADING LINE: TYPE H
    clear st_line.
    st_line-typ  = 'H'.
    st_line-info = sy-title.             "sy-title.
    append st_line to tbl_top_of_page.

*2- SELECTION LINE: TYPE S
    clear st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info  = v_head01.
    append st_line to tbl_top_of_page.

*3- ACTION LINE:  TYPE A
    clear st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info = v_head02.
    append st_line to tbl_top_of_page.

  endif.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = tbl_top_of_page.

endform.                               " ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  LOAD_ALV_EXCEL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_alv_excel_table .

  clear tbl_alv.
  refresh tbl_alv.

  loop at tbl_report.

    move-corresponding tbl_report to tbl_alv.

    v_quantity = tbl_report-erfmg.                          "drop .000
    write v_quantity to tbl_alv-erfmg.
    if v_quantity < 0.
      shift tbl_alv-erfmg by 1 places right circular.
      condense tbl_alv-erfmg no-gaps.
      shift tbl_alv-erfmg right deleting trailing ' '.
    endif.

    write tbl_report-budat to tbl_alv-budat.

    append tbl_alv.

  endloop.

endform.                    " LOAD_ALV_EXCEL_TABLE
*&---------------------------------------------------------------------*
*&      Form  get_material_desc_and_wbs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_material_desc_and_wbs .

  sort tbl_matnr.
  delete adjacent duplicates from tbl_matnr.
  sort tbl_pspnr.
  delete adjacent duplicates from tbl_pspnr.
  sort tbl_ebeln.
  delete adjacent duplicates from tbl_ebeln.

  clear: v_matnr_count, v_pspnr_count, v_ebeln_count.
  clear: tbl_makt, s_matnr, tbl_prps, s_pspnr, tbl_ekpo, s_ebeln.
  refresh: tbl_makt, s_matnr, tbl_prps, s_pspnr, tbl_ekpo, s_ebeln.

** Get all the material descriptions

  loop at tbl_matnr.
    s_matnr-sign = 'I'.
    s_matnr-option = 'EQ'.
    s_matnr-low = tbl_matnr-matnr.
    append s_matnr.
    v_matnr_count = v_matnr_count + 1.
    if v_matnr_count > 2500.
      select matnr maktx
        appending table tbl_makt
        from makt
        where matnr in s_matnr
          and spras = 'EN'.
      refresh s_matnr.
      clear v_matnr_count.
    endif.
  endloop.

  if not s_matnr[] is initial.
    select matnr maktx
         appending table tbl_makt
         from makt
         where matnr in s_matnr
           and spras = 'EN'.
  endif.

  sort tbl_makt by matnr.

  free: tbl_matnr, s_matnr.

* get all WBS elements

  loop at tbl_pspnr.
    s_pspnr-sign = 'I'.
    s_pspnr-option = 'EQ'.
    s_pspnr-low = tbl_pspnr-pspnr.
    append s_pspnr.
    v_pspnr_count = v_pspnr_count + 1.
    if v_pspnr_count > 2500.
      select pspnr poski
        appending table tbl_prps
        from prps
        where pspnr in s_pspnr.
      refresh s_pspnr.
      clear v_pspnr_count.
    endif.
  endloop.

  if not s_pspnr[] is initial.
    select pspnr poski
           appending table tbl_prps
           from prps
           where pspnr in s_pspnr.
  endif.

  sort tbl_prps by pspnr.

  free: tbl_pspnr, s_pspnr.

* get the purchasing items

  loop at tbl_ebeln.
    s_ebeln-sign = 'I'.
    s_ebeln-option = 'EQ'.
    s_ebeln-low = tbl_ebeln-ebeln.
    append s_ebeln.
    v_ebeln_count = v_ebeln_count + 1.
    if v_ebeln_count > 2500.
      select ebeln ebelp txz01
        appending table tbl_ekpo
        from ekpo
        where ebeln in s_ebeln.
      refresh s_ebeln.
      clear v_ebeln_count.
    endif.
  endloop.

  if not s_ebeln[] is initial.
    select ebeln ebelp txz01
           appending table tbl_ekpo
           from ekpo
           where ebeln in s_ebeln.
  endif.

  sort tbl_ekpo by ebeln ebelp.

  free: tbl_ebeln, s_ebeln.

endform.                    " get_material_desc_and_wbs
*&---------------------------------------------------------------------*
*&      Form  SETUP_TEMPORARY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_temporary_data .

  if tbl_mseg-matnr <> ' '.
    if tbl_mseg-matnr <> tbl_matnr-matnr.
      tbl_matnr-matnr = tbl_mseg-matnr.
      append tbl_matnr.
    endif.
  else.
    if tbl_mseg-ebeln <> tbl_ebeln-ebeln.
      tbl_ebeln-ebeln = tbl_mseg-ebeln.
      append tbl_ebeln.
    endif.
  endif.

  if tbl_mseg-ps_psp_pnr <> ' '.
    if tbl_mseg-ps_psp_pnr <> tbl_pspnr-pspnr.
      tbl_pspnr-pspnr = tbl_mseg-ps_psp_pnr.
      append tbl_pspnr.
    endif.
  endif.

endform.                    " SETUP_TEMPORARY_DATA

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      LV_BOL TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = P_FILE
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

*lv_dir = sep_path.
IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
*Check if directory path exist or not.
CALL METHOD cl_gui_frontend_services=>directory_exist
  EXPORTING
    directory            = sep_path      "lv_dir
  RECEIVING
    result               = lv_bol
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    wrong_parameter      = 3
    not_supported_by_gui = 4
    OTHERS               = 5.
IF lv_bol IS INITIAL.
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.
