*&---------------------------------------------------------------------*
*&  Include            ZMMR_SRO_STATUS_REPORT_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZMMR_SRO_STATUS_REPORT_F01                     *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 20-Dec-2013                                    *
*& Object ID          : R_PTP_MM_0001_SRO Status Report                *
*& Application Area   : MM                                             *
*& Description        : Display SRO data retrieved from cross functional
*                       modules like PS,MM,AP in a single report.      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_F4_HELP_PRESENTATION
*&---------------------------------------------------------------------*
*       F4 help for file
*----------------------------------------------------------------------*
FORM get_f4_help_presentation .
  DATA: lv_len  TYPE i,
        lv_extn TYPE char5.

  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fpath    TYPE string,
        lv_mask     TYPE string,
        lv_cmask    TYPE char255.

  lv_cmask = 'XLSX'.
  CALL METHOD cl_alv_bds=>create_mask_for_filefilter
    EXPORTING
      i_frontend          = 'Y'
*    IMPORTING
*      e_default_extension = 'XLSX'
    CHANGING
      c_mask              = lv_cmask.

  lv_mask = lv_cmask.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = 'Save As'
      default_extension    = 'XLSX'
*     default_file_name    =
*     with_encoding        =
      file_filter          = lv_mask "cl_gui_frontend_services=>filetype_excel
*     initial_directory    =
      prompt_on_overwrite  = space
    CHANGING
      filename             = lv_filename
      path                 = lv_path
      fullpath             = lv_fpath
*     user_action          =
*     file_encoding        =
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  p_file = lv_fpath.

  lv_len = strlen( p_file ).

  IF lv_len GT 5.
    lv_len = lv_len - 5.
    lv_extn = p_file+lv_len(5).
    TRANSLATE lv_extn TO UPPER CASE.
    IF lv_extn NE '.XLSX'.
      lv_extn = '.XLSX'.
    ELSE.
      CLEAR: lv_extn.
    ENDIF.
  ELSE.
    lv_extn = '.XLSX'.
  ENDIF.
  IF lv_extn NE space AND p_file IS NOT INITIAL.
    CONCATENATE p_file lv_extn INTO p_file.
  ENDIF.

ENDFORM.                    " GET_F4_HELP_PRESENTATION
*&---------------------------------------------------------------------*
*&      Form  GET_SRO_DETAILS
*&---------------------------------------------------------------------*
*       Get PO details
*----------------------------------------------------------------------*
FORM get_sro_details .
* Get PO header details
  SELECT ebeln
         bsart
         lifnr
         ekgrp
         kdatb
         kdate
         zzariba_approver
         FROM ekko
         INTO TABLE lt_ekko
         WHERE ebeln IN s_ebeln
         AND bukrs IN s_bukrs
         AND bsart IN s_bsart
         AND loekz = space
         AND lifnr IN s_lifnr
         AND ekgrp IN s_ekgrp
         AND bedat IN s_bedat
         AND kdate IN s_kdate.

  IF lt_ekko IS NOT INITIAL.
*   Get Vendor details
    SELECT lifnr
           name1
           FROM lfa1
           INTO TABLE lt_lfa1
           FOR ALL ENTRIES IN lt_ekko
           WHERE lifnr = lt_ekko-lifnr.

*   Get PO item details
    SELECT ebeln
           ebelp
           loekz
           txz01
           bukrs
           bednr
           netwr
           elikz
           erekz
           wepos
           packno
           afnam
           FROM ekpo
           INTO TABLE lt_ekpo
           FOR ALL ENTRIES IN lt_ekko
           WHERE ebeln = lt_ekko-ebeln
           AND knttp IN s_knttp.
*           AND vrtkz = space.
  ELSE.
    MESSAGE 'No data selected'(030) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  IF lt_ekpo IS NOT INITIAL.
*   Get Ext. Services Management data
    SELECT packno
           sumlimit
           commitment
           actvalue
           FROM esuh
           INTO TABLE lt_esuh
           FOR ALL ENTRIES IN lt_ekpo
           WHERE packno = lt_ekpo-packno.

*   Get Account Assignment in Purchasing document
    SELECT ebeln
           ebelp
           sakto
           kostl
           ps_psp_pnr
           FROM ekkn
           INTO TABLE lt_ekkn
           FOR ALL ENTRIES IN lt_ekpo
           WHERE ebeln = lt_ekpo-ebeln
           AND ebelp = lt_ekpo-ebelp
           AND sakto IN s_sakto
           AND kostl IN s_kostl
           AND ps_psp_pnr IN s_wbs .

    IF lt_ekkn IS NOT INITIAL.
*     Get WBS element and Project details
      SELECT a~pspnr
             a~posid
             a~psphi
             b~pspnr
             b~post1
             FROM prps AS a
             INNER JOIN proj AS b
             ON a~psphi = b~pspnr
             INTO TABLE lt_prps
             FOR ALL ENTRIES IN lt_ekkn
             WHERE a~pspnr = lt_ekkn-ps_psp_pnr
             AND b~pspnr IN s_pspnr.
    ENDIF.

*   Get PO history details
    SELECT ebeln
           ebelp
           vgabe
           gjahr
           belnr
           buzei
           FROM ekbe
           INTO TABLE lt_ekbe
           FOR ALL ENTRIES IN lt_ekpo
           WHERE ebeln = lt_ekpo-ebeln
           AND ebelp = lt_ekpo-ebelp
           AND vgabe IN ('2', 'P').
    IF sy-subrc = 0.
      SORT lt_ekbe BY ebeln ebelp gjahr belnr buzei.
      DELETE ADJACENT DUPLICATES FROM lt_ekbe.
    ENDIF.
  ELSE.
    MESSAGE 'No data selected'(030) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  IF lt_ekbe IS NOT INITIAL.
    LOOP AT lt_ekbe INTO lwa_ekbe.
      MOVE-CORRESPONDING lwa_ekbe TO lwa_ekbe_tmp.
      lwa_ekbe_tmp-buzei = lwa_ekbe-buzei.
      APPEND lwa_ekbe_tmp TO lt_ekbe_tmp.
      CLEAR lwa_ekbe_tmp.
    ENDLOOP.
*   Get Account assignment details in Invioce document
    SELECT belnr
           gjahr
           buzei
           kostl
           ps_psp_pnr
           saknr
           wrbtr
           shkzg
           xunpl
           FROM rbco
           INTO TABLE lt_rbco
           FOR ALL ENTRIES IN lt_ekbe_tmp
           WHERE belnr = lt_ekbe_tmp-belnr
           AND gjahr = lt_ekbe_tmp-gjahr
           AND buzei = lt_ekbe_tmp-buzei.
    IF sy-subrc = 0.
      SORT lt_rbco BY belnr gjahr buzei xunpl DESCENDING.
    ENDIF.

*   Get Invoice header document
    SELECT belnr
           gjahr
           blart
           xblnr
           FROM rbkp
           INTO TABLE lt_rbkp
           FOR ALL ENTRIES IN lt_ekbe
           WHERE belnr = lt_ekbe-belnr
           AND gjahr = lt_ekbe-gjahr.

*   Get Invoice item document
    SELECT belnr
           gjahr
           buzei
           wrbtr
           shkzg
           FROM rseg
           INTO TABLE lt_rseg
           FOR ALL ENTRIES IN lt_ekbe_tmp
           WHERE belnr = lt_ekbe_tmp-belnr
           AND gjahr = lt_ekbe_tmp-gjahr
           AND buzei = lt_ekbe_tmp-buzei.
  ENDIF.

ENDFORM.                    " GET_SRO_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       Get output data
*----------------------------------------------------------------------*
FORM get_output_data .
  DATA: lv_os_commit TYPE commitment. "Oustanding commitment
  DATA: lv_ekbe_tabix TYPE sy-tabix,
        lv_rbco_tabix TYPE sy-tabix.

  SORT lt_ekpo BY ebeln ebelp.
  SORT lt_ekbe_tmp BY ebeln ebelp belnr gjahr buzei.
  SORT lt_rbco BY belnr gjahr buzei.

  LOOP AT lt_ekpo INTO lwa_ekpo.
    CLEAR: lwa_output.
    READ TABLE lt_ekko INTO lwa_ekko WITH KEY ebeln = lwa_ekpo-ebeln.
    IF sy-subrc = 0.
      lwa_output-ekgrp = lwa_ekko-ekgrp.
      CONCATENATE lwa_ekko-kdatb(4) '/'
                  lwa_ekko-kdatb+4(2) '/'
                  lwa_ekko-kdatb+6(2)
                  INTO lwa_output-kdatb.
      CONCATENATE lwa_ekko-kdate(4) '/'
                  lwa_ekko-kdate+4(2) '/'
                  lwa_ekko-kdate+6(2)
                  INTO lwa_output-kdate.
      lwa_output-zzariba_approver = lwa_ekko-zzariba_approver.
      READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko-lifnr.
      IF sy-subrc = 0.
        lwa_output-name1 = lwa_lfa1-name1.
      ENDIF.
    ENDIF.
    READ TABLE lt_esuh INTO lwa_esuh WITH KEY packno = lwa_ekpo-packno.
    IF sy-subrc = 0.
      lwa_output-sumlimit = lwa_esuh-sumlimit.
      lwa_output-commitment = lwa_esuh-commitment.
    ENDIF.
    READ TABLE lt_ekkn INTO lwa_ekkn WITH KEY ebeln = lwa_ekpo-ebeln
                                              ebelp = lwa_ekpo-ebelp.
    IF sy-subrc = 0.
      IF lwa_ekkn-ps_psp_pnr = '00000000'.
        lwa_output-ps_psp_pnr   = space.
      ELSE.
        WRITE lwa_ekkn-ps_psp_pnr TO lwa_output-ps_psp_pnr .
      ENDIF.
      lwa_output-sakto = lwa_ekkn-sakto.
      lwa_output-kostl = lwa_ekkn-kostl.
      READ TABLE lt_prps INTO lwa_prps WITH KEY pspnr = lwa_ekkn-ps_psp_pnr.
      IF sy-subrc = 0.
        WRITE lwa_prps-pspnr_pr TO lwa_output-pspnr_pr.
        lwa_output-post1 = lwa_prps-post1.
      ENDIF.
    ENDIF.

    lwa_output-afnam = lwa_ekpo-afnam.
    lwa_output-ebeln = lwa_ekpo-ebeln.
    lwa_output-txz01 = lwa_ekpo-txz01.
    lwa_output-bukrs = lwa_ekpo-bukrs.
    lwa_output-erekz = lwa_ekpo-erekz.
    lwa_output-netwr = lwa_ekpo-netwr.
    lwa_output-bednr = lwa_ekpo-bednr.
    IF lwa_ekpo-loekz IS INITIAL.
      lwa_output-loekz = 'Active'(060).
    ELSE.
      lwa_output-loekz = 'Inactive'(061).
    ENDIF.

*   Delivery options - all items, completed items, undelivered items
    IF p_delall = 'X'.                               "All
    ELSEIF p_delvry = 'X' AND lwa_ekpo-elikz = 'X'.  "Del complete
    ELSEIF p_undelr = 'X' AND lwa_ekpo-elikz = ' '.  "Undelivered
    ELSE.
      CONTINUE.
    ENDIF.

*   Final invoices - all items, final invoicess, non-final invoices
    IF p_finall = 'X'.                               "All
    ELSEIF p_fin = 'X' AND lwa_ekpo-erekz = 'X'.     "Final Invoices
    ELSEIF p_nonfin = 'X' AND lwa_ekpo-erekz = ' '.  "Undelivered
    ELSE.
      CONTINUE.
    ENDIF.

*   Deleted Items - all items, deleted items, non-deleted items
    IF p_dltall = 'X'.                               "All
    ELSEIF p_delt = 'X' AND lwa_ekpo-loekz = 'L'.    "Deleted items
    ELSEIF p_nondel = 'X' AND lwa_ekpo-loekz = ' '.  "Non-deleted
    ELSE.
      CONTINUE.
    ENDIF.

*   Goods Receipts - all items, received, no receipts items
    IF p_grall = 'X'.                                "All
    ELSEIF p_gron = 'X' AND lwa_ekpo-wepos = 'X'.    "Received items
    ELSEIF p_groff = 'X' AND lwa_ekpo-wepos = ' '.   "No receipts
    ELSE.
      CONTINUE.
    ENDIF.

*   Outstanding commitment - all items, positive amts, zero/negative amts.
    IF p_comall = 'X'.                                "All
    ELSEIF p_compos = 'X' AND lv_os_commit > 0.       "Positive amts
    ELSEIF p_comneg = 'X' AND lv_os_commit <= 0.      "Negative amts
    ELSE.
      CONTINUE.
    ENDIF.
    lwa_output1 = lwa_output.
    READ TABLE lt_ekbe_tmp INTO lwa_ekbe_tmp WITH KEY ebeln = lwa_ekpo-ebeln
                                                      ebelp = lwa_ekpo-ebelp
                                                      BINARY SEARCH.
    IF sy-subrc = 0.
      lv_ekbe_tabix = sy-tabix.
      LOOP AT lt_ekbe_tmp INTO lwa_ekbe_tmp FROM lv_ekbe_tabix.
        IF ( lwa_ekbe_tmp-ebeln <> lwa_ekpo-ebeln OR lwa_ekbe_tmp-ebelp <> lwa_ekpo-ebelp ).
          EXIT.
        ENDIF.
        lwa_output = lwa_output1.
        READ TABLE lt_rbkp INTO lwa_rbkp WITH KEY belnr = lwa_ekbe_tmp-belnr
                                                  gjahr = lwa_ekbe_tmp-gjahr.
        IF sy-subrc = 0.
          lwa_output-xblnr = lwa_rbkp-xblnr.
          lwa_output-blart = lwa_rbkp-blart.
          lwa_output-belnr = lwa_rbkp-belnr.
        ENDIF.
        READ TABLE lt_rbco INTO lwa_rbco WITH KEY belnr = lwa_ekbe_tmp-belnr
                                                  gjahr = lwa_ekbe_tmp-gjahr
                                                  buzei = lwa_ekbe_tmp-buzei
                                                  BINARY SEARCH.
        IF sy-subrc = 0.
          lv_rbco_tabix = sy-tabix.
          LOOP AT lt_rbco INTO lwa_rbco FROM lv_rbco_tabix.
            IF ( lwa_rbco-belnr <> lwa_ekbe_tmp-belnr OR
               lwa_rbco-gjahr <> lwa_ekbe_tmp-gjahr OR
               lwa_rbco-buzei <> lwa_ekbe_tmp-buzei ).
              EXIT.
            ENDIF.
            WRITE lwa_rbco-ps_psp_pnr TO lwa_output-ps_psp_pnr_in.
            lwa_output-saknr = lwa_rbco-saknr.
            lwa_output-kostl_in = lwa_rbco-kostl.
            IF lwa_rbco-xunpl = space.
              READ TABLE lt_rseg INTO lwa_rseg WITH KEY belnr = lwa_ekbe_tmp-belnr
                                                        gjahr = lwa_ekbe_tmp-gjahr
                                                        buzei = lwa_ekbe_tmp-buzei.
              IF sy-subrc = 0.
                IF lwa_rseg-shkzg = 'H'.
                  lwa_rseg-wrbtr = lwa_rseg-wrbtr * -1.
                ENDIF.
*               Remaining balance
                CLEAR lv_os_commit.
                lv_os_commit = lwa_ekpo-netwr - lwa_rseg-wrbtr.
                MOVE lv_os_commit TO lwa_output-rem_bal.

*               Percentage spent
                IF lwa_ekpo-netwr IS NOT INITIAL.
                  lwa_output-per_spent = ( lwa_rseg-wrbtr / lwa_ekpo-netwr ) * 100.
                ENDIF.
              ENDIF.
            ELSE.
              CLEAR: lwa_output-per_spent,
                     lwa_output-rem_bal.
            ENDIF.
            IF lwa_rbco-shkzg = 'H'.
              lwa_rbco-wrbtr = lwa_rbco-wrbtr * -1.
            ENDIF.
            lwa_output-wrbtr = lwa_rbco-wrbtr.
            APPEND lwa_output TO lt_output.
            CLEAR: lwa_rbco,
                   lwa_rseg.
          ENDLOOP.
          CLEAR: lv_rbco_tabix.
        ELSE.
          READ TABLE lt_rseg INTO lwa_rseg WITH KEY belnr = lwa_ekbe_tmp-belnr
                                                    gjahr = lwa_ekbe_tmp-gjahr
                                                    buzei = lwa_ekbe_tmp-buzei.
          IF sy-subrc = 0.
            IF lwa_rseg-shkzg = 'H'.
              lwa_rseg-wrbtr = lwa_rseg-wrbtr * -1.
            ENDIF.
            lwa_output-wrbtr = lwa_rseg-wrbtr.
*           Remaining balance
            CLEAR lv_os_commit.
            lv_os_commit = lwa_ekpo-netwr - lwa_rseg-wrbtr.
            MOVE lv_os_commit TO lwa_output-rem_bal.
*           Percentage spent
            IF lwa_ekpo-netwr IS NOT INITIAL.
              lwa_output-per_spent = ( lwa_rseg-wrbtr / lwa_ekpo-netwr ) * 100.
            ENDIF.
          ENDIF.
          APPEND lwa_output TO lt_output.
        ENDIF.
        CLEAR: lwa_ekbe_tmp,
               lwa_rbkp,
               lwa_rbco,
               lwa_rseg,
               lwa_output.
      ENDLOOP.
      CLEAR lv_ekbe_tabix.
    ENDIF.
    CLEAR: lwa_ekbe_tmp,
           lwa_output,
           lwa_ekko,
           lwa_ekpo,
           lwa_ekkn,
           lwa_prps,
           lwa_lfa1,
           lwa_esuh.
  ENDLOOP.
  REFRESH: lt_ekko,
           lt_ekpo,
           lt_ekkn,
           lt_prps,
           lt_ekbe,
           lt_rbco,
           lt_rbkp,
           lt_rseg,
           lt_esuh,
           lt_lfa1.

  SORT lt_output BY afnam.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       Diplay output
*----------------------------------------------------------------------*
FORM display_output .
  IF p_rprt = 'X'.
    PERFORM create_output_report.
  ELSE.
    PERFORM create_spreadsheet.
  ENDIF.
  CLEAR:  p_file,
          lt_output.
  REFRESH lt_output.
ENDFORM.                    " DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_OUTPUT_REPORT
*&---------------------------------------------------------------------*
*       Report
*----------------------------------------------------------------------*
FORM create_output_report .
* Build Fieldnames table
  PERFORM build_fieldcatalog.

* Sort by Requisitioner
  CLEAR lwa_sort.
  lwa_sort-fieldname = 'AFNAM'.
  APPEND lwa_sort TO lit_sort.

* Optimize column width
  lwa_layout-colwidth_optimize = 'X'.
*  SET COUNTRY 'US'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = 'ZMMR_SRO_STATUS_REPORT'
      i_callback_top_of_page = 'DISPLAY_TOP_OF_PAGE'
      is_layout              = lwa_layout
      it_fieldcat            = lt_fieldcat[]
      it_sort                = lit_sort
    TABLES
      t_outtab               = lt_output[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " CREATE_OUTPUT_REPORT
*&---------------------------------------------------------------------*
*&      Form  CREATE_SPREADSHEET
*&---------------------------------------------------------------------*
*       Excel file
*----------------------------------------------------------------------*
FORM create_spreadsheet .
  DATA: lwa_layout_exl TYPE lvc_s_layo,
        lit_sort_exl   TYPE lvc_t_sort,
        lwa_sort_exl   TYPE lvc_s_sort.
  DATA: lv_len TYPE i,
        lv_extn TYPE char5,
        lv_filetype TYPE salv_bs_constant.

* Build Fieldnames table
  PERFORM build_fieldcatalog_exl.

* Check for File extensions - XLS/XLSX
  lv_len = strlen( p_file ).

  IF lv_len GT 5.
    lv_len = lv_len - 5.
    lv_extn = p_file+lv_len(5).
    TRANSLATE lv_extn TO UPPER CASE.
    IF lv_extn NE '.XLSX'.
      lv_extn = '.XLSX'.
    ELSE.
      CLEAR: lv_extn.
    ENDIF.
  ELSE.
    lv_extn = '.XLSX'.
  ENDIF.

  IF lv_extn NE space.
    CONCATENATE p_file lv_extn INTO p_file.
  ENDIF.

  lwa_sort_exl-fieldname = 'AFNAM'.
  APPEND lwa_sort_exl TO lit_sort_exl.
  lwa_layout_exl-cwidth_opt = 'X'.

  CALL METHOD zcl_utilities=>create_xls_from_itab
    EXPORTING
      i_filename  = p_file
      it_fieldcat = lt_fieldcat_exl[]
      it_sort     = lit_sort_exl
*     it_filt     =
      is_layout   = lwa_layout_exl
      i_filetype  = if_salv_bs_xml=>c_type_xlsx "lv_filetype  "IF_SALV_BS_XML=>C_TYPE_EXCEL_XML
    CHANGING
      ct_data     = lt_output.

ENDFORM.                    " CREATE_SPREADSHEET
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Display Top of page
*----------------------------------------------------------------------*
FORM display_top_of_page .
  IF lt_listheader[] IS INITIAL.
    lwa_listheader-typ               ='H'.
    lwa_listheader-info             = 'SRO Status Report'.
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.
*   Write List header
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_listheader.
  ENDIF.

ENDFORM.                    " DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  PERFORM create_catalog USING : 'AFNAM' 'Requistioner'(031).
  PERFORM create_catalog USING : 'EKGRP' 'Buyer'(032).
  PERFORM create_catalog USING : 'EBELN' 'SRO Number'(033).
  PERFORM create_catalog USING : 'TXZ01' 'Description'(034).
  PERFORM create_catalog USING : 'NAME1' 'Vendor Name'(035).
  PERFORM create_catalog USING : 'BUKRS' 'Company Code'(036).
  PERFORM create_catalog USING : 'PSPNR_PR' 'Project'(037).
  PERFORM create_catalog USING : 'POST1' 'Project/Cost CenterDescription'(038).
  PERFORM create_catalog USING : 'PS_PSP_PNR' 'PO WBS Element'(039).
  PERFORM create_catalog USING : 'SAKTO' 'PO G/L Account'(040).
  PERFORM create_catalog USING : 'KOSTL' 'PO Cost Center'(041).
  PERFORM create_catalog USING : 'PS_PSP_PNR_IN' 'INV WBS Element'(057).
  PERFORM create_catalog USING : 'SAKNR' 'INV G/L Account'(058).
  PERFORM create_catalog USING : 'KOSTL_IN' 'INV Cost Center'(059).
  PERFORM create_catalog USING : 'KDATB' 'Start date (SRO)'(042).
  PERFORM create_catalog USING : 'KDATE' 'End Date (SRO)'(043).
  PERFORM create_catalog USING : 'ZZARIBA_APPROVER' 'Service Confirmer'(044).
  PERFORM create_catalog USING : 'BELNR' 'Invoice Document No'(045).
  PERFORM create_catalog USING : 'EREKZ' 'Final Invoice'(046).
  PERFORM create_catalog USING : 'BLART' 'Invoice Document Type'(047).
  PERFORM create_catalog USING : 'XBLNR' 'Invoice Ref #'(048).
  PERFORM create_catalog USING : 'NETWR' 'SRO Amount (Line Value)'(049).
  PERFORM create_catalog USING : 'WRBTR' 'Invoice Amount'(050).
  PERFORM create_catalog USING : 'REM_BAL' 'Remaining Balance (Commitments)'(051).
  PERFORM create_catalog USING : 'PER_SPENT' '% of SRO Spent'(052).
  PERFORM create_catalog USING : 'SUMLIMIT' 'Over all Limit(Do not exceed amt on SRO)'(053).
  PERFORM create_catalog USING : 'COMMITMENT' 'Expected Value'(054).
  PERFORM create_catalog USING : 'LOEKZ' 'SRO Active/Inactive?'(055).
  PERFORM create_catalog USING : 'BEDNR' 'Track #'(056).

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG
*&---------------------------------------------------------------------*
*       Create catalog
*----------------------------------------------------------------------*
FORM create_catalog  USING    iv_fieldname TYPE slis_fieldname
                              iv_seltext   TYPE scrtext_l.
  DATA: lwa_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lwa_fieldcat.
  lwa_fieldcat-fieldname = iv_fieldname.
  lwa_fieldcat-seltext_l = iv_seltext.
  APPEND lwa_fieldcat TO lt_fieldcat.
ENDFORM.                    " CREATE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_EXL
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for Excel
*----------------------------------------------------------------------*
FORM build_fieldcatalog_exl .
  PERFORM create_catalog_exl USING : 'AFNAM' 'Requistioner'(031).
  PERFORM create_catalog_exl USING : 'EKGRP' 'Buyer'(032).
  PERFORM create_catalog_exl USING : 'EBELN' 'SRO Number'(033).
  PERFORM create_catalog_exl USING : 'TXZ01' 'Description'(034).
  PERFORM create_catalog_exl USING : 'NAME1' 'Vendor Name'(035).
  PERFORM create_catalog_exl USING : 'BUKRS' 'Company Code'(036).
  PERFORM create_catalog_exl USING : 'PSPNR_PR' 'Project'(037).
  PERFORM create_catalog_exl USING : 'POST1' 'Project/Cost CenterDescription'(038).
  PERFORM create_catalog_exl USING : 'PS_PSP_PNR' 'PO WBS Element'(039).
  PERFORM create_catalog_exl USING : 'SAKTO' 'PO G/L Account'(040).
  PERFORM create_catalog_exl USING : 'KOSTL' 'PO Cost Center'(041).
  PERFORM create_catalog_exl USING : 'PS_PSP_PNR_IN' 'INV WBS Element'(057).
  PERFORM create_catalog_exl USING : 'SAKNR' 'INV G/L Account'(058).
  PERFORM create_catalog_exl USING : 'KOSTL_IN' 'INV Cost Center'(059).
  PERFORM create_catalog_exl USING : 'KDATB' 'Start date (SRO)'(042).
  PERFORM create_catalog_exl USING : 'KDATE' 'End Date (SRO)'(043).
  PERFORM create_catalog_exl USING : 'ZZARIBA_APPROVER' 'Service Confirmer'(044).
  PERFORM create_catalog_exl USING : 'BELNR' 'Invoice Document No'(045).
  PERFORM create_catalog_exl USING : 'EREKZ' 'Final Invoice'(046).
  PERFORM create_catalog_exl USING : 'BLART' 'Invoice Document Type'(047).
  PERFORM create_catalog_exl USING : 'XBLNR' 'Invoice Ref #'(048).
  PERFORM create_catalog_exl USING : 'NETWR' 'SRO Amount (Line Value)'(049).
  PERFORM create_catalog_exl USING : 'WRBTR' 'Invoice Amount'(050).
  PERFORM create_catalog_exl USING : 'REM_BAL' 'Remaining Balance (Commitments)'(051).
  PERFORM create_catalog_exl USING : 'PER_SPENT' '% of SRO Spent'(052).
  PERFORM create_catalog_exl USING : 'SUMLIMIT' 'Over all Limit(Do not exceed amt on SRO)'(053).
  PERFORM create_catalog_exl USING : 'COMMITMENT' 'Expected Value'(054).
  PERFORM create_catalog_exl USING : 'LOEKZ' 'SRO Active/Inactive?'(055).
  PERFORM create_catalog_exl USING : 'BEDNR' 'Track #'(056).

ENDFORM.                    " BUILD_FIELDCATALOG_EXL
*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG_EXL
*&---------------------------------------------------------------------*
*       Create catalog for Excel
*----------------------------------------------------------------------*
FORM create_catalog_exl  USING iv_fieldname TYPE lvc_fname
                               iv_seltext   TYPE scrtext_l.
  DATA: lwa_fieldcat_exl TYPE lvc_s_fcat.

  CLEAR lwa_fieldcat_exl.
  lwa_fieldcat_exl-fieldname = iv_fieldname.
  lwa_fieldcat_exl-scrtext_s = iv_seltext.
  lwa_fieldcat_exl-scrtext_m = iv_seltext.
  lwa_fieldcat_exl-scrtext_l = iv_seltext.
  lwa_fieldcat_exl-col_opt = abap_true.
  lwa_fieldcat_exl-coltext = iv_seltext.
  APPEND lwa_fieldcat_exl TO lt_fieldcat_exl.

ENDFORM.                    " CREATE_CATALOG_EXL
