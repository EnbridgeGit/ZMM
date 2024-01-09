*&---------------------------------------------------------------------*
*&  Include           ZMMR_SC_STATUS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZMMR_SC_STATUS_F01                             *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 17-Jul-2014                                    *
*& Object ID          : SDP70149: PM-PR to SRM reference field         *
*& Application Area   : MM                                             *
*& Description        : Report displays Shopping cart status in ECC    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_details .
  CLEAR: lv_wo,
         lv_pr,
         lv_po,
         lv_sc.

* Get SRM RFC destination
  SELECT SINGLE value1
    FROM zvarsys
    INTO lv_rfcdest
    WHERE programm = lc_prosrm
      AND varname  = lc_varsrm
      AND varnum   = lc_varnum.
  IF sy-subrc <> 0.
  ENDIF.

* Get logical system
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.
  IF sy-subrc <> 0.
  ENDIF.

  IF s_aufnr IS NOT INITIAL  OR s_erdat IS NOT INITIAL "OR s_ddate IS NOT INITIAL
   OR s_auart IS NOT INITIAL OR s_ernam IS NOT INITIAL OR s_swerk IS NOT INITIAL
   OR s_iwerk IS NOT INITIAL.
    lv_wo = 'X'.
    PERFORM get_wo_details.
  ELSEIF s_banfn IS NOT INITIAL OR s_badat IS NOT INITIAL OR s_prcrt IS NOT INITIAL
    OR s_prnam IS NOT INITIAL OR s_bednr IS NOT INITIAL.
    lv_pr = 'X'.
    PERFORM get_pr_details.
  ELSEIF s_ebeln IS NOT INITIAL OR s_aedat IS NOT INITIAL OR s_crnam IS NOT INITIAL
    OR s_matnr IS NOT INITIAL   OR s_wbs IS NOT INITIAL   OR s_kostl IS NOT INITIAL
    OR s_knttp IS NOT INITIAL   OR s_ekgrp IS NOT INITIAL OR s_arbap IS NOT INITIAL
    OR s_afnam IS NOT INITIAL   OR s_lifnr IS NOT INITIAL.
    lv_po = 'X'.
    PERFORM get_po_details.
  ELSEIF s_objid IS NOT INITIAL OR s_cdate IS NOT INITIAL OR s_crtby IS NOT INITIAL.
    lv_sc = 'X'.
    PERFORM get_sc_details.
  ENDIF.

ENDFORM.                    " GET_DETAILS
*&---------------------------------------------------------------------*
*&      Form  FIELD_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM field_validation .
  DATA: lv_count TYPE i,
        lt_date TYPE ty_date_range.

  IF sy-ucomm IN lt_ucomm.
    RETURN.
  ENDIF.
  IF s_aufnr[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_erdat[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
*  IF s_ddate[] IS NOT INITIAL.
*    lv_count = lv_count + 1.
*  ENDIF.
  IF s_auart[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_womat[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_ernam[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_swerk[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_iwerk[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_banfn[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_badat[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
* BOI by PANUSURI Ticket 75531
  IF s_prcrt[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_prnam[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_bednr[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
* EOI by PANUSURI Ticket 75531
  IF s_ebeln[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_aedat[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_crnam[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_matnr[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_wbs[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_kostl[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_knttp[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_ekgrp[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_arbap[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_afnam[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_lifnr[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_objid[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_cdate[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.
  IF s_crtby[] IS NOT INITIAL.
    lv_count = lv_count + 1.
  ENDIF.

  IF lv_count < 2.
    MESSAGE 'Enter atleast two fields on the selection screen'(t02) TYPE 'E'.
  ENDIF.

  IF s_erdat[] IS NOT INITIAL.
    lt_date[] = s_erdat[].
    PERFORM f_date_range_check USING lt_date.
  ENDIF.
  IF s_cdate[] IS NOT INITIAL.
    lt_date = s_cdate[].
    PERFORM f_date_range_check USING lt_date.
  ENDIF.
  IF s_aedat[] IS NOT INITIAL.
    lt_date = s_aedat[].
    PERFORM f_date_range_check USING lt_date.
  ENDIF.
  IF s_badat[] IS NOT INITIAL.
    lt_date = s_badat[].
    PERFORM f_date_range_check USING lt_date.
  ENDIF.

ENDFORM.                    " FIELD_VALIDATION
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_output_data .
  DATA: lr_objid  TYPE RANGE OF crmd_orderadm_h-object_id,
        lwa_objid LIKE LINE OF lr_objid.
  FIELD-SYMBOLS: <lfs_output> TYPE ty_output.
**--START OF CHANGES BY AKMADASU CHG0116780
  TYPES: BEGIN OF LTY_USR21,
         BNAME       TYPE XUBNAME,
         PERSNUMBER  TYPE AD_PERSNUM,
         END OF LTY_USR21,
         BEGIN OF LTY_ADRP,
           PERSNUMBER type AD_PERSNUM,
           NAME_TEXT type  AD_NAMTEXT,
         END OF LTY_ADRP,
         BEGIN OF LTY_ADR6,
         PERSNUMBER type AD_PERSNUM,
         SMTP_ADDR  type AD_SMTPADR,
         END OF LTY_ADR6.

  DATA:LT_USR21 TYPE TABLE OF LTY_USR21,
       LT_ADRP  TYPE TABLE OF LTY_ADRP,
       LT_ADR6  TYPE TABLE OF LTY_ADR6,
       LS_ADRP  TYPE LTY_ADRP,
       LS_ADR6  TYPE LTY_ADR6,
       LS_USR21 TYPE LTY_USR21.
**--END OF CHANGES BY AKMADASU CHG0116780
  IF lv_wo = 'X'.
    PERFORM get_wo_output_data.
  ELSEIF lv_pr = 'X'.
    PERFORM get_pr_output_data.
  ELSEIF lv_po = 'X'.
    PERFORM get_po_output_data.
  ELSEIF lv_sc = 'X'.
    PERFORM get_sc_output_data.
  ENDIF.
**--START OF CHANGES BY AKMADASU CHG0116780
    CLEAR: LT_USR21[],LT_ADRP[],LT_ADR6[].
  IF LT_OUTPUT IS NOT INITIAL.
    SELECT BNAME PERSNUMBER FROM USR21 INTO TABLE LT_USR21
                            FOR ALL ENTRIES IN LT_OUTPUT
                            WHERE BNAME = LT_OUTPUT-PRCRT.
    IF LT_USR21 IS NOT INITIAL.
      SORT LT_USR21 BY BNAME.
      SELECT PERSNUMBER NAME_TEXT FROM ADRP INTO TABLE LT_ADRP
                                  FOR ALL ENTRIES IN LT_USR21
                                  WHERE PERSNUMBER = LT_USR21-PERSNUMBER.
      IF SY-SUBRC IS INITIAL.
        SORT LT_ADRP BY PERSNUMBER.
      ENDIF.
      SELECT PERSNUMBER SMTP_ADDR FROM ADR6 INTO TABLE LT_ADR6
                                  FOR ALL ENTRIES IN LT_USR21
                                  WHERE PERSNUMBER = LT_USR21-PERSNUMBER.
      IF SY-SUBRC IS INITIAL.
        SORT LT_ADR6 BY PERSNUMBER.
      ENDIF.
    ENDIF.
  ENDIF.
**-- END OF CHNAGES BY AKMADASU  CHG0116780
  IF lt_output IS NOT INITIAL AND lv_sc = space.
    LOOP AT lt_output ASSIGNING <lfs_output>.
**-- START OF CHNAGES BY AKMADASU  CHG0116780
      READ TABLE LT_USR21 INTO LS_USR21 WITH KEY BNAME = <lfs_output>-PRCRT BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        READ TABLE LT_ADRP INTO LS_ADRP WITH KEY PERSNUMBER  = LS_USR21-PERSNUMBER BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          <lfs_output>-NAME_TEXT = LS_ADRP-NAME_TEXT.
          clear:LS_ADRP.
        ENDIF.
        READ TABLE LT_ADR6 INTO LS_ADR6 WITH KEY PERSNUMBER  = LS_USR21-PERSNUMBER BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          <lfs_output>-SMTP_ADDR = LS_ADR6-SMTP_ADDR.
          CLEAR: LS_ADR6.
        ENDIF.
        CLEAR:LS_USR21.
      ENDIF.
**-- END OF CHNAGES BY AKMADASU  CHG0116780
      IF <lfs_output>-object_id IS NOT INITIAL.
        CLEAR:   lwa_objid,
                 lr_objid,
                 lwa_sc_details.
        REFRESH: lr_objid,
                 lt_sc_details.
        lwa_objid-sign = 'I'.
        lwa_objid-option = 'EQ'.
        lwa_objid-low = <lfs_output>-object_id.
        APPEND lwa_objid TO lr_objid.

*       Get shopping cart details from SRM system
        CALL FUNCTION 'ZSRM_SC_DETAILS' DESTINATION lv_rfcdest
          EXPORTING
            ex_logsys     = lv_logsys
          TABLES
            it_object_id  = lr_objid
            et_sc_details = lt_sc_details.

        READ TABLE lt_sc_details INTO lwa_sc_details INDEX 1.
        IF sy-subrc = 0.
          <lfs_output>-crt_date = lwa_sc_details-posting_date.
          <lfs_output>-crt_by = lwa_sc_details-created_by.
          <lfs_output>-stat = lwa_sc_details-status.
          <lfs_output>-cur_appr = lwa_sc_details-current_approver.
          <lfs_output>-next_appr = lwa_sc_details-next_approver.
        ENDIF.

        IF lt_sc_details IS NOT INITIAL.
          IF <lfs_output>-banfn IS NOT INITIAL AND <lfs_output>-bnfpo IS NOT INITIAL.
            READ TABLE lt_sc_details INTO lwa_sc_details WITH KEY ext_demid = <lfs_output>-banfn
                                                              ext_dem_posid = <lfs_output>-bnfpo.
            IF sy-subrc = 0.
              <lfs_output>-number_int = lwa_sc_details-number_int.
              <lfs_output>-srm_qty = lwa_sc_details-quantity.
              <lfs_output>-gross_price = lwa_sc_details-gross_price.
            ELSE.
              READ TABLE lt_sc_details INTO lwa_sc_details WITH KEY be_obj_item = <lfs_output>-ebelp
                                                        be_object_id = <lfs_output>-ebeln.
              IF sy-subrc = 0.
                <lfs_output>-number_int = lwa_sc_details-number_int.
                <lfs_output>-srm_qty = lwa_sc_details-quantity.
                <lfs_output>-gross_price = lwa_sc_details-gross_price.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE lt_sc_details INTO lwa_sc_details WITH KEY be_obj_item = <lfs_output>-ebelp
                                                      be_object_id = <lfs_output>-ebeln.
            IF sy-subrc = 0.
              <lfs_output>-number_int = lwa_sc_details-number_int.
              <lfs_output>-srm_qty = lwa_sc_details-quantity.
              <lfs_output>-gross_price = lwa_sc_details-gross_price.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF s_cdate IS NOT INITIAL.
        IF <lfs_output>-crt_date IN s_cdate.
        ELSE.
          DELETE TABLE lt_output FROM <lfs_output>.
        ENDIF.
      ENDIF.
      IF s_crtby IS NOT INITIAL.
        IF <lfs_output>-crt_by IN s_crtby.
        ELSE.
          DELETE TABLE lt_output FROM <lfs_output>.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT lt_output BY aufnr banfn bnfpo ebeln ebelp object_id number_int.
  ENDIF.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_report .
  DATA: lv_default TYPE char1.

* Build fieldcatalog
  PERFORM build_fieldcatalog.

  lwa_layout-colwidth_optimize = 'X'.

  lwa_variant-report = sy-repid.
  lwa_variant-username = sy-uname.
  IF p_layout IS INITIAL.
    lv_default = abap_true.
  ELSE.
    lv_default = abap_true.
    lwa_variant-variant = p_layout.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = 'ZMMR_SC_STATUS'
      i_callback_user_command = 'ALV_USER_COMMAND'
      i_callback_top_of_page  = 'DISPLAY_TOP_OF_PAGE'
      is_layout               = lwa_layout
      it_fieldcat             = lt_fieldcat[]
      i_default               = lv_default
      i_save                  = 'A'
      is_variant              = lwa_variant
    TABLES
      t_outtab                = lt_output[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  PERFORM create_catalog USING : 'AUFNR'  'WO Number'.
  PERFORM create_catalog USING : 'ERDAT'  'WO Creation Date'.
  PERFORM create_catalog USING : 'LOEKZ_WO' 'WO Status'.  "(+)PANUSURI Ticket ACR-56
*  PERFORM create_catalog USING : 'ZZDUE_DATE' 'WO Due Date'.
  PERFORM create_catalog USING : 'AUART'  'WO Order Type'.
  PERFORM create_catalog USING : 'WO_MAT'  'WO Material'.
  PERFORM create_catalog USING : 'ERNAM'  'WO Entered By'.
  PERFORM create_catalog USING : 'SWERK'  'Maint Plant'.
  PERFORM create_catalog USING : 'IWERK'  'Planning Plant'.
*  PERFORM create_catalog USING : 'BANFN'  'PR'.       "(-)PANUSURI Ticket 75531
  PERFORM create_catalog USING : 'BANFN'  'PR Number'. "(+)PANUSURI Ticket 75531
  PERFORM create_catalog USING : 'BNFPO'  'PR Item no'.
  PERFORM create_catalog USING : 'PR_MAT' 'PR Material'.
  PERFORM create_catalog USING : 'PR_TXT' 'PR Material/Service Desc'.
*  PERFORM create_catalog USING : 'BADAT'  'PR Date'.       "(-)PANUSURI Ticket 75531
  PERFORM create_catalog USING : 'BADAT'  'PR Created Date'."(+)PANUSURI Ticket 75531
*BOI by PANUSURI Ticket 75531
  PERFORM create_catalog USING : 'LFDAT'  'PR Delivery Date'.
  PERFORM create_catalog USING : 'PRNAM'  'PR Requisitioner'.
  PERFORM create_catalog USING : 'BEDNR'  'PR Tracking No/Org.PR#'.
  PERFORM create_catalog USING : 'PRCRT'  'PR Created By'.
*EOI by PANUSURI Ticket 75531
*BOI by PANUSURI Ticket ACR-56
  PERFORM create_catalog USING : 'IDNLF'  'Vendor Material number'.
  PERFORM create_catalog USING : 'LOEKZ'  'PR Status'.
*EOI by PANUSURI Ticket ACR-56
  PERFORM create_catalog USING : 'BDMNG'  'WO qty'.
  PERFORM create_catalog USING : 'SRM_QTY' 'SC qty'.
  PERFORM create_catalog USING : 'PR_QTY' 'PR qty'.
  PERFORM create_catalog USING : 'PO_QTY' 'PO qty'.
  PERFORM create_catalog USING : 'MEINS'  'UOM'.
  PERFORM create_catalog USING : 'GROSS_PRICE' 'SC price'.
  PERFORM create_catalog USING : 'PREIS'  'PR price'.
  PERFORM create_catalog USING : 'NETPR'  'PO Amount/Ser. Expect. Amount'.
  PERFORM create_catalog USING : 'WAERS'  'Currency'.
  PERFORM create_catalog USING : 'EBELN'  'PO'.
  PERFORM create_catalog USING : 'EBELP'  'PO Item number'.
  PERFORM create_catalog USING : 'LOEKZ_PO' 'PO Item Status'. "(+)PANUSURI Ticket ACR-56
  PERFORM create_catalog USING : 'AEDAT'  'PO Created on'.
  PERFORM create_catalog USING : 'PO_MAT' 'PO Material'.
  PERFORM create_catalog USING : 'PO_TXT' 'PO Material/Service Desc.'.
  PERFORM create_catalog USING : 'KNTTP'  'PO Acc Assign. type'.
  PERFORM create_catalog USING : 'KOSTL'  'PO Cost center'.
  PERFORM create_catalog USING : 'KTEXT'  'CC description'.
  PERFORM create_catalog USING : 'SAKTO'  'PO G/L Account'.
  PERFORM create_catalog USING : 'WBS'    'PO WBS element'.
  PERFORM create_catalog USING : 'EKGRP'  'Purchase Group'.
  PERFORM create_catalog USING : 'ZZARIBA_APPROVER' 'Recipient'.
  PERFORM create_catalog USING : 'AFNAM'  'Requisitioner'.
**-- start OF CHNAGES BY AKMADASU  CHG0116780
  PERFORM create_catalog USING : 'NAME_TEXT'  'Full Name'.
  PERFORM create_catalog USING : 'SMTP_ADDR'  'Email ID'.
**-- END OF CHNAGES BY AKMADASU  CHG0116780
  PERFORM create_catalog USING : 'LIFNR'  'PO Vendor'.
  PERFORM create_catalog USING : 'NAME1'  'PO Vendor Name'.
  PERFORM create_catalog USING : 'OBJECT_ID'  'SC number'.
  PERFORM create_catalog USING : 'NUMBER_INT' 'SC Item no'.
  PERFORM create_catalog USING : 'STAT'      'SC status'.
  PERFORM create_catalog USING : 'CRT_DATE'  'SC Date'.
  PERFORM create_catalog USING : 'CRT_BY'    'SC Created By'.
  PERFORM create_catalog USING : 'CUR_APPR'  'Who has it for Approval'.
  PERFORM create_catalog USING : 'NEXT_APPR' 'Next Approval'.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_catalog  USING    iv_fieldname TYPE slis_fieldname
                              iv_seltext   TYPE scrtext_l.
  DATA: lwa_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lwa_fieldcat.
  lwa_fieldcat-fieldname = iv_fieldname.
  IF   iv_fieldname = 'WO_MAT' OR iv_fieldname = 'PR_MAT'
    OR iv_fieldname = 'PO_MAT' OR iv_fieldname = 'AUFNR'
    OR iv_fieldname = 'BANFN'  OR iv_fieldname = 'SAKTO'
    OR iv_fieldname = 'KOSTL'  OR iv_fieldname = 'BEDNR'.
    lwa_fieldcat-no_zero = abap_true.
  ENDIF.
  IF   iv_fieldname = 'AUFNR' OR iv_fieldname = 'BANFN'
    OR iv_fieldname = 'EBELN' OR iv_fieldname = 'OBJECT_ID'.
    lwa_fieldcat-emphasize = 'C511'.
  ENDIF.

  lwa_fieldcat-seltext_l = iv_seltext.
  APPEND lwa_fieldcat TO lt_fieldcat.

ENDFORM.                    " CREATE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_user_command USING lv_ucomm     LIKE sy-ucomm
                            ls_selfield TYPE slis_selfield.

  IF ls_selfield-tabindex LE 0 OR ls_selfield-sumindex > 0.
    EXIT.
  ENDIF.
  CLEAR lwa_output.
  READ TABLE lt_output INTO lwa_output INDEX ls_selfield-tabindex.

  CASE lv_ucomm.
    WHEN '&IC1'.
      CASE ls_selfield-fieldname.
        WHEN 'EBELN'.
          IF ls_selfield-value IS NOT INITIAL."(+)PANUSURI Ticket 75531
            SET PARAMETER ID 'BES' FIELD lwa_output-ebeln.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.                              "(+)PANUSURI Ticket 75531
        WHEN 'BANFN'.
          IF ls_selfield-value IS NOT INITIAL."(+)PANUSURI Ticket 75531
            SET PARAMETER ID 'BAN' FIELD lwa_output-banfn.
            CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
          ENDIF.                              "(+)PANUSURI Ticket 75531
        WHEN 'AUFNR'.
          IF ls_selfield-value IS NOT INITIAL."(+)PANUSURI Ticket 75531
            SET PARAMETER ID 'ANR' FIELD lwa_output-aufnr.
            CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
          ENDIF.                              "(+)PANUSURI Ticket 75531
      ENDCASE.
  ENDCASE.

ENDFORM.                    "alv_user_command
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Display Top of page
*----------------------------------------------------------------------*
FORM display_top_of_page .
  DATA: lv_lines      TYPE i,
        lv_linesc(10) TYPE c,
        lv_text       TYPE string.
  IF lt_listheader[] IS INITIAL.
    lwa_listheader-typ               ='H'.
    lwa_listheader-info             = 'Shopping cart status'(004).
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.

    DESCRIBE TABLE lt_output LINES lv_lines.
    lv_linesc = lv_lines.
    CONCATENATE 'Total number of records:' lv_linesc INTO lv_text SEPARATED BY space.
    lwa_listheader-typ = 'S'.
    lwa_listheader-info = lv_text.
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.

*   Write List header
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_listheader.
  ENDIF.

ENDFORM.                    " DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  GET_WO_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_wo_details .
  DATA:  lt_ekko_ekpo_ekkn_tmp TYPE STANDARD TABLE OF ty_ekko_ekpo_ekkn,
         lv_tabix TYPE sy-tabix.

* Get details from VIAUFKST
  SELECT aufnr
         iwerk
         auart
         ernam
         erdat
         loekz  "(+)PANUSURI Ticket ACR-56
*         zzdue_date
         swerk
         FROM viaufkst
         INTO TABLE lt_viaufkst
         WHERE aufnr IN s_aufnr
         AND   iwerk IN s_iwerk
         AND   auart IN s_auart
         AND   ernam IN s_ernam
         AND   erdat IN s_erdat
*         AND   zzdue_date IN s_ddate
         AND   swerk IN s_swerk.
  IF sy-subrc = 0.
    SORT lt_viaufkst BY aufnr.
  ENDIF.

  IF lt_viaufkst IS NOT INITIAL.
    SELECT banfn
           bnfpo
           aufnr
           FROM ebkn
           INTO TABLE lt_ebkn
           FOR ALL ENTRIES IN lt_viaufkst
           WHERE banfn IN s_banfn
*           AND   loekz = space "(-)PANUSURI Ticket ACR-56
           AND   aufnr = lt_viaufkst-aufnr.
    IF sy-subrc = 0.
      SORT lt_ebkn BY aufnr.
    ENDIF.
  ENDIF.

  IF lt_ebkn IS NOT INITIAL.
    SELECT banfn
           bnfpo
           loekz "(+)PANUSURI Ticket ACR-56
           ernam "(+)PANUSURI Ticket 75531
           afnam "(+)PANUSURI Ticket 75531
           txz01
           matnr
           bednr "(+)PANUSURI Ticket 75531
           menge
           badat
           lfdat "(+)PANUSURI Ticket 75531
           preis
           ebeln
           ebelp
           arsnr
           arsps
           idnlf "(+)PANUSURI Ticket ACR-56
           FROM eban
           INTO TABLE lt_eban
           FOR ALL ENTRIES IN lt_ebkn
           WHERE banfn IN s_banfn
           AND   banfn = lt_ebkn-banfn
           AND   bnfpo = lt_ebkn-bnfpo
*           AND   loekz = space "(-)PANUSURI Ticket ACR-56
           AND   ernam IN s_prcrt
           AND   afnam IN s_prnam
           AND   bednr IN s_bednr
           AND   badat IN s_badat.
    IF sy-subrc = 0.
      SORT lt_eban BY banfn bnfpo.
    ENDIF.
  ENDIF.

  IF lt_eban IS NOT INITIAL.
    SELECT rsnum
           rspos
           matnr
           bdmng
           FROM resb
           INTO TABLE lt_resb
           FOR ALL ENTRIES IN lt_eban
           WHERE rsnum = lt_eban-arsnr
           AND   rspos = lt_eban-arsps
           AND   matnr IN s_matnr.
    IF sy-subrc = 0.
      SORT lt_resb BY rsnum rspos.
    ENDIF.
    SELECT a~ebeln
           a~aedat
           a~ernam
           a~lifnr
           a~ekgrp
           a~waers
           a~zzariba_approver
           b~ebelp
           b~loekz  "(+)PANUSURI Ticket ACR-56
           b~txz01
           b~matnr
           b~bednr
           b~menge
           b~meins
           b~netpr
           b~knttp
           b~banfn
           b~bnfpo
           b~afnam
           c~sakto
           c~kostl
           c~kokrs
           c~ps_psp_pnr
           FROM ekko AS a
           INNER JOIN ekpo AS b
           ON a~ebeln EQ b~ebeln
           LEFT OUTER JOIN ekkn AS c
           ON b~ebeln = c~ebeln
           AND b~ebelp = c~ebelp
           INTO TABLE lt_ekko_ekpo_ekkn
           FOR ALL ENTRIES IN lt_eban
           WHERE a~ebeln IN s_ebeln
*           AND   a~loekz = space "(-)PANUSURI Ticket ACR-56
           AND   a~aedat IN s_aedat
           AND   a~ernam IN s_crnam
           AND   a~lifnr IN s_lifnr
           AND   a~ekgrp IN s_ekgrp
           AND   a~zzariba_approver IN s_arbap
*           AND   b~loekz = space "(-)PANUSURI Ticket ACR-56
           AND   b~matnr IN s_matnr
           AND   b~knttp IN s_knttp
           AND   b~afnam IN s_afnam
*           AND   b~banfn = lt_eban-banfn
*           AND   b~bnfpo = lt_eban-bnfpo.
           AND   b~ebeln = lt_eban-ebeln    "(+)PANUSURI Ticket 75531
           AND   b~ebelp = lt_eban-ebelp.   "(+)PANUSURI Ticket 75531
    IF sy-subrc = 0.
*      SORT lt_ekko_ekpo_ekkn BY banfn bnfpo.
      SORT lt_ekko_ekpo_ekkn BY ebeln ebelp."(+)PANUSURI Ticket 75531
      DELETE lt_ekko_ekpo_ekkn WHERE: kostl NOT IN s_kostl,
                                      ps_psp_pnr NOT IN s_wbs.
    ENDIF.
  ENDIF.

  IF lt_ekko_ekpo_ekkn IS NOT INITIAL.
    SELECT kokrs
           kostl
           ktext
           FROM cskt
           INTO TABLE lt_cskt
           FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn
           WHERE spras = sy-langu
           AND   kokrs = lt_ekko_ekpo_ekkn-kokrs
           AND   kostl = lt_ekko_ekpo_ekkn-kostl
           AND   datbi = '99991231'.
    IF sy-subrc = 0.
      SORT lt_cskt BY kokrs kostl.
    ENDIF.

    lt_ekko_ekpo_ekkn_tmp[] = lt_ekko_ekpo_ekkn[].
    SORT lt_ekko_ekpo_ekkn_tmp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo_ekkn_tmp COMPARING lifnr.
    IF lt_ekko_ekpo_ekkn_tmp IS NOT INITIAL.
      SELECT lifnr
             name1
             FROM lfa1
             INTO TABLE lt_lfa1
             FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn_tmp
             WHERE lifnr = lt_ekko_ekpo_ekkn_tmp-lifnr.
      IF sy-subrc = 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_WO_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_PR_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_pr_details .
  DATA:  lt_ekko_ekpo_ekkn_tmp TYPE STANDARD TABLE OF ty_ekko_ekpo_ekkn,
         lv_tabix TYPE sy-tabix.

* Get details from EBAN
  SELECT banfn
         bnfpo
         loekz "(+)PANUSURI Ticket ACR-56
         ernam "(+)PANUSURI Ticket 75531
         afnam "(+)PANUSURI Ticket 75531
         txz01
         matnr
         bednr "(+)PANUSURI Ticket 75531
         menge
         badat
         lfdat "(+)PANUSURI Ticket 75531
         preis
         ebeln
         ebelp
         arsnr
         arsps
         idnlf  "(+)PANUSURI Ticket ACR-56
         FROM eban
         INTO TABLE lt_eban
         WHERE banfn IN s_banfn
*         AND   loekz = space "(-)PANUSURI Ticket ACR-56
         AND   ernam IN s_prcrt
         AND   afnam IN s_prnam
         AND   bednr IN s_bednr
         AND   badat IN s_badat.
  IF sy-subrc = 0.
    SORT lt_eban BY banfn bnfpo.
  ENDIF.

  IF lt_eban IS NOT INITIAL.
    SELECT rsnum
           rspos
           matnr
           bdmng
           FROM resb
           INTO TABLE lt_resb
           FOR ALL ENTRIES IN lt_eban
           WHERE rsnum = lt_eban-arsnr
           AND   rspos = lt_eban-arsps.
    IF sy-subrc = 0.
      SORT lt_resb BY rsnum rspos.
    ENDIF.
    SELECT a~ebeln
           a~aedat
           a~ernam
           a~lifnr
           a~ekgrp
           a~waers
           a~zzariba_approver
           b~ebelp
           b~loekz  "(+)PANUSURI Ticket ACR-56
           b~txz01
           b~matnr
           b~bednr
           b~menge
           b~meins
           b~netpr
           b~knttp
           b~banfn
           b~bnfpo
           b~afnam
           c~sakto
           c~kostl
           c~kokrs
           c~ps_psp_pnr
           FROM ekko AS a
           INNER JOIN ekpo AS b
           ON a~ebeln EQ b~ebeln
           LEFT OUTER JOIN ekkn AS c
           ON b~ebeln = c~ebeln
           AND b~ebelp = c~ebelp
           INTO TABLE lt_ekko_ekpo_ekkn
           FOR ALL ENTRIES IN lt_eban
           WHERE a~ebeln IN s_ebeln
*           AND   a~loekz = space "(-)PANUSURI Ticket ACR-56
           AND   a~aedat IN s_aedat
           AND   a~ernam IN s_crnam
           AND   a~lifnr IN s_lifnr
           AND   a~ekgrp IN s_ekgrp
           AND   a~zzariba_approver IN s_arbap
*           AND   b~loekz = space "(-)PANUSURI Ticket ACR-56
           AND   b~matnr IN s_matnr
           AND   b~knttp IN s_knttp
           AND   b~afnam IN s_afnam
*           AND   b~banfn = lt_eban-banfn
*           AND   b~bnfpo = lt_eban-bnfpo.
           AND   b~ebeln = lt_eban-ebeln  "(+)PANUSURI Ticket 75531
           AND   b~ebelp = lt_eban-ebelp. "(+)PANUSURI Ticket 75531
    IF sy-subrc = 0.
*      SORT lt_ekko_ekpo_ekkn BY banfn bnfpo.
      SORT lt_ekko_ekpo_ekkn BY ebeln ebelp."(+)PANUSURI Ticket 75531
      DELETE lt_ekko_ekpo_ekkn WHERE: kostl NOT IN s_kostl,
                                      ps_psp_pnr NOT IN s_wbs.
    ENDIF.

    SELECT banfn
           bnfpo
           aufnr
           FROM ebkn
           INTO TABLE lt_ebkn
           FOR ALL ENTRIES IN lt_eban
           WHERE banfn = lt_eban-banfn
           AND   bnfpo = lt_eban-bnfpo.
*           AND   loekz = space.  "(-)PANUSURI Ticket ACR-56
    IF sy-subrc = 0.
      SORT lt_ebkn BY banfn bnfpo.
    ENDIF.
  ENDIF.

  IF lt_ebkn IS NOT INITIAL.
    SELECT aufnr
           iwerk
           auart
           ernam
           erdat
           loekz  "(+)PANUSURI Ticket ACR-56
*           zzdue_date
           swerk
           FROM viaufkst
           INTO TABLE lt_viaufkst
           FOR ALL ENTRIES IN lt_ebkn
           WHERE aufnr IN s_aufnr
           AND   aufnr = lt_ebkn-aufnr
           AND   iwerk IN s_iwerk
           AND   auart IN s_auart
           AND   ernam IN s_ernam
           AND   erdat IN s_erdat
*           AND   zzdue_date IN s_ddate
           AND   swerk IN s_swerk.
    IF sy-subrc = 0.
      SORT lt_viaufkst BY aufnr.
    ENDIF.
  ENDIF.

  IF lt_ekko_ekpo_ekkn IS NOT INITIAL.
    SELECT kokrs
           kostl
           ktext
           FROM cskt
           INTO TABLE lt_cskt
           FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn
           WHERE spras = sy-langu
           AND   kokrs = lt_ekko_ekpo_ekkn-kokrs
           AND   kostl = lt_ekko_ekpo_ekkn-kostl
           AND   datbi = '99991231'.
    IF sy-subrc = 0.
      SORT lt_cskt BY kokrs kostl.
    ENDIF.

    lt_ekko_ekpo_ekkn_tmp[] = lt_ekko_ekpo_ekkn[].
    SORT lt_ekko_ekpo_ekkn_tmp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo_ekkn_tmp COMPARING lifnr.
    IF lt_ekko_ekpo_ekkn_tmp IS NOT INITIAL.
      SELECT lifnr
             name1
             FROM lfa1
             INTO TABLE lt_lfa1
             FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn_tmp
             WHERE lifnr = lt_ekko_ekpo_ekkn_tmp-lifnr.
      IF sy-subrc = 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_PR_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_PO_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_po_details .
  DATA: lt_ekko_ekpo_ekkn_tmp TYPE STANDARD TABLE OF ty_ekko_ekpo_ekkn,
        lv_tabix TYPE sy-tabix.

* Get details from EKKO, EKPO and EKKN
  SELECT a~ebeln
         a~aedat
         a~ernam
         a~lifnr
         a~ekgrp
         a~waers
         a~zzariba_approver
         b~ebelp
         b~loekz  "(+)PANUSURI Ticket ACR-56
         b~txz01
         b~matnr
         b~bednr
         b~menge
         b~meins
         b~netpr
         b~knttp
         b~banfn
         b~bnfpo
         b~afnam
         c~sakto
         c~kostl
         c~kokrs
         c~ps_psp_pnr
         FROM ekko AS a
         INNER JOIN ekpo AS b
         ON a~ebeln EQ b~ebeln
         LEFT OUTER JOIN ekkn AS c
         ON b~ebeln = c~ebeln
         AND b~ebelp = c~ebelp
         INTO TABLE lt_ekko_ekpo_ekkn
         WHERE a~ebeln IN s_ebeln
*         AND   a~loekz = space "(-)PANUSURI Ticket ACR-56
         AND   a~aedat IN s_aedat
         AND   a~ernam IN s_crnam
         AND   a~lifnr IN s_lifnr
         AND   a~ekgrp IN s_ekgrp
         AND   a~zzariba_approver IN s_arbap
*         AND   b~loekz = space "(-)PANUSURI Ticket ACR-56e
         AND   b~matnr IN s_matnr
         AND   b~knttp IN s_knttp
         AND   b~afnam IN s_afnam
         AND   b~banfn IN s_banfn.
  IF sy-subrc = 0.
    DELETE lt_ekko_ekpo_ekkn WHERE: kostl NOT IN s_kostl,
                                    ps_psp_pnr NOT IN s_wbs.
    SORT lt_ekko_ekpo_ekkn BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo_ekkn COMPARING ebeln ebelp.
*    SORT lt_ekko_ekpo_ekkn BY banfn bnfpo.
  ENDIF.

  IF lt_ekko_ekpo_ekkn IS NOT INITIAL.
    SELECT banfn
           bnfpo
           loekz "(+)PANUSURI Ticket ACR-56
           ernam "(+)PANUSURI Ticket 75531
           afnam "(+)PANUSURI Ticket 75531
           txz01
           matnr
           bednr "(+)PANUSURI Ticket 75531
           menge
           badat
           lfdat "(+)PANUSURI Ticket 75531
           preis
           ebeln
           ebelp
           arsnr
           arsps
           idnlf "(+)PANUSURI Ticket ACR-56
           FROM eban
           INTO TABLE lt_eban
           FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn
           WHERE banfn IN s_banfn
           AND   banfn = lt_ekko_ekpo_ekkn-banfn
           AND   bnfpo = lt_ekko_ekpo_ekkn-bnfpo
*           AND   loekz = space "(-)PANUSURI Ticket ACR-56
           AND   ernam IN s_prcrt
           AND   afnam IN s_prnam
           AND   bednr IN s_bednr
           AND   badat IN s_badat.
    IF sy-subrc = 0.
      SORT lt_eban BY banfn bnfpo.
    ENDIF.

    SELECT kokrs
           kostl
           ktext
           FROM cskt
           INTO TABLE lt_cskt
           FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn
           WHERE spras = sy-langu
           AND   kokrs = lt_ekko_ekpo_ekkn-kokrs
           AND   kostl = lt_ekko_ekpo_ekkn-kostl
           AND   datbi = '99991231'.
    IF sy-subrc = 0.
      SORT lt_cskt BY kokrs kostl.
    ENDIF.

    lt_ekko_ekpo_ekkn_tmp[] = lt_ekko_ekpo_ekkn[].
    SORT lt_ekko_ekpo_ekkn_tmp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo_ekkn_tmp COMPARING lifnr.
    IF lt_ekko_ekpo_ekkn_tmp IS NOT INITIAL.
      SELECT lifnr
             name1
             FROM lfa1
             INTO TABLE lt_lfa1
             FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn_tmp
             WHERE lifnr = lt_ekko_ekpo_ekkn_tmp-lifnr.
      IF sy-subrc = 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_eban IS NOT INITIAL.
    SELECT rsnum
           rspos
           matnr
           bdmng
           FROM resb
           INTO TABLE lt_resb
           FOR ALL ENTRIES IN lt_eban
           WHERE rsnum = lt_eban-arsnr
           AND   rspos = lt_eban-arsps.
    IF sy-subrc = 0.
      SORT lt_resb BY rsnum rspos.
    ENDIF.

    SELECT banfn
           bnfpo
           aufnr
           FROM ebkn
           INTO TABLE lt_ebkn
           FOR ALL ENTRIES IN lt_eban
           WHERE banfn = lt_eban-banfn
           AND   bnfpo = lt_eban-bnfpo.
*           AND   loekz = space.  "(-)PANUSURI Ticket ACR-56
    IF sy-subrc = 0.
      SORT lt_ebkn BY banfn bnfpo.
    ENDIF.
  ENDIF.

  IF lt_ebkn IS NOT INITIAL.
    SELECT aufnr
           iwerk
           auart
           ernam
           erdat
           loekz  "(+)PANUSURI Ticket ACR-56
*           zzdue_date
           swerk
           FROM viaufkst
           INTO TABLE lt_viaufkst
           FOR ALL ENTRIES IN lt_ebkn
           WHERE aufnr IN s_aufnr
           AND   aufnr = lt_ebkn-aufnr
           AND   iwerk IN s_iwerk
           AND   auart IN s_auart
           AND   ernam IN s_ernam
           AND   erdat IN s_erdat
*           AND   zzdue_date IN s_ddate
           AND   swerk IN s_swerk.
    IF sy-subrc = 0.
      SORT lt_viaufkst BY aufnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_PO_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_SC_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sc_details .
  DATA: lt_ekko_ekpo_ekkn_tmp TYPE STANDARD TABLE OF ty_ekko_ekpo_ekkn.

  CLEAR:   lwa_sc_details,
           lt_sc_details.
  REFRESH: lt_sc_details.

* Get shopping cart details from SRM system
  CALL FUNCTION 'ZSRM_SC_DETAILS' DESTINATION lv_rfcdest
    EXPORTING
      ex_logsys       = lv_logsys
    TABLES
      it_object_id    = s_objid
      it_posting_date = s_cdate
      it_created_by   = s_crtby
      et_sc_details   = lt_sc_details.

  IF lt_sc_details IS NOT INITIAL.
    LOOP AT lt_sc_details INTO lwa_sc_details.
      CLEAR: lwa_sc_pr_tmp, lwa_sc_po_tmp.
      lwa_sc_pr_tmp-ext_demid = lwa_sc_details-ext_demid.
      lwa_sc_pr_tmp-ext_dem_posid = lwa_sc_details-ext_dem_posid.
      IF lwa_sc_pr_tmp IS NOT INITIAL.
        APPEND lwa_sc_pr_tmp TO lt_sc_pr_tmp.
        CLEAR lwa_sc_pr_tmp.
      ENDIF.
      lwa_sc_po_tmp-be_obj_item = lwa_sc_details-be_obj_item.
      lwa_sc_po_tmp-be_object_id = lwa_sc_details-be_object_id.
      IF lwa_sc_po_tmp IS NOT INITIAL.
        APPEND lwa_sc_po_tmp TO lt_sc_po_tmp.
        CLEAR lwa_sc_po_tmp.
      ENDIF.
    ENDLOOP.
    SORT lt_sc_pr_tmp BY ext_demid ext_dem_posid.
    SORT lt_sc_po_tmp BY be_object_id be_obj_item.
    IF lt_sc_pr_tmp IS NOT INITIAL.
      SELECT banfn
             bnfpo
             loekz "(+)PANUSURI Ticket ACR-56
             ernam "(+)PANUSURI Ticket 75531
             afnam "(+)PANUSURI Ticket 75531
             txz01
             matnr
             bednr "(+)PANUSURI Ticket 75531
             menge
             badat
             lfdat "(+)PANUSURI Ticket 75531
             preis
             ebeln
             ebelp
             arsnr
             arsps
             idnlf "(+)PANUSURI Ticket ACR-56
             FROM eban
             INTO TABLE lt_eban
             FOR ALL ENTRIES IN lt_sc_pr_tmp
             WHERE banfn = lt_sc_pr_tmp-ext_demid
             AND   bnfpo = lt_sc_pr_tmp-ext_dem_posid
*             AND   loekz = space "(-)PANUSURI Ticket ACR-56
             AND   ernam IN s_prcrt
             AND   afnam IN s_prnam
             AND   bednr IN s_bednr.
      IF sy-subrc = 0.
        SORT lt_eban BY banfn bnfpo.
      ENDIF.
    ENDIF.
    IF lt_sc_po_tmp[] IS NOT INITIAL.
      SELECT a~ebeln
             a~aedat
             a~ernam
             a~lifnr
             a~ekgrp
             a~waers
             a~zzariba_approver
             b~ebelp
             b~loekz  "(+)PANUSURI Ticket ACR-56
             b~txz01
             b~matnr
             b~bednr
             b~menge
             b~meins
             b~netpr
             b~knttp
             b~banfn
             b~bnfpo
             b~afnam
             c~sakto
             c~kostl
             c~kokrs
             c~ps_psp_pnr
             FROM ekko AS a
             INNER JOIN ekpo AS b
             ON a~ebeln EQ b~ebeln
             LEFT OUTER JOIN ekkn AS c
             ON b~ebeln = c~ebeln
             AND b~ebelp = c~ebelp
             INTO TABLE lt_ekko_ekpo_ekkn
             FOR ALL ENTRIES IN lt_sc_po_tmp
*             WHERE a~loekz = space "(-)PANUSURI Ticket ACR-56
             WHERE b~ebeln = lt_sc_po_tmp-be_object_id
             AND   b~ebelp = lt_sc_po_tmp-be_obj_item.
*             AND   b~loekz = space."(-)PANUSURI Ticket ACR-56
      IF sy-subrc = 0.
*        SORT lt_ekko_ekpo_ekkn BY banfn bnfpo.
        SORT lt_ekko_ekpo_ekkn BY ebeln ebelp."(+)PANUSURI Ticket 75531
      ENDIF.
    ENDIF.

    IF lt_eban IS NOT INITIAL.
      SELECT rsnum
             rspos
             matnr
             bdmng
             FROM resb
             INTO TABLE lt_resb
             FOR ALL ENTRIES IN lt_eban
             WHERE rsnum = lt_eban-arsnr
             AND   rspos = lt_eban-arsps.
      IF sy-subrc = 0.
        SORT lt_resb BY rsnum rspos.
      ENDIF.
      SELECT a~ebeln
             a~aedat
             a~ernam
             a~lifnr
             a~ekgrp
             a~waers
             a~zzariba_approver
             b~ebelp
             b~loekz  "(+)PANUSURI Ticket ACR-56
             b~txz01
             b~matnr
             b~bednr
             b~menge
             b~meins
             b~netpr
             b~knttp
             b~banfn
             b~bnfpo
             b~afnam
             c~sakto
             c~kostl
             c~kokrs
             c~ps_psp_pnr
             FROM ekko AS a
             INNER JOIN ekpo AS b
             ON a~ebeln EQ b~ebeln
             LEFT OUTER JOIN ekkn AS c
             ON b~ebeln = c~ebeln
             AND b~ebelp = c~ebelp
             APPENDING TABLE lt_ekko_ekpo_ekkn
             FOR ALL ENTRIES IN lt_eban
*             WHERE a~loekz = space "(-)PANUSURI Ticket ACR-56
*             AND   b~loekz = space "(-)PANUSURI Ticket ACR-56
*             AND   b~banfn = lt_eban-banfn
*             AND   b~bnfpo = lt_eban-bnfpo.
             WHERE b~ebeln = lt_eban-ebeln  "(+)PANUSURI Ticket 75531
             AND   b~ebelp = lt_eban-ebelp. "(+)PANUSURI Ticket 75531
      IF sy-subrc = 0.
        SORT lt_ekko_ekpo_ekkn BY ebeln ebelp banfn bnfpo sakto kostl kokrs ps_psp_pnr.
        DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo_ekkn COMPARING ebeln ebelp banfn bnfpo sakto kostl kokrs ps_psp_pnr.
*        SORT lt_ekko_ekpo_ekkn BY banfn bnfpo.
        SORT lt_ekko_ekpo_ekkn BY ebeln ebelp."(+)PANUSURI Ticket 75531
      ENDIF.

      SELECT banfn
             bnfpo
             aufnr
             FROM ebkn
             INTO TABLE lt_ebkn
             FOR ALL ENTRIES IN lt_eban
             WHERE banfn = lt_eban-banfn
             AND   bnfpo = lt_eban-bnfpo.
*             AND   loekz = space.  "(-)PANUSURI Ticket ACR-56
      IF sy-subrc = 0.
        SORT lt_ebkn BY banfn bnfpo.
      ENDIF.
    ENDIF.

    IF lt_ebkn IS NOT INITIAL.
      SELECT aufnr
             iwerk
             auart
             ernam
             erdat
             loekz  "(+)PANUSURI Ticket ACR-56
*             zzdue_date
             swerk
             FROM viaufkst
             INTO TABLE lt_viaufkst
             FOR ALL ENTRIES IN lt_ebkn
             WHERE aufnr = lt_ebkn-aufnr.
      IF sy-subrc = 0.
        SORT lt_viaufkst BY aufnr.
      ENDIF.
    ENDIF.

    IF lt_ekko_ekpo_ekkn IS NOT INITIAL.
      SELECT kokrs
             kostl
             ktext
             FROM cskt
             INTO TABLE lt_cskt
             FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn
             WHERE spras = sy-langu
             AND   kokrs = lt_ekko_ekpo_ekkn-kokrs
             AND   kostl = lt_ekko_ekpo_ekkn-kostl
             AND   datbi = '99991231'.
      IF sy-subrc = 0.
        SORT lt_cskt BY kokrs kostl.
      ENDIF.

      lt_ekko_ekpo_ekkn_tmp[] = lt_ekko_ekpo_ekkn[].
      SORT lt_ekko_ekpo_ekkn_tmp BY lifnr.
      DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo_ekkn_tmp COMPARING lifnr.
      IF lt_ekko_ekpo_ekkn_tmp IS NOT INITIAL.
        SELECT lifnr
               name1
               FROM lfa1
               INTO TABLE lt_lfa1
               FOR ALL ENTRIES IN lt_ekko_ekpo_ekkn_tmp
               WHERE lifnr = lt_ekko_ekpo_ekkn_tmp-lifnr.
        IF sy-subrc = 0.
          SORT lt_lfa1 BY lifnr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_SC_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_WO_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_wo_output_data .
  DATA: lv_tabix      TYPE sy-tabix,
        lv_ebkn_tabix TYPE sy-tabix,
        lwa_output1   TYPE ty_output.

  LOOP AT lt_viaufkst INTO lwa_viaufkst.
    CLEAR: lwa_output,
           lwa_output1,
           lwa_resb,
           lwa_ebkn,
           lwa_eban,
           lwa_ekko_ekpo_ekkn,
           lwa_lfa1,
           lwa_cskt,
           lv_object_id.
    lwa_output-aufnr = lwa_viaufkst-aufnr.
    lwa_output-iwerk = lwa_viaufkst-iwerk.
    lwa_output-auart = lwa_viaufkst-auart.
    lwa_output-ernam = lwa_viaufkst-ernam.
    lwa_output-erdat = lwa_viaufkst-erdat.
    lwa_output-loekz_wo = lwa_viaufkst-loekz. "(+)PANUSURI Ticket ACR-56
*    lwa_output-zzdue_date = lwa_viaufkst-zzdue_date.
    lwa_output-swerk = lwa_viaufkst-swerk.

    READ TABLE lt_ebkn INTO lwa_ebkn WITH KEY aufnr = lwa_viaufkst-aufnr
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lv_ebkn_tabix = sy-tabix.
      LOOP AT lt_ebkn INTO lwa_ebkn FROM lv_ebkn_tabix.
        IF lwa_ebkn-aufnr <> lwa_viaufkst-aufnr.
          EXIT.
        ENDIF.
        lwa_output-banfn = lwa_ebkn-banfn.
        lwa_output-bnfpo = lwa_ebkn-bnfpo.
        CLEAR: lwa_eban,
               lwa_ekko_ekpo_ekkn,
               lwa_resb.
        READ TABLE lt_eban INTO lwa_eban WITH KEY banfn = lwa_ebkn-banfn
                                                  bnfpo = lwa_ebkn-bnfpo
                                                  BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_resb INTO lwa_resb WITH KEY rsnum = lwa_eban-arsnr
                                                    rspos = lwa_eban-arsps
                                                    BINARY SEARCH.
          IF sy-subrc = 0.
            lwa_output-wo_mat = lwa_resb-matnr.
            lwa_output-bdmng = lwa_resb-bdmng.
          ELSE.
            CLEAR: lwa_output-bdmng,
                   lwa_output-wo_mat.
          ENDIF.
          IF s_womat IS NOT INITIAL.
            IF lwa_resb-matnr IN s_womat.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          lwa_output-pr_txt = lwa_eban-txz01.
          lwa_output-pr_mat = lwa_eban-matnr.
          lwa_output-pr_qty = lwa_eban-menge.
          lwa_output-badat = lwa_eban-badat.
          lwa_output-preis = lwa_eban-preis.
*         BOI by PANUSURI Ticket 75531
          lwa_output-prcrt = lwa_eban-ernam.
          lwa_output-prnam = lwa_eban-afnam.
          lwa_output-bednr = lwa_eban-bednr.
          lwa_output-lfdat = lwa_eban-lfdat.
*         EOI by PANUSURI Ticket 75531
*         BOI by PANUSURI Ticket ACR-56
          lwa_output-idnlf = lwa_eban-idnlf.
          lwa_output-loekz = lwa_eban-loekz.
*         EOI by PANUSURI Ticket ACR-56
*          READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY banfn = lwa_ebkn-banfn
*                                                                        bnfpo = lwa_ebkn-bnfpo
*                                                                        BINARY SEARCH.
          READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY ebeln = lwa_eban-ebeln
                                                                        ebelp = lwa_eban-ebelp
                                                                        BINARY SEARCH."(+)PANUSURI Ticket 75531
          IF sy-subrc = 0.
            lwa_output1 = lwa_output.
            lv_tabix = sy-tabix.
            IF s_objid IS NOT INITIAL.
              IF lwa_ekko_ekpo_ekkn-bednr IN s_objid.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            CLEAR lwa_lfa1.
            READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko_ekpo_ekkn-lifnr
                                                      BINARY SEARCH.
            IF sy-subrc = 0.
              lwa_output1-name1 = lwa_lfa1-name1.
            ENDIF.
            LOOP AT lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn FROM lv_tabix.
*              IF lwa_ekko_ekpo_ekkn-banfn <> lwa_eban-banfn OR lwa_ekko_ekpo_ekkn-bnfpo <> lwa_eban-bnfpo.
              IF lwa_ekko_ekpo_ekkn-ebeln <> lwa_eban-ebeln OR lwa_ekko_ekpo_ekkn-ebelp <> lwa_eban-ebelp."(+)PANUSURI Ticket 75531
                EXIT.
              ENDIF.
              CLEAR lwa_cskt.
              READ TABLE lt_cskt INTO lwa_cskt WITH KEY kokrs = lwa_ekko_ekpo_ekkn-kokrs
                                                        kostl = lwa_ekko_ekpo_ekkn-kostl
                                                        BINARY SEARCH.
              IF sy-subrc = 0.
                lwa_output1-ktext = lwa_cskt-ktext.
              ENDIF.
              lwa_output1-ebeln = lwa_ekko_ekpo_ekkn-ebeln.
              lwa_output1-lifnr = lwa_ekko_ekpo_ekkn-lifnr.
              lwa_output1-ekgrp = lwa_ekko_ekpo_ekkn-ekgrp.
              lwa_output1-waers = lwa_ekko_ekpo_ekkn-waers.
              lwa_output1-zzariba_approver = lwa_ekko_ekpo_ekkn-zzariba_approver.
              lwa_output1-ebelp = lwa_ekko_ekpo_ekkn-ebelp.
              lwa_output1-loekz_po = lwa_ekko_ekpo_ekkn-loekz.  "(+)PANUSURI Ticket ACR-56
              lwa_output1-po_txt = lwa_ekko_ekpo_ekkn-txz01.
              lwa_output1-po_mat = lwa_ekko_ekpo_ekkn-matnr.
              lwa_output1-object_id = lwa_ekko_ekpo_ekkn-bednr.
              lwa_output1-po_qty = lwa_ekko_ekpo_ekkn-menge.
              lwa_output1-meins = lwa_ekko_ekpo_ekkn-meins.
              lwa_output1-netpr = lwa_ekko_ekpo_ekkn-netpr.
              lwa_output1-knttp = lwa_ekko_ekpo_ekkn-knttp.
              lwa_output1-banfn = lwa_ekko_ekpo_ekkn-banfn.
              lwa_output1-bnfpo = lwa_ekko_ekpo_ekkn-bnfpo.
              lwa_output1-afnam = lwa_ekko_ekpo_ekkn-afnam.
              lwa_output1-sakto = lwa_ekko_ekpo_ekkn-sakto.
              lwa_output1-kostl = lwa_ekko_ekpo_ekkn-kostl.
              lwa_output1-wbs = lwa_ekko_ekpo_ekkn-ps_psp_pnr.
              lwa_output1-aedat = lwa_ekko_ekpo_ekkn-aedat.

              APPEND lwa_output1 TO lt_output.
              CLEAR lwa_output1.
            ENDLOOP.
          ELSE.
            IF s_ebeln  IS NOT INITIAL OR s_aedat IS NOT INITIAL OR s_crnam IS NOT INITIAL
             OR s_matnr IS NOT INITIAL OR s_wbs   IS NOT INITIAL OR s_kostl IS NOT INITIAL
             OR s_knttp IS NOT INITIAL OR s_ekgrp IS NOT INITIAL OR s_arbap IS NOT INITIAL
             OR s_afnam IS NOT INITIAL OR s_lifnr IS NOT INITIAL.
              CONTINUE.
            ENDIF.
            CLEAR lv_object_id.
            CALL FUNCTION 'ZSRM_REQUISITION_SC' DESTINATION lv_rfcdest
              EXPORTING
                ex_ext_demid      = lwa_ebkn-banfn
                ex_ext_dem_posid  = lwa_ebkn-bnfpo
                ex_ext_dem_logsys = lv_logsys
              IMPORTING
                im_object_id      = lv_object_id.

            IF s_objid IS NOT INITIAL.
              IF lv_object_id IN s_objid.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            lwa_output-object_id = lv_object_id.
            APPEND lwa_output TO lt_output.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      IF s_banfn  IS NOT INITIAL OR s_badat IS NOT INITIAL OR s_prcrt IS NOT INITIAL
       OR s_prnam IS NOT INITIAL OR s_bednr IS NOT INITIAL OR s_ebeln IS NOT INITIAL
       OR s_aedat IS NOT INITIAL OR s_crnam IS NOT INITIAL OR s_matnr IS NOT INITIAL
       OR s_wbs   IS NOT INITIAL OR s_kostl IS NOT INITIAL OR s_knttp IS NOT INITIAL
       OR s_ekgrp IS NOT INITIAL OR s_arbap IS NOT INITIAL OR s_afnam IS NOT INITIAL
       OR s_lifnr IS NOT INITIAL OR s_objid IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      APPEND lwa_output TO lt_output.
    ENDIF.

    CLEAR lwa_output.
  ENDLOOP.

ENDFORM.                    " GET_WO_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PR_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_pr_output_data .
  DATA: lv_tabix TYPE sy-tabix.

  LOOP AT lt_eban INTO lwa_eban.
    CLEAR: lwa_output,
           lwa_resb,
           lwa_viaufkst,
           lwa_ebkn,
           lwa_ekko_ekpo_ekkn,
           lwa_lfa1,
           lwa_cskt,
           lv_object_id.

    lwa_output-banfn = lwa_eban-banfn.
    lwa_output-bnfpo = lwa_eban-bnfpo.
    lwa_output-pr_txt = lwa_eban-txz01.
    lwa_output-pr_mat = lwa_eban-matnr.
    lwa_output-pr_qty = lwa_eban-menge.
    lwa_output-badat = lwa_eban-badat.
    lwa_output-preis = lwa_eban-preis.
*   BOI by PANUSURI Ticket 75531
    lwa_output-prcrt = lwa_eban-ernam.
    lwa_output-prnam = lwa_eban-afnam.
    lwa_output-bednr = lwa_eban-bednr.
    lwa_output-lfdat = lwa_eban-lfdat.
*   EOI by PANUSURI Ticket 75531
*   BOI by PANUSURI Ticket ACR-56
    lwa_output-idnlf = lwa_eban-idnlf.
    lwa_output-loekz = lwa_eban-loekz.
*   EOI by PANUSURI Ticket ACR-56

    READ TABLE lt_resb INTO lwa_resb WITH KEY rsnum = lwa_eban-arsnr
                                              rspos = lwa_eban-arsps
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-wo_mat = lwa_resb-matnr.
      lwa_output-bdmng = lwa_resb-bdmng.
    ELSE.
      CLEAR: lwa_output-bdmng,
             lwa_output-wo_mat.
    ENDIF.
    IF s_womat IS NOT INITIAL.
      IF lwa_resb-matnr IN s_womat.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
    READ TABLE lt_ebkn INTO lwa_ebkn WITH KEY banfn = lwa_eban-banfn
                                              bnfpo = lwa_eban-bnfpo
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_viaufkst INTO lwa_viaufkst WITH KEY aufnr = lwa_ebkn-aufnr
                                                        BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_output-aufnr = lwa_viaufkst-aufnr.
        lwa_output-iwerk = lwa_viaufkst-iwerk.
        lwa_output-auart = lwa_viaufkst-auart.
        lwa_output-ernam = lwa_viaufkst-ernam.
        lwa_output-erdat = lwa_viaufkst-erdat.
        lwa_output-loekz_wo = lwa_viaufkst-loekz. "(+)PANUSURI Ticket ACR-56
*        lwa_output-zzdue_date = lwa_viaufkst-zzdue_date.
        lwa_output-swerk = lwa_viaufkst-swerk.
      ENDIF.
    ENDIF.

*    READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY banfn = lwa_eban-banfn
*                                                                  bnfpo = lwa_eban-bnfpo
*                                                                  BINARY SEARCH.
    READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY ebeln = lwa_eban-ebeln
                                                                  ebelp = lwa_eban-ebelp
                                                                  BINARY SEARCH."(+)PANUSURI Ticket 75531
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
      IF s_objid IS NOT INITIAL.
        IF lwa_ekko_ekpo_ekkn-bednr IN s_objid.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      CLEAR lwa_lfa1.
      READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko_ekpo_ekkn-lifnr
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_output-name1 = lwa_lfa1-name1.
      ENDIF.
      LOOP AT lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn FROM lv_tabix.
*        IF lwa_ekko_ekpo_ekkn-banfn <> lwa_eban-banfn OR lwa_ekko_ekpo_ekkn-bnfpo <> lwa_eban-bnfpo.
        IF lwa_ekko_ekpo_ekkn-ebeln <> lwa_eban-ebeln OR lwa_ekko_ekpo_ekkn-ebelp <> lwa_eban-ebelp."(+)PANUSURI Ticket 75531
          EXIT.
        ENDIF.
        CLEAR lwa_cskt.
        READ TABLE lt_cskt INTO lwa_cskt WITH KEY kokrs = lwa_ekko_ekpo_ekkn-kokrs
                                                  kostl = lwa_ekko_ekpo_ekkn-kostl
                                                  BINARY SEARCH.
        IF sy-subrc = 0.
          lwa_output-ktext = lwa_cskt-ktext.
        ENDIF.
        lwa_output-ebeln = lwa_ekko_ekpo_ekkn-ebeln.
        lwa_output-lifnr = lwa_ekko_ekpo_ekkn-lifnr.
        lwa_output-ekgrp = lwa_ekko_ekpo_ekkn-ekgrp.
        lwa_output-waers = lwa_ekko_ekpo_ekkn-waers.
        lwa_output-zzariba_approver = lwa_ekko_ekpo_ekkn-zzariba_approver.
        lwa_output-ebelp = lwa_ekko_ekpo_ekkn-ebelp.
        lwa_output-loekz_po = lwa_ekko_ekpo_ekkn-loekz. "(+)PANUSURI Ticket ACR-56
        lwa_output-po_txt = lwa_ekko_ekpo_ekkn-txz01.
        lwa_output-po_mat = lwa_ekko_ekpo_ekkn-matnr.
        lwa_output-object_id = lwa_ekko_ekpo_ekkn-bednr.
        lwa_output-po_qty = lwa_ekko_ekpo_ekkn-menge.
        lwa_output-meins = lwa_ekko_ekpo_ekkn-meins.
        lwa_output-netpr = lwa_ekko_ekpo_ekkn-netpr.
        lwa_output-knttp = lwa_ekko_ekpo_ekkn-knttp.
        lwa_output-afnam = lwa_ekko_ekpo_ekkn-afnam.
        lwa_output-sakto = lwa_ekko_ekpo_ekkn-sakto.
        lwa_output-kostl = lwa_ekko_ekpo_ekkn-kostl.
        lwa_output-wbs = lwa_ekko_ekpo_ekkn-ps_psp_pnr.
        lwa_output-aedat = lwa_ekko_ekpo_ekkn-aedat.

        APPEND lwa_output TO lt_output.
      ENDLOOP.
    ELSE.
      IF s_ebeln  IS NOT INITIAL OR s_aedat IS NOT INITIAL OR s_crnam IS NOT INITIAL
       OR s_matnr IS NOT INITIAL OR s_wbs   IS NOT INITIAL OR s_kostl IS NOT INITIAL
       OR s_knttp IS NOT INITIAL OR s_ekgrp IS NOT INITIAL OR s_arbap IS NOT INITIAL
       OR s_afnam IS NOT INITIAL OR s_lifnr IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR lv_object_id.
      CALL FUNCTION 'ZSRM_REQUISITION_SC' DESTINATION lv_rfcdest
        EXPORTING
          ex_ext_demid      = lwa_eban-banfn
          ex_ext_dem_posid  = lwa_eban-bnfpo
          ex_ext_dem_logsys = lv_logsys
        IMPORTING
          im_object_id      = lv_object_id.

      IF s_objid IS NOT INITIAL.
        IF lv_object_id IN s_objid.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      lwa_output-object_id = lv_object_id.
      APPEND lwa_output TO lt_output.
    ENDIF.

    CLEAR lwa_output.
  ENDLOOP.

ENDFORM.                    " GET_PR_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PO_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_po_output_data .
  DATA: lv_tabix TYPE sy-tabix.

  LOOP AT lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn.
    CLEAR: lwa_output,
           lwa_resb,
           lwa_eban,
           lwa_viaufkst,
           lwa_ebkn,
           lwa_lfa1,
           lwa_cskt,
           lv_object_id.
    IF s_objid IS NOT INITIAL.
      IF lwa_ekko_ekpo_ekkn-bednr IN s_objid.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE lt_eban INTO lwa_eban WITH KEY banfn = lwa_ekko_ekpo_ekkn-banfn
                                              bnfpo = lwa_ekko_ekpo_ekkn-bnfpo
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-pr_txt = lwa_eban-txz01.
      lwa_output-pr_mat = lwa_eban-matnr.
      lwa_output-pr_qty = lwa_eban-menge.
      lwa_output-badat = lwa_eban-badat.
      lwa_output-preis = lwa_eban-preis.
*     BOI by PANUSURI Ticket 75531
      lwa_output-prcrt = lwa_eban-ernam.
      lwa_output-prnam = lwa_eban-afnam.
      lwa_output-bednr = lwa_eban-bednr.
      lwa_output-lfdat = lwa_eban-lfdat.
*     EOI by PANUSURI Ticket 75531
*     BOI by PANUSURI Ticket ACR-56
      lwa_output-idnlf = lwa_eban-idnlf.
      lwa_output-loekz = lwa_eban-loekz.
*     EOI by PANUSURI Ticket ACR-56
      READ TABLE lt_resb INTO lwa_resb WITH KEY rsnum = lwa_eban-arsnr
                                                rspos = lwa_eban-arsps
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_output-wo_mat = lwa_resb-matnr.
        lwa_output-bdmng = lwa_resb-bdmng.
      ELSE.
        CLEAR: lwa_output-bdmng,
               lwa_output-wo_mat.
      ENDIF.
      IF s_womat IS NOT INITIAL.
        IF lwa_resb-matnr IN s_womat.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE lt_ebkn INTO lwa_ebkn WITH KEY banfn = lwa_eban-banfn
                                                bnfpo = lwa_eban-bnfpo
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_viaufkst INTO lwa_viaufkst WITH KEY aufnr = lwa_ebkn-aufnr
                                                          BINARY SEARCH.
        IF sy-subrc = 0.
          lwa_output-aufnr = lwa_viaufkst-aufnr.
          lwa_output-iwerk = lwa_viaufkst-iwerk.
          lwa_output-auart = lwa_viaufkst-auart.
          lwa_output-ernam = lwa_viaufkst-ernam.
          lwa_output-erdat = lwa_viaufkst-erdat.
          lwa_output-loekz_wo = lwa_viaufkst-loekz. "(+)PANUSURI Ticket ACR-56
*          lwa_output-zzdue_date = lwa_viaufkst-zzdue_date.
          lwa_output-swerk = lwa_viaufkst-swerk.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR lwa_lfa1.
    READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko_ekpo_ekkn-lifnr
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-name1 = lwa_lfa1-name1.
    ENDIF.
    CLEAR lwa_cskt.
    READ TABLE lt_cskt INTO lwa_cskt WITH KEY kokrs = lwa_ekko_ekpo_ekkn-kokrs
                                              kostl = lwa_ekko_ekpo_ekkn-kostl
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-ktext = lwa_cskt-ktext.
    ENDIF.
    lwa_output-ebeln = lwa_ekko_ekpo_ekkn-ebeln.
    lwa_output-lifnr = lwa_ekko_ekpo_ekkn-lifnr.
    lwa_output-ekgrp = lwa_ekko_ekpo_ekkn-ekgrp.
    lwa_output-waers = lwa_ekko_ekpo_ekkn-waers.
    lwa_output-zzariba_approver = lwa_ekko_ekpo_ekkn-zzariba_approver.
    lwa_output-ebelp = lwa_ekko_ekpo_ekkn-ebelp.
    lwa_output-loekz_po = lwa_ekko_ekpo_ekkn-loekz. "(+)PANUSURI Ticket ACR-56
    lwa_output-po_txt = lwa_ekko_ekpo_ekkn-txz01.
    lwa_output-po_mat = lwa_ekko_ekpo_ekkn-matnr.
    lwa_output-object_id = lwa_ekko_ekpo_ekkn-bednr.
    lwa_output-po_qty = lwa_ekko_ekpo_ekkn-menge.
    lwa_output-meins = lwa_ekko_ekpo_ekkn-meins.
    lwa_output-netpr = lwa_ekko_ekpo_ekkn-netpr.
    lwa_output-knttp = lwa_ekko_ekpo_ekkn-knttp.
    lwa_output-banfn = lwa_ekko_ekpo_ekkn-banfn.
    lwa_output-bnfpo = lwa_ekko_ekpo_ekkn-bnfpo.
    lwa_output-afnam = lwa_ekko_ekpo_ekkn-afnam.
    lwa_output-sakto = lwa_ekko_ekpo_ekkn-sakto.
    lwa_output-kostl = lwa_ekko_ekpo_ekkn-kostl.
    lwa_output-wbs = lwa_ekko_ekpo_ekkn-ps_psp_pnr.
    lwa_output-aedat = lwa_ekko_ekpo_ekkn-aedat.

    APPEND lwa_output TO lt_output.
    CLEAR: lwa_output, lwa_ekko_ekpo_ekkn.
  ENDLOOP.

ENDFORM.                    " GET_PO_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_SC_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sc_output_data .
  DATA: lv_tabix TYPE sy-tabix.

  LOOP AT lt_sc_details INTO lwa_sc_details.
    CLEAR: lwa_output,
           lwa_eban,
           lwa_resb,
           lwa_ebkn,
           lwa_viaufkst,
           lwa_ekko_ekpo_ekkn,
           lwa_lfa1,
           lwa_cskt.
    lwa_output-object_id = lwa_sc_details-object_id.
    lwa_output-crt_date = lwa_sc_details-posting_date.
    lwa_output-crt_by = lwa_sc_details-created_by.
    lwa_output-number_int = lwa_sc_details-number_int.
    lwa_output-srm_qty = lwa_sc_details-quantity.
    lwa_output-gross_price = lwa_sc_details-gross_price.
    lwa_output-stat = lwa_sc_details-status.
    lwa_output-cur_appr = lwa_sc_details-current_approver.
    lwa_output-next_appr = lwa_sc_details-next_approver.
    READ TABLE lt_eban INTO lwa_eban WITH KEY banfn = lwa_sc_details-ext_demid
                                              bnfpo = lwa_sc_details-ext_dem_posid
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-banfn = lwa_eban-banfn.
      lwa_output-bnfpo = lwa_eban-bnfpo.
      lwa_output-pr_txt = lwa_eban-txz01.
      lwa_output-pr_mat = lwa_eban-matnr.
      lwa_output-pr_qty = lwa_eban-menge.
      lwa_output-badat = lwa_eban-badat.
      lwa_output-preis = lwa_eban-preis.
*     BOI by PANUSURI Ticket 75531
      lwa_output-prcrt = lwa_eban-ernam.
      lwa_output-prnam = lwa_eban-afnam.
      lwa_output-bednr = lwa_eban-bednr.
      lwa_output-lfdat = lwa_eban-lfdat.
*     EOI by PANUSURI Ticket 75531
*     BOI by PANUSURI Ticket ACR-56
      lwa_output-idnlf = lwa_eban-idnlf.
      lwa_output-loekz = lwa_eban-loekz.
*     EOI by PANUSURI Ticket ACR-56

      READ TABLE lt_resb INTO lwa_resb WITH KEY rsnum = lwa_eban-arsnr
                                                rspos = lwa_eban-arsps
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_output-wo_mat = lwa_resb-matnr.
        lwa_output-bdmng = lwa_resb-bdmng.
      ELSE.
        CLEAR: lwa_output-bdmng,
               lwa_output-wo_mat.
      ENDIF.
      IF s_womat IS NOT INITIAL.
        IF lwa_resb-matnr IN s_womat.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE lt_ebkn INTO lwa_ebkn WITH KEY banfn = lwa_eban-banfn
                                                bnfpo = lwa_eban-bnfpo
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_viaufkst INTO lwa_viaufkst WITH KEY aufnr = lwa_ebkn-aufnr
                                                          BINARY SEARCH.
        IF sy-subrc = 0.
          lwa_output-aufnr = lwa_viaufkst-aufnr.
          lwa_output-iwerk = lwa_viaufkst-iwerk.
          lwa_output-auart = lwa_viaufkst-auart.
          lwa_output-ernam = lwa_viaufkst-ernam.
          lwa_output-erdat = lwa_viaufkst-erdat.
          lwa_output-loekz_wo = lwa_viaufkst-loekz. "(+)PANUSURI Ticket ACR-56
*          lwa_output-zzdue_date = lwa_viaufkst-zzdue_date.
          lwa_output-swerk = lwa_viaufkst-swerk.
        ENDIF.
      ENDIF.
*      READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY banfn = lwa_eban-banfn
*                                                                    bnfpo = lwa_eban-bnfpo
*                                                                    BINARY SEARCH.
      READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY ebeln = lwa_eban-ebeln
                                                                    ebelp = lwa_eban-ebelp
                                                                    BINARY SEARCH."(+)PANUSURI Ticket 75531
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        CLEAR lwa_lfa1.
        READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko_ekpo_ekkn-lifnr
                                                  BINARY SEARCH.
        IF sy-subrc = 0.
          lwa_output-name1 = lwa_lfa1-name1.
        ENDIF.
        LOOP AT lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn FROM lv_tabix.
*          IF lwa_ekko_ekpo_ekkn-banfn <> lwa_eban-banfn OR lwa_ekko_ekpo_ekkn-bnfpo <> lwa_eban-bnfpo.
          IF lwa_ekko_ekpo_ekkn-ebeln <> lwa_eban-ebeln OR lwa_ekko_ekpo_ekkn-ebelp <> lwa_eban-ebelp."(+)PANUSURI Ticket 75531
            EXIT.
          ENDIF.
          CLEAR lwa_cskt.
          READ TABLE lt_cskt INTO lwa_cskt WITH KEY kokrs = lwa_ekko_ekpo_ekkn-kokrs
                                                    kostl = lwa_ekko_ekpo_ekkn-kostl
                                                    BINARY SEARCH.
          IF sy-subrc = 0.
            lwa_output-ktext = lwa_cskt-ktext.
          ENDIF.
          lwa_output-ebeln = lwa_ekko_ekpo_ekkn-ebeln.
          lwa_output-lifnr = lwa_ekko_ekpo_ekkn-lifnr.
          lwa_output-ekgrp = lwa_ekko_ekpo_ekkn-ekgrp.
          lwa_output-waers = lwa_ekko_ekpo_ekkn-waers.
          lwa_output-zzariba_approver = lwa_ekko_ekpo_ekkn-zzariba_approver.
          lwa_output-ebelp = lwa_ekko_ekpo_ekkn-ebelp.
          lwa_output-loekz_po = lwa_ekko_ekpo_ekkn-loekz.  "(+)PANUSURI Ticket ACR-56
          lwa_output-po_txt = lwa_ekko_ekpo_ekkn-txz01.
          lwa_output-po_mat = lwa_ekko_ekpo_ekkn-matnr.
          lwa_output-object_id = lwa_ekko_ekpo_ekkn-bednr.
          lwa_output-po_qty = lwa_ekko_ekpo_ekkn-menge.
          lwa_output-meins = lwa_ekko_ekpo_ekkn-meins.
          lwa_output-netpr = lwa_ekko_ekpo_ekkn-netpr.
          lwa_output-knttp = lwa_ekko_ekpo_ekkn-knttp.
          lwa_output-afnam = lwa_ekko_ekpo_ekkn-afnam.
          lwa_output-sakto = lwa_ekko_ekpo_ekkn-sakto.
          lwa_output-kostl = lwa_ekko_ekpo_ekkn-kostl.
          lwa_output-wbs = lwa_ekko_ekpo_ekkn-ps_psp_pnr.
          lwa_output-aedat = lwa_ekko_ekpo_ekkn-aedat.
          APPEND lwa_output TO lt_output.
        ENDLOOP.
      ELSE.
        APPEND lwa_output TO lt_output.
        CLEAR lwa_output.
      ENDIF.
    ELSE.
      READ TABLE lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn WITH KEY ebeln = lwa_sc_details-be_object_id
                                                                    ebelp = lwa_sc_details-be_obj_item
                                                                    BINARY SEARCH.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        CLEAR lwa_lfa1.
        READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko_ekpo_ekkn-lifnr
                                                  BINARY SEARCH.
        IF sy-subrc = 0.
          lwa_output-name1 = lwa_lfa1-name1.
        ENDIF.
        LOOP AT lt_ekko_ekpo_ekkn INTO lwa_ekko_ekpo_ekkn FROM lv_tabix.
          IF lwa_ekko_ekpo_ekkn-ebeln <> lwa_sc_details-be_object_id
                                      OR lwa_ekko_ekpo_ekkn-ebelp <> lwa_sc_details-be_obj_item.
            EXIT.
          ENDIF.
          CLEAR lwa_cskt.
          READ TABLE lt_cskt INTO lwa_cskt WITH KEY kokrs = lwa_ekko_ekpo_ekkn-kokrs
                                                    kostl = lwa_ekko_ekpo_ekkn-kostl
                                                    BINARY SEARCH.
          IF sy-subrc = 0.
            lwa_output-ktext = lwa_cskt-ktext.
          ENDIF.
          lwa_output-ebeln = lwa_ekko_ekpo_ekkn-ebeln.
          lwa_output-lifnr = lwa_ekko_ekpo_ekkn-lifnr.
          lwa_output-ekgrp = lwa_ekko_ekpo_ekkn-ekgrp.
          lwa_output-waers = lwa_ekko_ekpo_ekkn-waers.
          lwa_output-zzariba_approver = lwa_ekko_ekpo_ekkn-zzariba_approver.
          lwa_output-ebelp = lwa_ekko_ekpo_ekkn-ebelp.
          lwa_output-loekz_po = lwa_ekko_ekpo_ekkn-loekz.  "(+)PANUSURI Ticket ACR-56
          lwa_output-po_txt = lwa_ekko_ekpo_ekkn-txz01.
          lwa_output-po_mat = lwa_ekko_ekpo_ekkn-matnr.
          lwa_output-object_id = lwa_ekko_ekpo_ekkn-bednr.
          lwa_output-po_qty = lwa_ekko_ekpo_ekkn-menge.
          lwa_output-meins = lwa_ekko_ekpo_ekkn-meins.
          lwa_output-netpr = lwa_ekko_ekpo_ekkn-netpr.
          lwa_output-knttp = lwa_ekko_ekpo_ekkn-knttp.
          lwa_output-afnam = lwa_ekko_ekpo_ekkn-afnam.
          lwa_output-sakto = lwa_ekko_ekpo_ekkn-sakto.
          lwa_output-kostl = lwa_ekko_ekpo_ekkn-kostl.
          lwa_output-wbs = lwa_ekko_ekpo_ekkn-ps_psp_pnr.
          lwa_output-aedat = lwa_ekko_ekpo_ekkn-aedat.
          APPEND lwa_output TO lt_output.
        ENDLOOP.
      ELSE.
        APPEND lwa_output TO lt_output.
        CLEAR lwa_output.
      ENDIF.
    ENDIF.
    CLEAR: lwa_output.
  ENDLOOP.
  SORT lt_output BY object_id number_int.

ENDFORM.                    " GET_SC_OUTPUT_DATA

*&---------------------------------------------------------------------*
*&      Form  f4_variant_inputhelp
*&---------------------------------------------------------------------*
FORM f4_variant_inputhelp USING uv_variant TYPE slis_vari.
  DATA: lv_exit,
        lt_objects    TYPE TABLE OF vanz,
        lt_valutab    TYPE TABLE OF rsparams,
        ls_objects    TYPE vanz,
        ls_variant    TYPE disvariant,
        ls_variant_x  TYPE disvariant.

  CLEAR: ls_variant,
         ls_variant_x.
  ls_variant-report =   sy-repid.
  ls_variant-username = sy-uname.
  ls_variant-variant  = uv_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_variant
      i_save        = 'A'
    IMPORTING
      e_exit        = lv_exit
      es_variant    = ls_variant_x
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc IS INITIAL AND lv_exit IS INITIAL.
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = sy-repid
        variant              = sy-slset
      TABLES
        valutab              = lt_valutab
        objects              = lt_objects
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    READ TABLE lt_objects INTO ls_objects WITH KEY name = 'P_LAYOUT'.
    IF NOT sy-subrc IS INITIAL.
      READ TABLE lt_objects INTO ls_objects WITH KEY name = 'SP_VARI'.
    ENDIF.
    IF ls_objects-protected IS INITIAL.
      ls_variant-variant = ls_variant_x-variant.
      uv_variant         = ls_variant_x-variant.
    ENDIF.
  ELSE.
    MESSAGE ID sy-msgid TYPE 'S'
                 NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F4_VARIANT_INPUTHELP
*&---------------------------------------------------------------------*
*&      Form  F_DATE_RANGE_CHECK
*&---------------------------------------------------------------------*
FORM f_date_range_check USING it_date TYPE ty_date_range.
  DATA: lv_days TYPE i.
  FIELD-SYMBOLS: <lfs_date> LIKE LINE OF it_date.

  READ TABLE it_date ASSIGNING <lfs_date> INDEX 1.
  IF sy-subrc EQ 0.
    IF <lfs_date>-option = 'BT'.
      lv_days = <lfs_date>-high - <lfs_date>-low.
      IF lv_days GT 90.
        MESSAGE 'Date Range cannot be more than 90 days' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_DATE_RANGE_CHECK
