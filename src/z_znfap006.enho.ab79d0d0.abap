"Name: \PR:ZNFAP006\FO:HR_REMITTANCE_ACKNOWLEDGE_RFC\SE:BEGIN\EI
ENHANCEMENT 0 Z_ZNFAP006.
*BOC by PANUSURI Ticket 46414
**BTBOUNDY
*  DATA:   lv_im_arib_com_sup  LIKE lfa1-emnfr,
*          lv_im_vnd_name1     TYPE name1_gp,
*          lv_po               TYPE esrre,
*          lv_gross_amt        TYPE dmbtr,
*          lv_paid_amt         TYPE rwbtr,
*          lv_disc_amt         TYPE sknto,
*          "lv_pymt_type        TYPE checf,
*          ls_bkpf             TYPE bkpf,
*          ls_bseg             TYPE bseg,
*          ls_reguh            TYPE reguh,
*          lt_reguh            LIKE TABLE OF ls_reguh,
*          ls_regup            TYPE regup,
*          lt_regup            LIKE TABLE OF ls_regup,
*          ls_zeftdup          TYPE zeftdup.
*
*  SELECT SINGLE *
*    FROM bkpf
*    INTO CORRESPONDING FIELDS OF ls_bkpf
*    WHERE xblnr = hrxblnr
*  .
*
*  "Get ISR Reference Number for this invoice
*  SELECT SINGLE ESRRE
*    FROM bseg
*    INTO lv_po
*    WHERE belnr = ls_bkpf-belnr
*      AND gjahr = ls_bkpf-gjahr
*      AND buzei = '1'
*  .
*
*  "Get all the Vendor headers
*  "Loop through them
*  SELECT *
*    FROM reguh
*    INTO CORRESPONDING FIELDS OF TABLE lt_reguh
*    WHERE laufd = zw_laufd
*      AND laufi = zw_laufi
*      AND zbukr = T001-BUKRS
*      AND xvorl = ''
*  .
*
*  LOOP AT lt_reguh INTO ls_reguh.
*
*    "Get all the invoices for each of the Vendor headers.
*    "Loop through them
*    SELECT *
*      FROM regup
*      INTO CORRESPONDING FIELDS OF TABLE lt_regup
*      WHERE laufd = ls_reguh-laufd
*        AND laufi = ls_reguh-laufi
*        AND xvorl = ls_reguh-xvorl
*        AND zbukr = T001-BUKRS
*        AND vblnr = ls_reguh-vblnr
*    .
*
*    LOOP AT lt_regup INTO ls_regup.
*
*      "Check the document type for this invoice
*      IF ls_regup-blart = 'ZR'.
*        "Get payment type from payr
*        "Not for EFT
*        "SELECT SINGLE chect
*          "FROM payr
*          "INTO lv_pymt_type
*          "WHERE laufd = ls_reguh-laufd
*            "AND laufi = ls_reguh-laufi
*            "AND vblnr = ls_reguh-vblnr
*        ".
*
*        "Get ariba common supplier
*        SELECT SINGLE emnfr
*          FROM lfa1
*          INTO lv_im_arib_com_sup
*          WHERE lifnr = ls_regup-lifnr
*        .
*
*        "Get vendor name
*        SELECT SINGLE name1
*          FROM lfa1
*          INTO lv_im_vnd_name1
*          WHERE lifnr = ls_regup-lifnr
*        .
*
*        "Get absolute values of the 3 amounts
*        IF ls_regup-dmbtr < 0.
*          lv_gross_amt  = ls_regup-dmbtr * - 1.
*        ELSE.
*          lv_gross_amt  = ls_regup-dmbtr.
*        ENDIF.
*
*        lv_paid_amt = ls_regup-dmbtr - ls_regup-sknto.
*        IF lv_paid_amt < 0.
*          lv_paid_amt  = lv_paid_amt * - 1.
*        ELSE.
*          lv_paid_amt  = lv_paid_amt.
*        ENDIF.
*
*        IF ls_regup-sknto < 0.
*          lv_disc_amt  = ls_regup-sknto * -1.
*        ELSE.
*          lv_disc_amt  = ls_regup-sknto.
*        ENDIF.
*
*        "Check for duplicate send
*        SELECT SINGLE *
*          FROM zeftdup
*          INTO CORRESPONDING FIELDS OF ls_zeftdup
*          WHERE laufd = ls_regup-laufd
*            AND laufi = ls_regup-laufi
*            AND belnr = ls_regup-belnr
*        .
*
*        IF sy-subrc = 4.
*          "No entry in the table.
*          "Send IDOC
*
*          ls_zeftdup-laufd = ls_regup-laufd.
*          ls_zeftdup-laufi = ls_regup-laufi.
*          ls_zeftdup-belnr = ls_regup-belnr.
*
*          CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
*            EXPORTING
*              im_doc_typ                   = 'ZR'
*              im_inv_doc_no                = ls_regup-belnr
*              im_vnd_no                    = ls_regup-lifnr
*              im_arib_com_sup              = lv_im_arib_com_sup
*              im_sup_inv_no                = ls_regup-xblnr
*              im_inv_status                = 'PAID'
*              im_ztext                     = ''
*              im_inv_amt                   = ''
*              im_year                      = ls_regup-gjahr
*              im_port                      = 'SONIC'
*              im_partno                    = 'SONIC'
*              im_parttype                  = 'LS'
*            EXCEPTIONS
*              error_idoc_control           = 1
*              error_idoc_status            = 2
*              error_idoc_data              = 3
*              error_logical_system_unknown = 4
*              error_other                  = 5
*              OTHERS                       = 6
*          .
*
*          CALL FUNCTION 'Z_IDOC_CREATE_ZARBREM01'
*            EXPORTING
*              im_inv_doc_no                 = ls_regup-belnr
*              im_vnd_no                     = ls_regup-lifnr
*              im_vnd_name1                  = lv_im_vnd_name1
*              im_sup_inv_no                 = ls_regup-xblnr
*              im_arib_com_sup               = lv_im_arib_com_sup
*              im_po                         = lv_po
*              im_gross_amt                  = lv_gross_amt
*              im_paid_amt                   = lv_paid_amt
*              im_pymt_date                  = ls_reguh-zaldt
*              im_disc_amt                   = lv_disc_amt
*              im_pymt_ref                   = 'T'
*              im_pymt_type                  = ls_regup-vblnr
*              im_doc_date                   = ls_regup-budat
*              im_port                       = 'SONIC'
*              im_partno                     = 'SONIC'
*              im_parttype                   = 'LS'
*            EXCEPTIONS
*              error_idoc_control            = 1
*              error_idoc_status             = 2
*              error_idoc_data               = 3
*              error_logical_system_unknown  = 4
*              error_other                   = 5
*              OTHERS                        = 6
*          .
*
*          "Mark this one as sent
*          INSERT zeftdup FROM ls_zeftdup.
*          COMMIT WORK.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*EOC by PANUSURI Ticket 46414
ENDENHANCEMENT.
