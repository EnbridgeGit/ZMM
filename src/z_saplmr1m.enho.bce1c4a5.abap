"Name: \PR:SAPLMR1M\FO:DELETE\SE:BEGIN\EI
ENHANCEMENT 0 Z_SAPLMR1M.
*BTBOUNDY
  DATA:   lv_im_arib_com_sup  LIKE lfa1-emnfr,
          lv_text             TYPE char255.

  IF rbkpv-blart = 'ZR'
    or rbkpv-blart = 'ZN'."(+)SKAPSE Ticket 78045

    "Get ariba common supplier
    SELECT SINGLE emnfr
      FROM lfa1
      INTO lv_im_arib_com_sup
      WHERE lifnr = rbkpv-lifnr
    .

    lv_text = rbkpv-belnr.
    SHIFT lv_text LEFT DELETING LEADING '0'.

    CONCATENATE sy-uname ' @ Date: ' sy-datlo(4) ':' sy-datlo+4(2) ':' sy-datlo+6(2)
                ' AND: ' sy-timlo(2) ':' sy-timlo+2(2) ':' sy-timlo+4(2)
                ' - INVOICE# ' lv_text ' WAS DELETED BY SPECTRA' INTO lv_text.

    CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
      EXPORTING
        "im_doc_typ                   = 'ZR'  "(-) skapse ticket 78045
        im_doc_typ                   = rbkpv-blart "(+)SKAPSE Ticket 78045
        im_inv_doc_no                = rbkpv-belnr
        im_vnd_no                    = rbkpv-lifnr
        im_arib_com_sup              = lv_im_arib_com_sup
        im_sup_inv_no                = rbkpv-xblnr
        im_inv_status                = 'DELETE'
        im_ztext                     = lv_text
        im_inv_amt                   = ''
        im_year                      = rbkpv-gjahr
        im_port                      = 'SONIC'
        im_partno                    = 'SONIC'
        im_parttype                  = 'LS'
      EXCEPTIONS
        error_idoc_control           = 1
        error_idoc_status            = 2
        error_idoc_data              = 3
        error_logical_system_unknown = 4
        error_other                  = 5
        OTHERS                       = 6
    .
  ENDIF.
ENDENHANCEMENT.
