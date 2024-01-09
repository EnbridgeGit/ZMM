FUNCTION zsample_interface_00001030.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) LIKE  BKDF STRUCTURE  BKDF
*"     VALUE(I_UF05A) LIKE  UF05A STRUCTURE  UF05A
*"     VALUE(I_XVBUP) LIKE  OFIWA-XVBUP DEFAULT 'X'
*"  TABLES
*"      T_AUSZ1 STRUCTURE  AUSZ1 OPTIONAL
*"      T_AUSZ2 STRUCTURE  AUSZ2 OPTIONAL
*"      T_AUSZ3 STRUCTURE  AUSZ_CLR OPTIONAL
*"      T_BKP1 STRUCTURE  BKP1
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEC STRUCTURE  BSEC
*"      T_BSED STRUCTURE  BSED
*"      T_BSEG STRUCTURE  BSEG
*"      T_BSET STRUCTURE  BSET
*"      T_BSEU STRUCTURE  BSEU
*"----------------------------------------------------------------------

  DATA: lv_im_arib_com_sup  LIKE lfa1-emnfr.

  CHECK t_bkpf-blart = 'ZR'
       OR t_bkpf-blart = 'ZN'."(+)SKAPSE Ticket 78045


  IF t_bkpf-tcode = 'FB08'.
    SELECT SINGLE emnfr
      FROM lfa1
      INTO lv_im_arib_com_sup
      WHERE lifnr = t_bseg-lifnr
    .

    CHECK lv_im_arib_com_sup <> ''.


    CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
      EXPORTING
        im_doc_typ                   = t_bkpf-blart
*       im_inv_doc_no                = t_bkpf-belnr
                                             "(-)PANUSURI Ticket 66052
        im_inv_doc_no                = t_bkpf-awkey(10)
                                             "(+)PANUSURI Ticket 66052
        im_vnd_no                    = t_bseg-lifnr
        im_arib_com_sup              = lv_im_arib_com_sup
        im_sup_inv_no                = t_bkpf-xblnr
        im_inv_status                = 'OPEN'
        im_ztext                     = ''
        im_inv_amt                   = ''
        im_year                      = t_bkpf-gjahr
        im_port                      = 'SONIC'
        im_partno                    = 'SONIC'
        im_parttype                  = 'LS'
      EXCEPTIONS
        error_idoc_control           = 1
        error_idoc_status            = 2
        error_idoc_data              = 3
        error_logical_system_unknown = 4
        error_other                  = 5
        OTHERS                       = 6.
    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.
ENDFUNCTION.
