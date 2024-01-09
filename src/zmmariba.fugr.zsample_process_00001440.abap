FUNCTION zsample_process_00001440.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"----------------------------------------------------------------------
  DATA:   lv_im_vnd_no        LIKE ekko-lifnr,
          lv_im_arib_com_sup  LIKE lfa1-emnfr,
          lv_im_sup_inv_no    TYPE xblnr,
          lv_int              TYPE integer.

  CHECK t_bkpf-blart = 'ZR'
       OR t_bkpf-blart = 'ZN'."(+)skapse Ticket 78045


  lv_int = strlen( t_bkpf-awkey ).

  lv_int = lv_int - 4.

  SELECT SINGLE lifnr
       FROM rbkp
       INTO lv_im_vnd_no
       WHERE belnr = t_bkpf-awkey(10)
         AND gjahr = t_bkpf-awkey+lv_int(4)
     .

  SELECT SINGLE emnfr
    FROM lfa1
    INTO lv_im_arib_com_sup
    WHERE lifnr = lv_im_vnd_no
  .

  "lv_im_sup_inv_no = t_bkpf-xblnr.

  CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
    EXPORTING
      im_doc_typ                   = t_bkpf-blart
*     im_inv_doc_no                = t_bkpf-belnr
                                         "(-)PANUSURI Ticket 66052
      im_inv_doc_no                = t_bkpf-awkey(10)
                                         "(+)PANUSURI Ticket 66052
      im_vnd_no                    = lv_im_vnd_no
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
ENDFUNCTION.
