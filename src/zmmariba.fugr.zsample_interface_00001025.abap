FUNCTION zsample_interface_00001025.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) LIKE  BKDF STRUCTURE  BKDF OPTIONAL
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
*"      T_BSEU STRUCTURE  BSEU OPTIONAL
*"----------------------------------------------------------------------

*BOC by PANUSURI Ticket 46414
*  DATA: ls_ausz1  LIKE LINE OF t_ausz1,
*        ls_bkpf   LIKE LINE OF t_bkpf,
*
*        ls_reguh  TYPE reguh,
*        lv_laufd  TYPE reguh-laufd,
*        lv_laufi  TYPE reguh-laufi,
*
*        lv_blart  TYPE bkpf-blart,
*        lv_xblnr  TYPE bkpf-xblnr,
*        lv_lifnr  TYPE bseg-lifnr,
*        lv_emnfr  TYPE lfa1-emnfr.
*
*  "Use Valid TCODE
*  READ TABLE t_bkpf INTO ls_bkpf INDEX 1.
*
*  IF ls_bkpf-tcode <> 'F110' AND ls_bkpf-tcode <> 'F111'.
*    EXIT.
*  ENDIF.
*
*  SPLIT ls_bkpf-bktxt AT '-' INTO lv_laufd lv_laufi.
*
*
*
*  LOOP AT t_ausz1 INTO ls_ausz1.
*    "Get Doc Type that is being paid
*    SELECT SINGLE blart xblnr
*      FROM bkpf
*      INTO (lv_blart,lv_xblnr)
*      WHERE belnr = ls_ausz1-belnr
*        AND bukrs = ls_ausz1-bukrs
*        AND gjahr = ls_ausz1-gjahr
*    .
*
*    "Make sure it is ZR for this invoice
*    IF lv_blart IS INITIAL OR lv_blart <> 'ZR' OR sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    "Get vendor for invoice
*    SELECT SINGLE lifnr
*      FROM bseg
*      INTO lv_lifnr
*      WHERE belnr = ls_ausz1-belnr
*        AND bukrs = ls_ausz1-bukrs
*        AND gjahr = ls_ausz1-gjahr
*        AND buzei = ls_ausz1-buzei
*    .
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    "Check if this is the payment run
*    SELECT SINGLE laufd laufi
*      INTO CORRESPONDING FIELDS OF ls_reguh
*      FROM reguh
*      WHERE laufd = lv_laufd
*        AND laufi = lv_laufi
*        AND lifnr = lv_lifnr.
*
*    IF sy-subrc <> 0.
*      "Proposal runs have no entry in table therefore this is a proposal run.
*      EXIT.
*    ENDIF.
*
*
*    "Get ariba vendor information from vendor master
*    SELECT SINGLE emnfr
*      FROM lfa1
*      INTO lv_emnfr
*      WHERE lifnr = lv_lifnr
*    .
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    "Create the ariba status idoc
*    CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
*      EXPORTING
*        im_doc_typ                   = 'ZR'
*        im_inv_doc_no                = ls_ausz1-belnr
*        im_vnd_no                    = lv_lifnr
*        im_arib_com_sup              = lv_emnfr
*        im_sup_inv_no                = lv_xblnr
*        im_inv_status                = 'PAID'
*        im_ztext                     = ''
*        im_inv_amt                   = ''
*        im_year                      = ls_ausz1-gjahr
*        im_port                      = 'SONIC'
*        im_partno                    = 'SONIC'
*        im_parttype                  = 'LS'
*      EXCEPTIONS
*        error_idoc_control           = 1
*        error_idoc_status            = 2
*        error_idoc_data              = 3
*        error_logical_system_unknown = 4
*        error_other                  = 5
*        OTHERS                       = 6.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*  ENDLOOP.
*EOC by PANUSURI Ticket 46414

ENDFUNCTION.
