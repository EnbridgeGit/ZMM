FUNCTION zsample_interface_00001040.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_XVBUP) LIKE  OFIWA-XVBUP DEFAULT 'X'
*"     REFERENCE(I_RF05R) LIKE  RF05R STRUCTURE  RF05R
*"  TABLES
*"      T_XRAGL STRUCTURE  RAGL1
*"----------------------------------------------------------------------


  DATA:   ls_xragl            TYPE ragl1,
          lv_im_vnd_no        LIKE ekko-lifnr,
          lv_im_arib_com_sup  LIKE lfa1-emnfr,
          lv_im_sup_inv_no    TYPE xblnr,
          lv_blart            TYPE bkpf-blart.
  DATA:   lv_awkey TYPE awkey.  "(+)PANUSURI Ticket 66052

  LOOP AT t_xragl INTO ls_xragl.

    SELECT SINGLE blart
           awkey "(+)PANUSURI Ticket 66052
           FROM bkpf
*           into lv_blart  "(-)PANUSURI Ticket 66052
           INTO (lv_blart, lv_awkey) "(+)PANUSURI Ticket 66052
           WHERE belnr = ls_xragl-belnr
           AND gjahr = ls_xragl-gjahr
       .

    IF lv_blart IS INITIAL OR lv_blart <> 'ZR'
      OR lv_blart <> 'ZN'."(+)SKAPSE Ticket 78045
      CONTINUE.
    ENDIF.



    SELECT SINGLE lifnr
      FROM bseg
      INTO lv_im_vnd_no
      WHERE belnr = ls_xragl-belnr
        AND gjahr = ls_xragl-gjahr
        AND buzei = '1'
    .

    SELECT SINGLE zuonr
      FROM bseg
      INTO lv_im_sup_inv_no
      WHERE belnr = ls_xragl-belnr
        AND gjahr = ls_xragl-gjahr
        AND buzei = '1'
    .


    SELECT SINGLE emnfr
      FROM lfa1
      INTO lv_im_arib_com_sup
      WHERE lifnr = lv_im_vnd_no
    .

    "lv_im_sup_inv_no = t_bkpf-xblnr.

    CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
      EXPORTING
*       im_doc_typ                   = 'ZR' "(-)SKAPSE Ticket 78045
        im_doc_typ                   = lv_blart "(+)SKAPSE Ticket 78045

*       im_inv_doc_no                = ls_xragl-belnr
                                             "(-)PANUSURI Ticket 66052
        im_inv_doc_no                = lv_awkey(10)
                                             "(+)PANUSURI Ticket 66052
        im_vnd_no                    = lv_im_vnd_no
        im_arib_com_sup              = lv_im_arib_com_sup
        im_sup_inv_no                = lv_im_sup_inv_no
        im_inv_status                = 'DELETE'
        im_ztext                     = ''
        im_inv_amt                   = ''
        im_year                      = ls_xragl-gjahr
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

    CLEAR lv_awkey. "(+)PANUSURI Ticket 66052
  ENDLOOP.

ENDFUNCTION.
