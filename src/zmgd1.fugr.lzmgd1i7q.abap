*----------------------------------------------------------------------*
***INCLUDE LMGD1I7Q .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PERPARE_SUB_DOCU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perpare_sub_docu INPUT.
  IF rmmzu-okcode = fcode_clck.
    rmmzu-okcode = 'CV140_CLICK'.
  ENDIF.
  CALL FUNCTION 'CV140_OBJLINK_PASS_OKCODE'
       EXPORTING
            okcode = rmmzu-okcode.
ENDMODULE.                             " PERPARE_SUB_DOCU  INPUT



*---------------------------------------------------------------------*
*       MODULE save_sub_docu INPUT                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE save_sub_docu INPUT.
  DATA tmp_drad LIKE itab_drad OCCURS 0 WITH HEADER LINE.
  CALL FUNCTION 'CV140_LINKS_SAVE'
       IMPORTING
            pf_are_changed    = rmmg2-vb_dokm
       TABLES
            pt_gt_drad        = itab_drad
            pt_gt_drad_change = tmp_drad
       EXCEPTIONS
            error             = 1
            OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF rmmzu-okcode(9) = 'CV140_CAD'.    "CAD-Schnittstelle
    CLEAR bildflag.
    rmmzu-okcode = fcode_ende.
    EXIT.
  ENDIF.
*wk/99c %_G added for Controls in DMS
  IF rmmzu-okcode(5) = 'CV140' OR rmmzu-okcode(3) = '%_G'
     OR rmmzu-okcode = 'WINCLOSED'.                      "note 393396
    CLEAR rmmzu-okcode.
    bildflag = x.
  ENDIF.
* Neuen Wert für das Kennzeichen Techn. Lieferbed. vorh. ermitteln.
  CALL FUNCTION 'MATERIAL_READ_DOCUMENTS'
       EXPORTING
            matnr      = rmmg1-matnr
            aktvstatus = aktvstatus
            kzrfb      = ' '
            neuflag    = neuflag
       IMPORTING
            kztlb      = rmmg2-kztlb
       CHANGING
            rmmzu      = rmmzu.
ENDMODULE.
