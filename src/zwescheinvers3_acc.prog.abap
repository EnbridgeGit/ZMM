*&---------------------------------------------------------------------*
*& Report  ZWESCHEINVERS3_ACC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZWESCHEINVERS3_ACC.
TABLES : EKPO, EKKN, ITCSY.
****/:    PERFORM ACCOUNT_NUMBER IN PROGRAM ZWESCHEINVERS3_ACC
****/:    USING &EKPO-EBELN&
****/:    USING &EKPO-EBELP&
****/:    USING &EKPO-KNTTP&
****/:    CHANGING &EKKN-AUFNR&
****/:    CHANGING &EKPO-TXZ01&
****/:    ENDPERFORM


Data lv_knttp type EKPO-KNTTP.
data : wa_par like  itcsy .
data : lv_account type EKKN-AUFNR.
data : lv_ebeln type EKPO-EBELN.
data : lv_ebelp type EKPO-EBELP.
data : lv_proj type char30.

data : lv_account1 type EKKN-PS_PSP_PNR.
data : lv_txz01 type EKPO-TXZ01.


FORM ACCOUNT_NUMBER TABLES IN_PAR STrUCTURE ITCSY
OUT_PAR STRUCTURE ITCSY.
  clear wa_par.
  read table in_par into wa_par  WITH  KEY name = 'EKPO-KNTTP'.
  if sy-subrc eq 0.
    lv_knttp = wa_par-value.
    case lv_knttp.
      when 'F'.
        clear: lv_account, lv_ebeln,lv_ebelp,wa_par.
        read table in_par into wa_par  WITH  KEY name = 'EKPO-EBELN'.
        if sy-subrc eq 0.
          lv_ebeln = wa_par-value.
        endif.
        clear wa_par.
        read table in_par into wa_par  WITH  KEY name = 'EKPO-EBELP'.
        if sy-subrc eq 0.
          lv_ebelp = wa_par-value.
        endif.

        select single aufnr from EKKN into lv_account where EBELN = LV_EBELN
                                                and EBELP = LV_EBELP.

        if sy-subrc eq 0.
          clear wa_par.
          read table out_par into wa_par with key name = 'EKKN-AUFNR'.
          if sy-subrc eq 0.
            wa_par-value = lv_account.
            "MODIFY out_par from wa_par TRANSPORTING value.
            MODIFY out_par from wa_par index 1 TRANSPORTING value.
          endif.
        endif.

      when 'P'.

        clear: lv_account, lv_ebeln,lv_ebelp,wa_par.
        read table in_par into wa_par  WITH  KEY name = 'EKPO-EBELN'.
        if sy-subrc eq 0.
          lv_ebeln = wa_par-value.
        endif.
        clear wa_par.
        read table in_par into wa_par  WITH  KEY name = 'EKPO-EBELP'.
        if sy-subrc eq 0.
          lv_ebelp = wa_par-value.
        endif.

        select single PS_PSP_PNR  from EKKN into lv_account1 where EBELN = LV_EBELN
                                                and EBELP = LV_EBELP.

        if sy-subrc eq 0.
          clear wa_par.
          read table out_par into wa_par with key name = 'EKKN-AUFNR'.
          if sy-subrc eq 0.

            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
              EXPORTING
                INPUT  = lv_account1
              IMPORTING
                OUTPUT = lv_proj.
            wa_par-value = lv_proj.

            MODIFY out_par from wa_par index 1 TRANSPORTING value.
          endif.
        endif.

    endcase.
  endif.
  " Read purchase order text in case  &MABDR-MAKTX&  is not available .
  clear wa_par.
  read table in_par into wa_par with key name = 'MABDR-MAKTX'.
  if sy-subrc eq 0.
    if wa_par-value is INITIAL.
      clear lv_txz01.
      select single TXZ01 from EKPO into lv_txz01 where EBELN = LV_EBELN
                                                    and ebelp = lv_ebelp.
      clear wa_par.
      wa_par-value = lv_txz01.
      MODIFY out_par from wa_par index 2 TRANSPORTING value.
    else.
      clear wa_par.
      read table in_par into wa_par with key name = 'MABDR-MAKTX'.
      if sy-subrc eq 0.
        MODIFY out_par from wa_par index 2 TRANSPORTING value.
      endif.
    endif.

  endif.


ENDFORM.
