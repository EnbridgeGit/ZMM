FUNCTION ZMM_CAPPO_DOWNPAY_REQUIRE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"  EXPORTING
*"     VALUE(EV_DOWNPAYMENT) TYPE  XFELD
*"----------------------------------------------------------------------

data: lt_ekpo TYPE TABLE OF ekpo,
      ls_ekpo type ekpo.

ev_downpayment = space.
  SELECT * FROM ekpo INTO TABLE lt_ekpo
    WHERE ebeln = IV_EBELN.
  LOOP AT lt_ekpo INTO ls_ekpo.
    IF ls_ekpo-dptyp IS NOT INITIAL.
      ev_downpayment = 'X'.
    ENDIF.
  ENDLOOP.


ENDFUNCTION.
