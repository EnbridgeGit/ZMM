FUNCTION zmm_cappo_decision.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"  EXPORTING
*"     VALUE(EV_COMMENTS) TYPE  WFSYST-RESULT
*"     VALUE(EV_FLAG) TYPE  CHAR01
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: lt_ekpo TYPE TABLE OF ekpo,
        ls_ekpo TYPE ekpo.

  gv_ebeln = iv_ebeln.
  "gv_releasecode = iv_releasecode.
  CLEAR: gv_decision,
         gv_okcode,
         gv_comments.

  CALL SCREEN 9001.

*ev_decision = gv_decision.
  ev_comments = gv_comments.


ENDFUNCTION.
