FUNCTION ZMM_CAPPO_DECISION_AP.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"--------------------------------------------------------------------

INCLUDE <cntn01>.

gv_ebeln = iv_ebeln.
"gv_releasecode = iv_releasecode.
clear: gv_Decision,
       gv_okcode,
       gv_comments.

call SCREEN 9003.

*ev_decision = gv_decision.

ENDFUNCTION.
