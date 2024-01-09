FUNCTION ZMM_SCHDAGR_DECISION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"     VALUE(IV_RELEASECODE) TYPE  T16FC-FRGCO
*"  EXPORTING
*"     VALUE(EV_COMMENTS) TYPE  WFSYST-RESULT
*"----------------------------------------------------------------------
 INCLUDE <cntn01>.

gv_ebeln = iv_ebeln.
gv_releasecode = iv_releasecode.
clear: gv_Decision,
       gv_okcode,
       gv_comments.

call SCREEN 9001.

*ev_decision = gv_decision.
ev_comments = gv_comments.

ENDFUNCTION.
