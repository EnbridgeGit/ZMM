FUNCTION zmm_schdagr_me38.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"  EXPORTING
*"     VALUE(EV_SCHEDULE_UPDATED) TYPE  EKKO-LOEKZ
*"----------------------------------------------------------------------
  INCLUDE <cntn01>.

  gv_ebeln = iv_ebeln.
  CLEAR: gv_okcode,
         gv_schedule_updated.

  CALL SCREEN 9002.

EV_SCHEDULE_UPDATED = gv_schedule_updated.

ENDFUNCTION.
