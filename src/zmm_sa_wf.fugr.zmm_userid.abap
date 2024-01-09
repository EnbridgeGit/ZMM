FUNCTION ZMM_USERID.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(USERID) TYPE  WFSYST-INITIATOR
*"  EXPORTING
*"     VALUE(BNAME) TYPE  USR21-BNAME
*"----------------------------------------------------------------------

 DATA: ls_usr21 TYPE usr21,
      lv_bname type usr21-bname.
INCLUDE <cntn01>.

  bname = userid+2(12).

ENDFUNCTION.
