FUNCTION ZMM_GET_ISNET_VENDORS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_ISNVDR) TYPE  Z_ISNVDR
*"     VALUE(IMP_LAND1) TYPE  LAND1_GP
*"     VALUE(IMP_REGIO) TYPE  REGIO
*"  EXPORTING
*"     VALUE(EXP_ISNQUAL) TYPE  Z_ISNQUAL
*"----------------------------------------------------------------------
  DATA: lv_zzisnvdr TYPE z_isnvdr.
* Get ISNET vendors
*Begin of addition by PANUSURI - change request
  SELECT SINGLE zzisnvdr
    FROM lfa1
    INTO lv_zzisnvdr
   WHERE lifnr = imp_isnvdr.

  IF sy-subrc = 0 AND lv_zzisnvdr IS NOT INITIAL.
*End of addition by PANUSURI - change request
    select single isnqual
           from zisnqual
           into exp_isnqual
*           where isnvdr = imp_isnvdr " Commented by PANUSURI - change request
           where isnvdr = lv_zzisnvdr " Addition by PANUSURI - change request
           and   land1 = imp_land1
           and   regio = imp_regio.
  ENDIF.
ENDFUNCTION.
