FUNCTION ZMM_GET_CONTRACT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EXP_EBELN) TYPE  EBELN
*"     VALUE(EXP_LIFNR) TYPE  ELIFN
*"  EXCEPTIONS
*"      NO_CONTRACT_FOUND
*"----------------------------------------------------------------------

*Get contract for the vendor
  data : lv_contract type ebeln.
  select single ebeln
           from ekko
           into lv_contract
           where ebeln = exp_ebeln
           and bstyp = 'K'
           and lifnr = exp_lifnr.
  if sy-subrc <> 0.
    raise no_contract.
  endif.


ENDFUNCTION.
