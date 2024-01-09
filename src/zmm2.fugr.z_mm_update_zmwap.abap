FUNCTION Z_MM_UPDATE_ZMWAP.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_ZMWAP) LIKE  ZMWAP STRUCTURE  ZMWAP
*"----------------------------------------------------------------------
  ZMWAP = I_ZMWAP.
  INSERT ZMWAP.
ENDFUNCTION.
