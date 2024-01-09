FUNCTION Z_MM_DELETE_ZMWAP.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_ZMWAP) LIKE  ZMWAP STRUCTURE  ZMWAP
*"----------------------------------------------------------------------
   ZMWAP = I_ZMWAP.
   DELETE ZMWAP.
ENDFUNCTION.
