FUNCTION Z_MM_DELETE_INDX.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_SRTFD) LIKE  INDX-SRTFD
*"       EXCEPTIONS
*"              FAILED
*"----------------------------------------------------------------------
  TABLES: INDX.

  CHECK I_SRTFD(3) = 'WAR'.
  DELETE FROM INDX WHERE RELID = 'ZW' AND
                         SRTFD = I_SRTFD.
ENDFUNCTION.
