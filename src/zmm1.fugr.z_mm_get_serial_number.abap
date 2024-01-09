FUNCTION Z_MM_GET_SERIAL_NUMBER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_MBLNR) LIKE  MSEG-MBLNR
*"             VALUE(P_MJAHR) LIKE  MSEG-MJAHR
*"             VALUE(P_ZEILE) LIKE  MSEG-ZEILE
*"       TABLES
*"              ZEQUI STRUCTURE  EQUI
*"----------------------------------------------------------------------

TABLES: SER03, OBJK, EQUI.

SELECT * FROM SER03
       WHERE MBLNR = P_MBLNR
       AND MJAHR = P_MJAHR
       AND ZEILE = P_ZEILE.
       SELECT * FROM OBJK
              WHERE OBKNR = SER03-OBKNR.
              SELECT * FROM EQUI
                     WHERE EQUNR = OBJK-EQUNR.
                     MOVE EQUI TO ZEQUI.
                     APPEND ZEQUI.
                     CLEAR ZEQUI.
              ENDSELECT.
        ENDSELECT.
ENDSELECT.
ENDFUNCTION.
