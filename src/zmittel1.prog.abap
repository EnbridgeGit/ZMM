REPORT ZMITTEL1.
PARAMETERS REMOVE DEFAULT ' ' AS CHECKBOX.
TABLES TSP02.
SELECT * FROM TSP02 WHERE PJSTATUS = 0.
WRITE: / TSP02-PJIDENT, TSP02-PJNUMMER, 20 TSP02+6.
IF REMOVE = 'X'.
   DELETE FROM TSP02 WHERE PJIDENT = TSP02-PJIDENT
                     AND   PJNUMMER = TSP02-PJNUMMER.
   WRITE: / 'DELETE->', SY-SUBRC.
ENDIF.
ENDSELECT.
