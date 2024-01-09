REPORT ZMAMRK67.

TABLES: EQUI, ZQUI.

*elect-options: usermat for equi-matnr.
PARAMETERS USERMAT LIKE EQUI-MATNR.

SELECT * FROM EQUI
* where matnr in usermat.
  WHERE MATNR EQ USERMAT.

  MOVE-CORRESPONDING EQUI TO ZQUI.
  INSERT ZQUI.
  WRITE: /001 EQUI-MATNR,
              EQUI-EQUNR,
              'has been archived'.
  DELETE EQUI.

ENDSELECT.
