REPORT ZMWMR001 LINE-COUNT 40(6) LINE-SIZE 62.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  This report will list Hazardous Materials from the Material master
*  and through the use of a user parameter list by Plant, Material,
*  and storage location.  A 'FROM' and 'TO' parameter will be provided
*  by the user for PLANT and STORAGE LOCATION.
*           This job was written by Dave Rixen
*           October 08, 1996.       Centra Gas Ontario Inc.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

TABLES: MARA.

DATA     : QTYHAND    TYPE I.
DATA     : BEGIN OF TABLE1 OCCURS 5000,
           MATNR    LIKE MARD-MATNR,


PARAMETERS:
