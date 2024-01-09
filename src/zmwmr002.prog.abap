REPORT ZMWMR002 LINE-COUNT 40(6) LINE-SIZE 62.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  This report will produce Bin Labels using selection criteria.
*  Plant, Storage Location, Material Group and/or Material Number,
*
***********************************************************************
*  CHANGES
*  2002/12/27 mdemeest #--- Created abap
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

TABLES: mara,                "Material Master
        ent1007,             "Storage Location Master
        makt.                "Description

DATA: BEGIN OF TABLE1 OCCURS 5000,
           werks    like mard-werks,
           lgort    like mard-lgort,
           matkl    like mara-matkl,
           MATNR    LIKE MARD-MATNR,
           maktx    like makt-maktx.
DATA: END OF TABLE1.

SELECTION-SCREEN SKIP.
selection-screen comment 1(80) text-001.
selection-screen begin of block box1 with frame.
PARAMETERS:      p_werks  like ent1007-werks obligatory,      "Plant
                 p_lgort  like ent1007-lgort obligatory,      "Stor Loc
                 p_matkl  like mara-matkl.                    "MatGroup
select-options:  s_matnr  for  ent1007-matnr no intervals.    "Material
selection-screen end of  block box1.

*----------------------------------------------------------------------*
START-OF-SELECTION.
   select * from ent1007
     where werks = p_werks
       and lgort = p_lgort
       and matnr in s_matnr.
       select single * from mara
           where ( matkl = p_matkl ).
           if sy-subrc = '0'.
              move ent1007-werks        to table1-werks.
              move mara-matkl           to table1-matkl.
              move ent1007-matnr        to table1-matnr.
              append table1.
           endif.
   endselect.

   sort table1 by werks matkl matnr.

   perform format-output.

write: / 'marylou'.


form format-output.
  loop at table1.
     write: /1 table1-werks, 20 table1-lgort, 30 table1-matnr,
                                        50 table1-matkl, table1-maktx.
  endloop.
endform.















