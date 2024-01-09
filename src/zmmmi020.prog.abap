REPORT ZMMMI020 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.

************************************************************************
* Program     :  ZMMMI019 - MM: EXTRACT MATERIAL MASTERS BASED on
*                           Material Group
* Programmer  :  M DeMeester
* Date        :  October 31,2002
************************************************************************
* 2002/10/31 mdemeest #--- Original Request - Using ZMMMI019 as a basis
*                          create extract.
************************************************************************
* This ABAP will retrieve all Material Numbers matching the Material
* Group entered in the variant.
************************************************************************
*****************************  TABLES   ********************************

TABLES: ENT1027, MBEW.

data: werks1 like mbew-bwkey.
data: werks2 like mbew-bwkey.

*------ Internal table for accumulating quantities for Company ---------
DATA: BEGIN OF TABLE_MATNR OCCURS 0,
        MATKL            LIKE ent1027-matkl,     "Material Group
        MATNR            LIKE ent1027-MATNR,    "Material Number
        maktx            like ent1027-maktx,    "Material Description
        lvorm            like ent1027-lvorm,    "Flagged as deleted
        value(4)         type c,
        verpr1           like mbew-verpr,
        verpr2           like mbew-verpr,
      END OF TABLE_MATNR.

data: record(100) type c.

**************************  DATA ELEMENTS  *****************************

*------ Characteristic Function Call Data Elements  --------------------
*ATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
*      CHARIC            LIKE CABN-ATNAM,       "REQUIRED
*      G_ATINN           LIKE CABN-ATINN,       "REQUIRED
*      G_ATINN_PRIMARY   LIKE CABN-ATINN,       "Primary Characteristic
*      G_ATINN_SECONDARY LIKE CABN-ATINN,       "Secondary Character
*      G_ATINN_MANUNAME  LIKE CABN-ATINN,       "Manufacturer Name
*      G_ATINN_MANUPART  LIKE CABN-ATINN,       "Manufacturer Part Numb
*      G_ATINN_MODEL     LIKE CABN-ATINN.       "Model Part Number

*ATA: BEGIN OF CHAR_TAB OCCURS 20.
*       INCLUDE STRUCTURE AUSP.
*ATA: END OF CHAR_TAB.

*-----------------------------------------------------------------------
*ATA: PRI_LGTH        LIKE AUSP-ATWRT,          "Primary Length
*     SEC_LGTH        LIKE AUSP-ATWRT,          "Secondary Length
*---------------------- Work Area --------------------------------------
*     W_MATKL         LIKE MARA-MATKL,          "Material Group
*     W_QOH           LIKE MBEW-LBKUM,          "Qty on Hand
*     W_VALUE         LIKE MBEW-SALK3,          "Dollar Value
*     W_SLOC_COUNT(2) TYPE P,                   "Storage Loc Count
*     W_AUP_TOTAL     LIKE MBEW-VERPR,          "Total Average Price
*     MATRCTR         TYPE I,                   "Line ctr for material
*     MANUCTR         LIKE MATRCTR,             "Line ctr for manufact
*     MODELCTR        LIKE MATRCTR.             "Line ctr for part/model

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
parameters:     p_MATKL like ent1027-matkl  OBLIGATORY,
                p_value(4)  type c          obligatory.
Select-options: s_werks for mbew-bwkey.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN SKIP.


***************************  MAIN ROUTINE  *****************************
*top-of-page is NOT being used for the headings since the heading is to
* appear only once on the report for ease when the report gets dumped to

* excel
*-----------------------------------------------------------------------
START-OF-SELECTION.
  loop at s_werks.
    if werks1 = space.
       move s_werks+3(4) to werks1.
    else.
       if werks2 = space.
          move s_werks+3(4) to werks2.
       endif.
    endif.
  endloop.

*-------------- read material master for a material group --------------
  SELECT * FROM ent1027
    WHERE MATKL = p_matkl.

    clear table_matnr.
    move ent1027-matnr to table_matnr-matnr.
    move ent1027-maktx to table_matnr-maktx.
    move ent1027-matkl to table_matnr-matkl.
    move ent1027-lvorm to table_matnr-lvorm.
    move p_value       to table_matnr-value.

    select * from mbew
        where matnr = ent1027-matnr
          and bwkey in S_werks.
        if mbew-bwkey = werks1.
           move mbew-verpr to table_matnr-verpr1.
        else.
           if mbew-bwkey = werks2.
               move mbew-verpr to table_matnr-verpr2.
           endif.
        endif.
    endselect.
       append table_matnr.
  endselect.

  sort table_matnr by matnr.

  loop at table_matnr.
    write: /1 table_matnr-matnr,
           20 table_matnr-maktx,
           50 table_matnr-matkl,
           70 table_matnr-lvorm,
           75 table_matnr-value,
           85 table_matnr-verpr1,
           100 table_matnr-verpr2.
  endloop.


***************************  END OF PROGRAM  ***************************





