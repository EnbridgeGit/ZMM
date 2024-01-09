*&---------------------------------------------------------------------*
*& Include MZMM_SLOC_UPDATETOP                                         *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZMM_SLOC_UPDATE           .

* Table declaration
TABLES: MARD,         "Materials by Storage location
        MAKT.


* Internal tables and data declarations

Data: Begin of ITAB occurs 0,
       WERKS like MARD-WERKS,     "Plant
       MATNR like MARD-MATNR,     "Material Number
       LGORT like MARD-LGORT,     "Storage location
       LGPBE like MARD-LGPBE,     "Storage bin location
       NEW_LGPBE like MARD-LGPBE, "New Storage bin location
*       message(40)              , "Status message (-)PANUSURI Ticket 47969
       message(45)               , "Status message (+)PANUSURI Ticket 47969
       maktx like makt-maktx,     "Material Description
       return_message type standard table of BAPI_MATRETURN2, "(+)PANUSURI Ticket 47969
      End of ITAB.

Data: OKCODE like sy-ucomm,
      LINE_COUNT TYPE I.

"Intermediate fields
Data:  NEW_LGPBE like MARD-LGPBE,   "New Storage Bin location
       P_WERKS like MARD-WERKS,     "Plant
       P_MATNR like MARD-MATNR,     "Material Number
             P_LGORT like MARD-LGORT,     "Storage location
       P_LGPBE like MARD-LGPBE.     "Storage bin location

*Data: bdcdata  like bdcdata occurs 50 with header line.

* Control declaration
CONTROLS: TC_STORAGEBIN  TYPE TABLEVIEW USING SCREEN 200,
          TC_STORAGEBIN2 TYPE TABLEVIEW USING SCREEN 300.
DATA  SEL(1). "Selection from table control


* INCLUDE ZCABDC_ROUTINES.                      "UGL
INCLUDE ZMWMI010_BDC_ROUTINES.                  "UGL
