FUNCTION-POOL zmms2c
   MESSAGE-ID cd                  .
*===> Generated by program RSSCD000
*===> This module is not to be changed.
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzmms2ct00                              . "view rel. data dcl.





*Data for LZMMS2CO01 and LZMMS2CI01
TYPE-POOLS: abap.




DATA:           lv_aktyp            TYPE aktyp,
                gv_isnvdr           TYPE z_isnvdr,
                lv_table_change(1)  TYPE c VALUE abap_false.

FIELD-SYMBOLS:  <isnvdr>.
CONSTANTS:      "c_field_zzisnvdr(23) VALUE '(SAPMF02K)LFA1-ZZISNVDR'.
                c_field_zzisnvdr(23) VALUE 'lfa1-zzisnvdr'.



TYPES: BEGIN OF ty_zisnqual,
          selected(1) TYPE c,
          land1       LIKE zisnqual-land1,
          regio       LIKE zisnqual-regio,
          isnqual     LIKE zisnqual-isnqual,
       END OF ty_zisnqual.

DATA:     ls_zisnqual           TYPE ty_zisnqual,
          lt_zisnqual           TYPE TABLE OF ty_zisnqual,
          ls_zisnqualdb         TYPE zisnqual,
          ls_zisnqual2        TYPE zisnqual,
          lv_tblchanged(1)      TYPE c,
          lv_zisnqual_copied(1) TYPE c VALUE abap_false,
          lv_msg                TYPE string,
          lv_answer(1)          TYPE c,
          lv_objid              TYPE cdobjectv.

CONTROLS: tc_zisnqual TYPE TABLEVIEW USING SCREEN 9001.

DATA:     ls_col LIKE LINE OF tc_zisnqual-cols,
          selected(1) TYPE c.

CONSTANTS: c_okcode(17) TYPE c VALUE '(SAPMF02K)OK-CODE'.

DATA: wa_okcode TYPE sy-ucomm,
      lv_okcode LIKE sy-ucomm.

FIELD-SYMBOLS: <fs_okcode> TYPE ANY.