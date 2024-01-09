class ZCL_ME_CHANGE_OUTTAB_CUS definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_CHANGE_OUTTAB_CUS .
protected section.
*"* protected components of class ZCL_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_ME_CHANGE_OUTTAB_CUS IMPLEMENTATION.


METHOD if_ex_me_change_outtab_cus~fill_outtab.
*&---------------------------------------------------------------------*
*& Program Name       : ZME_CHANGE_OUTTAB_CUS                          *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 30-Aug-2016                                    *
*& Object ID          : ACR-2161                                       *
*& Application Area   : MM                                             *
*& Description        : Enhance ME2J report to display split accounting*
*&                      values.                                        *
*&---------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 30-Aug-2016                                          *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : ACR-2161                                             *
* Description   : Initial version.                                     *
*----------------------------------------------------------------------*
* Date          : 30-April-2021                                        *
* Modified By   : Achala                                               *
* Description   : COG change to include MSA in ME3L,ME2N,ME3N,ME2M,ME3M*
*----------------------------------------------------------------------*
  DATA: lv_vproz TYPE ekkn-vproz,
        lv_zzparty TYPE zmmt_mastagree-zzparty_agmt_id, " I-   MEDISETA
        lv_zzmsa TYPE zmmt_mastagree-zzparty_agmt_id.   " I-   MEDISETA

  FIELD-SYMBOLS: <fs_outtab>   TYPE any,
                 <fs_ebeln>    TYPE ebeln,
                 <fs_ebelp>    TYPE ebelp,
                 <fs_zekkn>    TYPE dzekkn,
                 <fs_netpr>    TYPE netpr,
                 <fs_wtinv>    TYPE merep_wtinv,
                 <fs_wtlief>   TYPE merep_wtlief,
                 <fs_zzmsa>    TYPE zzmsa.  " I-   MEDISETA


* check that a Account assignment view is displayed
*  CHECK im_struct_name EQ 'MEREP_OUTTAB_ACCOUNTING' AND      "view: account assignment " C -   MEDISETA
  IF im_struct_name EQ 'MEREP_OUTTAB_ACCOUNTING' AND          "view: account assignment " I-   MEDISETA
       im_id EQ 'RM06EKPS'.                                 "T-code: ME2J
* loop at the output table and assign a field symbol
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
*-- assign the purchasing document number to a field symbol
      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_ebeln>.
      CHECK sy-subrc = 0.
*-- assign the purchasing document item number to a field symbol
      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_ebelp>.
      CHECK sy-subrc = 0.
*-- assign the sequential Number of Account Assignment to a field symbol
      ASSIGN COMPONENT 'ZEKKN' OF STRUCTURE <fs_outtab> TO <fs_zekkn>.
      CHECK sy-subrc = 0.
*-- assign the Net Price in Purchasing Document to a field symbol
      ASSIGN COMPONENT 'NETPR' OF STRUCTURE <fs_outtab> TO <fs_netpr>.
      CHECK sy-subrc = 0.
      SELECT SINGLE vproz FROM ekkn INTO lv_vproz
        WHERE ebeln = <fs_ebeln>
          AND ebelp = <fs_ebelp>
          AND zekkn = <fs_zekkn>.
      IF sy-subrc EQ 0 AND lv_vproz IS NOT INITIAL.
        <fs_netpr> = ( <fs_netpr> * lv_vproz ) / 100.
        ASSIGN COMPONENT 'WTLIEF' OF STRUCTURE <fs_outtab> TO <fs_wtlief>.
        IF sy-subrc EQ 0.
          <fs_wtlief> = ( <fs_wtlief> * lv_vproz ) / 100.
        ENDIF.
        ASSIGN COMPONENT 'WTINV' OF STRUCTURE <fs_outtab> TO <fs_wtinv>.
        IF sy-subrc EQ 0.
          <fs_wtinv> = ( <fs_wtinv> * lv_vproz ) / 100.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.                                " I-   MEDISETA
* Begin of insert COG  by MEDISETA
* check that Purchase Document view is displayed
  IF im_struct_name EQ 'MEREP_OUTTAB_PURCHDOC' AND      "view: Purchase document
        ( im_id EQ 'RM06EL00' OR im_id EQ 'RM06EN00' OR im_id EQ 'RM06EM00' ) .   "T-code: ME3L,ME2N,ME3N,ME2M,ME3M
* loop at the output table and assign a field symbol
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
*-- assign the purchasing document number to a field symbol
      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_ebeln>.
      SELECT SINGLE zzparty_agmt_id
             FROM ekko
             INTO lv_zzparty
        WHERE ebeln = <fs_ebeln>.
      IF sy-subrc = 0.
        SELECT SINGLE zzmsa
               FROM zmmt_mastagree
          INTO lv_zzmsa
          WHERE zzparty_agmt_id = lv_zzparty.
        IF sy-subrc = 0.
*-- assign the purchasing document number to a field symbol
          ASSIGN COMPONENT 'ZZMSA' OF STRUCTURE <fs_outtab> TO <fs_zzmsa>.
          IF sy-subrc = 0.
            <fs_zzmsa> = lv_zzmsa.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
* End of insert COG by MEDISETA
ENDMETHOD.
ENDCLASS.
