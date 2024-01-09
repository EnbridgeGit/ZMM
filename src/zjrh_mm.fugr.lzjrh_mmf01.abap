*----------------------------------------------------------------------*
***INCLUDE LZJRH_MMF01 .
*----------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Function Group Name:  ZARIBA_CONTRACT                               *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 17, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        : This FM creates Service contracts through BDC  *
*&                      Call Transaction and Material/Quantity contract*
*&                      using BAPI: BAPI_CONTRACT_CREATE               *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : DECK914182                                           *
* Description   : Initial development                                  *
*----------------------------------------------------------------------*
**----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR gwa_bdcdata.
  gwa_bdcdata-program  = program.
  gwa_bdcdata-dynpro   = dynpro.
  gwa_bdcdata-dynbegin = 'X'.
  APPEND gwa_bdcdata TO git_bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
*  IF fval <> nodata.
  CLEAR gwa_bdcdata.
  gwa_bdcdata-fnam = fnam.
  gwa_bdcdata-fval = fval.
  APPEND gwa_bdcdata TO git_bdcdata.
*  ENDIF.
ENDFORM.                    "bdc_field

*&---------------------------------------------------------------------*
*&      Form  F_BDC_CONTRACTCREATE
*&---------------------------------------------------------------------*
*       Create Service contract using BDC
*----------------------------------------------------------------------*
FORM f_srv_contractcreate  USING  p_wa_contract TYPE zcontractdata_ug.

  DATA: lwa_contract TYPE zcontractdata_ug. " Local work area
  DATA: lv_date(10)     TYPE c, " Agriment creation date
        lv_start(10)    TYPE c, " Agriment start date
        lv_end(10)      TYPE c, " Agriment end date
        lv_qty(20)      TYPE c, " Quantity conversion
        lv_prc(15)      TYPE c. " Price conversion
  DATA: opt TYPE ctu_params.

  opt-dismode = 'N'.
  opt-updmode = 'A'.
  opt-defsize = 'X'.
*  opt-nobinpt = 'X'.
*  opt-nobiend = 'X'.
  MOVE p_wa_contract TO lwa_contract.

**Date formate for BDC
  CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+0(4) INTO lv_date SEPARATED BY '/'.

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'EKKO-EKGRP'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field       USING 'EKKO-LIFNR'  lwa_contract-vendor. " Vendor
  PERFORM bdc_field       USING 'RM06E-EVART' lwa_contract-doc_type. "Document Type
  PERFORM bdc_field       USING 'RM06E-VEDAT' lv_date. " Contract Creation date
  PERFORM bdc_field       USING 'EKKO-EKORG' lwa_contract-purch_org. " Purchase Org
  PERFORM bdc_field       USING 'EKKO-EKGRP' lwa_contract-pur_group. " Purchase Grp

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0514'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'EKKO-BUKRS'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=ENTE'.
  PERFORM bdc_field       USING 'EKKO-BUKRS' lwa_contract-comp_code. " Company code

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EKKO-KTWRT'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  CLEAR lv_start.
  CONCATENATE lwa_contract-agri_start+4(2)
              lwa_contract-agri_start+6(2)
              lwa_contract-agri_start+0(4)
              INTO lv_start SEPARATED BY '/'.
  PERFORM bdc_field       USING 'EKKO-KDATB' lv_start. " Contract Start
  CLEAR lv_end.
  CONCATENATE lwa_contract-agri_end+4(2)
              lwa_contract-agri_end+6(2)
              lwa_contract-agri_end+0(4)
              INTO lv_end SEPARATED BY '/'.
  PERFORM bdc_field       USING 'EKKO-KDATE' lv_end. " Contract end

  CLEAR lv_prc.
  lv_prc =  lwa_contract-target_value.
  CONDENSE lv_prc.
  PERFORM bdc_field       USING 'EKKO-KTWRT' lv_prc. "TARGET_VALUE
*      PERFORM bdc_field       USING 'EKKO-WAERS' wa_contract-curr_key. "Currency Key
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06E-EVRTP(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=DETA'.
  PERFORM bdc_field       USING 'RM06E-EPSTP(01)' lwa_contract-item_cat. "Item Category
  PERFORM bdc_field       USING 'EKPO-KNTTP(01)' lwa_contract-acctasscat. "Acc Assignment cat
  PERFORM bdc_field       USING 'EKPO-TXZ01(01)' lwa_contract-service_txt. "Material text
  PERFORM bdc_field       USING 'EKPO-MATKL(01)' lwa_contract-matl_group. "Material Group

**  Tax code related
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0211'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'EKPO-MWSKZ'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field       USING 'EKPO-MWSKZ' 'C1'. " Tax code defaulted
**  End of Tax code

  PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ESB'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'ESLL-TBTWR(01)'.
  PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                '10'.
  PERFORM bdc_field       USING 'ESLL-KTEXT1(01)' lwa_contract-service_txt. "Service text

  CLEAR lv_qty.
  lv_qty =   lwa_contract-serv_qty.
  CONDENSE lv_qty.
  PERFORM bdc_field       USING 'ESLL-MENGE(01)' lv_qty. "Quantity

  PERFORM bdc_field       USING 'ESLL-MEINS(01)' lwa_contract-uom. " Unit Of Mesure

  CLEAR lv_prc.
  lv_prc =  lwa_contract-gross_price.
  CONDENSE lv_prc.
  PERFORM bdc_field       USING 'ESLL-TBTWR(01)' lv_prc. "Gross price
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0211'.
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06E-EVRTP(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TXP'.
  PERFORM bdc_field       USING 'RM06E-EBELP'
                                '1'.
  PERFORM bdc_field       USING 'RM06E-TCSELFLAG(01)'
                                'X'.
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0106'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06E-LTEX1(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0106'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06E-LTEX1(04)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'RM06E-LTEX1(02)' lwa_contract-contractid. " ERef Text CTR

  PERFORM bdc_dynpro      USING 'SAPLSPO1' '0300'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=YES'.

  IF git_bdcdata[] IS NOT INITIAL.
** Call BDC transaction for OLA create in ECC
    CALL TRANSACTION 'ME31K' USING git_bdcdata
*                     MODE   'N'
*                     UPDATE 'A'
                     OPTIONS FROM opt
                     MESSAGES INTO git_messtab2.
    REFRESH git_bdcdata.
**   Insert OLA_ID in custom table ZCONTRACT_OLAID
    IF sy-subrc EQ 0.
      CLEAR: gwa_olaid,gwa_messtab.
      LOOP AT git_messtab2 INTO gwa_messtab WHERE msgtyp EQ 'S'
                                             AND msgid  EQ '06'
                                             AND msgnr  = '017'. " Created Status
        IF gwa_messtab-msgv2 IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = gwa_messtab-msgv2
            IMPORTING
              output = gwa_olaid-ola_id.
          IF gwa_olaid-ola_id IS NOT INITIAL.
            gwa_olaid-contract_id = lwa_contract-contractid.
            gwa_olaid-create_date = sy-datum.
            APPEND gwa_olaid TO git_olaid.
            CLEAR gwa_olaid.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    REFRESH git_bdcdata.
  ENDIF.

ENDFORM.                    " F_SRV_CONTRACTCREATE
*&---------------------------------------------------------------------*
*&      Form  F_SRV_CONTRACTCHANGE
*&---------------------------------------------------------------------*
*      Service contract header change using BDC
*----------------------------------------------------------------------*
FORM f_srv_contractchange  USING p_wa_contract TYPE zcontractdata_ug.
  DATA: lwa_contract TYPE zcontractdata_ug. " Local work area
  DATA: lv_date(10)     TYPE c, " Agriment creation date
        lv_start(10)    TYPE c, " Agriment start date
        lv_end(10)      TYPE c, " Agriment end date
        lv_prc(15)      TYPE c. " Price conversion

  MOVE p_wa_contract TO lwa_contract.

**Date formate for BDC
  CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+0(4) INTO lv_date SEPARATED BY '/'.

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0205'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RM06E-EVRTN'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field       USING 'RM06E-EVRTN' gv_olaid. " OLAID in ECC

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EKPO-EMATN(03)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KOPF'.
  PERFORM bdc_field       USING 'RM06E-EBELP'
                                '10'.

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'EKKO-EKGRP'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.

  IF lwa_contract-pur_group NE space.
    PERFORM bdc_field       USING 'EKKO-EKGRP' lwa_contract-pur_group. " Purchase Grp
  ENDIF.

  IF lwa_contract-agri_start IS NOT INITIAL.
    CLEAR lv_start.
    CONCATENATE lwa_contract-agri_start+4(2)
                lwa_contract-agri_start+6(2)
                lwa_contract-agri_start+0(4)
                INTO lv_start SEPARATED BY '/'.
    PERFORM bdc_field       USING 'EKKO-KDATB' lv_start. " Contract Start
  ENDIF.

  IF lwa_contract-agri_end IS NOT INITIAL.
    CLEAR lv_end.
    CONCATENATE lwa_contract-agri_end+4(2)
                lwa_contract-agri_end+6(2)
                lwa_contract-agri_end+0(4)
                INTO lv_end SEPARATED BY '/'.
    PERFORM bdc_field       USING 'EKKO-KDATE' lv_end. " Contract end
  ENDIF.

  IF lwa_contract-target_value IS NOT INITIAL.
    CLEAR lv_prc.
    lv_prc =  lwa_contract-target_value.
    CONDENSE lv_prc.
    PERFORM bdc_field       USING 'EKKO-KTWRT' lv_prc. "TARGET_VALUE
  ENDIF.

  PERFORM bdc_dynpro      USING 'SAPLSPO1' '0300'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=YES'.
  IF git_bdcdata[] IS NOT INITIAL.
** Call BDC transaction
    CALL TRANSACTION 'ME32K' USING git_bdcdata
                     MODE   'N'
                     UPDATE 'A'
                     MESSAGES INTO git_messtab1.
    REFRESH git_bdcdata.
  ENDIF.
  CLEAR gv_olaid.
ENDFORM.                    " F_SRV_CONTRACTCHANGE

*&---------------------------------------------------------------------*
*&      Form  F_MAT_CONTRACTCREATE
*&---------------------------------------------------------------------*
*      Material/Quantity contract create using BAPI
*----------------------------------------------------------------------*
FORM f_mat_contractcreate  USING  p_wa_contract TYPE zcontractdata_ug.
  DATA: lwa_contract TYPE zcontractdata_ug,                   " Local work area
        wa_hdr      TYPE bapimeoutheader,                     " Contract Header for BAPI
        wa_hdrx     TYPE bapimeoutheaderx,
        it_item     TYPE STANDARD TABLE OF bapimeoutitem,     "Item data for BAPI
        wa_item     TYPE bapimeoutitem,                       " Work Area for Item
        it_itemx    TYPE STANDARD TABLE OF bapimeoutitemx,    " ItemX data for BAPI
        wa_itemx    TYPE bapimeoutitemx,                      " Work Area for ItemX
        it_itemtxt  TYPE STANDARD TABLE OF bapimeoutitemtext, " Internal table for Item_text
        wa_itemtxt  TYPE bapimeoutitemtext.                  " Work Area for Item_text

  DATA: curr_key  TYPE waers.  " Currency key for company code

  MOVE p_wa_contract TO lwa_contract.

**Get currency key for company code
  SELECT SINGLE waers FROM lfm1 INTO curr_key WHERE lifnr = lwa_contract-vendor.

**  Contract Header
  wa_hdr-comp_code  = lwa_contract-comp_code.
  wa_hdrx-comp_code = 'X'.
  wa_hdr-doc_type   = lwa_contract-doc_type.
  wa_hdrx-doc_type  = 'X'.
  wa_hdr-vendor     = lwa_contract-vendor.
  wa_hdrx-vendor    = 'X'.
  wa_hdr-purch_org  = lwa_contract-purch_org.
  wa_hdrx-purch_org = 'X'.
  wa_hdr-pur_group  = lwa_contract-pur_group.
  wa_hdrx-pur_group = 'X'.

  IF curr_key IS NOT INITIAL.
    wa_hdr-currency  = curr_key.  " Currency from vendor master
    wa_hdrx-currency = 'X'.
  ELSE.
    wa_hdr-currency  = lwa_contract-curr_key. " currency key on file
    wa_hdrx-currency = 'X'.
  ENDIF.

  wa_hdr-vper_start  = lwa_contract-agri_start.
  wa_hdrx-vper_start = 'X'.
  wa_hdr-vper_end    = lwa_contract-agri_end.
  wa_hdrx-vper_end   = 'X'.

**  This is required when doc_type is WK.
  IF lwa_contract-doc_type EQ 'WK'.
    wa_hdr-acum_value  = lwa_contract-target_value.
    wa_hdrx-acum_value = 'X'.
  ENDIF.

** Item Data
  IF lwa_contract-material_txt IS NOT INITIAL.
    wa_item-short_text  = lwa_contract-material_txt.
    wa_itemx-short_text = 'X'.
  ENDIF.

  IF lwa_contract-matl_group IS NOT INITIAL.
    wa_item-matl_group  = lwa_contract-matl_group.
    wa_itemx-matl_group = 'X'.
    wa_item-tax_code   = 'C1'.
    wa_itemx-tax_code  = 'X'.
  ENDIF.

  IF lwa_contract-po_unit IS NOT INITIAL.
    wa_item-po_unit  = lwa_contract-po_unit.
    wa_itemx-po_unit = 'X'.
  ENDIF.

  IF lwa_contract-order_unit IS NOT INITIAL.
    wa_item-orderpr_un  = lwa_contract-order_unit.
    wa_itemx-orderpr_un = 'X'.
  ENDIF.

  IF lwa_contract-conv_num1 IS NOT INITIAL.
    wa_item-conv_num1  = lwa_contract-conv_num1.
    wa_itemx-conv_num1 = 'X'.
  ENDIF.

  IF lwa_contract-conv_den1 IS NOT INITIAL.
    wa_item-conv_den1  = lwa_contract-conv_den1.
    wa_itemx-conv_den1 = 'X'.
  ENDIF.

  IF lwa_contract-price_unit IS NOT INITIAL.
    wa_item-price_unit  = lwa_contract-price_unit.
    wa_itemx-price_unit = 'X'.
  ENDIF.

  IF lwa_contract-acctasscat IS NOT INITIAL.
    wa_item-acctasscat  = lwa_contract-acctasscat.
    wa_itemx-acctasscat = 'X'.
  ENDIF.

  IF lwa_contract-target_qty IS NOT INITIAL.
    wa_item-target_qty  = lwa_contract-target_qty.
    wa_itemx-target_qty = 'X'.
  ENDIF.

  IF lwa_contract-net_price IS NOT INITIAL.
    wa_item-net_price   = lwa_contract-net_price.
    wa_itemx-net_price  = 'X'.
    wa_item-price_date  = '20251231'.
    wa_itemx-price_date = 'X'.
  ENDIF.

  APPEND wa_item TO it_item.
  APPEND wa_itemx TO it_itemx.

**  Item_text data
*      wa_itemtxt-ITEM_NO     = '00010'.
  wa_itemtxt-text_id     = 'K06'.
  wa_itemtxt-text_form   = 'TX'.
  wa_itemtxt-text_line   = lwa_contract-contractid.
  APPEND wa_itemtxt TO it_itemtxt.

  CALL FUNCTION 'BAPI_CONTRACT_CREATE'
    EXPORTING
      header    = wa_hdr
      headerx   = wa_hdrx
    TABLES
      return    = git_msg2
      item      = it_item
      itemx     = it_itemx
      item_text = it_itemtxt.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

**   Insert OLA_ID in custom table ZCONTRACT_OLAID
  IF sy-subrc EQ 0.
    CLEAR: gwa_olaid, gwa_msg.
    LOOP AT git_msg2 INTO gwa_msg WHERE type EQ 'S'
                                   AND id   EQ '06'
                                   AND number = '017'. " Created status
      IF gwa_msg-message_v2 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gwa_msg-message_v2
          IMPORTING
            output = gwa_olaid-ola_id.
        IF gwa_olaid-ola_id IS NOT INITIAL.
          gwa_olaid-contract_id = lwa_contract-contractid.
          gwa_olaid-create_date = sy-datum.
          APPEND gwa_olaid TO git_olaid.
          CLEAR gwa_olaid.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_MAT_CONTRACTCREATE
*&---------------------------------------------------------------------*
*&      Form  F_MAT_CONTRACTCHANGE
*&---------------------------------------------------------------------*
*   Material/Quantity contract change using BAPI
*----------------------------------------------------------------------*
FORM f_mat_contractchange  USING  p_wa_contract TYPE zcontractdata_ug.

  DATA: lwa_contract TYPE zcontractdata_ug,                   " Local work area
        wa_hdr       TYPE bapimeoutheader,                     " Contract Header for BAPI
        wa_hdrx      TYPE bapimeoutheaderx,                    " Contract Headerx for BAPI
        wa_exphdr    TYPE bapimeoutheader.                     " Contract Export header

  MOVE p_wa_contract TO lwa_contract.

  IF lwa_contract-pur_group NE space.
    wa_hdr-pur_group  = lwa_contract-pur_group.
    wa_hdrx-pur_group = 'X'.
  ENDIF.

*        IF wa_contract-curr_key NE space.
*          wa_hdr-currency   = wa_contract-curr_key.
*          wa_hdrx-currency  = 'X'.
*        ENDIF.

  IF lwa_contract-agri_start IS NOT INITIAL.
    wa_hdr-vper_start = lwa_contract-agri_start.
    wa_hdrx-vper_start = 'X'.
  ENDIF.

  IF lwa_contract-agri_end IS NOT INITIAL.
    wa_hdr-vper_end   = lwa_contract-agri_end.
    wa_hdrx-vper_end  = 'X'.
  ENDIF.

  IF lwa_contract-target_value IS NOT INITIAL.
    wa_hdr-acum_value = lwa_contract-target_value.
    wa_hdrx-acum_value = 'X'.
  ENDIF.

  CALL FUNCTION 'BAPI_CONTRACT_CHANGE'
    EXPORTING
      purchasingdocument = gv_olaid
      header             = wa_hdr
      headerx            = wa_hdrx
    IMPORTING
      exp_header         = wa_exphdr
    TABLES
      return             = git_msg1.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.
  CLEAR gv_olaid.
ENDFORM.                    " F_MAT_CONTRACTCHANGE
