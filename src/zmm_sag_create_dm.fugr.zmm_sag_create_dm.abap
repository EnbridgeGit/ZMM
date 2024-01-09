FUNCTION zmm_sag_create_dm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LV_MODE) TYPE  CHAR1 DEFAULT 'N'
*"     VALUE(LV_UPDATE) TYPE  CHAR1 DEFAULT 'S'
*"     VALUE(HEADER) TYPE  ZMM_SAGHEADER_DM
*"  EXPORTING
*"     VALUE(LV_SUBRC) LIKE  SYST-SUBRC
*"     VALUE(LV_OUTPUT) TYPE  CHAR100
*"  TABLES-
*"      IT_ITEMSAG STRUCTURE  ZMM_ITEMSAG
*"      IT_PRICONDSAG STRUCTURE  ZMM_PRICECONDSAG
*"      IT_SAGMAINTAIN STRUCTURE  ZMM_SAGMAINTAIN
*"      IT_RETURN STRUCTURE  ZMM_RETURNMSG
*"---------------------------------------------------------------------_
  DATA:   lv_ebeln     TYPE ebeln,
          lv_tcselflag TYPE string,
          lv_matnr TYPE string,
          lv_ktmng TYPE string,
          lv_meins TYPE string,
          lv_matkl TYPE string,
          lv_knttp TYPE string,
          lv_netpr TYPE string,
          lv_lines TYPE i,
          lv_cntp TYPE aufep VALUE '1',
          lv_count(5) VALUE 0,       "without zeros
          lv_countl TYPE i,"(5) VALUE 0,       "without zeros
          lv_count1(5),              " zeros
          lv_lifnr TYPE lifnr,
          lv_adrlines TYPE i,
          lv_ordadr TYPE char1,
          lv_shadr TYPE char1.

* Work Area Declarations *
  DATA: lwa_itemsag       TYPE zmm_itemsag,
        lwa_pricondsag    TYPE zmm_pricecondsag,
        lwa_sagmaintain   TYPE zmm_sagmaintain,
        lwa_returnmsg     TYPE zmm_returnmsg,
        t_wyt3 TYPE STANDARD TABLE OF wyt3.

  DATA:   "BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
         lta_message LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
         lwa_message TYPE bdcmsgcoll.

  lv_subrc = 0.
  CLEAR : lwa_pricondsag,lwa_sagmaintain, lwa_returnmsg, lwa_message, lv_lifnr,
          lv_shadr,lv_ordadr.
  REFRESH : lta_message, bdcdata, t_wyt3.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = header-lifnr
    IMPORTING
      output = lv_lifnr.

  IF NOT lv_lifnr IS INITIAL.
    SELECT * FROM wyt3 INTO TABLE t_wyt3
      WHERE lifnr = lv_lifnr
        AND parvw = 'BA'.
    IF sy-subrc = 0.
      DESCRIBE TABLE t_wyt3 LINES lv_adrlines.
      IF lv_adrlines GT 1.
        lv_ordadr = 'X'.
      ENDIF.
    ENDIF.
    REFRESH:t_wyt3.
    CLEAR lv_adrlines.
    SELECT * FROM wyt3 INTO TABLE t_wyt3
      WHERE lifnr = lv_lifnr
        AND parvw = 'WL'.
    IF sy-subrc = 0.
      DESCRIBE TABLE t_wyt3 LINES lv_adrlines.
      IF lv_adrlines GT 1.
        lv_shadr = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

**********************Hedaer Details*********************
*********************************************************

  PERFORM bdc_dynpro      USING 'SAPMM06E' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06E-LGORT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EKKO-LIFNR'
                                header-lifnr.          " '0000052478'.
  PERFORM bdc_field       USING 'RM06E-EVART'
                                header-evart.          "'DAWN'.
  PERFORM bdc_field       USING 'RM06E-VEDAT'
                                header-vedat.          "'DOC Date'.
  PERFORM bdc_field       USING 'EKKO-EKORG'
                                header-ekorg.          "'GASA'.
  PERFORM bdc_field       USING 'EKKO-EKGRP'
                                header-ekgrp.          "'GP2'.
  PERFORM bdc_field       USING 'RM06E-WERKS'
                                header-werks.          "'GEGD'.
  PERFORM bdc_field       USING 'RM06E-LGORT'
                                header-lgort.          "'A001'
  IF lv_ordadr = 'X'.
    PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  '04/02'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SELV'.
  ENDIF.
  IF lv_shadr = 'X'.
    PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  '04/02'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SELV'.
  ENDIF.
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0201'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

*  PERFORM bdc_field       USING 'EKKO-EKGRP'
*                                header-ekgrp.         "'GP2'.
  PERFORM bdc_field       USING 'EKKO-KDATB'          "'27.04.2021'.
                                header-kdatb.
  PERFORM bdc_field       USING 'EKKO-KDATE'          "'29.06.2021'.
                                header-kdate.
  PERFORM bdc_field       USING 'EKKO-ZTERM'
                                header-zterm.         "'25TH'.
  PERFORM bdc_field       USING 'EKKO-WAERS'
                                header-waers.         "'USD/CAD'.
  PERFORM bdc_field       USING 'EKKO-INCO1'
                                header-inco1.         "'GCD'.
  PERFORM bdc_field       USING 'EKKO-INCO2'
                                header-inco2.         "'Chicago'.
  PERFORM bdc_field       USING 'EKKO-VERKF'
                                header-verkf.         "'Deal Id'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EKKO-ZZTRLOC1'.
  PERFORM bdc_field       USING 'EKKO-ZZCONDAYQTY'
                                header-zzcondayqty.   "'10,000 MMBTU/DAY'.
  PERFORM bdc_field       USING 'EKKO-ZZCONPRICE'
                                header-zzconprice.    "'-.15 USD/MMBTU'.
  PERFORM bdc_field       USING 'ZMMT_MASTAGREE-ZZMSA'
                                header-zzmsa.         "'CGY_NAESB(C)_612'.
  PERFORM bdc_field       USING 'EKKO-ZZEKGRP'
                                header-zzekgrp.       "'G02'.
  PERFORM bdc_field       USING 'EKKO-ZZPARTY'
                                header-zzparty.         " PEPL
  PERFORM bdc_field       USING 'EKKO-ZZTRLOC1'
                                header-zztrloc1.            "'02513'
***Condition for location
  IF header-zztrloc2 IS NOT INITIAL.
    PERFORM bdc_field     USING 'EKKO-ZZTRLOC2'
                                header-zztrloc2.            "'02513'
  ENDIF.
  IF header-zztrloc3 IS NOT INITIAL.
    PERFORM bdc_field     USING 'EKKO-ZZTRLOC3'
                                header-zztrloc3.            "'02513'
  ENDIF.
  IF header-zztrloc4 IS NOT INITIAL.
    PERFORM bdc_field     USING 'EKKO-ZZTRLOC4'
                               header-zztrloc4.             "'02513'
  ENDIF.
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06E-EVRTP(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=DETA'.
**********************Item Details***********************
*********************************************************
  DESCRIBE TABLE it_itemsag LINES lv_lines.
  LOOP AT it_itemsag INTO lwa_itemsag.
    lv_countl = lv_countl + 1.
    IF lv_cntp GE 2.

    ELSE.
      lv_count = lv_count + 1.                    "item Count
    ENDIF.
*  PERFORM convert_count CHANGING lv_count1 USING  lv_count.
***convert with leading zero
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_count
      IMPORTING
        output = lv_count1.
****Field with screen count
    CONCATENATE 'RM06E-TCSELFLAG(' lv_count1 ')' INTO lv_tcselflag.
    CONCATENATE 'EKPO-EMATN(     ' lv_count1 ')' INTO lv_matnr.
    CONCATENATE 'EKPO-KTMNG(     ' lv_count1 ')' INTO lv_ktmng.
    CONCATENATE 'EKPO-MEINS(     ' lv_count1 ')' INTO lv_meins.
    CONCATENATE 'EKPO-MATKL(     ' lv_count1 ')' INTO lv_matkl.

    PERFORM bdc_field       USING lv_tcselflag  'X'.                  "'RM06E-TCSELFLAG(01)'
    PERFORM bdc_field       USING lv_matnr    lwa_itemsag-matnr.      "'NATGAS'.          "'EKPO-matnr(01)'
    PERFORM bdc_field       USING lv_ktmng    lwa_itemsag-ktmng.      "'10000'.           "'EKPO-KTMNG(01)'
    PERFORM bdc_field       USING lv_meins    lwa_itemsag-meins.      " GJ/MMB            "'EKPO-MEINS(01)'
    PERFORM bdc_field       USING lv_matkl    lwa_itemsag-matkl.      "'2000'.            "'EKPO-MATKL(01)'

    IF lwa_itemsag-matnr NE 'NATGAS'.
      CONCATENATE 'EKPO-KNTTP(     ' lv_count1 ')' INTO lv_knttp.
      PERFORM bdc_field      USING lv_knttp    lwa_itemsag-knttp.     "                   "'EKPO-KNTTP(01)'
      CONCATENATE 'EKPO-NETPR(     ' lv_count1 ')' INTO lv_netpr.
      PERFORM bdc_field      USING lv_netpr    lwa_itemsag-netpr.     "                   "'EKPO-NETPR(01)'
    ENDIF.

    PERFORM bdc_dynpro      USING 'SAPMM06E'   '0211'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'EKPO-BEDNR'.

*    PERFORM bdc_field       USING 'EKPO-TXZ01' lwa_itemsag-txz01.      "'Natural gas (for purchasing & sales)'.
    PERFORM bdc_field       USING 'EKPO-BEDNR' lwa_itemsag-bednr. "'202104'.
    PERFORM bdc_field       USING 'EKPO-MWSKZ' lwa_itemsag-mwskz.       "'I0'.
    PERFORM bdc_field       USING 'EKPO-EVERS' lwa_itemsag-evers.       "'FI'.

    IF lwa_itemsag-matnr = 'NATGAS'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=DETZ'.
      PERFORM bdc_dynpro      USING 'SAPMM06E'   '0212'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'EKPO-MEPRF'.
      PERFORM bdc_field       USING 'EKPO-MEPRF' '5'.

      PERFORM bdc_field       USING 'BDC_OKCODE' '=KO'.
    ELSE.
*      PERFORM bdc_field       USING 'BDC_OKCODE' '=KN'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=DETZ'.

*Account Assignment
*      GL Account
      PERFORM bdc_dynpro      USING 'SAPMM06E'   '0511'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'EKKN-SAKTO'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=ENTE'.
      PERFORM bdc_field        USING 'EKKN-SAKTO' lwa_itemsag-sakto.       " GL Account 302010

*     Internal Order
      PERFORM bdc_dynpro      USING 'SAPLKACB'   '0002'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'COBL-KOSTL'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=ENTE'.
      PERFORM bdc_field       USING 'COBL-KOSTL' lwa_itemsag-kostl.      " Cost Center ++

      PERFORM bdc_dynpro      USING 'SAPMM06E'   '0212'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'EKPO-MEPRF'.
      PERFORM bdc_field       USING 'EKPO-MEPRF' '5'.

      IF  lv_lines EQ lv_countl.
        PERFORM bdc_field     USING 'BDC_OKCODE' '=BU'.
        EXIT.
      ENDIF.
    ENDIF.

**********************Pricing codition*******************
*********************************************************
    IF lwa_itemsag-matnr = 'NATGAS'.
      PERFORM bdc_dynpro      USING 'SAPLV69A' '9000'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'KOMV-KBETR(09)'.

      DATA: lv_prirow TYPE i.
      DESCRIBE TABLE it_pricondsag LINES lv_prirow.
      DATA: lv_pricnt(2) VALUE '09',
            lv_kschl TYPE string,
            lv_koein TYPE string,
            lv_kbetr TYPE string,
            lv_kpein TYPE string.

      LOOP AT it_pricondsag INTO lwa_pricondsag.
        IF  ( ( lwa_pricondsag-kschl = 'PBxx' ) OR ( lwa_pricondsag-kschl = 'PBXX' ) ).        "fixed
          PERFORM bdc_field       USING 'KOMV-KBETR(02)'  lwa_pricondsag-kbetr.                "'-0.15'.
          PERFORM bdc_field       USING 'RV61A-KOEIN(02)' header-waers.                        "USD
          PERFORM bdc_field       USING 'KOMV-KPEIN(02)' '1000'.
        ELSEIF ( ( lwa_pricondsag-kschl = 'ZC01' ) OR ( lwa_pricondsag-kschl = 'ZC00')  ).     "surcharge - zc00 " discount - zc01

          IF lv_prirow EQ 1 .
            lv_pricnt = '09'.
          ELSEIF lv_prirow > 1.
            lv_pricnt = lv_pricnt + 1.
          ENDIF.

          CONCATENATE 'KOMV-KSCHL(     ' lv_pricnt ')' INTO lv_kschl.
          CONCATENATE 'KOMV-KBETR(     ' lv_pricnt ')' INTO lv_kbetr.
          CONCATENATE 'RV61A-KOEIN(     ' lv_pricnt ')' INTO lv_koein.
          CONCATENATE 'KOMV-KPEIN(     ' lv_pricnt ')' INTO lv_kpein.
          PERFORM bdc_field       USING lv_kschl  lwa_pricondsag-kschl.              "'ZC01'.
          PERFORM bdc_field       USING lv_kbetr  lwa_pricondsag-kbetr.              "'-0.15'.
          PERFORM bdc_field       USING lv_koein  header-waers.                      "'USD'.
          PERFORM bdc_field       USING lv_kpein  '1000'.
          CLEAR: lwa_pricondsag.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF lv_count1 GE 15.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
      lv_cntp = lv_cntp + 1.
      PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '/00'.
      PERFORM bdc_field       USING 'RM06E-EBELP'
                                     lv_cntp.
    ENDIF.
*    PERFORM bdc_dynpro      USING 'SAPMM06E'   '0211'.
    PERFORM bdc_dynpro      USING 'SAPMM06E'   '0220'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'RM06E-EVRTP(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=DETA'.

    IF  lv_lines EQ lv_countl.
      PERFORM bdc_field     USING 'BDC_OKCODE' '=BU'.
    ENDIF.
    CLEAR: lwa_itemsag.
  ENDLOOP.

***call transaction***
  CLEAR :lv_output,lv_ebeln.

  DATA: wa_opt LIKE ctu_params.
  wa_opt-dismode = lv_mode.
  wa_opt-updmode = lv_update.
  wa_opt-defsize = 'X'.

*  PERFORM bdc_transaction TABLES lta_message
*                          USING  'ME31L'
*                                  'X'
*                                  lv_mode
*                                  lv_update.

  CALL TRANSACTION 'ME31L' USING bdcdata
                           OPTIONS FROM wa_opt
                           MESSAGES INTO lta_message.
  IF sy-subrc EQ 0.
    CLEAR lv_ebeln.
    LOOP AT lta_message INTO lwa_message WHERE msgtyp EQ 'S'.
      CLEAR : lv_output, lwa_message.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = sy-msgid
          lang      = '-D'
          no        = sy-msgno
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = lv_output
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc EQ 0.
        lwa_returnmsg-errormessage = lv_output.
        lwa_returnmsg-errortype    = 'S'.
        APPEND lwa_returnmsg TO it_return.
        lv_ebeln = sy-msgv2.
        SORT it_return BY errortype .
        DELETE ADJACENT DUPLICATES FROM it_return COMPARING errormessage.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR : lv_output, lwa_message.
    LOOP AT lta_message INTO lwa_message WHERE msgtyp EQ 'E'.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = sy-msgid
          lang      = '-D'
          no        = sy-msgno
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = lv_output
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc EQ 0.
        lwa_returnmsg-errormessage = lv_output.
        lwa_returnmsg-errortype    = 'E'.
        APPEND lwa_returnmsg TO it_return.
      ENDIF.
      CLEAR : lv_output, lwa_message.
    ENDLOOP.
    IF it_return[] IS INITIAL.
      lwa_returnmsg-errormessage = 'Validate input data.'.
      lwa_returnmsg-errortype    = 'W'.
      APPEND lwa_returnmsg TO it_return.
    ENDIF.
  ENDIF.
*************************************************************************
**Successfully crate
** Put condiion here
*************************************************************************
  REFRESH bdcdata.
  CLEAR: lv_count1, lv_cntp,lv_countl.
  IF lv_ebeln IS NOT INITIAL.

**********************Scheduling Agreemnt Maintain******************
********************************************************************
    DATA:   lv_evrtp      TYPE  string,
            lv_tcselflag1 TYPE string,
            lv_rows       TYPE i,
            lv_cnt(5)     VALUE 0,
            "without zeros
            lv_cnt1(5).                 " zeros
    lv_cntp = lv_cntp + 1.
    DESCRIBE TABLE it_sagmaintain LINES lv_rows.

    PERFORM bdc_dynpro      USING 'SAPMM06E'    '0205'.
    PERFORM bdc_field       USING 'BDC_CURSOR'  'RM06E-EVRTN'.
    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
    PERFORM bdc_field       USING 'RM06E-EVRTN' lv_ebeln.

    LOOP AT it_sagmaintain INTO lwa_sagmaintain.
      lv_countl = lv_countl + 1.                    "item Count
      IF lv_cntp GE 2.
*         lv_cnt1 = 14.
      ELSE.
        lv_cnt = lv_cnt + 1.
***convert with leading zero
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_cnt
          IMPORTING
            output = lv_cnt1.
      ENDIF.
****Field with screen count
      CONCATENATE 'RM06E-EVRTP(    ' lv_cnt1 ')' INTO lv_evrtp.
      CONCATENATE 'RM06E-TCSELFLAG(' lv_cnt1 ')' INTO lv_tcselflag1.

      PERFORM bdc_dynpro      USING 'SAPMM06E' '0222'.
      PERFORM bdc_field       USING 'BDC_CURSOR' lv_evrtp.                             "'RM06E-EVRTP(01)'.  "counter

      PERFORM bdc_field       USING 'BDC_OKCODE' '=ET'.
      PERFORM bdc_field       USING 'RM06E-EBELP' '1'.

      PERFORM bdc_field       USING lv_tcselflag1  'X'.                                "'RM06E-TCSELFLAG(   "counter

      PERFORM bdc_dynpro      USING 'SAPMM06E' '1117'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'EKET-MENGE(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
      PERFORM bdc_field       USING 'RM06E-LPEIN(01)'  lwa_sagmaintain-lpein.          "'RM06E-LPEIN(01)'
      PERFORM bdc_field       USING 'RM06E-EEIND(01)'  lwa_sagmaintain-eeind.          "'RM06E-EEIND(01)'
      PERFORM bdc_field       USING 'EKET-MENGE(01)'   lwa_sagmaintain-menge.          "'EKET-MENGE(01)'

      PERFORM bdc_dynpro      USING 'SAPMM06E'    '1117'.
      PERFORM bdc_field       USING 'BDC_CURSOR'  'EKET-MENGE(01)'.
      PERFORM bdc_field     USING 'RM06E-ETNR1' '1'.
*      IF lv_cnt < lv_rows.
      IF lv_countl < lv_rows.
        PERFORM bdc_field     USING 'BDC_OKCODE'  '=NEXP'.
        PERFORM bdc_field     USING 'RM06E-ETNR1' '1'.
      ENDIF.
      IF lv_countl EQ lv_rows.
        PERFORM bdc_field     USING 'BDC_OKCODE'  '=BU'.
*        PERFORM bdc_field     USING 'RM06E-ETNR1' '1'.
        PERFORM bdc_dynpro    USING 'SAPLSPO1'    '0300'.
        PERFORM bdc_field     USING 'BDC_OKCODE'  '=YES'.
      ELSE.
        IF lv_cnt1 GE 14.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
          lv_cntp = lv_cntp + 1.
          PERFORM bdc_dynpro      USING 'SAPMM06E' '0222'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                         '/00'.
          PERFORM bdc_field       USING 'RM06E-EBELP'
                                         lv_cntp.
        ENDIF.
      ENDIF.
      CLEAR:lwa_sagmaintain.
    ENDLOOP.

    DATA: wa_opt1 TYPE ctu_params.
    wa_opt1-dismode = lv_mode.
    wa_opt1-updmode = lv_update.
    wa_opt1-defsize = 'X'.

*    PERFORM bdc_transaction TABLES lta_message
*                            USING  'ME38'
*                                  'X'
*                                  lv_mode
*                                  lv_update.

    CALL TRANSACTION 'ME38' USING bdcdata
                               OPTIONS FROM wa_opt1
                               MESSAGES INTO lta_message.
    IF sy-subrc EQ 0.
      CLEAR : lv_output, lwa_message.
      LOOP AT lta_message INTO lwa_message WHERE msgtyp EQ 'S'.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = sy-msgid
            lang      = '-D'
            no        = sy-msgno
            v1        = sy-msgv1
            v2        = sy-msgv2
            v3        = sy-msgv3
            v4        = sy-msgv4
          IMPORTING
            msg       = lv_output
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        IF sy-subrc EQ 0.
          lwa_returnmsg-errormessage = lv_output.
          lwa_returnmsg-errortype    = 'S'.
          APPEND lwa_returnmsg TO it_return.
          SORT it_return BY errortype .
          DELETE ADJACENT DUPLICATES FROM it_return COMPARING errormessage.
        ENDIF.
        CLEAR:lwa_message.
      ENDLOOP.
    ELSE.
      CLEAR : lv_output, lwa_message.
      LOOP AT lta_message INTO lwa_message WHERE msgtyp EQ 'E'.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = sy-msgid
            lang      = '-D'
            no        = sy-msgno
            v1        = sy-msgv1
            v2        = sy-msgv2
            v3        = sy-msgv3
            v4        = sy-msgv4
          IMPORTING
            msg       = lv_output
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc EQ 0.
          lwa_returnmsg-errormessage = lv_output.
          lwa_returnmsg-errortype    = 'E'.
          APPEND lwa_returnmsg TO it_return.
        ENDIF.
        CLEAR:lwa_message.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFUNCTION.
*INCLUDE bdcrecxy .
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> space.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.                    "BDC_FIELD
