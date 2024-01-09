FUNCTION zmm_generate_sa.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MATERIAL) TYPE  CHAR18 DEFAULT 'NATGAS'
*"     VALUE(IV_PLANT) TYPE  WERKS_D
*"     VALUE(IV_STORLOC) TYPE  LGORT_D DEFAULT 'A001'
*"     VALUE(IV_DEAL_SDATE) TYPE  ZZFROMDATE
*"     VALUE(IV_DEAL_EDATE) TYPE  ZZTODATE
*"     VALUE(IV_UOM) TYPE  CHAR3
*"     VALUE(IV_TAXCODE) TYPE  MWSKZ
*"     VALUE(IV_SHIPPINGINST) TYPE  CHAR2 DEFAULT 'FI'
*"     VALUE(IV_DAILYQUAN) TYPE  Z_CONDAYQTY
*"     VALUE(IV_AGREEMTYPE) TYPE  BSART OPTIONAL
*"     VALUE(IV_PEAKMAT) TYPE  CHAR18 OPTIONAL
*"     VALUE(IV_NO_OF_MONTHS) TYPE  CHAR13
*"     VALUE(IV_MATGRP) TYPE  MATKL
*"     VALUE(IV_PEAKMATGRP) TYPE  MATKL
*"     VALUE(IV_MONTHLY_AMOUNT) TYPE  KBETR OPTIONAL
*"     VALUE(IV_PURORG) TYPE  EKORG
*"     VALUE(IV_INDEX) TYPE  INCO1
*"     VALUE(IV_FIXPRICE) TYPE  KBETR
*"     VALUE(IV_CURCY) TYPE  WAERS
*"  EXPORTING
*"     VALUE(ES_ERROR) TYPE  ZMMS_ERROR
*"  TABLES
*"      T_SALINEITEMS STRUCTURE  ZMMS_GEN_SA
*"  EXCEPTIONS
*"      NO_RECORD_FOUND
*"----------------------------------------------------------------------
*  FM to Generate Schedule Agreement lines
*"----------------------------------------------------------------------
*Date       User    TR#        Description                           *
*08/25/2021 NAGIRIR D30K931505 As part of SIT2, fix for GFX Index
*01/26/2022 NAGIRIR D30K931988 format issue for field MONTH_YEAR
**********************************************************************
  CONSTANTS: c_e TYPE char1 VALUE 'E',
             c_gfx TYPE char3 VALUE 'GFX', " Add for CHG0216959
             c_ea  TYPE char2 VALUE 'EA'.  " Add for CHG0216959
  DATA: lv_begin     TYPE vtbbewe-dbervon,
        lv_end       TYPE vtbbewe-dberbis,
*        LV_MONTHS    TYPE VTBBEWE-ATAGE,
        lv_days      TYPE vtbbewe-atage,
        lv_no_month  TYPE int3 VALUE '0',
        lv_olddate   TYPE sy-datum ,
        lv_newdate   TYPE sy-datum,
        lv_index     TYPE sy-index,
        lv_varkey    TYPE char100,
*        LV_KNUMH     TYPE KNUMH,
*        LV_GLACCOUNT TYPE SAKNR,
*        LV_ORDER     TYPE AUFNR,
        lv_kbetr     TYPE kbetr_kond,
        lv_konwa     TYPE konwa,
        lv_kpein     TYPE kpein,
        lv_kmein     TYPE kmein,
        lv_date      TYPE d,
        lv_date_b    TYPE d,
        lv_etabix    TYPE flag,
        lv_date_e    TYPE d.
  TYPES: BEGIN OF ty_konp,
          kbetr     TYPE kbetr_kond,
          konwa     TYPE konwa,
          kpein     TYPE kpein,
          kmein     TYPE kmein,
         END OF ty_konp.
  DATA: lt_konp TYPE STANDARD TABLE OF ty_konp,
        lw_konp TYPE ty_konp.
  DATA : lt_gen   TYPE STANDARD TABLE OF zmms_gen_sa,
         ls_gen   TYPE zmms_gen_sa,
         lv_p_m_y TYPE char7.

*  CALCULATE NO OF MONTHS
  IF ( iv_deal_sdate IS INITIAL OR iv_deal_edate IS INITIAL ) OR ( iv_deal_sdate = '000000' OR iv_deal_edate = '0000000' ).
    es_error-type    = c_e.
    es_error-message = text-010.
    RETURN.
  ELSE.
    IF iv_index <> c_gfx. " Add for Change D30K931505
      lv_begin = iv_deal_sdate.
      lv_end   = iv_deal_edate.
      CLEAR: lv_begin , lv_end.
* Get Varkey (concatenate Purchase Organization and Incoterm) to fetch price
      CONCATENATE iv_purorg iv_index INTO lv_varkey.
*JOIN Logic for KONH & KONP
      CLEAR: lv_kbetr, lv_konwa, lv_kpein, lv_kmein, lw_konp.
      SELECT        kbetr
                    konwa
                    kpein
                    kmein
        INTO TABLE lt_konp "(LV_KBETR,LV_KONWA,LV_KPEIN,LV_KMEIN)
        FROM konp INNER JOIN konh
        ON konh~knumh = konp~knumh
        WHERE konh~kschl = 'MP01' AND konh~vakey = lv_varkey
        AND  ( konh~datab LE sy-datum AND konh~datbi GE sy-datum )
        AND  konp~loevm_ko = ''
        ORDER BY konh~datab DESCENDING.
      IF sy-subrc <> 0.
        es_error-type    = c_e.
        es_error-message = text-009.
        RETURN.
      ELSE.
        READ TABLE lt_konp INTO lw_konp INDEX 1.
        IF sy-subrc = 0.
          lv_kbetr = lw_konp-kbetr.
          lv_konwa = lw_konp-konwa.
          lv_kpein = lw_konp-kpein.
          lv_kmein = lw_konp-kmein.
        ENDIF.
      ENDIF.
*Start of Change D30K931505
    ELSE.
      lv_kbetr = iv_fixprice. " Amount
      lv_konwa = iv_curcy.    " Currency
      lv_kpein = '1000'.      " Pricing Unit
      lv_kmein = iv_uom.      " Condition Unit/Unit Key GJ/MMB/EA..
    ENDIF.
*End of Change D30K931505
*    SELECT SINGLE GL_ACCOUNT
*                  INT_ORDER FROM ZMMT_SA INTO (LV_GLACCOUNT , LV_ORDER).
    lv_index = 1.
    DO ."LV_MONTHS TIMES.
*  calculate no of days for each month based on start date of month and end date of month
      lv_olddate = iv_deal_sdate.
      CALL FUNCTION 'MONTH_PLUS_DETERMINE'
        EXPORTING
          months  = lv_no_month
          olddate = lv_olddate
        IMPORTING
          newdate = lv_newdate.
      lv_date = lv_newdate.
      CLEAR lv_newdate.
      CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
        EXPORTING
          iv_date             = lv_date
        IMPORTING
          ev_month_begin_date = lv_date_b
          ev_month_end_date   = lv_date_e.
      IF lv_index = 1.
        CLEAR lv_date_b.
        lv_date_b = lv_olddate.
        CLEAR lv_index.
      ENDIF.
      IF ( iv_deal_edate GE lv_date_b AND iv_deal_edate LE lv_date_e ).
        CLEAR lv_date_e.
        lv_date_e = iv_deal_edate.
        lv_etabix = 'X'.
      ENDIF.
* Calculate no of days
      CLEAR : lv_begin , lv_end.
      lv_begin = lv_date_b.
      lv_end   = lv_date_e.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from    = lv_begin
*         I_KEY_DAY_FROM =
          i_date_to      = lv_end
*         I_KEY_DAY_TO   =
*         I_FLG_SEPARATE = ' '
        IMPORTING
          e_days         = lv_days.
*{Begin of Change D30K931988
      DATA: lv_dumdate TYPE char10.
      lv_dumdate = |{ lv_begin DATE = USER }|.
      FIND '.' IN lv_dumdate.
      IF sy-subrc = 0.
        CONCATENATE lv_begin+4(2) lv_begin+0(4) INTO lv_begin SEPARATED BY '.'.
      ELSE.
        FIND '/' IN lv_dumdate.
        IF sy-subrc = 0.
          CONCATENATE lv_begin+4(2) lv_begin+0(4) INTO lv_begin SEPARATED BY '/'.
        ELSE.
          FIND '-' IN lv_dumdate.
          IF sy-subrc = 0.
            CONCATENATE lv_begin+4(2) lv_begin+0(4) INTO lv_begin SEPARATED BY '-'.
          ENDIF.
        ENDIF.
      ENDIF.
*End of Change D30K931988}
      ls_gen-material_no = iv_material.
      ls_gen-matgrp      = iv_matgrp.
      ls_gen-plant       = iv_plant.
      ls_gen-storage_loc = iv_storloc.
      ls_gen-month_year  = lv_begin.
      IF iv_dailyquan GT 0.
        ls_gen-target_quan = ( lv_days + 1 ) * iv_dailyquan.
      ELSE.
        ls_gen-target_quan = 1.
      ENDIF.
      ls_gen-uom         = iv_uom.
      ls_gen-kbetr       = lv_kbetr.
      ls_gen-konwa       = lv_konwa.
      ls_gen-kpein       = lv_kpein.
      ls_gen-kmein       = lv_kmein.
*      IF IV_INDEX = C_GFX." Condition Add for CHG0216959 " COmmented for D30K931505
*        LS_GEN-KBETR = IV_FIXPRICE. " Commented for D30K931505
*        LS_GEN-PER_UNIT    = '1000'.
*      ELSE.
*        LS_GEN-PRICE = LV_KBETR.
*      ENDIF.
*      LS_GEN-PER_UNIT    = '1000'.
*      LS_GEN-TAX_CODE     = IV_TAXCODE.
*      LS_GEN-SHIPPING_INS = IV_SHIPPINGINST.
      IF iv_agreemtype = 'PEAK' AND lv_no_month = 0.
        lv_p_m_y = ls_gen-month_year.
      ENDIF.
*   ls_gen-DELIVERY_DATE
      APPEND ls_gen TO lt_gen.
      IF lv_etabix IS NOT INITIAL AND iv_agreemtype = 'PEAK' AND ( iv_monthly_amount IS NOT INITIAL AND iv_no_of_months IS NOT INITIAL ) .
        CLEAR ls_gen.
        ls_gen-material_no    = iv_peakmat.
        ls_gen-matgrp         = iv_peakmatgrp.
        ls_gen-plant          = iv_plant.
        ls_gen-storage_loc    = iv_storloc.
        ls_gen-month_year     = lv_p_m_y."LV_BEGIN.
        ls_gen-target_quan    = iv_no_of_months.
        ls_gen-uom            = c_ea.
        ls_gen-kbetr          = iv_monthly_amount.
        ls_gen-konwa          = iv_curcy.
        ls_gen-kpein          = '1'.
        ls_gen-kmein          = c_ea.
*        LS_GEN-TAX_CODE       = IV_TAXCODE.
*        LS_GEN-SHIPPING_INS   = IV_SHIPPINGINST.
*        LS_GEN-DELIVERY_DATE  = LV_P_M_Y."LV_BEGIN.
*        LS_GEN-SAKTO          = LV_GLACCOUNT.
*        LS_GEN-AUFNR          = LV_ORDER.
        SHIFT  ls_gen-material_no LEFT DELETING LEADING '0'.
*        SHIFT  LS_GEN-SAKTO  LEFT DELETING LEADING '0'.
*        SHIFT  LS_GEN-AUFNR   LEFT DELETING LEADING '0'.
        APPEND ls_gen TO lt_gen.
        CLEAR :lv_etabix , ls_gen , lv_p_m_y.
      ENDIF.
      IF iv_deal_edate+4(2) EQ lv_begin(2) AND iv_deal_edate(4) EQ lv_begin+3(4)  .
        EXIT. " to add missed month
      ENDIF.
      CLEAR: lv_begin , lv_end , lv_days , lv_begin , ls_gen.
      lv_no_month = 1 + lv_no_month.
    ENDDO.
*    APPEND LINES OF LT_GEN TO ET_SAGEN.
    t_salineitems[] = lt_gen[].
  ENDIF.
ENDFUNCTION.
