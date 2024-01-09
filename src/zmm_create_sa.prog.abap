************************************************************************
*Module Pool : ZMM_CREATE_SA                                           *
*Programmer  : NAGIRIR(Rajendra Nagiri/Tech Mahindra)                  *
*Date        : June 15 2021                                            *
*Description : This program will accept inputs from screen, and creates*
*              Schedule line agreement.                                *
*----------------------------  CHANGE LOG  ----------------------------*
* Date      Programmer TR#         Description                         *
*&-------   ---------- ----------  ------------------------------------*
*31/01/2022 NAGIRIR    D30K932000  Inserted fld PLANT to table ZMMT_SA *
*                                  modified selction logic accordintly *
*23/02/2022 NAGIRIR    D30K932040 Tracking num YYYYMM format issue fix *
*08/03/2022 NAGIRIR    D30K932056 Condition records issue fix ZC00/ZC01*
*22/03/2022 NAGIRIR    D30K932103 Non GFX fialing becuse of GFX Fix    *
*28/03/2022 NAGIRIR    D30K932114 Surcharge/discount NOT Working       *
*&---------------------------------------------------------------------*

PROGRAM  zmm_create_sa." MESSAGE-ID zm.

CONSTANTS: c_tbyr  TYPE ektel VALUE 'TRADINGBUYER',
           c_peak  TYPE char4 VALUE 'PEAK',
           c_index TYPE inco1 VALUE 'GFX',
           c_usd   TYPE waers VALUE 'USD',
           c_cad   TYPE waers VALUE 'CAD',
           c_mmb   TYPE meins VALUE 'MMB',
           c_gj    TYPE meins VALUE 'GJ1',
           c_gp2   TYPE ekgrp VALUE 'GP2',
           c_21    TYPE sy-cucol VALUE 21,
           c_x     TYPE char1 VALUE 'X',
           c_e     TYPE char1 VALUE 'E',
           c_i     TYPE char1 VALUE 'I',
           c_s     TYPE char1 VALUE 'S',
           c_w     TYPE char1 VALUE 'W'.
*TYPES Declarations
TYPES: BEGIN OF ty_uom,
  zmseh3  TYPE mseh3, " External UOM in Commercial Format (3-Char.)
  zmseh6  TYPE mseh6, " External UOM Technical Format (6-Char.)
  zmsehl  TYPE msehl, " Unit of Measurement Text Internal
END OF ty_uom.
TYPES: BEGIN OF ty_t007a,
         kalsm TYPE char6,
         mwskz TYPE char2,
         END OF ty_t007a.
TYPES: BEGIN OF ty_t007s,
       kalsm TYPE char6,
       mwskz TYPE char2,
       text1 TYPE char50,
       END OF ty_t007s.
*Structure Declarations
DATA: BEGIN OF w_ekgrp1 ##NEEDED,
        ekgrp TYPE ekgrp,
        eknam TYPE eknam,
        ektel TYPE ektel,
         END OF w_ekgrp1.
DATA: t_t024   LIKE TABLE OF w_ekgrp1 ##NEEDED.
DATA: r_ekgrp  TYPE RANGE OF ekgrp    ##NEEDED,
      w_ekgrp  LIKE LINE OF r_ekgrp   ##NEEDED.

DATA: t_kalsm TYPE STANDARD TABLE OF ty_t007a  ##NEEDED,
      t_text  TYPE STANDARD TABLE OF ty_t007s  ##NEEDED,
      w_kalsm TYPE ty_t007a                    ##NEEDED,
      w_text  TYPE ty_t007s                    ##NEEDED.
*Data Declaration for F4 Functions
DATA: t_loc       TYPE zmmtt_location     ##NEEDED,
      t_agtyp     TYPE zmmtt_agrem_type   ##NEEDED,
      w_agtype    LIKE LINE OF t_agtyp    ##NEEDED,
      t_curcy     TYPE zmmtt_curr         ##NEEDED,
      w_curcy     LIKE LINE OF t_curcy    ##NEEDED,
      t_plant     TYPE zmmtt_plant        ##NEEDED,
      t_uom       TYPE STANDARD TABLE OF ty_uom ##NEEDED,
      w_uom       TYPE ty_uom             ##NEEDED,
      t_index     TYPE zmmtt_index        ##NEEDED,
      w_index     TYPE zmms_index         ##NEEDED,
      t_zterm     TYPE zmmtt_payment      ##NEEDED,
      w_zterm     TYPE zmms_payment       ##NEEDED,
      t_taxcode   TYPE zmmtt_taxcode      ##NEEDED,
      w_taxcode   TYPE zmms_taxcode       ##NEEDED,
      t_rettab    TYPE STANDARD TABLE OF ddshretval ##NEEDED,
      w_rettab    TYPE ddshretval         ##NEEDED,
      t_dynfld    TYPE STANDARD TABLE OF dynpread   ##NEEDED,
      w_dynfld    TYPE dynpread           ##NEEDED,
      v_mseh6     TYPE mseh6              ##NEEDED.

*Definitions for Schedule agreement creation Function module
DATA: w_saheader   TYPE zmm_sagheader     ##NEEDED,
      t_saitems    TYPE STANDARD TABLE OF zmm_itemsag ##NEEDED," Table for SA Create FM Item level
      w_saitems    TYPE zmm_itemsag       ##NEEDED," Structure for SA Create FM Item level
      t_sapricond  TYPE STANDARD TABLE OF zmm_pricecondsag ##NEEDED,"  Table for SA Create FM Price cond
      w_sapricond  TYPE zmm_pricecondsag  ##NEEDED,"  Structure for SA Create FM Price cond
      t_samaintain TYPE STANDARD TABLE OF zmm_sagmaintain ##NEEDED," Table for SA Create FM SA MAIN
      w_samaintain TYPE zmm_sagmaintain   ##NEEDED," Structure for SA Create FM SA MAIN
      t_return     TYPE STANDARD TABLE OF zmm_returnmsg   ##NEEDED," Table for Return msg in SAG
      w_return     TYPE zmm_returnmsg     ##NEEDED." Structure for Return msg in SAG
*GLOBAL Variables for SCREEN Fields
DATA:
*NAESB Details
      i_msa       TYPE zzmsa,
*      i_msanum    TYPE zmmt_mastagree-zzparty_agmt_id,
      d_lifnr     TYPE lifnr,
      d_lifnrname TYPE name1,
      d_bukrs     TYPE bukrs,
      d_ekorg     TYPE ekorg,
      d_ekgrp     TYPE ekgrp,
      d_mdate     TYPE zmsasgndt,
      d_mstdt     TYPE zzfromdate,
      d_mendt     TYPE zztodate,
      i_tbyer     TYPE ekgrp,
      d_tbyername TYPE eknam,
*Agreement Details
      i_agtyp     TYPE bsart,
      i_curcy     TYPE waers,
      i_agrdt     TYPE dats,
      i_dstdt     TYPE dats,
      i_dendt     TYPE dats,
*index/inco terms & quntities
      i_index     TYPE unit, "char3,
      i_dlyqty    TYPE int4, "ekpo-ktmng,
      i_uom       TYPE meins,
      d_uomname   TYPE char30,
      d_indexname TYPE char30,
      i_ppline    TYPE zparty,
      i_loc1      TYPE ztrloc,
      i_loc2      TYPE ztrloc,
      i_loc3      TYPE ztrloc,
      i_loc4      TYPE ztrloc,
*Pricing Details
      i_fixprc    TYPE kbetr ,"char11," Fix Price
      i_surchr    TYPE kbetr,"char11," Surcharge
      i_discnt    TYPE kbetr,"char11," Discount
      i_zterm     TYPE char4,
      i_taxcod    TYPE mwskz,
      d_ztermname TYPE text1_052,
      d_taxcodname TYPE text1_007s,
*item Details
      d_matnr     TYPE matnr,
      i_werks     TYPE werks_d,
      d_lgort     TYPE lgort_d,
      d_shpins    TYPE zship_ins,
      d_matgrp    TYPE matkl,
      d_shpins_detail TYPE zship_desc,
*peak details
      i_pkmat     TYPE matnr,
      i_months    TYPE char13,
      i_mntamt    TYPE kbetr,
      d_pmatgrp   TYPE matkl,
*      d_intord    TYPE zint_order," Commented for Change D30K931505
      d_kostl    TYPE kostl, " Added for Change D30K931505
      d_glacc     TYPE hkont.
***
DATA: w_sa       TYPE zmmt_sa        ##NEEDED,
      t_locmast2 TYPE zmmtt_location ##NEEDED,
      t_locmast3 TYPE zmmtt_location ##NEEDED,
      t_locmast4 TYPE zmmtt_location ##NEEDED.
*GLOBAL Variables to capture SCREEN Field values
DATA: v_genchk    TYPE c          ##NEEDED,
      v_createchk TYPE c          ##NEEDED,
      v_enablepk  TYPE c          ##NEEDED,
      v_msa       TYPE zzmsa      ##NEEDED,
      v_dstdt     TYPE zzfromdate ##NEEDED,
      v_dendt     TYPE zztodate   ##NEEDED,
      v_agrdt     TYPE zztodate   ##NEEDED,
      v_curcy     TYPE waers      ##NEEDED,
      v_index     TYPE unit       ##NEEDED,
      v_dlyqty    TYPE ekpo-ktmng ##NEEDED,
      v_uom       TYPE meins      ##NEEDED,
      v_fixprc    TYPE kbetr      ##NEEDED,
     v_surchr    TYPE kbetr      ##NEEDED,
      v_discnt    TYPE kbetr      ##NEEDED,
      v_mwskz     TYPE mwskz      ##NEEDED,
      v_werks     TYPE werks_d    ##NEEDED,
      v_bukrs     TYPE bukrs      ##NEEDED,
      v_emsg      TYPE char80     ##NEEDED,
      v_lifnr     TYPE lifnr      ##NEEDED,
      v_ekorg     TYPE ekorg      ##NEEDED,
      v_agtyp     TYPE bsart      ##NEEDED,
      v_ppline    TYPE zzparty    ##NEEDED,
      v_loc1      TYPE ztrloc     ##NEEDED,
      v_loc2      TYPE ztrloc     ##NEEDED,
      v_loc3      TYPE ztrloc     ##NEEDED,
      v_loc4      TYPE ztrloc     ##NEEDED.
DATA: BEGIN OF ls_zzmsa ##NEEDED,
          zzparty_agmt_id TYPE zmmt_mastagree-zzparty_agmt_id,
          zzmsa           TYPE zmmt_mastagree-zzmsa,
          bukrs           TYPE bukrs,
          zzdescp         TYPE zmmt_mastagree-zzdescp,
          zzmsasgndt      TYPE zmmt_mastagree-zzmsasgndt,
          zzfromdate      TYPE zmmt_mastagree-zztodate,
          zztodate        TYPE zmmt_mastagree-zztodate,
          zzstatus        TYPE zmmt_mastagree-zzstatus,
          zzmsatype       TYPE zmmt_mastagree-zzmsatype,
          zzcpid          TYPE zmmt_mastagree-zzcpid,
        END OF ls_zzmsa,

        BEGIN OF ls_lfm1  ##NEEDED,
          lifnr TYPE lfm1-lifnr,
          ekorg TYPE lfm1-ekorg,
          zterm TYPE lfm1-zterm,
          ekgrp TYPE lfm1-ekgrp,
          eikto TYPE lfm1-eikto,
        END OF ls_lfm1,

        BEGIN OF ls_lfa1  ##NEEDED,
          lifnr TYPE lfm1-lifnr,
          name1 TYPE lfa1-name1,
          name2 TYPE lfa1-name2,  "Added for change D30K932593
          regio TYPE lfa1-regio,  "Added for change D30K932593
          land1 TYPE lfa1-land1,  "Added for change D30K932593
         END OF ls_lfa1.

DATA: lt_lfa1     LIKE TABLE OF ls_lfa1   ##NEEDED,
      lt_lfm1     LIKE TABLE OF ls_lfm1   ##NEEDED,
      lt_zzmsa    LIKE TABLE OF ls_zzmsa  ##NEEDED,
      lt_zzmsa_f4 TYPE zmmtt_zzmsa        ##NEEDED,
      ls_zzmsa_f4 TYPE zmms_zzmsa         ##NEEDED,
      et_zzmsa    TYPE zmmtt_zzmsa        ##NEEDED.

*Data declaration for ALV table
DATA: t_salineitems TYPE STANDARD TABLE OF zmms_gen_sa  ##NEEDED,
      w_salineitems TYPE zmms_gen_sa  ##NEEDED,
      t_salineerror TYPE zmms_error   ##NEEDED.
DATA : go_custom_container TYPE REF TO cl_gui_custom_container  ##NEEDED,
       go_alv              TYPE REF TO cl_gui_alv_grid  ##NEEDED,
       gs_layout           TYPE lvc_s_layo  ##NEEDED,
       gt_fieldcat         TYPE lvc_t_fcat  ##NEEDED,
       gs_fcat             TYPE lvc_s_fcat  ##NEEDED.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE'.
*Begin of Comment
  CHECK d_bukrs IS NOT INITIAL. " Add for Change D30K931505
  IF w_sa IS INITIAL. " Add for Change D30K932000
*    CLEAR: w_sa.     " Commented for Change D30K932000
    SELECT SINGLE * FROM zmmt_sa                            "#EC WARNOK
      INTO w_sa
      WHERE comp_code = d_bukrs.
    IF sy-subrc = 0.
      d_matnr   = w_sa-material.
      d_shpins  = w_sa-ship_inst.
      d_shpins_detail = w_sa-ship_desc.
      d_ekgrp   = w_sa-pur_gp.
      IF i_agtyp = c_peak.
        i_pkmat   = w_sa-peak_mat.
        SELECT SINGLE matkl FROM mara INTO d_pmatgrp WHERE matnr = w_sa-peak_mat.
        d_glacc   = w_sa-gl_account.
*      d_intord  = w_sa-int_order.  " Commented for Change D30K931505
        d_kostl   = w_sa-cost_center. "Add for Change D30K931505
      ENDIF.
      SELECT SINGLE matkl FROM mara INTO d_matgrp WHERE matnr = w_sa-material.
    ENDIF.
*{Begin of change D30K932000
  ELSE.
    d_matnr   = w_sa-material.
    d_shpins  = w_sa-ship_inst.
    d_shpins_detail = w_sa-ship_desc.
    d_ekgrp   = w_sa-pur_gp.
    i_pkmat   = w_sa-peak_mat.
    SELECT SINGLE matkl FROM mara INTO d_pmatgrp WHERE matnr = w_sa-peak_mat.
    d_glacc   = w_sa-gl_account.
    d_kostl   = w_sa-cost_center.
    SELECT SINGLE matkl FROM mara INTO d_matgrp WHERE matnr = w_sa-material.
  ENDIF.
*End of Change D30K932000 }
*End of
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_MSA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_msa INPUT.

  REFRESH : lt_zzmsa_f4, lt_zzmsa,lt_lfm1,et_zzmsa,lt_lfa1,lt_zzmsa_f4,t_rettab.
  CLEAR:ls_zzmsa,ls_lfm1,ls_lfa1,ls_zzmsa_f4,w_rettab.



  SELECT zzparty_agmt_id                                "#EC CI_NOFIELD
         zzmsa
         bukrs
         zzdescp
         zzmsasgndt
         zzfromdate
         zztodate
         zzstatus
         zzmsatype
         zzcpid
    FROM zmmt_mastagree
    INTO TABLE lt_zzmsa WHERE zzmsatype = 'G'
                         AND  zzstatus = 'A'.
  IF lt_zzmsa IS NOT INITIAL.
    SELECT lifnr ekorg zterm ekgrp eikto FROM lfm1      "#EC CI_NOFIRST
      INTO TABLE lt_lfm1
      FOR ALL ENTRIES IN lt_zzmsa
      WHERE eikto = lt_zzmsa-zzcpid
       AND  ekorg = 'GASA'.
    IF lt_lfm1 IS NOT INITIAL.
      SELECT lifnr name1 name2 regio land1
        FROM lfa1
        INTO TABLE lt_lfa1
        FOR ALL ENTRIES IN lt_lfm1
        WHERE lifnr = lt_lfm1-lifnr.
    ENDIF.
  ENDIF.

  LOOP AT lt_zzmsa INTO ls_zzmsa.
    ls_zzmsa_f4-zzparty_agmt_id = ls_zzmsa-zzparty_agmt_id.
    ls_zzmsa_f4-zzmsa = ls_zzmsa-zzmsa.
    ls_zzmsa_f4-bukrs = ls_zzmsa-bukrs.
    ls_zzmsa_f4-zzdescp_t = ls_zzmsa-zzdescp+0(60).
    ls_zzmsa_f4-zzfromdate = ls_zzmsa-zzfromdate.
    ls_zzmsa_f4-zztodate = ls_zzmsa-zztodate.
    ls_zzmsa_f4-zzmsatype = ls_zzmsa-zzmsatype.
    CLEAR: ls_lfm1.
    READ TABLE lt_lfm1 INTO ls_lfm1 WITH KEY eikto = ls_zzmsa-zzcpid.
    IF sy-subrc = 0.
      CLEAR: ls_lfa1.
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_lfm1-lifnr.
      IF sy-subrc = 0.
        ls_zzmsa_f4-name1 = ls_lfa1-name1.
        ls_zzmsa_f4-name2 = ls_lfa1-name2. "Added for change D30K932593
        ls_zzmsa_f4-regio = ls_lfa1-regio. "Added for change D30K932593
        ls_zzmsa_f4-land1 = ls_lfa1-land1. "Added for change D30K932593
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_lfm1-lifnr
        IMPORTING
          output = ls_lfm1-lifnr.

      ls_zzmsa_f4-lifnr = ls_lfm1-lifnr.
    ENDIF.

    APPEND ls_zzmsa_f4 TO lt_zzmsa_f4.
    CLEAR : ls_zzmsa_f4,ls_zzmsa,ls_lfa1,ls_lfm1.
  ENDLOOP.

  APPEND LINES OF lt_zzmsa_f4 TO et_zzmsa.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'ZZMSA'
      dynpprog     = sy-cprog
      dynpnr       = sy-dynnr
      dynprofield  = 'I_MSA'
      value_org    = 'S'
      window_title = 'OpenLink MSA'(037)
    TABLES
      value_tab    = et_zzmsa
      return_tab   = t_rettab.

*Read value picked by user
  CLEAR: w_rettab, w_dynfld.
  READ TABLE t_rettab INTO w_rettab INDEX 1.
  IF sy-subrc = 0.
    READ TABLE lt_zzmsa INTO ls_zzmsa WITH KEY zzmsa = w_rettab-fieldval. "#EC WARNOK
    IF sy-subrc = 0.
      w_dynfld-fieldname  = 'I_MSA'.
      w_dynfld-fieldvalue = w_rettab-fieldval.
      APPEND w_dynfld TO t_dynfld.
      CLEAR: w_dynfld.
      w_dynfld-fieldname  = 'D_BUKRS'.
      w_dynfld-fieldvalue = ls_zzmsa-bukrs.
      APPEND w_dynfld TO t_dynfld.
      CLEAR: w_dynfld.
      w_dynfld-fieldname  = 'D_MDATE'.
      w_dynfld-fieldvalue = |{ ls_zzmsa-zzmsasgndt DATE = USER }|.
      APPEND w_dynfld TO t_dynfld.
      CLEAR: w_dynfld.
      w_dynfld-fieldname  = 'D_MSTDT'.
      w_dynfld-fieldvalue = |{ ls_zzmsa-zzfromdate DATE = USER }|.
      APPEND w_dynfld TO t_dynfld.
      CLEAR: w_dynfld.
      w_dynfld-fieldname  = 'D_MENDT'.
      w_dynfld-fieldvalue = |{ ls_zzmsa-zztodate DATE = USER }|.
      APPEND w_dynfld TO t_dynfld.
      CLEAR: w_dynfld, ls_lfm1.
      READ TABLE lt_lfm1 INTO ls_lfm1 WITH KEY eikto = ls_zzmsa-zzcpid.
      IF sy-subrc = 0.
        w_dynfld-fieldname  = 'D_LIFNR'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_lfm1-lifnr
          IMPORTING
            output = ls_lfm1-lifnr.
        w_dynfld-fieldvalue = ls_lfm1-lifnr.
        APPEND w_dynfld TO t_dynfld.
        CLEAR: w_dynfld.
        w_dynfld-fieldname  = 'D_EKORG'.
        w_dynfld-fieldvalue = ls_lfm1-ekorg.
        APPEND w_dynfld TO t_dynfld.
        CLEAR: w_dynfld.
        w_dynfld-fieldname  = 'I_ZTERM'.
        w_dynfld-fieldvalue = ls_lfm1-zterm.
        APPEND w_dynfld TO t_dynfld.
        CLEAR: w_dynfld.
        IF ls_lfm1-zterm IS NOT INITIAL.
          CLEAR: w_zterm.
          SELECT SINGLE a~zterm
          b~text1 FROM t052 AS a                            "#EC WARNOK
            INNER JOIN t052u AS b                      "#EC CI_BUFFJOIN
            ON a~zterm = b~zterm AND a~ztagg = b~ztagg
            INTO w_zterm
            WHERE a~zterm = ls_lfm1-zterm
            AND spras = sy-langu.
          IF sy-subrc = 0.
            w_dynfld-fieldname  = 'D_ZTERMNAME'.
            w_dynfld-fieldvalue = w_zterm-text1.
            APPEND w_dynfld TO t_dynfld.
          ENDIF.
          CLEAR: w_dynfld.
          w_dynfld-fieldname  = 'D_EKGRP'.
          IF ls_lfm1-ekgrp IS NOT INITIAL.
            w_dynfld-fieldvalue = ls_lfm1-ekgrp.
          ELSE.
            w_dynfld-fieldvalue = c_gp2.
          ENDIF.
          APPEND w_dynfld TO t_dynfld.
        ENDIF.
        CLEAR: ls_lfa1, w_dynfld.
        READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_lfm1-lifnr.
        IF sy-subrc = 0.
          w_dynfld-fieldname  = 'D_LIFNRNAME'.
          w_dynfld-fieldvalue = ls_lfa1-name1.
          APPEND w_dynfld TO t_dynfld.
        ENDIF.
      ENDIF.
*Begin of Change
      CLEAR: w_sa.
      SELECT SINGLE * FROM zmmt_sa                          "#EC WARNOK
        INTO w_sa
        WHERE comp_code = ls_zzmsa-bukrs.
      IF sy-subrc = 0.
        d_matnr   = w_sa-material.
        d_shpins  = w_sa-ship_inst.
        d_shpins_detail = w_sa-ship_desc.
        d_ekgrp   = w_sa-pur_gp.
        IF i_agtyp = c_peak.
          i_pkmat   = w_sa-peak_mat.
          SELECT SINGLE matkl FROM mara INTO d_pmatgrp WHERE matnr = w_sa-peak_mat.
          d_glacc   = w_sa-gl_account.
          d_kostl   = w_sa-cost_center.
          CLEAR: w_dynfld.
          w_dynfld-fieldname  = 'I_PKMAT'.
          w_dynfld-fieldvalue = i_pkmat.
          APPEND w_dynfld TO t_dynfld.
          CLEAR: w_dynfld.
          w_dynfld-fieldname  = 'D_GLACC'.
          w_dynfld-fieldvalue = d_glacc.
          APPEND w_dynfld TO t_dynfld.
          CLEAR: w_dynfld.
          w_dynfld-fieldname  = 'D_KOSTL'.
          w_dynfld-fieldvalue = d_kostl.
          APPEND w_dynfld TO t_dynfld.
          CLEAR: w_dynfld.
          w_dynfld-fieldname  = 'D_PMATGRP'.
          w_dynfld-fieldvalue = d_pmatgrp.
          APPEND w_dynfld TO t_dynfld.
        ENDIF.
        SELECT SINGLE matkl FROM mara INTO d_matgrp WHERE matnr = w_sa-material.
        CLEAR: w_dynfld.
        w_dynfld-fieldname  = 'D_MATNR'.
        w_dynfld-fieldvalue = d_matnr.
        APPEND w_dynfld TO t_dynfld.
        CLEAR: w_dynfld.
        w_dynfld-fieldname  = 'D_SHPINS'.
        w_dynfld-fieldvalue = d_shpins.
        APPEND w_dynfld TO t_dynfld.
        CLEAR: w_dynfld.
        w_dynfld-fieldname  = 'D_SHPINS_DETAIL'.
        w_dynfld-fieldvalue = d_shpins_detail.
        APPEND w_dynfld TO t_dynfld.
        CLEAR: w_dynfld.
        w_dynfld-fieldname  = 'D_MATGRP'.
        w_dynfld-fieldvalue = d_matgrp.
        APPEND w_dynfld TO t_dynfld.
      ENDIF.
*End of Change
    ENDIF.
  ENDIF.
*Update values in display fields
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.

ENDMODULE.                 " F4_MSA  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TRADING_BUYER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_trading_buyer INPUT.

  REFRESH: t_t024, r_ekgrp, t_rettab.
  CLEAR: w_ekgrp1.

  w_ekgrp-sign   = 'I'.
  w_ekgrp-option = 'BT'.
  w_ekgrp-low    = 'G01'.
  w_ekgrp-high   = 'G99'.
  APPEND w_ekgrp TO r_ekgrp.

  SELECT ekgrp
         eknam
         ektel
     FROM t024
     INTO TABLE t_t024
    WHERE ekgrp IN r_ekgrp
    AND  ektel = c_tbyr.
  IF sy-subrc = 0.
    SORT t_t024 BY ekgrp.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'EKGRP'
      dynpprog     = sy-cprog
      dynpnr       = sy-dynnr
      dynprofield  = 'I_TBYER'
      value_org    = 'S'
      window_title = 'Trader Buyer'(038)
    TABLES
      value_tab    = t_t024
      return_tab   = t_rettab.

  CLEAR: w_rettab, w_dynfld.
  READ TABLE t_rettab INTO w_rettab INDEX 1.
  w_dynfld-fieldname  = 'I_TBYER'.
  w_dynfld-fieldvalue = w_rettab-fieldval.
  APPEND w_dynfld TO t_dynfld.

  CLEAR: w_dynfld.
  SELECT SINGLE eknam FROM t024
    INTO w_dynfld-fieldvalue
    WHERE ekgrp = w_rettab-fieldval.
  w_dynfld-fieldname  = 'D_TBYERNAME'.
  APPEND w_dynfld TO t_dynfld.
*Update Trading buyer name
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.
ENDMODULE.                 " F4_TRADING_BUYER  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_LOCATION1  INPUT
*&---------------------------------------------------------------------*
*  hAVE TO CHECK TO HOW TO WRITE DYNAMIC F4IF.. INSTEAD OF CALLING FOR EVERY FIELD
*----------------------------------------------------------------------*
MODULE f4_location1 INPUT .
  REFRESH: t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_PPLINE'.
  APPEND w_dynfld TO t_dynfld.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_PPLINE'.
  IF sy-subrc = 0.
    CLEAR: i_ppline.
    i_ppline = w_dynfld-fieldvalue.

    SELECT zztrloc                                      "#EC CI_NOFIRST
            zztrlocalias
            zzconvndid
            zzparty
            zzactflg
            zzfrequsdflg
       FROM zmmt_locmast
       INTO TABLE t_loc WHERE zzparty = i_ppline AND
                                  zzactflg = 'Y'.
    IF sy-subrc = 0.
      SORT t_loc DESCENDING BY zzfrequsdflg.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ZZTRLOC'
        dynpprog    = sy-cprog
        dynpnr      = sy-dynnr
        dynprofield = 'I_LOC1'
        value_org   = 'S'
      TABLES
        value_tab   = t_loc
        return_tab  = t_rettab.
*  ELSE.
**nothing
  ENDIF.
ENDMODULE.                 " F4_LOCATION1  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_LOCATION2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_location2 INPUT.
  CLEAR: i_loc1, i_loc2, i_loc3, i_loc4, i_ppline, t_loc, w_dynfld, t_locmast2[].
  REFRESH: t_dynfld.
  w_dynfld-fieldname = 'I_PPLINE'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC1'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC2'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC3'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC4'.
  APPEND w_dynfld TO t_dynfld.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_PPLINE'.
  IF sy-subrc = 0.
    i_ppline = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC1'.
  IF sy-subrc = 0.
    i_loc1 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC2'.
  IF sy-subrc = 0.
    i_loc2 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC3'.
  IF sy-subrc = 0.
    i_loc3 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC4'.
  IF sy-subrc = 0.
    i_loc4 = w_dynfld-fieldvalue.
  ENDIF.

  IF i_ppline IS NOT INITIAL AND
     i_loc1 IS NOT INITIAL AND
     i_loc3 IS     INITIAL AND
     i_loc4 IS     INITIAL.
    SELECT zztrloc                                      "#EC CI_NOFIRST
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE t_locmast2 WHERE zztrloc NE i_loc1
                               AND zzparty = i_ppline
                               AND zzactflg = 'Y'.
  ENDIF.

  IF i_loc1 IS NOT INITIAL AND
     i_loc3 IS NOT INITIAL AND
     i_loc4 IS NOT INITIAL AND
     i_ppline IS NOT INITIAL.
    CLEAR:t_locmast2[].
    SELECT zztrloc                                      "#EC CI_NOFIRST
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE t_locmast2 WHERE zztrloc NE i_loc1
                               AND zztrloc NE i_loc3
                               AND zztrloc NE i_loc4
                               AND zzparty = i_ppline
                               AND zzactflg = 'Y'.
*  ELSE.
*    MESSAGE text-011 TYPE c_e.
  ENDIF.
  APPEND LINES OF t_locmast2 TO t_loc.
  SORT t_loc DESCENDING BY zzfrequsdflg.
*  IF t_loc IS NOT INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZZTRLOC'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_LOC2'
      value_org   = 'S'
    TABLES
      value_tab   = t_loc
      return_tab  = t_rettab.
*  ELSE.
*    MESSAGE 'Check Location1 & Pipiline Combination'(034) TYPE c_e.
*  ENDIF.
ENDMODULE.                 " F4_LOCATION2  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_LOCATION3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_location3 INPUT.

  CLEAR: i_loc1, i_loc2, i_loc3, i_loc4, i_ppline, t_loc, w_dynfld, t_locmast3.
  REFRESH: t_dynfld.
  w_dynfld-fieldname = 'I_PPLINE'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC1'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC2'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC3'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC4'.
  APPEND w_dynfld TO t_dynfld.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_PPLINE'.
  IF sy-subrc = 0.
    i_ppline = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC1'.
  IF sy-subrc = 0.
    i_loc1 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC2'.
  IF sy-subrc = 0.
    i_loc2 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC3'.
  IF sy-subrc = 0.
    i_loc3 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC4'.
  IF sy-subrc = 0.
    i_loc4 = w_dynfld-fieldvalue.
  ENDIF.

  IF i_loc1 IS NOT INITIAL AND
     i_loc2 IS NOT INITIAL AND
     i_loc4 IS INITIAL AND
     i_ppline IS NOT INITIAL.
    SELECT zztrloc                                      "#EC CI_NOFIRST
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE t_locmast3 WHERE zztrloc NE i_loc1
                               AND zztrloc NE i_loc2
                               AND zzparty = i_ppline
                               AND zzactflg = 'Y'.
  ENDIF.
  IF i_loc1 IS NOT INITIAL AND
     i_loc2 IS NOT INITIAL AND
     i_loc4 IS NOT INITIAL AND
     i_ppline IS NOT INITIAL.

    CLEAR:t_locmast3[].
    SELECT zztrloc                                      "#EC CI_NOFIRST
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE t_locmast3 WHERE zztrloc NE i_loc1
                               AND zztrloc NE i_loc2
                               AND zztrloc NE i_loc4
                               AND zzparty = i_ppline
                               AND zzactflg = 'Y'.
  ENDIF.
  APPEND LINES OF t_locmast3 TO t_loc.
  SORT t_loc DESCENDING BY zzfrequsdflg.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZZTRLOC'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_LOC3'
      value_org   = 'S'
    TABLES
      value_tab   = t_loc
      return_tab  = t_rettab.
ENDMODULE.                 " F4_LOCATION3  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_LOCATION4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_location4 INPUT.

  CLEAR: i_loc1, i_loc2, i_loc3, i_loc4, i_ppline, t_loc, w_dynfld, t_locmast4.
  REFRESH: t_dynfld.
  w_dynfld-fieldname = 'I_PPLINE'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC1'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC2'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC3'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'I_LOC4'.
  APPEND w_dynfld TO t_dynfld.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.

  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_PPLINE'.
  IF sy-subrc = 0.
    i_ppline = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC1'.
  IF sy-subrc = 0.
    i_loc1 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC2'.
  IF sy-subrc = 0.
    i_loc2 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC3'.
  IF sy-subrc = 0.
    i_loc3 = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_LOC4'.
  IF sy-subrc = 0.
    i_loc4 = w_dynfld-fieldvalue.
  ENDIF.
  IF i_loc1 IS NOT INITIAL AND
     i_loc2 IS NOT INITIAL AND
     i_loc3 IS NOT INITIAL AND
     i_ppline IS NOT INITIAL.

    SELECT zztrloc                                      "#EC CI_NOFIRST
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE t_locmast4 WHERE zztrloc NE i_loc1
                               AND zztrloc NE i_loc2
                               AND zztrloc NE i_loc3
                               AND zzparty = i_ppline
                               AND zzactflg = 'Y'.
  ENDIF.
  APPEND LINES OF t_locmast4 TO t_loc.
  SORT t_loc DESCENDING BY zzfrequsdflg.
*  IF t_loc IS NOT INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZZTRLOC'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_LOC4'
      value_org   = 'S'
    TABLES
      value_tab   = t_loc
      return_tab  = t_rettab.
*  ELSE.
**    MESSAGE text-036 TYPE c_e.
*  ENDIF.
ENDMODULE.                 " F4_LOCATION4  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_PLANT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_plant INPUT.

  REFRESH: t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'D_BUKRS'.
  APPEND w_dynfld TO t_dynfld.
  CLEAR: w_dynfld.
  w_dynfld-fieldname = 'D_EKORG'.
  APPEND w_dynfld TO t_dynfld.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynfld.

  CLEAR: w_dynfld, v_bukrs.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'D_BUKRS'.
  IF sy-subrc = 0.
    v_bukrs = w_dynfld-fieldvalue.
  ENDIF.
  CLEAR: w_dynfld, v_ekorg.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'D_EKORG'.
  IF sy-subrc = 0.
    v_ekorg = w_dynfld-fieldvalue.
  ENDIF.

  SELECT t024w~werks
                 name1
                 name2
                 INTO TABLE t_plant
                 FROM  t024w INNER JOIN t001w
                 ON      t024w~werks = t001w~werks
                 WHERE   t024w~ekorg = v_ekorg AND
                         t024w~werks IN ( SELECT bwkey FROM t001k WHERE bukrs = v_bukrs ). "#EC CI_BUFFSUBQ
  CLEAR: t_rettab.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WERKS'" Plant
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_WERKS'
      value_org   = 'S'
    TABLES
      value_tab   = t_plant
      return_tab  = t_rettab.
  CLEAR: w_rettab.
  READ TABLE t_rettab INTO w_rettab INDEX 1.
  IF sy-subrc = 0.
    w_dynfld-fieldname = 'I_WERKS'.
    w_dynfld-fieldvalue = w_rettab-fieldval.
    APPEND w_dynfld TO t_dynfld.
    CLEAR: v_werks.
    v_werks = w_rettab-fieldval.
    SELECT SINGLE lgort FROM t001l                          "#EC WARNOK
      INTO w_dynfld-fieldvalue
      WHERE werks = v_werks.
    w_dynfld-fieldname = 'D_LGORT'.
    APPEND w_dynfld TO t_dynfld.
*Update values in display fields
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = t_dynfld.
  ENDIF.

ENDMODULE.                 " F4_PLANT  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_PIPE_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_pipe_line INPUT.

  SELECT zztrloc                                        "#EC CI_NOFIELD
         zztrlocalias
         zzconvndid
         zzparty
         zzactflg
         zzfrequsdflg
     FROM zmmt_locmast
     INTO TABLE t_loc WHERE zzactflg = 'Y'.
  IF sy-subrc = 0.
    SORT t_loc DESCENDING BY zzfrequsdflg.
  ENDIF.
  CLEAR:t_rettab.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZZPARTY' " Pipeline
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_PPLINE'
      value_org   = 'S'
    TABLES
      value_tab   = t_loc
      return_tab  = t_rettab.

ENDMODULE.                 " F4_PIPE_LINE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_AGREEMENT_TYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_agreement_type INPUT.

  SELECT a~bsart
         b~batxt
    FROM  t161 AS a INNER JOIN t161t AS b              "#EC CI_BUFFJOIN
    ON a~bsart = b~bsart
    INTO TABLE t_agtyp
    WHERE a~bstyp = 'L'
    AND b~spras = sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'BSART' "Agrement Type
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_AGTYP'
      value_org   = 'S'
    TABLES
      value_tab   = t_agtyp
      return_tab  = t_rettab.

  CLEAR: w_dynfld, v_agtyp.
  READ TABLE t_dynfld INTO w_dynfld WITH KEY fieldname = 'I_AGTYP'.
  IF sy-subrc = 0.
    v_agtyp = w_dynfld-fieldvalue.
    IF v_agtyp = c_peak.
      LOOP AT SCREEN.
        IF screen-name = 'I_MONTHS' OR screen-name = 'I_MNTAMT'.
          screen-input = '0'. " It makes screen field in EDIT mode
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMODULE.                 " F4_AGREEMENT_TYPE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_PAYMENT_TERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_payment_term INPUT.

  SELECT a~zterm
          b~text1 FROM t052 AS a
    INNER JOIN t052u AS b                              "#EC CI_BUFFJOIN
    ON a~zterm = b~zterm AND a~ztagg = b~ztagg
    INTO TABLE t_zterm
    WHERE spras = sy-langu .
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZTERM' "Payment Term
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_ZTERM'
      value_org   = 'S'
    TABLES
      value_tab   = t_zterm
      return_tab  = t_rettab.

  CLEAR: w_rettab, w_dynfld.
  READ TABLE t_rettab INTO w_rettab INDEX 1.
  IF sy-subrc = 0.
    w_dynfld-fieldname = 'I_ZTERM'.
    w_dynfld-fieldvalue = w_rettab-fieldval.
    APPEND w_dynfld TO t_dynfld.
    CLEAR: w_dynfld.
    READ TABLE t_zterm INTO w_zterm WITH KEY zterm = w_rettab-fieldval. "#EC WARNOK
    IF sy-subrc = 0.
      w_dynfld-fieldvalue = w_zterm-text1.
      w_dynfld-fieldname = 'D_ZTERMNAME'.
      APPEND w_dynfld TO t_dynfld.
*Update values in display fields
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-cprog
          dynumb     = sy-dynnr
        TABLES
          dynpfields = t_dynfld.
    ENDIF.

  ENDIF.
ENDMODULE.                 " F4_PAYMENT_TERM  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TAXCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_taxcode INPUT.

  CLEAR: t_kalsm, t_text, w_kalsm, w_text.
  SELECT kalsm
         mwskz FROM t007a INTO TABLE t_kalsm.
  IF sy-subrc = 0.
    SELECT kalsm
           mwskz
           text1
           FROM t007s INTO CORRESPONDING FIELDS OF TABLE  t_text
      FOR ALL ENTRIES IN t_kalsm
      WHERE mwskz = t_kalsm-mwskz AND
            spras = sy-langu.
    SORT t_kalsm BY mwskz.
    SORT t_text BY mwskz.
    LOOP AT t_kalsm INTO w_kalsm.
      CLEAR w_text.
      READ TABLE t_text INTO w_text WITH KEY kalsm = w_kalsm-kalsm
                                             mwskz = w_kalsm-mwskz.
      w_taxcode-mwskz    =  w_kalsm-mwskz.
      w_taxcode-text1    =  w_text-text1.
      APPEND w_taxcode TO t_taxcode.
      CLEAR: w_kalsm , w_taxcode .
    ENDLOOP.
  ENDIF.
  SORT t_taxcode BY mwskz.
  DELETE ADJACENT DUPLICATES FROM t_taxcode COMPARING mwskz.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MWSKZ' "Tax Code
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_TAXCOD'
      value_org   = 'S'
    TABLES
      value_tab   = t_taxcode
      return_tab  = t_rettab.

  CLEAR: w_rettab, w_dynfld.
  READ TABLE t_rettab INTO w_rettab INDEX 1.
  IF sy-subrc = 0.
    w_dynfld-fieldname = 'I_TAXCOD'.
    w_dynfld-fieldvalue = w_rettab-fieldval.
    APPEND w_dynfld TO t_dynfld.
    READ TABLE t_taxcode INTO w_taxcode WITH KEY mwskz = w_rettab-fieldval. "#EC WARNOK
    IF sy-subrc = 0.                                        "#EC WARNOK
      w_dynfld-fieldvalue = w_taxcode-text1.
      w_dynfld-fieldname  = 'D_TAXCODNAME'.
      APPEND w_dynfld TO t_dynfld.
*Update values in display fields
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-cprog
          dynumb     = sy-dynnr
        TABLES
          dynpfields = t_dynfld.
    ENDIF.
  ENDIF.
ENDMODULE.                 " F4_TAXCODE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_UOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_uom INPUT.

  SELECT mseh3 mseh6 msehl
    FROM t006a
    INTO TABLE t_uom
    WHERE spras = sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZMSEH3' "UOM
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_UOM'
      value_org   = 'S'
    TABLES
      value_tab   = t_uom
      return_tab  = t_rettab.

  REFRESH t_dynfld.
  CLEAR: w_rettab.
  READ TABLE t_rettab INTO w_rettab INDEX 1.
  IF sy-subrc  = 0.
    w_dynfld-fieldname = 'I_UOM'.
    w_dynfld-fieldvalue = w_rettab-fieldval.
    APPEND w_dynfld TO t_dynfld.
    CLEAR: w_uom, w_dynfld.
    READ TABLE t_uom INTO w_uom WITH KEY zmseh3 = w_rettab-fieldval. "#EC WARNOK
    IF sy-subrc = 0.
      w_dynfld-fieldname  = 'D_UOMNAME'.
      w_dynfld-fieldvalue = w_uom-zmsehl.
      APPEND w_dynfld TO t_dynfld.
      v_mseh6 = w_uom-zmseh6.
    ENDIF.

*Update values in display fields
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = t_dynfld.
  ENDIF.

ENDMODULE.                 " F4_UOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_STDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_stdate INPUT.

  CALL FUNCTION 'F4_DATE' ##FM_SUBRC_OK
    EXPORTING
      date_for_first_month         = i_dstdt
      display                      = ' '
    IMPORTING
      select_date                  = i_dstdt
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

ENDMODULE.                 " F4_STDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_EDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_edate INPUT.

  CALL FUNCTION 'F4_DATE' ##FM_SUBRC_OK
    EXPORTING
      date_for_first_month         = i_dendt
      display                      = ' '
    IMPORTING
      select_date                  = i_dendt
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.

ENDMODULE.                 " F4_EDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_CURRENCY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_currency INPUT.
  SELECT a~waers
         b~ltext
    FROM tcurc AS a INNER JOIN tcurt AS b              "#EC CI_BUFFJOIN
    ON a~waers = b~waers
    INTO TABLE t_curcy
    WHERE spras = sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZWAERS' "CURRENCY
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'I_CURCY'
      value_org   = 'S'
    TABLES
      value_tab   = t_curcy
      return_tab  = t_rettab.

ENDMODULE.                 " F4_CURRENCY  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_INDEX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_index INPUT.
  SELECT a~inco1
         b~bezei
    FROM tinc AS a INNER JOIN tinct AS b               "#EC CI_BUFFJOIN
    ON a~inco1 = b~inco1
    INTO TABLE t_index
    WHERE spras = sy-langu.
  IF sy-subrc = 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ZINCO1' "INDEX/INCOTERMS
        dynpprog    = sy-cprog
        dynpnr      = sy-dynnr
        dynprofield = 'I_INDEX'
        value_org   = 'S'
      TABLES
        value_tab   = t_index
        return_tab  = t_rettab.
    REFRESH t_dynfld.
    CLEAR: w_rettab, w_dynfld.
    READ TABLE t_rettab INTO w_rettab INDEX 1.
    IF sy-subrc  = 0.
      w_dynfld-fieldname = 'I_INDEX'.
      w_dynfld-fieldvalue = w_rettab-fieldval.
      APPEND w_dynfld TO t_dynfld.
      CLEAR: w_index, w_dynfld.
      READ TABLE t_index INTO w_index WITH KEY zinco1 = w_rettab-fieldval. "#EC WARNOK
      IF sy-subrc = 0.
        w_dynfld-fieldname  = 'D_INDEXNAME'.
        w_dynfld-fieldvalue = w_index-zbezei.
        APPEND w_dynfld TO t_dynfld.
      ENDIF.

*Update values in display fields
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-cprog
          dynumb     = sy-dynnr
        TABLES
          dynpfields = t_dynfld.
    ENDIF.
  ENDIF.

ENDMODULE.                 " F4_INDEX  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IMSA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_imsa INPUT.

  CLEAR:ls_zzmsa, ls_lfm1, ls_lfa1,  ls_zzmsa_f4, w_rettab, v_lifnr.
  SELECT SINGLE zzparty_agmt_id       "#EC WARNOK       "#EC CI_NOFIELD
         zzmsa
         bukrs
         zzdescp
         zzmsasgndt
         zzfromdate
         zztodate
         zzstatus
         zzmsatype
         zzcpid
    FROM zmmt_mastagree
    INTO ls_zzmsa WHERE zzmsa = i_msa
                  AND zzmsatype = 'G'
                  AND zzstatus = 'A'.
  IF sy-subrc = 0.                                          "#EC WARNOK
*    i_msanum = ls_zzmsa-zzparty_agmt_id.
    d_bukrs = ls_zzmsa-bukrs.
    d_mstdt = ls_zzmsa-zzfromdate.
    d_mendt = ls_zzmsa-zztodate.
    d_mdate = ls_zzmsa-zzmsasgndt.
    SELECT SINGLE lifnr ekorg zterm ekgrp eikto FROM lfm1 "#EC WARNOK    "#EC CI_NOFIRST
      INTO ls_lfm1
      WHERE eikto = ls_zzmsa-zzcpid
       AND  ekorg = 'GASA'.
    IF sy-subrc = 0.
      CLEAR: v_bukrs, v_lifnr.                              "#EC WARNOK
      SELECT SINGLE lifnr bukrs ##NEEDED
       FROM lfb1
       INTO (v_lifnr , v_bukrs )
       WHERE lifnr = ls_lfm1-lifnr
       AND   bukrs = ls_zzmsa-bukrs.
      IF sy-subrc <> 0.
        CLEAR: v_emsg.
        CONCATENATE 'Vendor Does Not Exist For Company Code'(030)
                    ls_zzmsa-bukrs INTO v_emsg SEPARATED BY space.
        MESSAGE v_emsg  TYPE c_e.
      ENDIF.
      d_ekorg = ls_lfm1-ekorg.
      i_zterm = ls_lfm1-zterm.
      d_lifnr = v_lifnr.
      SELECT SINGLE lifnr name1 name2 regio land1
        FROM lfa1
        INTO ls_lfa1
        WHERE lifnr = ls_lfm1-lifnr.
      IF sy-subrc = 0.
        d_lifnrname = ls_lfa1-name1.
      ENDIF.
      IF i_zterm IS NOT INITIAL.
        CLEAR: w_zterm.
        SELECT SINGLE a~zterm
        b~text1 FROM t052 AS a                              "#EC WARNOK
          INNER JOIN t052u AS b                        "#EC CI_BUFFJOIN
          ON a~zterm = b~zterm AND a~ztagg = b~ztagg
          INTO w_zterm
          WHERE a~zterm = i_zterm
          AND spras = sy-langu.
        IF sy-subrc = 0.
          d_ztermname = w_zterm-text1.
        ENDIF.
      ENDIF.
    ENDIF.
*Begin of Change
    CLEAR: w_sa.
    SELECT SINGLE * FROM zmmt_sa                            "#EC WARNOK
      INTO w_sa
      WHERE comp_code = ls_zzmsa-bukrs.
    IF sy-subrc = 0.
      d_matnr   = w_sa-material.
      d_shpins  = w_sa-ship_inst.
      d_shpins_detail = w_sa-ship_desc.
      d_ekgrp   = w_sa-pur_gp.
      IF i_agtyp = c_peak.
        i_pkmat   = w_sa-peak_mat.
        SELECT SINGLE matkl FROM mara INTO d_pmatgrp WHERE matnr = w_sa-peak_mat.
        d_glacc   = w_sa-gl_account.
        d_kostl   = w_sa-cost_center.
      ENDIF.
      SELECT SINGLE matkl FROM mara INTO d_matgrp WHERE matnr = w_sa-material.
    ENDIF.
*End of Change
  ELSE.
    MESSAGE 'Enter Valid MSA'(002) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_IMSA  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  IF v_createchk IS NOT INITIAL.
    REFRESH: t_salineitems.
    CALL METHOD go_alv->refresh_table_display.
    CALL METHOD go_custom_container->free.
    CLEAR: gs_layout, go_custom_container, go_alv, v_createchk.
  ELSE.
    IF t_salineitems IS NOT INITIAL.
      IF i_agtyp <> c_peak.
        DELETE t_salineitems WHERE material_no = i_pkmat.
      ENDIF.
      REFRESH:gt_fieldcat.
      " Create a custom container control for our ALV Control
      CREATE OBJECT go_custom_container
        EXPORTING
          container_name              = 'CONT'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.
      IF sy-subrc NE 0.
        MESSAGE 'The Custom Control Could Not Be Created'(003) TYPE c_e.
        RETURN.
      ENDIF.

      " Create ALV
      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_custom_container.
      " Title on the ALV grid
      gs_layout-grid_title = 'Schedule Items'(004).
      gs_layout-no_toolbar = c_x.

      gs_fcat-fieldname = 'PLANT'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-ref_field = 'PLANT'.
      gs_fcat-outputlen = '8'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'STORAGE_LOC'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-ref_field = 'STORAGE_LOC'.
      gs_fcat-outputlen = '8'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'UOM'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-ref_field = 'UOM'.
      gs_fcat-outputlen = '4'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KBETR'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-scrtext_s = 'Amount'(046).
      gs_fcat-scrtext_m = 'Amount'(046).
      gs_fcat-scrtext_l = 'Amount'(046).
      gs_fcat-seltext = 'Amount'(046).
      gs_fcat-reptext = gs_fcat-scrtext_s.
      gs_fcat-outputlen = '15'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KONWA'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-scrtext_s = 'Crcy'(047).
      gs_fcat-scrtext_m = 'Crcy'(047).
      gs_fcat-scrtext_l = 'Crcy'(047).
      gs_fcat-seltext = 'Currency'(048).
      gs_fcat-reptext = gs_fcat-scrtext_s.
      gs_fcat-outputlen = '5'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KPEIN'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-scrtext_s = 'Per'(049).
      gs_fcat-scrtext_m = 'Per'(049).
      gs_fcat-scrtext_l = 'Per'(049).
      gs_fcat-seltext = 'Per Unit'(050).
      gs_fcat-reptext = gs_fcat-scrtext_s.
      gs_fcat-outputlen = '5'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.

      gs_fcat-fieldname = 'KMEIN'.
      gs_fcat-ref_table = 'ZMMS_GEN_SA'.
      gs_fcat-ref_field = 'UOM'.
      gs_fcat-outputlen = '4'.
      APPEND gs_fcat TO gt_fieldcat.
      CLEAR gs_fcat.
      " Configuration for first display.
      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZMMS_GEN_SA'
          is_layout        = gs_layout
        CHANGING
          it_outtab        = t_salineitems
          it_fieldcatalog  = gt_fieldcat.

    ENDIF.  " /* eo go_custom_container IS NOT BOUND. */
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ITBUYER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_itbyer INPUT.

  REFRESH:t_t024, r_ekgrp, t_rettab.
  CLEAR: w_ekgrp1, w_ekgrp.
  w_ekgrp-sign = 'I'.
  w_ekgrp-option = 'BT'.
  w_ekgrp-low = 'G01'.
  w_ekgrp-high = 'G99'.
  APPEND w_ekgrp TO r_ekgrp.

  SELECT ekgrp
         eknam
         ektel
     FROM t024
     INTO TABLE t_t024
    WHERE ekgrp IN r_ekgrp
    AND   ektel = c_tbyr.
  IF sy-subrc = 0.
    SORT t_t024 BY ekgrp.
    CLEAR: w_ekgrp1.
    READ TABLE t_t024 INTO w_ekgrp1 WITH KEY ekgrp = i_tbyer.
    IF sy-subrc <> 0.
      MESSAGE 'Enter Valid Telephone TRADINGBUYER Between G01 & G99'(005) TYPE c_e.
    ELSE.
      i_tbyer     = w_ekgrp1-ekgrp.
      d_tbyername = w_ekgrp1-eknam.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ITBYER  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHEECK_IAGTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iagtyp INPUT.

  CLEAR: w_agtype.
  SELECT SINGLE a~bsart                                     "#EC WARNOK
         b~batxt
    FROM  t161 AS a INNER JOIN t161t AS b              "#EC CI_BUFFJOIN
    ON a~bsart = b~bsart
    INTO w_agtype
    WHERE a~bsart = i_agtyp
    AND   a~bstyp = 'L'
    AND   b~spras = sy-langu.
  IF sy-subrc <> 0.                                         "#EC WARNOK
    MESSAGE 'Enter Valid Agreement Type'(006) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHEECK_IAGTYP  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ICURCY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_icurcy INPUT.
  CLEAR: w_curcy.
  SELECT SINGLE a~waers                                     "#EC WARNOK
         b~ltext
    FROM tcurc AS a INNER JOIN tcurt AS b              "#EC CI_BUFFJOIN
    ON a~waers = b~waers
    INTO w_curcy
    WHERE a~waers = i_curcy
    AND spras = sy-langu.
  IF sy-subrc <> 0.                                         "#EC WARNOK
    MESSAGE 'Enter Valid Currecy Key'(008) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_ICURCY  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IUOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iuom INPUT.
  CLEAR: w_uom.
  SELECT SINGLE mseh3 mseh6 msehl
    FROM t006a
    INTO w_uom
    WHERE msehi = i_uom
    AND spras = sy-langu.
  IF sy-subrc <> 0.
    MESSAGE 'Enter Valid UOM'(009) TYPE c_e.
  ELSE.
    d_uomname = w_uom-zmsehl.
    v_mseh6   = w_uom-zmseh6.
  ENDIF.
  IF i_curcy IS NOT INITIAL AND i_uom IS NOT INITIAL.
    IF i_curcy = c_usd AND i_uom <> c_mmb.
      MESSAGE 'Check Currency and UOM Entry'(032) TYPE c_e.
    ENDIF.
    IF i_curcy = c_cad AND i_uom <> c_gj.
      MESSAGE 'Check Currency and UOM Entry'(032) TYPE c_e.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_IUOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IPPLINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ippline INPUT.
  CLEAR: v_ppline.
  CONDENSE: i_ppline.
  SELECT SINGLE zzparty     "#EC WARNOK                 "#EC CI_NOFIRST
     FROM zmmt_locmast
     INTO v_ppline WHERE zzparty = i_ppline
     AND zzactflg = 'Y'.
  IF sy-subrc <> 0.                                         "#EC WARNOK
    MESSAGE 'Enter Valid Pipe Line'(010) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_IPPLINE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ILOC1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iloc1 INPUT.
  CLEAR: v_loc1, v_ppline.
  IF i_ppline IS NOT INITIAL AND
     i_loc1   IS NOT INITIAL.
    CONDENSE: i_ppline, i_loc1.
    SELECT SINGLE zztrloc
                  zzparty
      FROM zmmt_locmast
      INTO (v_loc1, v_ppline)
      WHERE zztrloc = i_loc1
      AND   zzparty = i_ppline AND
           zzactflg = 'Y'.
    IF sy-subrc <> 0.
      MESSAGE 'Enter Valid Location1'(011) TYPE c_e.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ILOC1  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ILOC2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iloc2 INPUT.

  IF i_ppline IS NOT INITIAL AND
     i_loc1   IS NOT INITIAL AND
     i_loc2   IS NOT INITIAL.
    CONDENSE: i_ppline, i_loc1, i_loc2.
    SELECT SINGLE zztrloc
      FROM zmmt_locmast
      INTO v_loc2 WHERE zztrloc = i_loc2
                    AND zzparty = i_ppline
                    AND zzactflg = 'Y'.
    IF sy-subrc <> 0.
      MESSAGE 'Invalid Combination of Location2 With Location1'(012) TYPE c_e.
    ELSE.
      IF v_loc1 = v_loc2.
        MESSAGE 'Location1 & Location2 Can Not Be Same'(013) TYPE c_e.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ILOC2  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ILOC3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iloc3 INPUT.

  IF i_ppline IS NOT INITIAL AND
     i_loc1   IS NOT INITIAL AND
     i_loc2   IS NOT INITIAL AND
     i_loc3   IS NOT INITIAL.
    CONDENSE: i_ppline, i_loc1, i_loc2, i_loc3.
    SELECT SINGLE zztrloc
      FROM zmmt_locmast
      INTO v_loc3
      WHERE zztrloc = i_loc3
      AND zzparty = i_ppline
      AND zzactflg = 'Y'.
    IF sy-subrc <> 0.
      MESSAGE 'Enter Valid Values In Location3'(014) TYPE c_e.
    ELSE.
      IF v_loc3 = v_loc2 OR v_loc3 = v_loc1.
        MESSAGE 'Enter Valid Location3 In Combination With Location1/Location2'(015) TYPE c_e.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Location1 & Location 2 Should NOT Be Initial'(016) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_ILOC3  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ILOC4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iloc4 INPUT.

  IF i_ppline IS NOT INITIAL AND
     i_loc1   IS NOT INITIAL AND
     i_loc2   IS NOT INITIAL AND
     i_loc3   IS NOT INITIAL AND
     i_loc4   IS NOT INITIAL.
    CONDENSE: i_ppline, i_loc1, i_loc2, i_loc3, i_loc4.
*    SHIFT: i_ppline, i_loc1, i_loc2, i_loc3, i_loc4 LEFT DELETING LEADING '0'.
    SELECT SINGLE zztrloc
      FROM zmmt_locmast
      INTO v_loc4
      WHERE zztrloc = i_loc4
      AND zzparty = i_ppline
      AND zzactflg = 'Y'.
    IF sy-subrc <> 0.
      MESSAGE 'Enter Valid Values In Location1/Location2/Location3'(017) TYPE c_e.
    ELSE.
      IF v_loc4 = v_loc1 OR v_loc4 = v_loc2 OR v_loc4 = v_loc3.
        MESSAGE 'Enter Valid Location4 In Combination with Location1/Location2/Location3'(018) TYPE c_e.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Location1,Location 2 & Location3 Should NOT Be Initial'(019) TYPE c_e.
  ENDIF.

ENDMODULE.                 " CHECK_ILOC4  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IWERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iwerks INPUT.

  IF d_bukrs IS INITIAL OR d_ekorg IS INITIAL.
    MESSAGE 'Company Code/Purch Org Empty. Use F4 on MSA Fieled'(020) TYPE c_e.
  ELSE.
    SELECT SINGLE t001w~werks                               "#EC WARNOK
          INTO v_werks
          FROM  t001w INNER JOIN t001k                 "#EC CI_BUFFJOIN
          ON    t001w~werks = t001k~bwkey
          WHERE t001w~werks = i_werks
           AND  t001k~bukrs = d_bukrs.
    IF sy-subrc = 0.                                        "#EC WARNOK
      SELECT SINGLE lgort FROM t001l INTO d_lgort WHERE werks = i_werks. "#EC WARNOK
*{Begin of Change D30K932000
      CLEAR: w_sa.
      SELECT SINGLE * FROM zmmt_sa                          "#EC WARNOK
        INTO w_sa
        WHERE comp_code = d_bukrs
        AND   plant     = i_werks.
      IF  sy-subrc <> 0 AND i_agtyp = c_peak.
        CLEAR: v_emsg.
        CONCATENATE 'PEAK Accounting Information Is Not Maintained For Plant'(027)
            i_werks INTO v_emsg SEPARATED BY space.
        MESSAGE v_emsg TYPE c_e.
      ENDIF.
*End of Change D30K932000 }
    ELSE.
      MESSAGE 'Enter Valid Plant'(021) TYPE c_e.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_IWERKS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IZTERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_izterm INPUT.
  SELECT SINGLE a~zterm
          b~text1 FROM t052 AS a                            "#EC WARNOK
    INNER JOIN t052u AS b                              "#EC CI_BUFFJOIN
    ON a~zterm = b~zterm AND a~ztagg = b~ztagg
    INTO w_zterm
    WHERE a~zterm = i_zterm
    AND spras = sy-langu.
  IF sy-subrc <> 0.
    MESSAGE 'Enter Valid Payment Term'(022) TYPE c_e.
  ELSE.
    d_ztermname = w_zterm-text1.
  ENDIF.
ENDMODULE.                 " CHECK_IZTERM  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ITAXCOD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_itaxcod INPUT.
  CLEAR: v_mwskz.
  CONDENSE i_taxcod.
  SELECT SINGLE mwskz FROM t007a
    INTO v_mwskz                                            "#EC WARNOK
    WHERE mwskz = i_taxcod.
  IF sy-subrc = 0.
    SELECT SINGLE                                           "#EC WARNOK
           text1
      FROM t007s
      INTO d_taxcodname
      WHERE spras = sy-langu
      AND mwskz = v_mwskz.
  ELSE.
    MESSAGE 'Enter Valid Tax Code'(023) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_ITAXCOD  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IINDEX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_iindex INPUT.
  CLEAR: w_index.
  SELECT SINGLE inco1
                "b~bezei
    FROM tinc "AS a INNER JOIN tinct AS b               "#EC CI_BUFFJOIN
    "ON a~inco1 = b~inco1
    INTO w_index-zinco1
    WHERE inco1 = i_index.
  "AND spras = sy-langu.
  IF sy-subrc <> 0.
    MESSAGE 'Enter Valid Index/IncoTerms'(024) TYPE c_e.
  ELSE.
    SELECT SINGLE bezei
      FROM tinct
      INTO w_index-zbezei
      WHERE inco1 = i_index
        AND spras = sy-langu.
    IF sy-subrc = 0.
      d_indexname = w_index-zbezei.
    ENDIF.

  ENDIF.
ENDMODULE.                 " CHECK_IINDEX  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IFIXPRC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ifixprc.
  IF i_index = c_index AND i_fixprc IS INITIAL.
    CLEAR: i_surchr, i_discnt.
    MESSAGE 'Enter Fixed Price for Index GFX'(025) TYPE c_e.
  ELSEIF i_fixprc IS NOT INITIAL AND i_index <> c_index.
    CLEAR: i_surchr, i_discnt, v_fixprc.
    MESSAGE 'Check Index for Fixed Price'(031) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_IFIXPRC  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL'." OR 'EXIT'.
*Write code for a pop-up here? asking for 'Are you Sure you want to Exit?'.
      LEAVE TO SCREEN 0.
    WHEN 'DSH'." Generate/ Display Schedule
      PERFORM generate_schedule_agreement.
    WHEN 'CSH'." Create Schedule
      PERFORM validate_data_changes.
      PERFORM create_schedule_agreement.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SCHEDULE_AGREEMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_schedule_agreement .
*Store Screen Fields in Global variables
  v_msa     = i_msa.
  v_dstdt   = i_dstdt.
  v_dendt   = i_dendt.
  v_agrdt   = i_agrdt.
  v_curcy   = i_curcy.
  v_index   = i_index.
  v_dlyqty  = i_dlyqty.
  v_uom     = i_uom.
  v_werks   = i_werks.

  DATA: lv_dailyqty TYPE z_condayqty.
  lv_dailyqty = i_dlyqty.
  CALL FUNCTION 'ZMM_GENERATE_SA' ##FM_SUBRC_OK
    EXPORTING
      iv_plant          = i_werks
      iv_deal_sdate     = i_dstdt
      iv_deal_edate     = i_dendt
      iv_uom            = i_uom
      iv_taxcode        = i_taxcod
      iv_dailyquan      = lv_dailyqty
      iv_agreemtype     = i_agtyp
      iv_peakmat        = i_pkmat
      iv_no_of_months   = i_months
      iv_matgrp         = d_matgrp
      iv_peakmatgrp     = d_pmatgrp
      iv_monthly_amount = i_mntamt
      iv_purorg         = d_ekorg
      iv_index          = i_index
      iv_fixprice       = i_fixprc
      iv_curcy          = i_curcy
    IMPORTING
      es_error          = t_salineerror
    TABLES
      t_salineitems     = t_salineitems
    EXCEPTIONS
      no_record_found   = 1
      OTHERS            = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
  IF t_salineerror IS NOT INITIAL.
    MESSAGE t_salineerror-message TYPE c_e.
  ENDIF.
  IF t_salineitems IS INITIAL.
    MESSAGE 'ZERO line items generated.Please Check/Change Input'(001) TYPE c_e.
  ELSE.
    v_genchk = c_x.
    MESSAGE 'Schedule Generated'(044) TYPE c_s.
  ENDIF.
ENDFORM.                    " GENERATE_SCHEDULE_AGREEMENT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA_CHANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data_changes .
*check if there are any chagnes in the input field AFTER Generate Function
  DATA: lv_changeflag TYPE c.
  CLEAR: lv_changeflag.
  IF v_genchk IS NOT INITIAL.
    IF i_msa <> v_msa.
      lv_changeflag = c_x.
    ENDIF.
    IF i_dstdt <> v_dstdt.
      lv_changeflag = c_x.
    ENDIF.
    IF i_dendt <> v_dendt.
      lv_changeflag = c_x.
    ENDIF.
    IF i_agrdt <> v_agrdt.
      lv_changeflag = c_x.
    ENDIF.
    IF i_curcy <> v_curcy.
      lv_changeflag = c_x.
    ENDIF.
    IF i_index <> v_index.
      lv_changeflag = c_x.
    ENDIF.
    IF i_dlyqty <> v_dlyqty.
      lv_changeflag = c_x.
    ENDIF.
    IF i_uom <> v_uom.
      lv_changeflag = c_x.
    ENDIF.
    IF i_werks <> v_werks.
      lv_changeflag = c_x.
    ENDIF.
    IF lv_changeflag IS NOT INITIAL.
      MESSAGE text-026 TYPE c_e.
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDATE_DATA_CHANGES
*&---------------------------------------------------------------------*
*&      Form  CREATE_SCHEDULE_AGREEMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_schedule_agreement .
  DATA: lv_m        TYPE char2,
        lv_year     TYPE char4,
        lv_msg      TYPE sy-msgv1,
        lv_quantity TYPE char16,
        lv_pkmat    TYPE matnr,
        lv_zcondamt TYPE int4.
  REFRESH: t_saitems, t_sapricond, t_samaintain.
  CLEAR:   w_saheader, w_sapricond, w_samaintain, v_createchk, lv_pkmat.
  IF t_salineitems IS INITIAL.
    MESSAGE 'Click On "GENERATE" Button to Generate Line Items'(028) TYPE c_e.
  ELSE.
*Update Structure w_saheader
    w_saheader-lifnr = d_lifnr. " Account Number of Vendor or Creditor
    w_saheader-evart = i_agtyp. " Order Type (Purchasing)
    w_saheader-ekorg = d_ekorg. " Purchasing Organization
    w_saheader-ekgrp = d_ekgrp. " Purchasing Group
    w_saheader-werks = i_werks. " Plant
    w_saheader-lgort = d_lgort. " Storage Location
*    w_saheader-kdatb = i_dstdt. " From Date
*    w_saheader-kdate = i_dendt. " To date
*    w_saheader-kdatb = |{ i_dstdt DATE = USER }|.
*    w_saheader-kdate = |{ i_dendt DATE = USER }|.
    WRITE i_dstdt TO w_saheader-kdatb.
    WRITE i_dendt TO w_saheader-kdate.
    WRITE i_agrdt TO w_saheader-agrdate.
    w_saheader-zterm = i_zterm. " Terms of Payment Key
    w_saheader-waers = i_curcy. " Currency Key
    w_saheader-inco1 = i_index. " Incoterms (Part 1)
    w_saheader-inco2 = d_indexname. " Incoterms (Part 2)
    w_saheader-zzparty = i_ppline. " PipeLine Short Text
    w_saheader-zztrloc1 = i_loc1. " Trading Location1
    w_saheader-zztrloc2 = i_loc2. " Trading Location2
    w_saheader-zztrloc3 = i_loc3.  " Trading Location3
    w_saheader-zztrloc4 = i_loc4.  " Trading Location4
*Daily Quantity CHAR40
    TRANSLATE v_mseh6 TO UPPER CASE.
    IF i_dlyqty IS NOT INITIAL.
      CLEAR: lv_quantity, lv_zcondamt.
      lv_quantity = |{ i_dlyqty NUMBER = USER }|.
*      lv_zcondamt = i_dlyqty.
*      lv_quantity = lv_zcondamt.
      CONDENSE lv_quantity.
      CONCATENATE lv_quantity v_mseh6 INTO w_saheader-zzcondayqty RESPECTING BLANKS.
      CONCATENATE w_saheader-zzcondayqty '/Day'(033) INTO w_saheader-zzcondayqty.
      CONDENSE w_saheader-zzcondayqty.
    ENDIF.
*Surchare CHAR16
    IF i_surchr IS NOT INITIAL.
      CLEAR: lv_quantity.
      lv_quantity = i_surchr / 1000.
      CONDENSE lv_quantity.
      CONCATENATE lv_quantity i_curcy INTO w_saheader-zzconprice SEPARATED BY space.
      CONDENSE w_saheader-zzconprice.
      CONCATENATE w_saheader-zzconprice '/' INTO w_saheader-zzconprice.
      CONDENSE w_saheader-zzconprice.
      CONCATENATE w_saheader-zzconprice v_mseh6 INTO w_saheader-zzconprice.
      CONDENSE w_saheader-zzconprice.
    ENDIF.
*Discount CHAR16
    IF i_discnt IS NOT INITIAL.
      CLEAR: lv_quantity.
      lv_quantity = i_discnt / 1000.
      CONDENSE lv_quantity.
      CONCATENATE '-' lv_quantity INTO w_saheader-zzconprice .
      CONDENSE w_saheader-zzconprice.
      CONCATENATE w_saheader-zzconprice i_curcy INTO  w_saheader-zzconprice SEPARATED BY space.
      CONDENSE w_saheader-zzconprice.
      CONCATENATE w_saheader-zzconprice '/' INTO w_saheader-zzconprice.
      CONDENSE w_saheader-zzconprice.
      CONCATENATE w_saheader-zzconprice v_mseh6 INTO w_saheader-zzconprice.
      CONDENSE w_saheader-zzconprice.
    ENDIF.
    w_saheader-zzmsa = i_msa. "i_msanum.  "	Master Service Agreement Name
    w_saheader-zzekgrp = i_tbyer.  "  Trading Buyer
*Update Table t_itemsag

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = i_pkmat
      IMPORTING
        output = lv_pkmat.

    LOOP AT t_salineitems INTO w_salineitems.
      w_saitems-matnr = w_salineitems-material_no. "  Material Number
      w_saitems-ktmng = w_salineitems-target_quan. "  Character field 13 digits Target Quantity
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT' ##FM_SUBRC_OK
        EXPORTING
          input          = i_uom
          language       = sy-langu
        IMPORTING
          output         = w_saitems-meins "  Base Unit of Measure
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      w_saitems-matkl = w_salineitems-matgrp. "	Material Group
*{Begin of Change D30K932040
      FIND '.' IN w_salineitems-month_year.
      IF sy-subrc = 0.
        SPLIT w_salineitems-month_year AT '.' INTO lv_m lv_year .
      ELSE.
        FIND '/' IN w_salineitems-month_year.
        IF sy-subrc = 0.
          SPLIT w_salineitems-month_year AT '/' INTO lv_m lv_year . " NEED TO CHECK THE DATE FORMAT
        ELSE.
          FIND '-' IN w_salineitems-month_year.
          IF sy-subrc = 0.
            SPLIT w_salineitems-month_year AT '-' INTO lv_m lv_year .
          ENDIF.
        ENDIF.
      ENDIF.
*End of Change D30K932040}
      CONCATENATE lv_year lv_m INTO w_saitems-bednr . " Requirement Tracking Number
      CLEAR : lv_m , lv_year.
      w_saitems-mwskz = i_taxcod. "  Tax on sales/purchases code
      w_saitems-evers = d_shpins. "	Shipping Instructions
      w_saitems-koein = i_curcy.
      IF w_saitems-matnr = lv_pkmat.""" eNTER peak MEATERIAL EHRE
        w_saitems-koein = 'EA'.
        w_saitems-meins = 'EA'.
        w_saitems-sakto = d_glacc.  " Cost element
*        w_saitems-aufnrKOSTL = d_intord." Order Number "Commented For Change D30K931505
        w_saitems-kostl = d_kostl." Add for Change D30K931505
        w_saitems-knttp = 'K'."'Y'. "  Account Assignment Category Modified For Change D30K931505
        w_saitems-netpr = w_salineitems-kbetr." Net Price
      ENDIF.
      w_samaintain-ebelp = sy-tabix. "  Item Number of Purchasing Document
      w_samaintain-lpein = 'M'."  Category of Delivery Date
      w_samaintain-eeind = w_salineitems-month_year. "  Delivery Date
      w_samaintain-menge = w_salineitems-target_quan."  Quantity
      APPEND w_samaintain TO t_samaintain .
      APPEND w_saitems TO t_saitems.
      CLEAR: w_saitems, w_samaintain.
    ENDLOOP.
*Update Table t_pricondsag
*{Begin of change D30K932056
*Modified below IF ELSE Code to add new IF ELSE for each condition type
    IF i_fixprc IS NOT INITIAL.
      w_sapricond-kbetr = i_fixprc. "  Rate (condition amount or percentage)
      w_sapricond-kschl = 'PBXX'.   " Condition Type
      APPEND w_sapricond TO t_sapricond.
      CLEAR w_sapricond.
    ENDIF.                       " Commented for D30K932103/Uncommented for D30K932114
    IF i_surchr IS NOT INITIAL.  " Commented for D30K932103/Uncommented for D30K932114
      w_sapricond-kbetr = i_surchr.
      w_sapricond-kschl = 'ZC00'.
      APPEND w_sapricond TO t_sapricond.
      CLEAR w_sapricond.
    ELSE.                        " Commented for D30K932103/Uncommented for D30K932114
      w_sapricond-kbetr = 0.
      w_sapricond-kschl = 'ZC00'. " Modified for D30K932103/Re-placed to ZC00 For D30K932114
      APPEND w_sapricond TO t_sapricond.
      CLEAR w_sapricond.
    ENDIF.
*{Begin of Comment D30K932103
*{Begin of Uncomment D30K932114
    IF i_discnt IS NOT INITIAL.
      w_sapricond-kbetr = i_discnt .
      w_sapricond-kschl = 'ZC01'.
      APPEND w_sapricond TO t_sapricond.
      CLEAR w_sapricond.
    ELSE.
      w_sapricond-kbetr = 0 .
      w_sapricond-kschl = 'ZC01'.
      APPEND w_sapricond TO t_sapricond.
      CLEAR w_sapricond.
    ENDIF.
*End of Uncomment D30K932114 }
*{Begin of Comment D30K932114
*    IF i_surchr IS NOT INITIAL.
*      w_sapricond-kbetr = i_surchr.
*      w_sapricond-kschl = 'ZC00'.
*      APPEND w_sapricond TO t_sapricond.
*      CLEAR w_sapricond.
*    ENDIF.
*End of Comment D30K932114 }
*End of Comment D30K932103}
*End of Change D30K932056 }
    REFRESH: t_return.
* Call FM to Create Schedule Agreement
    CALL FUNCTION 'ZMM_SAG_CREATE'
      EXPORTING
*   LV_MODE              = 'N'
*   LV_UPDATE            = 'S'
        header               = w_saheader
* IMPORTING
*   LV_SUBRC             =
*   LV_OUTPUT            =
      TABLES
        it_itemsag           = t_saitems
        it_pricondsag        = t_sapricond
        it_sagmaintain       = t_samaintain
        it_return            = t_return.

    READ TABLE t_return INTO w_return WITH KEY errortype = c_e.
    IF sy-subrc = 0.
      MESSAGE w_return-errormessage TYPE c_e.
    ENDIF.
    READ TABLE t_return INTO w_return WITH KEY errortype = c_s.
    IF sy-subrc = 0.
      v_createchk = c_x.
      CONCATENATE 'Scheduling Agreement'(041) w_return-sanum 'Created'(042)
        INTO lv_msg SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
        EXPORTING
          titel        = 'Success Message'(029)
          textline1    = lv_msg
          start_column = c_21
          start_row    = 6.

      CLEAR: i_msa, d_lifnr, d_lifnrname, d_bukrs, d_ekorg, d_ekgrp,
             d_mdate, d_mstdt, d_mendt, d_tbyername, i_tbyer, i_agtyp,
             i_curcy, i_dstdt, i_dendt, i_index, d_indexname, i_dlyqty,
             i_uom, d_uomname, i_ppline, i_loc1, i_loc2, i_loc3, i_loc4,
             i_fixprc, i_surchr, i_discnt, i_zterm, d_ztermname, i_taxcod,
             d_taxcodname, d_matnr, i_werks, d_lgort, d_shpins, i_agrdt,
             d_shpins_detail, d_matgrp, i_pkmat, i_months, i_mntamt,
             d_pmatgrp, d_kostl, d_glacc, v_genchk." d_intorder For change D30K931505
      REFRESH: t_salineitems,t_return.

      REFRESH: t_salineitems.
      CALL METHOD go_alv->refresh_table_display.
      CALL METHOD go_custom_container->free.
      CALL METHOD cl_gui_cfw=>flush.
      CLEAR: gs_layout, go_custom_container, go_alv, v_createchk.
    ENDIF.
    IF v_createchk IS INITIAL.
      READ TABLE t_return INTO w_return WITH KEY errortype = c_w.
      IF sy-subrc = 0.
        MESSAGE w_return-errormessage TYPE c_i.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_SCHEDULE_AGREEMENT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  IF sy-ucomm = 'EXIT'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  ENABLE_PEAK_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE enable_peak_fields OUTPUT.

  IF i_agtyp IS NOT INITIAL AND i_agtyp = c_peak.
*    CLEAR:i_pkmat, i_months,i_mntamt.
    LOOP AT SCREEN.
      IF screen-name = 'I_MONTHS' OR screen-name = 'I_MNTAMT'.
        screen-input = 1. " It makes screen field in EDIT mode
        MODIFY SCREEN.
        v_enablepk = c_x.
      ENDIF.
    ENDLOOP.
  ELSEIF i_agtyp IS INITIAL  OR ( i_agtyp IS NOT INITIAL AND i_agtyp <> c_peak ) ##BOOL_OK.
    CLEAR:i_pkmat, i_months,i_mntamt, v_enablepk.
    LOOP AT SCREEN.
      IF screen-name = 'I_MONTHS' OR screen-name = 'I_MNTAMT'.
        screen-input = 0. " It makes screen field in NON EDIT mode
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " ENABLE_PEAK_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PEAK_FIELDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_peak_fields.
  IF v_enablepk IS NOT INITIAL.
    IF i_months IS INITIAL OR i_mntamt IS INITIAL.
      MESSAGE 'Enter Months and Amount for PEAK Deal'(007) TYPE c_e.
*    ELSEIF i_months IS NOT INITIAL AND i_mntamt IS INITIAL.
*      MESSAGE 'Enter Months and Amount for PEAK Deal'(007) TYPE c_e.
*    ELSEIF i_months IS INITIAL AND i_mntamt IS NOT INITIAL.
*      MESSAGE 'Enter Months and Amount for PEAK Deal'(007) TYPE c_e.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_PEAK_FIELDS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_dates INPUT.
  IF i_dstdt IS  NOT INITIAL AND i_dendt IS NOT INITIAL AND i_agrdt IS NOT INITIAL.
    IF i_dendt GE i_dstdt.
      " Do Nothing
    ELSE.
      MESSAGE  'Deal End Date Must be Greate Than Deal Start Date'(034) TYPE c_e.
    ENDIF.
    IF i_dstdt NOT BETWEEN d_mstdt AND d_mendt.
      MESSAGE  'Deal Start Date Is Outside of Contract Date'(036) TYPE c_e.
    ENDIF.
    IF i_dstdt LT i_agrdt.
      MESSAGE  'Deal Start Date cannot be before Agreement Date'(043) TYPE c_e.
    ENDIF.
    IF i_dendt NOT BETWEEN d_mstdt AND d_mendt.
      MESSAGE 'Deal End Date Is Outside of Contract Date'(039) TYPE c_e.
    ENDIF.
    IF d_mstdt GE i_agrdt.
      MESSAGE 'Agreement Date Should Be Greater Than Deal Start Date'(045) TYPE c_e.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_DATES  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_AGRDT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_agrdt INPUT.
  CALL FUNCTION 'F4_DATE' ##FM_SUBRC_OK
    EXPORTING
      date_for_first_month         = i_agrdt
      display                      = ' '
    IMPORTING
      select_date                  = i_agrdt
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
ENDMODULE.                 " F4_AGRDT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SURDISCNT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_surdiscnt.
  IF  i_fixprc IS NOT INITIAL AND
      ( i_surchr IS NOT INITIAL OR i_discnt IS NOT INITIAL ).
    MESSAGE 'Enter only one of Fix Price OR Surcharge OR Discount'(035) TYPE c_e.
  ELSEIF i_surchr IS NOT INITIAL AND
         ( i_fixprc IS NOT INITIAL OR i_discnt IS NOT INITIAL ).
    MESSAGE 'Enter only one of Fix Price OR Surcharge OR Discount'(035) TYPE c_e.
  ELSEIF i_discnt IS NOT INITIAL AND
        ( i_fixprc IS NOT INITIAL OR i_surchr IS NOT INITIAL ).
    MESSAGE 'Enter only one of Fix Price OR Surcharge OR Discount'(035) TYPE c_e.
  ENDIF.
ENDMODULE.                 " CHECK_SURDISCNT  INPUT
