 REPORT  zlmmr003_gastrans_recon.
*&---------------------------------------------------------------------*
*& Report  ZLMMR003_GASTRANS_RECON                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*  Author:      Brian Boundy                                           *
*  Date:        March, 2011.                                           *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*    - This program will report daily gas transportation purchases by  *
*    selecting material documents created by the CARE-to-SAP interface.*
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*& 2011/07/06 gymana TR804 - Add table EKSN to report                  *
*& 2021/02/17 birudurd COG changes to include Plant on Selection Screen*
*&---------------------------------------------------------------------*
 TYPE-POOLS: slis.

 TABLES: essr, esll, eskn, ekpo.

 DATA:  BEGIN OF lt_report OCCURS 1,
           count       LIKE  sy-tabix,
           lblni       LIKE  essr-lblni,
           lblne       LIKE  essr-lblne,
           ernam       LIKE  essr-ernam,
           erdat       LIKE  essr-erdat,
           aedat       LIKE  essr-aedat,
           aenam       LIKE  essr-aenam,
           sbnamag     LIKE  essr-sbnamag,
           sbnaman     LIKE  essr-sbnaman,
           dlort       LIKE  essr-dlort,
           lbldt       LIKE  essr-lbldt,
           lzvon       LIKE  essr-lzvon,
           lzbis       LIKE  essr-lzbis,
           waers       LIKE  essr-waers,
           packno      LIKE  essr-packno,
           txz01       LIKE  essr-txz01,
           ebeln       LIKE  essr-ebeln,
           ebelp       LIKE  essr-ebelp,
           loekz       LIKE  essr-loekz,
           kzabn       LIKE  essr-kzabn,
           final       LIKE  essr-final,
           frggr       LIKE  essr-frggr,
           frgsx       LIKE  essr-frgsx,
           frgkl       LIKE  essr-frgkl,
           frgzu       LIKE  essr-frgzu,
           frgrl       LIKE  essr-frgrl,
           f_lock      LIKE  essr-f_lock,
           bldat       LIKE  essr-bldat,
           budat       LIKE  essr-budat,
           xblnr       LIKE  essr-xblnr,
           bktxt       LIKE  essr-bktxt,
           knttp       LIKE  essr-knttp,
           kzvbr       LIKE  essr-kzvbr,
           netwr       LIKE  essr-netwr,
           banfn       LIKE  essr-banfn,
           bnfpo       LIKE  essr-bnfpo,
           user2       LIKE  essr-user2,
           lpackno     LIKE  esll-packno,
           introw      LIKE  esll-introw,
           extrow      LIKE  esll-extrow,
           del         LIKE  esll-del,
           srvpos      LIKE  esll-srvpos,
           extsrvno    LIKE  esll-extsrvno,
           menge       LIKE  esll-menge,
           meins       LIKE  esll-meins,
           peinh       LIKE  esll-peinh,
           brtwr       LIKE  esll-brtwr,
           lnetwr      LIKE  esll-netwr,
           ktext1      LIKE  esll-ktext1,
           pln_packno  LIKE  esll-pln_packno,
           pln_introw  LIKE  esll-pln_introw,
           inform      LIKE  esll-inform,
           mwskz       LIKE  esll-mwskz,
           matkl       LIKE  esll-matkl,
           userf1_num  LIKE  esll-userf1_num,
           userf2_num  LIKE  esll-userf2_num,
           userf1_txt  LIKE  esll-userf1_txt,
           userf2_txt  LIKE  esll-userf2_txt,
           kpackno     LIKE  eskn-packno,
           knetwr      LIKE  eskn-netwr,
           sakto       LIKE  eskn-sakto,
           kostl       LIKE  eskn-kostl,
           aufnr       LIKE  eskn-aufnr,
           kmwskz      LIKE  eskn-mwskz,
           werks       like  ekpo-werks, " COG
         END OF lt_report.


 DATA: lv_month(3)   TYPE c,
       lv_year(4)    TYPE c,
       lv_int        TYPE i,

       w_head01(60)  TYPE c,
       w_head02(60)  TYPE c,
       es_variant    LIKE disvariant,
       is_variant    LIKE disvariant,
       w_mjahr  LIKE mkpf-mjahr.

***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
 SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
 SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-101.
 PARAMETERS p_year   TYPE gjahr.                     "Year
 PARAMETERS p_month  TYPE perio.                     "Period(Month)
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 1(3) text-102.
 SELECTION-SCREEN END OF LINE.
 SELECT-OPTIONS s_bldat FOR essr-bldat.
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 1(3) text-102.
 SELECTION-SCREEN END OF LINE.
 SELECT-OPTIONS: s_lblni FOR essr-lblni.             "Entry Sheet Number
 SELECTION-SCREEN END OF BLOCK box1.

 SELECT-OPTIONS:
     s_budat       FOR essr-budat,                  "Posting Date
     s_lblne       FOR essr-lblne,                  "External Number
     s_xblnr       FOR essr-xblnr,                  "Reference
     s_ernam       FOR essr-ernam,                  "User
     s_lbldt       FOR essr-lbldt,                  "Price Reference
     s_userf2      FOR esll-userf2_txt,             "SAP Contract #
     s_ebeln       FOR essr-ebeln,                  "Purchasing Document
     s_erdat       FOR essr-erdat,                  "creation date
     s_werks       FOR ekpo-werks.

 SELECTION-SCREEN SKIP.
 PARAMETERS p_varint   LIKE  disvariant-variant.        "Display Variant

 SELECTION-SCREEN END OF BLOCK box.

***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
 AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varint.
   is_variant-report = 'ZLMMR003_GASTRANS_RECON'.
   CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
     EXPORTING
       is_variant    = is_variant
       i_save        = 'A'
     IMPORTING
       es_variant    = es_variant
     EXCEPTIONS
       not_found     = 1
       program_error = 2
       OTHERS        = 3.
   p_varint = es_variant-variant.

 AT SELECTION-SCREEN.
   lv_int = 0.

   IF p_month <> '0' OR p_year <> '0'.
     lv_int = lv_int + 1.
     IF p_month = '0' OR p_year = '0'.
       "Check if one of them is blank.
       MESSAGE e019(zs) WITH text-008.
     ENDIF.
   ENDIF.
   IF s_bldat-low <> '00000000'.
     lv_int = lv_int + 1.
   ENDIF.
   IF s_lblni-low <> ''.
     lv_int = lv_int + 1.
   ENDIF.


   IF lv_int = 0.
     "No selection criteria
     MESSAGE e019(zs) WITH text-006.
   ENDIF.
   IF lv_int > 1.
     "Multiple selection criteria
     MESSAGE e019(zs) WITH text-007.
   ENDIF.

***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
 START-OF-SELECTION.
   "Using Year/Period
   IF p_month <> '0'.
     PERFORM get_date_from_period.
   ENDIF.
   PERFORM get_db_data.
   PERFORM display_alv_grid_data.

***********************************************************************
*                     GET_DATE_FROM_PERIOD                            *
***********************************************************************
 FORM get_date_from_period.
   "Create an entry in s_bldat for the selected month.
   lv_year = p_year.
   lv_month = p_month.
   IF p_month < 10.
     lv_month(1) = '0'.
   ENDIF.

   s_bldat-sign = 'I'.
   s_bldat-option = 'BT'.
   CONCATENATE lv_year lv_month(2) '01' INTO s_bldat-low.
   CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
     EXPORTING
       day_in            = s_bldat-low
     IMPORTING
       last_day_of_month = s_bldat-high
     EXCEPTIONS
       day_in_no_date    = 1.

   APPEND s_bldat.
 ENDFORM.                    "GET_DATE_FROM_PERIOD
***********************************************************************
*                             GET_DB_DATA                             *
***********************************************************************
 FORM get_db_data.

   "If selecting by Service Entry Sheet Number
   "Set the date range to infinite.
   IF s_lblni-low <> ''.
     CLEAR s_bldat.
   ENDIF.

   SELECT  essr~lblni essr~lblne essr~ernam essr~erdat essr~aedat
           essr~aenam essr~sbnamag essr~sbnaman essr~dlort essr~lbldt
           essr~lzvon essr~lzbis essr~waers essr~packno essr~txz01
           essr~ebeln essr~ebelp essr~loekz essr~kzabn essr~final
           essr~frggr essr~frgsx essr~frgkl essr~frgzu essr~frgrl
           essr~f_lock essr~bldat essr~budat essr~xblnr essr~bktxt
           essr~knttp essr~kzvbr essr~netwr essr~banfn essr~bnfpo
           essr~user2
           esll~packno AS lpackno esll~introw esll~extrow esll~del
           esll~srvpos esll~extsrvno esll~menge esll~meins esll~peinh
           esll~brtwr esll~netwr AS lnetwr esll~ktext1 esll~pln_packno
           esll~pln_introw esll~inform esll~mwskz esll~matkl
           esll~userf1_num esll~userf2_num esll~userf1_txt
           esll~userf2_txt
           eskn~packno AS kpackno eskn~netwr AS knetwr
           eskn~sakto eskn~kostl eskn~aufnr eskn~mwskz AS kmwskz
           ekpo~werks                                           "COG
     INTO CORRESPONDING FIELDS OF TABLE lt_report
     FROM  ( ( essr  INNER JOIN esll AS esllmid
                     ON essr~packno = esllmid~packno
                   INNER JOIN esll
                     ON esllmid~sub_packno = esll~packno
* Start of change COG
                   INNER JOIN ekpo
                     ON ekpo~ebeln = essr~ebeln
                     AND ekpo~ebelp = essr~ebelp  )
* End of change COG
                   LEFT OUTER JOIN eskn
                     ON essr~lblni = eskn~packno )
     WHERE essr~bldat      IN s_bldat
       AND essr~budat      IN s_budat
       AND essr~lblne      IN s_lblne
       AND essr~xblnr      IN s_xblnr
       AND essr~ernam      IN s_ernam
       AND essr~lblni      IN s_lblni
       AND essr~lbldt      IN s_lbldt
       AND esll~userf2_txt IN s_userf2
       AND essr~ebeln      IN s_ebeln
       AND essr~erdat      IN s_erdat
       AND ekpo~werks      IN s_werks .

   IF sy-subrc <> 0.
     WRITE: /1 'NO DATA SELECTED'.
     STOP.
   ENDIF.

   LOOP AT lt_report.
     MOVE '1' TO lt_report-count.
     MODIFY lt_report.
   ENDLOOP.

   "SORT lt_report BY xblnr lblne lblni.
 ENDFORM.                    "GET_DB_DATA

***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
 FORM display_alv_grid_data.

   DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
         ls_fieldcat LIKE LINE OF lt_fieldcat,
         ls_layout   TYPE slis_layout_alv,
         ls_variant  LIKE disvariant,
         lt_sort     TYPE slis_t_sortinfo_alv,
         ls_sort     LIKE LINE OF lt_sort.
*         i_event     TYPE slis_t_event.

   DATA: w_date_from(10) TYPE c,
         w_date_to(10)   TYPE c.

   CONCATENATE s_bldat-low(4) '/' s_bldat-low+4(2) '/' s_bldat-low+6(2)
                                                       INTO w_date_from.
   CONCATENATE s_bldat-high(4) '/' s_bldat-high+4(2) '/'
                                   s_bldat-high+6(2)   INTO w_date_to.

   CONCATENATE text-004 w_date_from text-005 w_date_to
               INTO w_head01 SEPARATED BY space.

   MOVE text-clt  TO w_head02+0(7).
   MOVE sy-sysid  TO w_head02+8(5).
   MOVE sy-mandt  TO w_head02+14(4).
   MOVE text-dte  TO w_head02+21(5).
   WRITE sy-datum TO w_head02+27(10).
   MOVE text-tme  TO w_head02+40(5).
   WRITE sy-uzeit TO w_head02+46(10).

   ls_layout-colwidth_optimize = 'X'.
   ls_layout-zebra = 'X'.

   ls_variant-report = sy-repid.
   ls_variant-variant = p_varint.

   CLEAR:  lt_fieldcat.

* create field catalog
   CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
     EXPORTING
       i_program_name         = sy-repid
       i_internal_tabname     = 'LT_REPORT'
       i_inclname             = sy-repid
     CHANGING
       ct_fieldcat            = lt_fieldcat
     EXCEPTIONS
       inconsistent_interface = 1
       program_error          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
   DEFINE hide_column.
     loop at lt_fieldcat into ls_fieldcat.
       case ls_fieldcat-fieldname.
         when &1.
           ls_fieldcat-no_out = 'X'.          " Hide Columns
           ls_fieldcat-key    = ' '.          " Key columns-not first
       endcase.
       modify lt_fieldcat from ls_fieldcat.
     endloop.
   END-OF-DEFINITION.

   hide_column:
          'ERNAM','ERDAT','AEDAT','AENAM','SBNAMAG','SBNAMAN',
          'DLORT','LBLDT','LZVON','LZBIS','WAERS','PACKNO','TXZ01',
          'EBELN','EBELP','LOEKZ','KZABN','FINAL','FRGGR','FRGSX',
          'FRGKL','FRGZU','FRGRL','F_LOCK','BLDAT','BUDAT','BKTXT',
          'KNTTP','KZVBR','NETWR','BANFN','BNFPO','USER2',

          'LPACKNO','INTROW','EXTROW','DEL','SRVPOS','EXTSRVNO','MENGE',
          'PEINH','BRTWR','LNETWR','KTEXT1','PLN_PACKNO',
          'PLN_INTROW','INFORM','MWSKZ','MATKL','USERF1_NUM',
          'USERF1_TXT','USERF2_TXT'.


   "Fix names of custom column
   LOOP AT lt_fieldcat INTO ls_fieldcat.
     CASE ls_fieldcat-fieldname.

       WHEN 'COUNT'.
         ls_fieldcat-seltext_l = 'Record Counter'.
         ls_fieldcat-seltext_m = 'Record Counter'.
         ls_fieldcat-seltext_s = 'Rec Count'.
         ls_fieldcat-reptext_ddic = 'Rec Counter'.
       WHEN 'TXZ01'.
         ls_fieldcat-seltext_l = 'SES Short Text'.
         ls_fieldcat-seltext_m = 'SES Short Text'.
         ls_fieldcat-seltext_s = 'SES ShText'.
         ls_fieldcat-reptext_ddic = 'SES Short Text'.
       WHEN 'EBELP'.
         ls_fieldcat-seltext_l = 'Purch Doc Item Number'.
         ls_fieldcat-seltext_m = 'Pur Doc Item No'.
         ls_fieldcat-seltext_s = 'PuDoc Item'.
         ls_fieldcat-reptext_ddic = 'PurDoc Item'.
       WHEN 'LOEKZ'.
         ls_fieldcat-seltext_l = 'SES Delete Indicator'.
         ls_fieldcat-seltext_m = 'SES Delete Ind'.
         ls_fieldcat-seltext_s = 'SES Delete'.
         ls_fieldcat-reptext_ddic = 'SES Del Ind'.
       WHEN 'NETWR'.
         ls_fieldcat-seltext_l = 'SES Net Value'.
         ls_fieldcat-seltext_m = 'SES Net Value'.
         ls_fieldcat-seltext_s = 'SES NetVal'.
         ls_fieldcat-reptext_ddic = 'SES Net Value'.
       WHEN 'BNFPO'.
         ls_fieldcat-seltext_l = 'Purch Req Item Number'.
         ls_fieldcat-seltext_m = 'Pur Req Item No'.
         ls_fieldcat-seltext_s = 'PuReq Item'.
         ls_fieldcat-reptext_ddic = 'Pur Req Item No'.
       WHEN 'LPACKNO'.
         ls_fieldcat-seltext_l = 'ESLL Package Number'.
         ls_fieldcat-seltext_m = 'ESLL Package No'.
         ls_fieldcat-seltext_s = 'ESLL PkgNo'.
         ls_fieldcat-reptext_ddic = 'ESLL Package No'.
       WHEN 'DEL'.
         ls_fieldcat-seltext_l = 'ESLL Deletion Indicator'.
         ls_fieldcat-seltext_m = 'ESLL Del Ind'.
         ls_fieldcat-seltext_s = 'ESLL DelId'.
         ls_fieldcat-reptext_ddic = 'ESLL Delete Ind'.
       WHEN 'LNETWR'.
         ls_fieldcat-seltext_l = 'ESLL Net Value'.
         ls_fieldcat-seltext_m = 'ESLL Net Value'.
         ls_fieldcat-seltext_s = 'ESLL NtVal'.
         ls_fieldcat-reptext_ddic = 'ESLL Net Value'.
       WHEN 'KTEXT1'.
         ls_fieldcat-seltext_l = 'ESLL Short Text'.
         ls_fieldcat-seltext_m = 'ESLL Short Text'.
         ls_fieldcat-seltext_s = 'ESLL ShTxt'.
         ls_fieldcat-reptext_ddic = 'ESLL Short Text'.
       WHEN 'PLN_PACKNO'.
         ls_fieldcat-seltext_l = 'ESLL Src Package Number'.
         ls_fieldcat-seltext_m = 'ESLL Src Package No'.
         ls_fieldcat-seltext_s = 'ESLLSrcPkg'.
         ls_fieldcat-reptext_ddic = 'ESLL Src Package No'.
       WHEN 'PLN_INTROW'.
         ls_fieldcat-seltext_l = 'ESLL Entry: Package Line'.
         ls_fieldcat-seltext_m = 'ESLL Package Ln'.
         ls_fieldcat-seltext_s = 'ESLL PkgLn'.
         ls_fieldcat-reptext_ddic = 'ESLL Package Line'.
       WHEN 'MWSKZ'.
         ls_fieldcat-seltext_l = 'ESLL Tax code'.
         ls_fieldcat-seltext_m = 'ESLL Tax code'.
         ls_fieldcat-seltext_s = 'ESLL TaxCd'.
         ls_fieldcat-reptext_ddic = 'ESLL Tax Code'.
       WHEN 'USERF1_NUM'.
         ls_fieldcat-seltext_l = 'User Tag'.
         ls_fieldcat-seltext_m = 'User Tag'.
         ls_fieldcat-seltext_s = 'User Tag'.
         ls_fieldcat-reptext_ddic = 'User Tag'.
       WHEN 'USERF2_NUM'.
         ls_fieldcat-seltext_l = 'CARE Qty'.
         ls_fieldcat-seltext_m = 'CARE Qty'.
         ls_fieldcat-seltext_s = 'CARE Qty'.
         ls_fieldcat-reptext_ddic = 'CARE Qty'.
       WHEN 'USERF1_TXT'.
         ls_fieldcat-seltext_l = 'CARE Interface Data'.
         ls_fieldcat-seltext_m = 'CARE Interface Data'.
         ls_fieldcat-seltext_s = 'CARE Int'.
         ls_fieldcat-reptext_ddic = 'CARE Interface Data'.
       WHEN 'USERF2_TXT'.
         ls_fieldcat-seltext_l = 'Original SES'.
         ls_fieldcat-seltext_m = 'Original SES'.
         ls_fieldcat-seltext_s = 'Orig SES'.
         ls_fieldcat-reptext_ddic = 'Original SES'.
       WHEN 'KPACKNO'.
         ls_fieldcat-seltext_l = 'ESKN Packno'.
         ls_fieldcat-seltext_m = 'ESKN Packno'.
         ls_fieldcat-seltext_s = 'ESKN Packno'.
         ls_fieldcat-reptext_ddic = 'ESKN Packno'.
       WHEN 'KNETWR'.
         ls_fieldcat-seltext_l = 'ESKN Acct Assgt'.
         ls_fieldcat-seltext_m = 'ESKN Acct Assgt'.
         ls_fieldcat-seltext_s = 'ESKN Acct Assgt'.
         ls_fieldcat-reptext_ddic = 'ESKN Acct Assgt'.
       WHEN 'SAKTO'.
         ls_fieldcat-seltext_l = 'ESKN G/L Acct'.
         ls_fieldcat-seltext_m = 'ESKN G/L Acct'.
         ls_fieldcat-seltext_s = 'ESKN G/L Acct'.
         ls_fieldcat-reptext_ddic = 'ESKN G/L Acct'.
       WHEN 'KOSTL'.
         ls_fieldcat-seltext_l = 'ESKN CC'.
         ls_fieldcat-seltext_m = 'ESKN CC'.
         ls_fieldcat-seltext_s = 'ESKN CC'.
         ls_fieldcat-reptext_ddic = 'ESKN CC'.
       WHEN 'AUFNR'.
         ls_fieldcat-seltext_l = 'ESKN Order'.
         ls_fieldcat-seltext_m = 'ESKN Order'.
         ls_fieldcat-seltext_s = 'ESKN Order'.
         ls_fieldcat-reptext_ddic = 'ESKN Order'.
       WHEN 'KMWSKZ'.
         ls_fieldcat-seltext_l = 'ESKN tax code'.
         ls_fieldcat-seltext_m = 'ESKN tax code'.
         ls_fieldcat-seltext_s = 'ESKN tax code'.
         ls_fieldcat-reptext_ddic = 'ESKN tax code'.
     ENDCASE.
     MODIFY lt_fieldcat FROM ls_fieldcat.
   ENDLOOP.


   CLEAR: ls_sort, lt_sort.
   ls_sort-spos = 1.
   ls_sort-fieldname = 'XBLNR'.
   APPEND ls_sort TO lt_sort.

   CLEAR ls_sort.
   ls_sort-spos = 2.
   ls_sort-fieldname = 'LBLNE'.
   APPEND ls_sort TO lt_sort.

   CLEAR ls_sort.
   ls_sort-spos = 3.
   ls_sort-fieldname = 'LBLNI'.
   APPEND ls_sort TO lt_sort.

   ls_layout-countfname = 'COUNT'.

*  CONSTANTS : c_formname_subtotal_text TYPE slis_formname VALUE
*              'METHOD_SUBTOTAL_TEXT'.

*  DATA: ls_event TYPE slis_alv_event.


*  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
*    EXPORTING
*      i_list_type     = 4
*    IMPORTING
*      et_events       = i_event
*    EXCEPTIONS
*      list_type_wrong = 0
*      OTHERS          = 0.

* Subtotal
*  READ TABLE i_event  INTO ls_event
*                    WITH KEY name = slis_ev_subtotal_text.
*  IF sy-subrc = 0.
*    MOVE c_formname_subtotal_text TO ls_event-form.
*    MODIFY i_event FROM ls_event INDEX sy-tabix.
*  ENDIF.

* Display ALV
   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
       it_fieldcat            = lt_fieldcat
       is_layout              = ls_layout
       i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
       i_callback_program     = sy-repid
       i_save                 = 'A'
       is_variant             = ls_variant
       it_sort                = lt_sort
*       it_events              = i_event
     TABLES
       t_outtab               = lt_report
     EXCEPTIONS
       program_error          = 1
       OTHERS                 = 2.

 ENDFORM.                    "DISPLAY_ALV_GRID_DATA

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

 FORM alv_top_of_page.
   DATA: ls_line TYPE slis_listheader.
   DATA: lt_top_of_page TYPE slis_t_listheader.

*1- Heading Line: Type H
   CLEAR ls_line.
   ls_line-typ  = 'H'.
   ls_line-info = sy-title.             "sy-title.
   APPEND ls_line TO lt_top_of_page.

*3- Heading Line: Type H
   CLEAR ls_line.
   ls_line-typ   = 'H'.
   ls_line-key   = ''.
   ls_line-info = w_head01.
   APPEND ls_line TO lt_top_of_page.

*2- Action Line:  Type A
   CLEAR ls_line.
   ls_line-typ   = 'A'.
   ls_line-key   = ''.
   ls_line-info = w_head02.
   APPEND ls_line TO lt_top_of_page.

   CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
     EXPORTING
       it_list_commentary = lt_top_of_page.
 ENDFORM.                    "ALV_TOP_OF_PAGE

************************************************************************
*       Form  method_subtotal_text
*----------------------------------------------------------------------*
*       Build subtotal text
************************************************************************
* FORM method_subtotal_text USING es_subtottxt_info TYPE lvc_s_stxt
*               ep_subtot_line TYPE REF TO data
*               e_event_data TYPE REF TO cl_alv_event_data.

*   DATA ls_report LIKE lt_report.

*   FIELD-SYMBOLS: <fs1> STRUCTURE lt_report DEFAULT ls_report,
*                  <fs2>.

*   IF es_subtottxt_info-criteria = 'COUNT'.
*     ASSIGN ep_subtot_line->* TO <fs1>.
*     ASSIGN e_event_data->m_data->* TO <fs2>.
*     CONCATENATE es_subtottxt_info-keyword ': '
*                 <fs1>-count INTO <fs2>.
*   ENDIF.
* ENDFORM.                    "method_subtotal_text
