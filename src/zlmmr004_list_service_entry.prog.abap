REPORT  zlmmr004_list_service_entry.
************************************************************************
*  Client:    Spectra Energy.                                          *
*  Author:    Brian Boundy                                             *
*  Date:      February, 2011.                                          *
*  Track #:   TR804.                                                   *
*                                                                      *
*  Description:                                                        *
*     - This is a copy of ML84, with some tweeks to allow some some    *
*       more selections                                                *
*********************************
** Changes:
** TR804-COG: Updates for COG
** essr_rel changed to zessr_rel
*********************************
************************************************************************
* ---------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
*--------  ---- ------- ---------------------------------------------- *
*                                                                      *
*                                                                      *
************************************************************************

************************************************************************
*     Servive Entry Sheets - List Display
************************************************************************


* type-pool (contains field catalog)
TYPE-POOLS:  slis.


* tables
TABLES: zessr_rel,                   "View über ekko, ekpo, essr
        essr,*essr,                  "Erfassungsblatt
        ekko,                        "Bestellkopf
        ekpo,                       "Bestellposition
        t024e,                       "Einkaufsorganisation
        t024,                        "Einkäufergruppe
        lfa1,                        "Lieferantenstamm
        eket,                        "Einteilungen
        t001,                        "Texte Buchungskreis
        t001w,                        "Texte Werk
        t023t,                          "Texte Warengruppe
        t16ft,                       "Texte Freigabestrategie
        t16fm.                       "Texte Freigabekennzeichen

INCLUDE <icon>.
INCLUDE <symbol>.

* internal tables

** Übergabestruktur an ALV
DATA: BEGIN OF t_merep_outtab_srvdoc OCCURS 0.   "ALV Grid
INCLUDE TYPE merep_outtab_srvdoc.
DATA:  lpein LIKE eket-lpein,
       lpein_ext LIKE rm06e-lpein,
       eindt_ext LIKE rm06e-eeind,
       ekotx LIKE t024e-ekotx,
       eknam LIKE t024-eknam,
       wnam LIKE t001w-name1,
       wgbez LIKE t023t-wgbez,
       frgxt LIKE t16ft-frgxt,
       frget LIKE t16fm-frget,
       butxt LIKE t001-butxt,
       abn_icon(30),
       erf_icon(30),
       sperr_icon(30),
       no_price_auth  TYPE c,
       no_accept_auth TYPE c.
DATA: END OF t_merep_outtab_srvdoc.            "ALV Grid


DATA: l_value TYPE string40, "user parameter ME_USE_GRID
      l_repid TYPE syrepid.

* icons
DATA: g_icon_green_light  TYPE merep_icon_status,
      g_icon_yellow_light TYPE merep_icon_status,
      g_icon_red_light    TYPE merep_icon_status,
      g_icon_final_entry  TYPE merep_icon_status,
      g_icon_locked       TYPE merep_icon_status.

DATA: x_keyinfo TYPE slis_keyinfo_alv.

* Interne Tabelle für Feldkatalog
DATA: xfield TYPE slis_t_fieldcat_alv.

* Interne Tabelle für Feldkatalog ALV-GRID
DATA: BEGIN OF ofield OCCURS 1.
        INCLUDE STRUCTURE lvc_s_fcat.
DATA: END OF ofield.
DATA: ls_layout TYPE lvc_s_layo,
      ls_variant TYPE disvariant.

* header table
DATA : BEGIN OF xdata OCCURS 0.
        INCLUDE STRUCTURE zessr_rel.
DATA: name1 LIKE lfa1-name1,
      lpein LIKE eket-lpein,
      lpein_ext LIKE rm06e-lpein,
      eindt_ext LIKE rm06e-eeind,
      ekotx LIKE t024e-ekotx,
      eknam LIKE t024-eknam,
      wnam LIKE t001w-name1,
      wgbez LIKE t023t-wgbez,
      frgxt LIKE t16ft-frgxt,
      frget LIKE t16fm-frget,
      butxt LIKE t001-butxt,
      abn_icon(30),
      erf_icon(30),
      sperr_icon(30).
DATA:   no_price_auth  TYPE c,
        no_accept_auth TYPE c.
DATA: END OF xdata.
DATA: wa LIKE xdata.
DATA: wb LIKE xdata.

* item table
DATA : BEGIN OF datax OCCURS 0.
        INCLUDE STRUCTURE zessr_rel.
DATA: name1 LIKE lfa1-name1,
      lpein LIKE eket-lpein,
      lpein_ext LIKE rm06e-lpein,
      eindt_ext LIKE rm06e-eeind,
      ekotx LIKE t024e-ekotx,
      eknam LIKE t024-eknam,
      wnam LIKE t001w-name1,
      wgbez LIKE t023t-wgbez,
      frgxt LIKE t16ft-frgxt,
      frget LIKE t16fm-frget,
      butxt LIKE t001-butxt,
      abn_icon(30),
      erf_icon(30),
      sperr_icon(30).
DATA: END OF datax.


* internal tables for text elements
DATA: BEGIN OF xlfa1 OCCURS 10.             "vendor
        INCLUDE STRUCTURE lfa1.
DATA: END OF xlfa1.

DATA: xt024e LIKE t024e OCCURS 0 WITH HEADER LINE. "Einkaufsorganisation
DATA: xt024 LIKE t024 OCCURS 0 WITH HEADER LINE. "Einkäufergruppe
DATA: xt001w LIKE t001w OCCURS 0 WITH HEADER LINE. "Werke
DATA: xt023t LIKE t023t OCCURS 0 WITH HEADER LINE. "Warengruppe
DATA: xt16ft LIKE t16ft OCCURS 0 WITH HEADER LINE. "Freigabestrategie
DATA: xt16fm LIKE t16fm OCCURS 0 WITH HEADER LINE. "Freigabekennzeichen
DATA: xt001 LIKE t001 OCCURS 0 WITH HEADER LINE. "Buchungskreis

*Interne Tabelle für das Lieferdatum
DATA: BEGIN OF xeket OCCURS 20.             "Lieferdatum
        INCLUDE STRUCTURE eket.
DATA:   eindt_ext LIKE rm06e-eeind,
        lpein_ext LIKE rm06e-lpein.
DATA: END OF xeket.

*-- Felder Berechtigungspruefung---------------------------------------*
DATA: auth_doc    TYPE c,                     "Flag Berechtigung Beleg
      auth_price  TYPE c,                     "Flag Preisanzeigeberecht
      auth_change TYPE c,                     "Flag Aend.anzeigeberecht
      auth_accept TYPE c,                     "Flag Abnahmeberechtigung
      akt_anz    LIKE tact-actvt VALUE '03',  "Akt.typ Anzeigen
      akt_price  LIKE tact-actvt VALUE '09',  "Akt.typ Preisanzeige
      akt_change LIKE tact-actvt VALUE '08',  "Akt.typ Anz. Aenderung
      akt_accept LIKE tact-actvt VALUE '75'.  "Akt.typ Abnehmen


DATA: kzabn1 TYPE c, lock1 TYPE c,
      kzabn2 TYPE c, lock2 TYPE c.

********TR804-COG
DATA: es_variant    LIKE disvariant,
      is_variant    LIKE disvariant.
********TR804-COG

*************** Selektionsbildschirm *********************************
SELECTION-SCREEN BEGIN OF BLOCK ekko              "Bestelldaten
                 WITH FRAME TITLE text-s02.

SELECT-OPTIONS: s_ebeln FOR zessr_rel-ebeln
                        MATCHCODE OBJECT mekk,
                s_bedat FOR zessr_rel-bedat,
                s_bsart FOR ekko-bsart,
                s_lifnr FOR zessr_rel-lifnr
                        MATCHCODE OBJECT kred,
                s_ekorg FOR zessr_rel-ekorg,
                s_ekgrp FOR zessr_rel-ekgrp.
SELECT-OPTIONS: s_werks FOR zessr_rel-werks,
                s_matkl FOR zessr_rel-matkl,
                s_bednr FOR zessr_rel-bednr.                "TR804-COG
SELECTION-SCREEN END OF BLOCK ekko.

SELECTION-SCREEN BEGIN OF BLOCK essr              "Erfassungsblatt
                 WITH FRAME TITLE text-s04.
SELECT-OPTIONS: s_lblni FOR zessr_rel-lblni
                        MATCHCODE OBJECT essr,
                s_lblne FOR zessr_rel-lblne,
                s_erdat FOR zessr_rel-erdat,
                s_spec  FOR essr-spec_no
                                         MATCHCODE OBJECT spec,
                s_banfn FOR essr-banfn
                                       MATCHCODE OBJECT mban,
                s_warpl FOR essr-warpl,
                s_fknum FOR essr-fknum,
                s_konnr FOR ekko-konnr.

SELECTION-SCREEN SKIP 1.

********TR804-COG
SELECTION-SCREEN BEGIN OF BLOCK variant WITH FRAME TITLE text-038.
PARAMETERS p_varint   LIKE  disvariant-variant.   "Display Variant
SELECTION-SCREEN END OF BLOCK variant.
********TR804-COG


SELECTION-SCREEN BEGIN OF BLOCK lock WITH FRAME TITLE text-030.
PARAMETERS: p_lock_n RADIOBUTTON GROUP radi DEFAULT 'X'.
PARAMETERS: p_lock_j RADIOBUTTON GROUP radi.
PARAMETERS: p_lock_a RADIOBUTTON GROUP radi.
SELECTION-SCREEN END OF BLOCK lock.

SELECTION-SCREEN BEGIN OF BLOCK accept WITH FRAME TITLE text-028.
PARAMETERS: p_kzab_n RADIOBUTTON GROUP rad2.
PARAMETERS: p_kzab_j RADIOBUTTON GROUP rad2.
PARAMETERS: p_kzab_a RADIOBUTTON GROUP rad2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK accept.

SELECTION-SCREEN END OF BLOCK essr.


********TR804-COG
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varint.
  is_variant-report = sy-repid.
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
********TR804-COG



INITIALIZATION.

  CLEAR: g_icon_green_light,
         g_icon_yellow_light,
         g_icon_red_light,
         g_icon_final_entry,
         g_icon_locked.

* icon green light
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_green_light
      info                  = text-s62
    IMPORTING
      RESULT                = g_icon_green_light
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* icon yellow light
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_yellow_light
      add_stdinf            = 'X'
    IMPORTING
      RESULT                = g_icon_yellow_light
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* icon red light
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_red_light
      info                  = text-s61
    IMPORTING
      RESULT                = g_icon_red_light
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* icon final entry indicator
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_status_reverse
      info                  = text-037
    IMPORTING
      RESULT                = g_icon_final_entry
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* icon locked
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_locked
      info                  = text-s52
    IMPORTING
      RESULT                = g_icon_locked
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.









START-OF-SELECTION.

  x_keyinfo-header01 = 'EBELN'.
  x_keyinfo-item01 = 'EBELN'.
  x_keyinfo-header02 = 'EBELP'.
  x_keyinfo-item02 = 'EBELP'.
  CLEAR wa.
  CLEAR wb.


***********************************************************************
***                     Main Program                                ***
***********************************************************************


*..Moegliche Werte kzabn
  IF p_kzab_n EQ 'X'.
    kzabn1 = kzabn2 = space.
  ELSEIF p_kzab_j EQ 'X'.
    kzabn1 = kzabn2 = 'X'.
  ELSE.
    kzabn1 = space.
    kzabn2 = 'X'.
  ENDIF.
*..Moegliche Werte f_lock
  IF p_lock_n EQ 'X'.
    lock1 = lock2 = space.
  ELSEIF p_lock_j EQ 'X'.
    lock1 = lock2 = 'X'.
  ELSE.
    lock1 = space.
    lock2 = 'X'.
  ENDIF.

  SELECT * FROM zessr_rel INTO TABLE xdata
                       WHERE kzabn   IN (kzabn1, kzabn2)
                         AND f_lock  IN (lock1, lock2)
                         AND   pstyp EQ '9'
                         AND   bsart   IN s_bsart
                         AND   lifnr   IN s_lifnr
                         AND   ekorg   IN s_ekorg
                         AND   ekgrp   IN s_ekgrp
                         AND   bedat   IN s_bedat
                         AND   ebeln   IN s_ebeln
                         AND   werks   IN s_werks
                         AND   matkl   IN s_matkl
                         AND   lblni   IN s_lblni
                         AND   lblne   IN s_lblne
                         AND   erdat   IN s_erdat
                         AND   spec_no IN s_spec
                         AND   banfn_sh IN s_banfn
                         AND   warpl   IN s_warpl
                         AND   fknum   IN s_fknum
                         AND   loekz   EQ space
                         AND   loekz_p EQ space
                         AND   loekz_h EQ space
                         AND   bednr   IN s_bednr
                         AND   konnr   IN s_konnr.          "TR804-COG

  SORT xdata BY ebeln ebelp lblni.
  PERFORM authority_list.
*******************Übertrag von Headertabelle auf Itemtabelle***********
  PERFORM read_vendor.     "Lieferantenname in Headertabelle
  CLEAR datax.

  l_repid = sy-repid.

* ALV grid or ALV hierseq.?
  "GET PARAMETER ID 'ME_USE_GRID' FIELD l_value. "TR804-COG
  l_value = ''.                                             "TR804-COG
  IF l_value IS INITIAL.
    LOOP AT xdata INTO wa .
      PERFORM icon.
      PERFORM read_data.
      APPEND wa TO datax.
      IF wa-ebeln = wb-ebeln AND wa-ebelp = wb-ebelp.
        DELETE xdata.
      ENDIF.
      wb = wa.
      CLEAR wa.
    ENDLOOP. " at xdata

    PERFORM read_delivery_date. "Lieferdatum in Headertabelle
    PERFORM fields.       "Feldtabelle für das Listtool

***********************************************************************
************Listtool für hierachisch sequentielle Tabelle***************
************************************************************************
    ls_variant-report = l_repid.                            "TR804-COG
    ls_variant-variant = p_varint.                          "TR804-COG
    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
      EXPORTING
        i_callback_program      = l_repid
        i_callback_user_command = 'USER_COMMAND'
        it_fieldcat             = xfield
        i_save                  = 'A'
        i_tabname_header        = 'XDATA'
        i_tabname_item          = 'DATAX'
        is_keyinfo              = x_keyinfo
        is_variant              = ls_variant                "TR804-COG
      TABLES
        t_outtab_header         = xdata
        t_outtab_item           = datax
      EXCEPTIONS
*error_message           = 1
        program_error           = 1                         "1236632
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.                " ALV Grid

    PERFORM read_delivery_date. "Lieferdatum in Headertabelle

    REFRESH t_merep_outtab_srvdoc. CLEAR t_merep_outtab_srvdoc. "915967

    LOOP AT xdata INTO wa .
      PERFORM read_data.
      MOVE-CORRESPONDING wa TO t_merep_outtab_srvdoc.
      t_merep_outtab_srvdoc-persext = t_merep_outtab_srvdoc-brtwr.
      CONDENSE t_merep_outtab_srvdoc-persext.
      CONCATENATE
        t_merep_outtab_srvdoc-ebeln
        t_merep_outtab_srvdoc-ebelp
        t_merep_outtab_srvdoc-waers
        t_merep_outtab_srvdoc-persext
        INTO t_merep_outtab_srvdoc-persext
        SEPARATED BY space.
      MOVE wa-txz01_p TO t_merep_outtab_srvdoc-ktext1.
      PERFORM icon.         "Kennzeichen -> Icons
      MOVE-CORRESPONDING wa TO t_merep_outtab_srvdoc.
      APPEND t_merep_outtab_srvdoc.
    ENDLOOP. " at xdata

    PERFORM fields.       "Feldtabelle für das Listtool

    "ls_layout-zebra = 'X'.
    "ls_variant-handle = 'GRID'.
    ls_variant-report = l_repid.
    ls_variant-variant = p_varint.                          "TR804-COG

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = l_repid
        i_callback_pf_status_set = 'SET_PF_STATUS_ALV'
        i_callback_user_command  = 'USER_COMMAND_GRID'
        is_layout_lvc            = ls_layout
        it_fieldcat_lvc          = ofield[]
        i_save                   = 'A'
        is_variant               = ls_variant
      TABLES
        t_outtab                 = t_merep_outtab_srvdoc[]
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.


  EXIT.


***********************************************************************
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  FIELDS
*&---------------------------------------------------------------------*
*       Feldkatalog mit Feldbeschreibungen der Ausgabefelder aufbauen  *
*----------------------------------------------------------------------*
*  <->  XFIELD    Globaler Feldkatalog mit Feldbeschr. für Listausgabe
*----------------------------------------------------------------------*

FORM fields.

* ALV oder nicht ALV
  "GET PARAMETER ID 'ME_USE_GRID' FIELD l_value. "TR804-COG
  l_value = ''.                                             "TR804-COG
  IF l_value IS INITIAL.

    DATA: afield TYPE slis_fieldcat_alv.
    REFRESH xfield.

************************************************************************
********************Headerliste erste Zeile*****************************

* Bestellung
    CLEAR afield.
    afield-fieldname = 'EBELN'.
    afield-key = 'X'.
    afield-tabname = 'XDATA'.
    afield-reptext_ddic = text-001.
    afield-seltext_s = text-001.
    afield-ref_tabname = 'EKKO'.
    afield-hotspot = 'X'.
*  afield-emphasize = 'X'.
    afield-col_pos = '1'.
    afield-row_pos = '1'.
    APPEND afield TO xfield.

*Einkaufsorganisation
    CLEAR afield.
    afield-fieldname = 'EKORG'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKKO'.
    afield-col_pos = '2'.
    afield-row_pos = '1'.
    afield-reptext_ddic = text-003.
    APPEND afield TO xfield.


* Einkäufergruppe
    CLEAR afield.
    afield-fieldname = 'EKGRP'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKKO'.
    afield-row_pos = '1'.
    afield-col_pos = '3'.
    afield-outputlen = 6.
    afield-reptext_ddic = text-006.
    APPEND afield TO xfield.

* Lieferant
    CLEAR afield.
    afield-fieldname = 'LIFNR'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKKO'.
    afield-row_pos = '1'.
    afield-col_pos = '4'.
    afield-outputlen = 9.
*  afield-reptext_ddic = text-007.
    APPEND afield TO xfield.

* Lieferantenname
    CLEAR afield.
    afield-fieldname = 'NAME1'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'LFA1'.
    afield-row_pos = '1'.
    afield-col_pos = '5'.
    afield-outputlen = 40.
*  afield-reptext_ddic = text-008.
    APPEND afield TO xfield.

* Währung
    CLEAR afield.
    afield-fieldname = 'WAERS_H'.
    afield-just = 'C'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'ZESSR_REL'.
    afield-row_pos = '1'.
    afield-col_pos = '6'.
    afield-outputlen = 20.
    afield-reptext_ddic = text-005.
    APPEND afield TO xfield.

* Bestelldatum
    CLEAR afield.
    afield-fieldname = 'BEDAT'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKKO'.
    afield-row_pos = '1'.
    afield-col_pos = '7'.
    afield-reptext_ddic = text-009.
    APPEND afield TO xfield.

************************************************************************
**********************Headerliste zweite Zeile**************************

* Position
    CLEAR afield.
    afield-fieldname = 'EBELP'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKPO'.
    afield-outputlen = 10.
    afield-key = 'X'.
    afield-col_pos = '1'.
    afield-row_pos = '2'.
    afield-reptext_ddic = text-002.
    APPEND afield TO xfield.

* Werk
    CLEAR afield.
    afield-fieldname = 'WERKS'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKPO'.
*  afield-reptext_ddic = text-004.
    afield-outputlen = 5.
    afield-col_pos = '2'.
    afield-row_pos = '2'.
    APPEND afield TO xfield.

* Enderfassungskennzeichen
    CLEAR afield.
    afield-fieldname = 'ELIKZ'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKPO'.
    afield-row_pos = '2'.
    afield-outputlen = 6.
    afield-col_pos = '3'.
    afield-reptext_ddic = text-010.
    APPEND afield TO xfield.


* Warengruppe
    CLEAR afield.
    afield-fieldname = 'MATKL'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKPO'.
    afield-row_pos = '2'.
    afield-col_pos = '4'.
*  afield-reptext_ddic = text-011.
    APPEND afield TO xfield.

* Kurztext1
    CLEAR afield.
    afield-fieldname = 'TXZ01_P'.
    afield-ref_tabname = 'ZESSR_REL'.
    afield-outputlen = 40.
    afield-tabname = 'XDATA'.
    afield-row_pos = '2'.
*  afield-hotspot = 'X'.
    afield-col_pos = '5'.
    afield-reptext_ddic = text-012.
    APPEND afield TO xfield.

* Nettowert Bestellposition
*  clear afield.
    afield-fieldname = 'NETWR_P'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'ZESSR_REL'.
    afield-row_pos = '2'.
    afield-col_pos = '6'.
    afield-outputlen = 20.
    afield-cfieldname = 'WAERS_H'.                          "356903
    afield-ctabname = 'XDATA'.                              "356903
    afield-reptext_ddic = text-013.
    APPEND afield TO xfield.

* Lieferdatum
    CLEAR afield.
    afield-fieldname = 'EINDT_EXT'.
    afield-tabname = 'XDATA'.
    afield-ref_tabname = 'EKET'.
    afield-row_pos = '2'.
    afield-col_pos = '7'.
    afield-reptext_ddic = text-014.
    APPEND afield TO xfield.
************************************************************************
**********************Itemtabelle***************************************

*Erfassungsblatt
    CLEAR afield.
    afield-fieldname = 'LBLNI'.
    afield-ref_tabname = 'ESSR'.
    afield-tabname = 'DATAX'.
    afield-col_pos = '1'.
    afield-hotspot = 'X'.
    afield-key = 'X'.
    afield-reptext_ddic = text-015.
    APPEND afield TO xfield.

*Abnahmekennzeichen
    CLEAR afield.
    afield-fieldname = 'ABN_ICON'.
    afield-icon = 'X'.
    afield-ref_tabname = 'ESSR'.
    afield-tabname = 'DATAX'.
    afield-outputlen = 30.
    afield-col_pos = '2'.
    afield-reptext_ddic = text-016.
    APPEND afield TO xfield.

*Enderfassungskenzeichen
    CLEAR afield.
    afield-fieldname = 'ERF_ICON'.
    afield-icon = 'X'.
    afield-ref_tabname = 'ESSR'.
    afield-tabname = 'DATAX'.
    afield-col_pos = '3'.
    afield-outputlen = 30.
    afield-reptext_ddic = text-017.
    APPEND afield TO xfield.

*Sperrkennzeichen
    CLEAR afield.
    afield-fieldname = 'SPERR_ICON'.
    afield-icon = 'X'.
    afield-ref_tabname = 'ESSR'.
    afield-tabname = 'DATAX'.
    afield-col_pos = '4'.
    afield-outputlen = 30.
    afield-reptext_ddic = text-018.
    APPEND afield TO xfield.

* Kurztext2
    CLEAR afield.
    afield-fieldname = 'TXZ01'.
    afield-tabname = 'DATAX'.
    afield-ref_tabname = 'ESSR'.
    afield-col_pos = '5'.
    afield-reptext_ddic = text-019.
    APPEND afield TO xfield.

*Nettowert Erfassungsblatt
    CLEAR afield.
    afield-fieldname = 'NETWR'.
    afield-ref_tabname = 'ESSR'.
    afield-tabname = 'DATAX'.
    afield-col_pos = '6'.
    afield-outputlen = 20.
    afield-reptext_ddic = text-020.
    afield-seltext_s = text-020.
    afield-seltext_m = text-020.
    afield-seltext_l = text-020.
    afield-cfieldname = 'WAERS_H'.                          "356903
    afield-ctabname = 'DATAX'.                              "356903
    APPEND afield TO xfield.

*Erfassungsdatum
    CLEAR afield.
    afield-fieldname = 'ERDAT'.
    afield-ref_tabname = 'ESSR'.
    afield-tabname = 'DATAX'.
    afield-col_pos = '8'.
    afield-seltext_s = text-021.
    afield-reptext_ddic = text-021.
    APPEND afield TO xfield.

***********************************************************************
************ausgeblendete Felder Headertabelle**************************
************************************************************************

*Einkaufsbelegart
    CLEAR afield.
    afield-fieldname = 'BSART'.
    afield-ref_tabname = 'EKKO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Sachbearbeiter
    CLEAR afield.
    afield-fieldname = 'ERNAM_H'.
    afield-no_out = 'X'.
    afield-ref_tabname = 'ZESSR_REL'.
    afield-reptext_ddic = text-022.
    afield-seltext_s = text-023.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

* description of the purchasing organization
    CLEAR afield.
    afield-fieldname = 'EKOTX'.
    afield-no_out = 'X'.
    afield-ref_tabname = 'T024E'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Einkäufergruppe
    CLEAR afield.
    afield-fieldname = 'EKNAM'.
    afield-no_out = 'X'.
    afield-ref_tabname = 'T024'.
*  afield-reptext_ddic = text-022.
*  afield-seltext_s = text-023.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Menge
    CLEAR afield.
    afield-fieldname = 'MENGE'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Bestellmengeneinheit
    CLEAR afield.
    afield-fieldname = 'MEINS'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Bruttowert in Bestellwährung
    CLEAR afield.
    afield-fieldname = 'BRTWR'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    afield-cfieldname = 'WAERS_H'.
    afield-ctabname = 'XDATA'.
    APPEND afield TO xfield.

*Endrechnungskenzeichen
    CLEAR afield.
    afield-fieldname = 'EREKZ'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Positionstyp im Einkaufsbeleg
    CLEAR afield.
    afield-fieldname = 'PSTYP'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Kontierungstyp
    CLEAR afield.
    afield-fieldname = 'KNTTP'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*automatische Wareneingangsrechnung
    CLEAR afield.
    afield-fieldname = 'XERSY'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Nummer des übergeordneten Vertrages
    CLEAR afield.
    afield-fieldname = 'KONNR'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Positionsnummer des übergordneten Vertrages
    CLEAR afield.
    afield-fieldname = 'KTPNR'.
    afield-ref_tabname = 'EKPO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Buchungskreis
    CLEAR afield.
    afield-fieldname = 'BUKRS'.
    afield-ref_tabname = 'EKKO'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Texte zum Buchungskreis
    CLEAR afield.
    afield-fieldname = 'BUTXT'.
    afield-ref_tabname = 'T001'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Texte zum Werk
    CLEAR afield.
    afield-fieldname = 'WNAM'.
    afield-ref_tabname = ''.
    afield-reptext_ddic = text-026.
*  afield-seltext_s = text-023.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.

*Texte zur Warengruppe
    CLEAR afield.
    afield-fieldname = 'WGBEZ'.
    afield-ref_tabname = 'T023T'.
    afield-no_out = 'X'.
    afield-tabname = 'XDATA'.
    APPEND afield TO xfield.


***********************************************************************
************ausgeblendete Felder Itemtabelle**************************
************************************************************************

*Externe Erfassungsblattnr.
    CLEAR afield.
    afield-fieldname = 'LBLNE'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Sachbearbeiter der das Objekt hinzugefügt hat
    CLEAR afield.
    afield-fieldname = 'ERNAM'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Datum der letzte Änderung
    CLEAR afield.
    afield-fieldname = 'AEDAT'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Sachbearbeiter der das Objekt geändert hat
    CLEAR afield.
    afield-fieldname = 'AENAM'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Sachbearbeiter intern
    CLEAR afield.
    afield-fieldname = 'SBNAMAG'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Sachbearbeiter extern
    CLEAR afield.
    afield-fieldname = 'SBNAMAN'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Leistungsort
    CLEAR afield.
    afield-fieldname = 'DLORT'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Preisbezugsdatum des Erfassungsblattes
    CLEAR afield.
    afield-fieldname = 'LBLDT'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Zeitraum von
    CLEAR afield.
    afield-fieldname = 'LZVON'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Zeitraum bis
    CLEAR afield.
    afield-fieldname = 'LZBIS'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Wert der Leistungen
    CLEAR afield.
    afield-fieldname = 'LWERT'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    afield-cfieldname = 'WAERS_H'.                          "356903
    afield-ctabname = 'DATAX'.                              "356903
    APPEND afield TO xfield.

*Anteil aus ungeplanten Leistungen
    CLEAR afield.
    afield-fieldname = 'UWERT'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    afield-cfieldname = 'WAERS_H'.                          "356903
    afield-ctabname = 'DATAX'.                              "356903
    APPEND afield TO xfield.

*Löschkenzeichen im Erfassungsblatt
    CLEAR afield.
    afield-fieldname = 'LOEKZ'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Note für Qualität der Leistung
    CLEAR afield.
    afield-fieldname = 'PWWE'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Note für Termintreue der Leistung
    CLEAR afield.
    afield-fieldname = 'PWFR'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Bestellanforderungsnummer
    CLEAR afield.
    afield-fieldname = 'BANFN_SH'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-reptext_ddic = text-024.
    afield-seltext_s = text-025.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Wartungsplan
    CLEAR afield.
    afield-fieldname = 'WARPL'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Frachtkostennummer
    CLEAR afield.
    afield-fieldname = 'FKNUM'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Muster-LV-Nummer
    CLEAR afield.
    afield-fieldname = 'SPEC_NO'.
    afield-ref_tabname = 'ESSR'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

*Freigabestrategie
    CLEAR afield.
    afield-fieldname = 'FRGXT'.
    afield-ref_tabname = 'T16FT'.
    afield-no_out = 'X'.
    afield-tabname = 'DATAX'.
    APPEND afield TO xfield.

  ELSE.                                  "ALV GRID
* Bestellung
    CLEAR ofield.
    ofield-fieldname = 'EBELN'.
    ofield-key = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-reptext = text-001.
    ofield-seltext = text-001.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-hotspot = 'X'.
*  OFIELD-emphasize = 'X'.
    APPEND ofield TO ofield.

* Position
    CLEAR ofield.
    ofield-fieldname = 'EBELP'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 3.
    ofield-key = 'X'.
    ofield-reptext = text-002.
    APPEND ofield TO ofield.

*Erfassungsblatt
    CLEAR ofield.
    ofield-fieldname = 'LBLNI'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-hotspot = 'X'.
    ofield-key = 'X'.
    ofield-reptext = text-015.
    APPEND ofield TO ofield.

*Abnahmekennzeichen
    CLEAR ofield.
    ofield-fieldname = 'ABN_ICON'.
    ofield-icon = 'X'.
    ofield-ref_table = ''.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 30.
    ofield-reptext = text-016.
    ofield-tooltip = text-028.
    APPEND ofield TO ofield.

* Enderfassungskennzeichen
    CLEAR ofield.
    ofield-fieldname = 'ERF_ICON'.
    ofield-icon = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = ''.
    ofield-outputlen = 30.
    ofield-reptext = text-010.
    ofield-tooltip = text-029.
    APPEND ofield TO ofield.

*Sperrkennzeichen
    CLEAR ofield.
    ofield-fieldname = 'SPERR_ICON'.
    ofield-icon = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = ''.
    ofield-outputlen = 30.
    ofield-reptext = text-018.
    ofield-tooltip = text-030.
    APPEND ofield TO ofield.

* Kurztext1
    CLEAR ofield.
    ofield-fieldname = 'KTEXT1'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 20.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-reptext = text-012.
    APPEND ofield TO ofield.

* short description service entry sheet
    CLEAR ofield.
    ofield-fieldname = 'TXZ01'.
    ofield-outputlen = 20.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-reptext = text-019.
    ofield-tooltip = text-031.
    APPEND ofield TO ofield.

* Währung
    CLEAR ofield.
    ofield-fieldname = 'WAERS'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 7.
    ofield-reptext = text-005.
    APPEND ofield TO ofield.

* Nettowert Bestellposition
    CLEAR ofield.
    ofield-fieldname = 'BRTWR'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 12.
    ofield-cfieldname = 'WAERS'.
    ofield-reptext = text-013.
    APPEND ofield TO ofield.

*Nettowert Erfassungsblatt
    CLEAR ofield.
    ofield-fieldname = 'NETWR'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 12.
    ofield-reptext = text-020.
    ofield-cfieldname = 'WAERS'.
    APPEND ofield TO ofield.

* Lieferantenname
    CLEAR ofield.
    ofield-fieldname = 'NAME1'.
    ofield-reptext = text-027.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 20.
    APPEND ofield TO ofield.

*Einkaufsorganisation
    CLEAR ofield.
    ofield-fieldname = 'EKORG'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-reptext = text-003.
    ofield-tooltip = text-032.
    APPEND ofield TO ofield.


* Einkäufergruppe
    CLEAR ofield.
    ofield-fieldname = 'EKGRP'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 4.
    ofield-reptext = text-006.
    APPEND ofield TO ofield.

* Werk
    CLEAR ofield.
    ofield-fieldname = 'WERKS'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 4.
    APPEND ofield TO ofield.

* Warengruppe
    CLEAR ofield.
    ofield-fieldname = 'MATKL'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-outputlen = 4.
    APPEND ofield TO ofield.

* Bestelldatum
    CLEAR ofield.
    ofield-fieldname = 'BEDAT'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-reptext = text-009.
    APPEND ofield TO ofield.

* Lieferdatum
    CLEAR ofield.
    ofield-fieldname = 'EINDT_EXT'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = ''.
    ofield-reptext = text-014.
    APPEND ofield TO ofield.

*Erfassungsdatum
    CLEAR ofield.
    ofield-fieldname = 'ERDAT'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

***********************************************************************
************ausgeblendete Felder Headertabelle**************************
************************************************************************

*Einkaufsbelegart
    CLEAR ofield.
    ofield-fieldname = 'BSART'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

* description of the purchasing organization
    CLEAR ofield.
    ofield-fieldname = 'EKOTX'.
    ofield-no_out = 'X'.
    ofield-ref_table = 'T024E'.
    ofield-ref_field = 'EKOTX'.
    ofield-tooltip = text-033.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

* purchasing group
    CLEAR ofield.
    ofield-fieldname = 'EKNAM'.
    ofield-no_out = 'X'.
    ofield-ref_table = 'EKKO'.
    ofield-ref_field = 'EKGRP'.
    ofield-tooltip = text-038.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Menge
    CLEAR ofield.
    ofield-fieldname = 'MENGE'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-qfieldname = 'MEINS'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Bestellmengeneinheit
    CLEAR ofield.
    ofield-fieldname = 'MEINS'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Endrechnungskenzeichen
    CLEAR ofield.
    ofield-fieldname = 'EREKZ'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Positionstyp im Einkaufsbeleg
    CLEAR ofield.
    ofield-fieldname = 'PSTYP'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Kontierungstyp
    CLEAR ofield.
    ofield-fieldname = 'KNTTP'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*automatische Wareneingangsrechnung
    CLEAR ofield.
    ofield-fieldname = 'XERSY'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Nummer des übergeordneten Vertrages
    CLEAR ofield.
    ofield-fieldname = 'KONNR'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Positionsnummer des übergordneten Vertrages
    CLEAR ofield.
    ofield-fieldname = 'KTPNR'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    ofield-tooltip = text-034.
    APPEND ofield TO ofield.

* company code
    CLEAR ofield.
    ofield-fieldname = 'BUKRS'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

* description of the company code
    CLEAR ofield.
    ofield-fieldname = 'BUTXT'.
    ofield-ref_table = 'T001'.
    ofield-ref_field = 'BUTXT'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

* description of the plant
    CLEAR ofield.
    ofield-fieldname = 'WNAM'.
    ofield-ref_table = ''.
    ofield-reptext = text-026.
    ofield-tooltip = text-035.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

* description of the material group
    CLEAR ofield.
    ofield-fieldname = 'WGBEZ'.
    ofield-ref_table = 'EKPO'.
    ofield-ref_field = 'MATKL'.
    ofield-tooltip = text-036.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.


***********************************************************************
************ausgeblendete Felder Itemtabelle**************************
************************************************************************

*Sachbearbeiter der das Objekt hinzugefügt hat
    CLEAR ofield.
    ofield-fieldname = 'ERNAM'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Datum der letzte Änderung
    CLEAR ofield.
    ofield-fieldname = 'AEDAT'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Preisbezugsdatum des Erfassungsblattes
    CLEAR ofield.
    ofield-fieldname = 'LBLDT'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Löschkenzeichen im Erfassungsblatt
    CLEAR ofield.
    ofield-fieldname = 'LOEKZ'.
    ofield-ref_table = 'MEREP_OUTTAB_SRVDOC'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

*Ferigabestrategie
    CLEAR ofield.
    ofield-fieldname = 'FRGXT'.
    ofield-ref_table = 'ESSR'.
    ofield-ref_field = 'FRGSX'.
    ofield-no_out = 'X'.
    ofield-tabname = 'T_MEREP_OUTTAB_SRVDOC'.
    APPEND ofield TO ofield.

  ENDIF.                  "ALV Grid
ENDFORM.                               " FIELDS

*&--------------------------------------------------------------------*
*&      Form  READ_VENDOR
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM read_vendor.
*----------------------------------------------------------------------*
*                                                    *
*----------------------------------------------------------------------*
*    Interne Tabellen mit Textelementen anlegen
*----------------------------------------------------------------------*
*Lieferant
  REFRESH xlfa1.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM lfa1 INTO TABLE xlfa1
             FOR ALL ENTRIES IN xdata
             WHERE lifnr EQ xdata-lifnr
             ORDER BY PRIMARY KEY.
  ENDIF.
*Einkaufsorganisation
  REFRESH xt024e.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM t024e INTO TABLE xt024e
             FOR ALL ENTRIES IN xdata
             WHERE ekorg EQ xdata-ekorg
             ORDER BY PRIMARY KEY.
  ENDIF.

*Einkäufergruppe
  REFRESH xt024.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM t024 INTO TABLE xt024
             FOR ALL ENTRIES IN xdata
             WHERE ekgrp EQ xdata-ekgrp
             ORDER BY PRIMARY KEY.
  ENDIF.

*Werk
  REFRESH xt001w.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM t001w INTO TABLE xt001w
             FOR ALL ENTRIES IN xdata
             WHERE werks EQ xdata-werks
             ORDER BY PRIMARY KEY.
  ENDIF.

*Warengruppe
  REFRESH xt023t.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM t023t INTO TABLE xt023t
             FOR ALL ENTRIES IN xdata
             WHERE matkl EQ xdata-matkl
             AND spras EQ sy-langu
             ORDER BY PRIMARY KEY.
  ENDIF.

*Freigabestrategie
  REFRESH xt16ft.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM t16ft INTO TABLE xt16ft
             FOR ALL ENTRIES IN xdata
             WHERE frgsx EQ xdata-frgsx
             AND spras EQ sy-langu
             ORDER BY PRIMARY KEY.
  ENDIF.

*Freigabekennzeichen
*  refresh xt16fm.

*  describe table xdata lines sy-tfill.
*  if sy-tfill > 0.
*    select * from t16fm into table xt16fm
*             for all entries in xdata
*             where frgkl eq xdata-frgkl
*             and spras eq sy-langu
*             order by primary key.
*  endif.

*Buchungskreis
  REFRESH xt001.

  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM t001 INTO TABLE xt001
             FOR ALL ENTRIES IN xdata
             WHERE bukrs EQ xdata-bukrs
             ORDER BY PRIMARY KEY.
  ENDIF.
ENDFORM.                    "READ_VENDOR

***********************************************************************
* Daten einfügen in xdata
***********************************************************************
FORM read_data.

  READ TABLE xlfa1
       WITH KEY lifnr = wa-lifnr
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-name1 = xlfa1-name1.
  ENDIF.


  READ TABLE xt024e
       WITH KEY ekorg = wa-ekorg
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-ekotx = xt024e-ekotx.
  ENDIF.


  READ TABLE xt024
       WITH KEY ekgrp = wa-ekgrp
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-eknam = xt024-eknam.
  ENDIF.


  READ TABLE xt001w
       WITH KEY werks = wa-werks
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-wnam = xt001w-name1.
  ENDIF.


  READ TABLE xt023t
       WITH KEY matkl = wa-matkl
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-wgbez = xt023t-wgbez.
  ENDIF.


  READ TABLE xt16ft
       WITH KEY frgsx = wa-frgsx
       frggr = wa-frggr
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-frgxt = xt16ft-frgxt.
  ENDIF.

  READ TABLE xt001
       WITH KEY bukrs = wa-bukrs
       BINARY SEARCH.
  IF sy-subrc = 0.
    wa-butxt = xt001-butxt.
  ENDIF.

*    wa-name1 = xlfa1-name1.
*    wa-ekotx = xt024e-ekotx.
*    wa-eknam = xt024-eknam.
*    wa-wnam = xt001w-name1.
*    wa-wgbez = xt023t-wgbez.
*    wa-frgxt = xt16ft-frgxt.
*    wa-frget = xt16fm-frget.
  MODIFY xdata FROM wa.

ENDFORM.                    "READ_DATA

*&--------------------------------------------------------------------*
*&      Form  READ_DELIVERY_DATE
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM read_delivery_date.
*----------------------------------------------------------------------*
*       FORM READ_DELIVERY_DATE                                        *
*----------------------------------------------------------------------*
*       Lieferdatum lesen                                              *
*----------------------------------------------------------------------*
  REFRESH xeket.

*..Lieferdatum lesen
  DESCRIBE TABLE xdata LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM eket INTO TABLE xeket
             FOR ALL ENTRIES IN xdata
             WHERE ebeln EQ xdata-ebeln
             AND   ebelp EQ xdata-ebelp
             ORDER BY PRIMARY KEY.
  ENDIF.

*..Lieferdatum zur Ausgabe aufbereiten
  LOOP AT xeket.
    CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
      EXPORTING
        internal_date   = xeket-eindt
        internal_period = xeket-lpein
      IMPORTING
        external_date   = xeket-eindt_ext
        external_period = xeket-lpein_ext
      EXCEPTIONS
        date_invalid    = 01
        periode_invalid = 02.
    IF sy-subrc EQ 0.
      MODIFY xeket.
    ENDIF.
  ENDLOOP.

*..Lieferdatum an xdata uebergeben
  LOOP AT xdata.
    IF xeket-ebeln NE xdata-ebeln OR
       xeket-ebelp NE xdata-ebelp.
      READ TABLE xeket
           WITH KEY ebeln = xdata-ebeln
                    ebelp = xdata-ebelp
           BINARY SEARCH.
    ENDIF.
    xdata-lpein     = xeket-lpein.
    xdata-lpein_ext = xeket-lpein_ext.
    xdata-eindt_ext = xeket-eindt_ext.
    MODIFY xdata.
  ENDLOOP.

ENDFORM.                    "READ_DELIVERY_DATE
************************************************************************
*****************Icons in interne Tabelle*******************************
************************************************************************
FORM icon.
  CASE wa-kzabn.
    WHEN 'X'.
      wa-abn_icon = g_icon_green_light.
    WHEN ' '.
      wa-abn_icon = g_icon_red_light.
    WHEN OTHERS.
      wa-abn_icon = g_icon_yellow_light.
  ENDCASE.
  IF wa-final EQ 'X'.
    wa-erf_icon = g_icon_final_entry.
  ENDIF.

  CASE wa-f_lock.
    WHEN 'X'.
      wa-sperr_icon = g_icon_locked.
    WHEN OTHERS.
      wa-sperr_icon = ''.
  ENDCASE.
ENDFORM.                    "ICON


*&--------------------------------------------------------------------*
*&      Form  authority_list
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM authority_list.
*----------------------------------------------------------------------*
*       FORM AUTHORITY_LIST                                            *
*----------------------------------------------------------------------*
*       Berechtigungspruefung Listdaten                                *
*----------------------------------------------------------------------*
  LOOP AT xdata.
    PERFORM authority_po_header.
    PERFORM authority_po_item.
    IF auth_doc EQ space.
      DELETE xdata.
    ELSEIF auth_price EQ space.
      xdata-no_price_auth = 'X'.
      MODIFY xdata.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "AUTHORITY_LIST

*&--------------------------------------------------------------------*
*&      Form  AUTHORITY_RELEASE
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM authority_release.
*----------------------------------------------------------------------*
*       FORM AUTHORITY_RELEASE                                         *
*----------------------------------------------------------------------*
*       Berechtigungspruefung Freigabe                                 *
*----------------------------------------------------------------------*
  LOOP AT xdata.
    PERFORM authority_po_header.
    PERFORM authority_po_item.
    IF auth_price EQ space.
      xdata-no_price_auth = 'X'.
    ENDIF.
    IF auth_accept EQ space.
      xdata-no_accept_auth = 'X'.
    ENDIF.
    IF auth_price EQ space OR
       auth_accept EQ space.
      MODIFY xdata.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "AUTHORITY_RELEASE

*&--------------------------------------------------------------------*
*&      Form  AUTHORITY_PO_HEADER
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM authority_po_header.
*----------------------------------------------------------------------*
*       FORM AUTHORITY_PO_HEADER                                       *
*----------------------------------------------------------------------*
*       Pruefen Anzeigeberechtigungen Bestellkopf                      *
*----------------------------------------------------------------------*
  DATA: xobjekt(10) TYPE c VALUE 'M_BEST_'.         "Bestellung

  auth_doc    = 'X'.
  auth_price  = 'X'.
  auth_change = 'X'.
  auth_accept = 'X'.

*..Einkaufsorganisation
  xobjekt+7(3) = 'EKO'.

  AUTHORITY-CHECK OBJECT xobjekt                  "Beleg anzeigen
       ID 'ACTVT' FIELD akt_price
       ID 'EKORG' FIELD xdata-ekorg.
  IF sy-subrc NE 0.
    CLEAR: auth_doc.
  ENDIF.

  AUTHORITY-CHECK OBJECT xobjekt                 "Preis anzeigen
       ID 'ACTVT' FIELD akt_price
       ID 'EKORG' FIELD xdata-ekorg.
  IF sy-subrc NE 0.
    CLEAR auth_price.
  ENDIF.

  AUTHORITY-CHECK OBJECT xobjekt                 "Aenderungen anzeigen
      ID 'ACTVT' FIELD akt_change
      ID 'EKORG' FIELD xdata-ekorg.
  IF sy-subrc NE 0.
    CLEAR auth_change.
  ENDIF.

  AUTHORITY-CHECK OBJECT xobjekt                 "Abnehmen
      ID 'ACTVT' FIELD akt_accept
      ID 'EKORG' FIELD xdata-ekorg.
  IF sy-subrc NE 0.
    CLEAR auth_accept.
  ENDIF.

*..Einkäufergruppe
  xobjekt+7(3) = 'EKG'.

  IF auth_doc EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                "Beleg anzeigen
         ID 'ACTVT' FIELD akt_price
         ID 'EKGRP' FIELD xdata-ekgrp.
    IF sy-subrc NE 0.
      CLEAR: auth_doc.
    ENDIF.
  ENDIF.
  IF auth_price EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                "Preis anzeigen
         ID 'ACTVT' FIELD akt_price
         ID 'EKGRP' FIELD xdata-ekgrp.
    IF sy-subrc NE 0.
      CLEAR auth_price.
    ENDIF.
  ENDIF.
  IF auth_change EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                "Aenderungen anzeigen
        ID 'ACTVT' FIELD akt_change
        ID 'EKGRP' FIELD xdata-ekgrp.
    IF sy-subrc NE 0.
      CLEAR auth_change.
    ENDIF.
  ENDIF.
  IF auth_accept EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                "Abnehmen
        ID 'ACTVT' FIELD akt_accept
        ID 'EKGRP' FIELD xdata-ekgrp.
    IF sy-subrc NE 0.
      CLEAR auth_accept.
    ENDIF.
  ENDIF.

*..Belegart
  xobjekt+7(3) = 'BSA'.

  IF auth_doc EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt               "Beleg anzeigen
         ID 'ACTVT' FIELD akt_price
         ID 'BSART' FIELD xdata-bsart.
    IF sy-subrc NE 0.
      CLEAR: auth_doc.
    ENDIF.
  ENDIF.
  IF auth_price EQ 'X'.                          "Preis anzeigen
    AUTHORITY-CHECK OBJECT xobjekt
         ID 'ACTVT' FIELD akt_price
         ID 'BSART' FIELD xdata-bsart.
    IF sy-subrc NE 0.
      CLEAR auth_price.
    ENDIF.
  ENDIF.
  IF auth_change EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt               "Aenderungen anzeigen
        ID 'ACTVT' FIELD akt_change
        ID 'BSART' FIELD xdata-bsart.
    IF sy-subrc NE 0.
      CLEAR auth_change.
    ENDIF.
  ENDIF.
  IF auth_accept EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                "Abnehmen
        ID 'ACTVT' FIELD akt_accept
        ID 'BSART' FIELD xdata-bsart.
    IF sy-subrc NE 0.
      CLEAR auth_accept.
    ENDIF.
  ENDIF.

ENDFORM.                    "AUTHORITY_PO_HEADER

*&--------------------------------------------------------------------*
*&      Form  AUTHORITY_PO_ITEM
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM authority_po_item.
*----------------------------------------------------------------------*
*       FORM AUTHORITY_PO_ITEM                                         *
*----------------------------------------------------------------------*
*       Pruefen Anzeigeberechtigungen Bestellposition                  *
*----------------------------------------------------------------------*
  DATA: xobjekt(10) TYPE c VALUE 'M_BEST_'.         "Bestellung

*..Werk
  xobjekt+7(3) = 'WRK'.

  IF auth_doc EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                 "Beleg anzeigen
         ID 'ACTVT' FIELD akt_anz
         ID 'WERKS' FIELD xdata-werks.
    IF sy-subrc NE 0.
      CLEAR: auth_doc.
    ENDIF.
  ENDIF.

  IF auth_accept EQ 'X'.
    AUTHORITY-CHECK OBJECT xobjekt                  "Abnehmen
        ID 'ACTVT' FIELD akt_accept
        ID 'WERKS' FIELD xdata-werks.
    IF sy-subrc NE 0.
      CLEAR auth_accept.
    ENDIF.
  ENDIF.

ENDFORM.                    "AUTHORITY_PO_ITEM
**********************************************************************
*************           user command                      ************
**********************************************************************
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CLEAR xdata-ebeln.
  IF r_ucomm = '&IC1'.
    IF rs_selfield-tabindex NE 0.
      READ TABLE xdata INDEX rs_selfield-tabindex.
      READ TABLE datax INDEX rs_selfield-tabindex.
      CASE rs_selfield-sel_tab_field.
        WHEN 'XDATA-EBELN'.
*           SET PARAMETER ID 'BES' FIELD XDATA-EBELN.
*           CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
          CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
            EXPORTING
              i_ebeln      = xdata-ebeln
              i_enjoy      = 'X'
            EXCEPTIONS
              not_found    = 1
              no_authority = 2
              invalid_call = 3
              OTHERS       = 4.
        WHEN 'DATAX-LBLNI'.
          SET PARAMETER ID 'BES' FIELD datax-ebeln.
          SET PARAMETER ID 'LBL' FIELD datax-lblni.
          CALL TRANSACTION 'ML81' AND SKIP FIRST SCREEN.    "TR804-COG
*          CALL TRANSACTION 'ML81N'. "TR804-COG
      ENDCASE.
    ENDIF.
  ENDIF.
*  endif.
  CLEAR xdata-ebeln.
ENDFORM.                               " SELSAVE
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_pf_status_alv  USING table TYPE slis_t_extab.
  SET PF-STATUS 'GRID_LISTE'.
ENDFORM.                    " SET_PF_STATUS_ALV
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_grid USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CLEAR t_merep_outtab_srvdoc-ebeln.
  IF r_ucomm = '&IC1'.
    IF rs_selfield-tabindex NE 0.
      READ TABLE t_merep_outtab_srvdoc INDEX rs_selfield-tabindex.
      CASE rs_selfield-sel_tab_field.
        WHEN 'T_MEREP_OUTTAB_SRVDOC-EBELN'.
          CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
            EXPORTING
              i_ebeln      = t_merep_outtab_srvdoc-ebeln
              i_enjoy      = 'X'
            EXCEPTIONS
              not_found    = 1
              no_authority = 2
              invalid_call = 3
              OTHERS       = 4.
        WHEN 'T_MEREP_OUTTAB_SRVDOC-LBLNI'.
          SET PARAMETER ID 'BES' FIELD datax-ebeln.
          SET PARAMETER ID 'LBL' FIELD datax-lblni.
          CALL TRANSACTION 'ML81' AND SKIP FIRST SCREEN.    "TR804-COG
*          CALL TRANSACTION 'ML81N'. "TR804-COG
      ENDCASE.
    ENDIF.
  ENDIF.
  CLEAR t_merep_outtab_srvdoc-ebeln.

ENDFORM.                    " USER_COMMAND_GRID
