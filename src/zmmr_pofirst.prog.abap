*&---------------------------------------------------------------------*
*& Report  ZMMR_POFIRST
*&---------------------------------------------------------------------*
*&  ZPOFIRST - Non PO Invoices Reference report
*&---------------------------------------------------------------------*

REPORT  zmmr_pofirst.
*&---------------------------------------------------------------------*
*& Program Name       :  ZMMR_POFIRST                                  *
*& Author             :  Santosh Kapase                                *
*& Creation Date      :  27-Feb-2015                                   *
*& Object ID          :  SDP-81515                                     *
*& Application Area   :  SC-MM-PUR                                     *
*& Description        :  Non PO Invoices Reference report              *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :  1.0 (Initial Version)                               *
* Date          :  23-Feb-2015                                         *
* Modified By   :  Santosh Kapase                                      *
* Correction No :  D30K925388                                          *
* Description   :  Non PO Invoices Reference report - Init Version     *                                                    *
*----------------------------------------------------------------------*
* Version No    :  2.0                                                 *
* Date          :  29-Jun-2015                                         *
* Modified By   :  Praveena Anusuri                                    *
* Correction No :  SDP87312                                            *
* Description   :  Changed Document Header text default search term from
*                  "PO First" to "Invoice First".                      *
*----------------------------------------------------------------------*

DATA: gwa_rbkp  TYPE rbkp,
      gv_afnam  TYPE ekpo-afnam.
*----------------------------------------------------------------------*
*                 T Y P E S   D E C L A R A T I O N S                  *
*----------------------------------------------------------------------*
" Program logic.
TYPES: BEGIN OF ty_rbkp,
        belnr   TYPE rbkp-belnr,
         gjahr  TYPE rbkp-gjahr,
         blart  TYPE rbkp-blart,
         bldat  TYPE rbkp-bldat,
         budat  TYPE rbkp-budat,
         xblnr  TYPE rbkp-xblnr,
         bukrs  TYPE rbkp-bukrs,
         lifnr  TYPE rbkp-lifnr,
         waers  TYPE rbkp-waers,
         bktxt  TYPE rbkp-bktxt,
         sgtxt  TYPE rbkp-sgtxt,
      END OF ty_rbkp,

      BEGIN OF ty_rseg,
        belnr     TYPE rseg-belnr,
        gjahr     TYPE rseg-gjahr,
        buzei     TYPE rseg-buzei,
        ebeln     TYPE rseg-ebeln,
        ebelp     TYPE rseg-ebelp,
        zekkn     TYPE rseg-zekkn,
        matnr     TYPE rseg-matnr,
        werks     TYPE rseg-werks,
        wrbtr     TYPE rseg-wrbtr,
        shkzg     TYPE rseg-shkzg,
        lfbnr     TYPE rseg-lfbnr,
        lfgja     TYPE rseg-lfgja,
        lfpos     TYPE rseg-lfpos,
      END OF ty_rseg,

      BEGIN OF ty_ekbe,
        ebeln TYPE ekbe-ebeln,
        ebelp TYPE ekbe-ebelp,
        zekkn TYPE ekbe-zekkn,
        vgabe TYPE ekbe-vgabe,
        gjahr TYPE ekbe-gjahr,
        belnr TYPE ekbe-belnr,
        buzei TYPE ekbe-buzei,
        xblnr TYPE ekbe-xblnr,
        ernam TYPE ekbe-ernam,
      END OF ty_ekbe,

*      BEGIN OF ty_mkpf,
*        mblnr     TYPE mkpf-mblnr,
*        mjahr     TYPE mkpf-mjahr,
*        usnam     TYPE mkpf-usnam,
*      END OF ty_mkpf,

      BEGIN OF ty_po_item,
        ebeln               TYPE ekko-ebeln,
        bukrs               TYPE ekko-bukrs,
        lifnr               TYPE ekko-lifnr,
        bedat               TYPE ekko-bedat,
        ekgrp               TYPE ekko-ekgrp,
        zzariba_approver    TYPE ekko-zzariba_approver,
        ebelp               TYPE ekpo-ebelp,
        matnr               TYPE ekpo-matnr,
        txz01               TYPE ekpo-txz01,
        bednr               TYPE ekpo-bednr,    "(+)DECK914368
        afnam               TYPE ekpo-afnam,
      END OF ty_po_item,

      BEGIN OF ty_lfa1,
        lifnr   TYPE lfa1-lifnr,
        name1   TYPE lfa1-name1,
      END OF ty_lfa1,

      BEGIN OF ty_bkpf,
        bukrs   TYPE bkpf-bukrs,
        belnr   TYPE bkpf-belnr,
        gjahr   TYPE bkpf-gjahr,
        blart   TYPE bkpf-blart,
        bstat   TYPE bkpf-bstat,
        xblnr   TYPE bkpf-xblnr,
      END OF ty_bkpf,

      BEGIN OF ty_output,
        bukrs     TYPE rbkp-bukrs,
        lifnr     TYPE rbkp-lifnr,
        name1     TYPE lfa1-name1,
        orig_inv  TYPE string,
        status    TYPE string,
        xblnr     TYPE rbkp-xblnr,
        ebeln     TYPE ekpo-ebeln,
        ebelp     TYPE ekpo-ebelp,
        bedat     TYPE ekko-bedat,
        txz01     TYPE ekpo-txz01,
        bednr     TYPE ekpo-bednr,        "(+)DECK914368
        ekgrp     TYPE ekko-ekgrp,
        belnr     TYPE rseg-belnr,
        gjahr     TYPE rseg-gjahr,
        buzei     TYPE rseg-buzei,
        budat     TYPE rbkp-budat,
        wrbtr     TYPE rseg-wrbtr,
        waers     TYPE rbkp-waers,
        bktxt     TYPE rbkp-bktxt,
        afnam     TYPE ekpo-afnam,
        zzariba_approver    TYPE ekko-zzariba_approver,
        gr_ernam  TYPE mkpf-usnam,
        gr_mblnr  TYPE mkpf-mblnr,
        sgtxt     TYPE rbkp-sgtxt,
      END OF ty_output.

*----------------------------------------------------------------------*
*        I N T E R N A L   T A B L E    D E C L A R A T I O N S        *
*----------------------------------------------------------------------*

DATA: it_rbkp       TYPE STANDARD TABLE OF ty_rbkp,         "#EC NEEDED
      it_rseg       TYPE STANDARD TABLE OF ty_rseg,         "#EC NEEDED
      it_po_item    TYPE STANDARD TABLE OF ty_po_item,      "#EC NEEDED
*      it_mkpf       TYPE STANDARD TABLE OF ty_mkpf,         "#EC NEEDED
      it_ekbe       TYPE STANDARD TABLE OF ty_ekbe,         "#EC NEEDED
      it_lfa1       TYPE STANDARD TABLE OF ty_lfa1,         "#EC NEEDED
      it_bkpf       TYPE STANDARD TABLE OF ty_bkpf,         "#EC NEEDED

      it_output     TYPE STANDARD TABLE OF ty_output.       "#EC NEEDED

*----------------------------------------------------------------------*
*        S E L E C T I O N - S C R E E N    D E F I N I T I O N        *
*----------------------------------------------------------------------*
"selection Screen Design.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_bukrs FOR gwa_rbkp-bukrs,
                s_afnam FOR gv_afnam OBLIGATORY,
                s_budat FOR gwa_rbkp-budat OBLIGATORY.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
SELECT-OPTIONS:
                s_bktxt FOR gwa_rbkp-bktxt MODIF ID m1
*                    DEFAULT '*PO First*'  OPTION CP SIGN I      "(-)PANUSURI Ticket 87312  "#EC NOTEXT
                     DEFAULT '*Invoice First*'  OPTION CP SIGN I "(-)PANUSURI Ticket 87312
                          NO INTERVALS NO-EXTENSION,
                s_blart  FOR gwa_rbkp-blart DEFAULT 'RE'
                      OBLIGATORY NO INTERVALS ,
                s_blart1 FOR gwa_rbkp-blart DEFAULT 'KR'
                      OBLIGATORY NO INTERVALS  .
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*   A T   S E L E C T I O N - S C R E E N    O U T P U T   E V E N T   *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
*- Modify selection screen
  PERFORM f_modify_selscr.

*----------------------------------------------------------------------*
*          S T A R T - O F - S E L E C T I O N     E V E N T           *
*----------------------------------------------------------------------*

START-OF-SELECTION.
*-- Get Data from the database
  PERFORM f_get_data_from_db.
*--- Consolidate data and build final table
  PERFORM f_consolidate_data.

*----------------------------------------------------------------------*
*           E  N D - O F - S E L E C T I O N     E V E N T             *
*----------------------------------------------------------------------*

END-OF-SELECTION.
*- Display Report Output in ALV.
  PERFORM f_display_salv.

*&---------------------------------------------------------------------*
*&      Form  F_CONSOLIDATE_DATA
*&---------------------------------------------------------------------*
* Consolidate data fetched from DB tbls into a final internal table
*----------------------------------------------------------------------*
FORM f_consolidate_data .

  DATA: lv_index  TYPE sy-tabix,
        lv_lines  TYPE i,
        lv_clines TYPE char22,
        lv_text   TYPE char100.

  FIELD-SYMBOLS: <lfs_rbkp>     LIKE LINE OF it_rbkp,
                 <lfs_rseg>     LIKE LINE OF it_rseg,
                 <lfs_po_item>  LIKE LINE OF it_po_item,
*                 <lfs_mkpf>     LIKE LINE OF it_mkpf,
                 <lfs_bkpf>     LIKE LINE OF it_bkpf,
                 <lfs_ekbe>     LIKE LINE OF it_ekbe,
                 <lfs_lfa1>     LIKE LINE OF it_lfa1,

                 <lfs_output>   LIKE LINE OF it_output.

  IF it_po_item[] IS NOT INITIAL.
    SORT: it_rbkp BY lifnr xblnr belnr gjahr,
          it_rseg BY belnr gjahr buzei,
          it_po_item BY ebeln ebelp,
          it_lfa1    BY lifnr.
    PERFORM f_display_progress_bar USING 60
                                         'Consolidating Data...'(002).

    LOOP AT it_rbkp ASSIGNING <lfs_rbkp>.
      READ TABLE it_rseg TRANSPORTING NO FIELDS WITH KEY belnr = <lfs_rbkp>-belnr
                                                         gjahr = <lfs_rbkp>-gjahr
                                                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
        LOOP AT it_rseg ASSIGNING <lfs_rseg> FROM lv_index.
          IF <lfs_rbkp>-belnr EQ <lfs_rseg>-belnr AND
             <lfs_rbkp>-gjahr EQ <lfs_rseg>-gjahr.
            READ TABLE it_po_item ASSIGNING <lfs_po_item>
                                  WITH KEY ebeln = <lfs_rseg>-ebeln
                                           ebelp = <lfs_rseg>-ebelp
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
              APPEND INITIAL LINE TO it_output ASSIGNING <lfs_output>.
              lv_lines = lv_lines + 1.
              lv_clines = lv_lines.
              CONDENSE lv_clines.
              CONCATENATE 'Building ALV table...'(007)
                          lv_clines
                          'records'(008)
                     INTO lv_text SEPARATED BY space.
              PERFORM f_display_progress_bar USING 80
                                                   lv_text.

              <lfs_output>-bukrs = <lfs_rbkp>-bukrs.
              <lfs_output>-lifnr = <lfs_rbkp>-lifnr.
              READ TABLE it_lfa1 ASSIGNING <lfs_lfa1> WITH KEY lifnr = <lfs_rbkp>-lifnr
                                                            BINARY SEARCH.
              IF sy-subrc EQ 0.
                <lfs_output>-name1 = <lfs_lfa1>-name1.
              ENDIF.
              <lfs_output>-xblnr  = <lfs_rbkp>-xblnr.

              <lfs_output>-ebeln  = <lfs_po_item>-ebeln.
              <lfs_output>-ebelp  = <lfs_po_item>-ebelp.
              <lfs_output>-bedat  = <lfs_po_item>-bedat.
              <lfs_output>-txz01  = <lfs_po_item>-txz01.
              <lfs_output>-bednr  = <lfs_po_item>-bednr.      "(+)DECK914368
              <lfs_output>-ekgrp  = <lfs_po_item>-ekgrp.

              <lfs_output>-belnr = <lfs_rseg>-belnr.
              <lfs_output>-gjahr = <lfs_rseg>-gjahr.
              <lfs_output>-buzei = <lfs_rseg>-buzei.
              <lfs_output>-budat = <lfs_rbkp>-budat.
              IF <lfs_rseg>-shkzg = 'H'.
                <lfs_output>-wrbtr = <lfs_rseg>-wrbtr * -1.
              ELSE.
                <lfs_output>-wrbtr = <lfs_rseg>-wrbtr.
              ENDIF.
              <lfs_output>-waers = <lfs_rbkp>-waers.

              <lfs_output>-bktxt = <lfs_rbkp>-bktxt.
              <lfs_output>-afnam = <lfs_po_item>-afnam.
              <lfs_output>-zzariba_approver = <lfs_po_item>-zzariba_approver.

              READ TABLE it_bkpf ASSIGNING <lfs_bkpf> WITH KEY bukrs = <lfs_rbkp>-bukrs
                                                               xblnr = <lfs_rbkp>-xblnr
                                                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                CONCATENATE <lfs_bkpf>-bukrs
                            <lfs_bkpf>-belnr
                            <lfs_bkpf>-gjahr
                       INTO <lfs_output>-orig_inv SEPARATED BY '/'.
                IF <lfs_bkpf>-bstat = 'Z'.
                  <lfs_output>-status = 'Deleted'(009).
                ELSEIF <lfs_bkpf>-bstat = 'V'.
                  <lfs_output>-status = 'Parked'(010).
                ENDIF.
              ENDIF.

*              IF <lfs_rseg>-lfbnr IS NOT INITIAL.
*                READ TABLE it_mkpf ASSIGNING <lfs_mkpf> WITH KEY mblnr = <lfs_rseg>-lfbnr
*                                                                 mjahr = <lfs_rseg>-lfgja
*                                                                          BINARY SEARCH.
              READ TABLE it_ekbe ASSIGNING <lfs_ekbe> WITH KEY ebeln = <lfs_rseg>-ebeln
                                                               ebelp = <lfs_rseg>-ebelp
*                                                               zekkn = <lfs_rseg>-zekkn
                                                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                <lfs_output>-gr_ernam   = <lfs_ekbe>-ernam.
                <lfs_output>-gr_mblnr   = <lfs_ekbe>-belnr.
              ENDIF.
*              ENDIF.
              <lfs_output>-sgtxt = <lfs_rbkp>-sgtxt.
            ENDIF.
          ELSE.
            CLEAR: lv_index.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " F_CONSOLIDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_FROM_DB
*&---------------------------------------------------------------------*
*  Fetch data from the DB tables for the inputted data
*----------------------------------------------------------------------*
FORM f_get_data_from_db .

  DATA:
          lit_rseg      TYPE STANDARD TABLE OF ty_rseg,
          lit_rbkp      TYPE STANDARD TABLE OF ty_rbkp.


  CLEAR: it_rbkp[],
         it_rseg[],
         it_output[],
         it_po_item[],
         it_lfa1[],
         it_ekbe[].
*         it_mkpf[].

  PERFORM f_display_progress_bar USING 10
                                       'Fetching Invoice Details...'(003).


  SELECT belnr
         gjahr
         blart
         bldat
         budat
         xblnr
         bukrs
         lifnr
         waers
         bktxt
         sgtxt
    FROM rbkp INTO TABLE it_rbkp                        "#EC CI_NOFIELD
    WHERE blart IN s_blart
      AND budat IN s_budat
      AND bktxt IN s_bktxt
      AND bukrs IN s_bukrs.
  IF sy-subrc EQ 0.
    lit_rbkp[] = it_rbkp[].
    SORT lit_rbkp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lit_rbkp COMPARING lifnr.
    SELECT lifnr
           name1
      FROM lfa1 INTO TABLE it_lfa1
      FOR ALL ENTRIES IN lit_rbkp
      WHERE lifnr = lit_rbkp-lifnr.
    IF sy-subrc EQ 0.
      SORT it_lfa1 BY lifnr.
    ENDIF.
    PERFORM f_display_progress_bar USING 25
                                         'Fetching Invoice Item Details...'(005).

    SELECT belnr
           gjahr
           buzei
           ebeln
           ebelp
           zekkn
           matnr
           werks
           wrbtr
           shkzg
           lfbnr
           lfgja
           lfpos
      FROM rseg INTO TABLE it_rseg
      FOR ALL ENTRIES IN it_rbkp
      WHERE belnr = it_rbkp-belnr
        AND gjahr = it_rbkp-gjahr.
    IF sy-subrc EQ 0.
      lit_rseg[] = it_rseg[].
      SORT lit_rseg BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM lit_rseg COMPARING ebeln ebelp.
      IF lit_rseg[] IS NOT INITIAL.
        PERFORM f_display_progress_bar USING 40
                                             'Fetching PO/SRO Details...'(004).

        SELECT a~ebeln
               a~bukrs
               a~lifnr
               a~bedat
               a~ekgrp
               a~zzariba_approver
               b~ebelp
               b~matnr
               b~txz01
               b~bednr          "(+) DECK914368
               b~afnam
          FROM ekpo AS b INNER JOIN ekko AS a
          ON a~ebeln = b~ebeln
          INTO TABLE it_po_item
          FOR ALL ENTRIES IN lit_rseg
          WHERE b~ebeln EQ lit_rseg-ebeln
            AND b~ebelp EQ lit_rseg-ebelp
            AND b~afnam IN s_afnam.
        IF sy-subrc EQ 0.
          SORT it_po_item BY ebeln ebelp.
*- Get the GR document details from EKBE (Trans Type: 1 -> Goods Receipt)
          SELECT ebeln
                 ebelp
                 zekkn
                 vgabe
                 gjahr
                 belnr
                 buzei
                 xblnr
                 ernam
            FROM ekbe INTO TABLE it_ekbe
            FOR ALL ENTRIES IN it_po_item
            WHERE ebeln = it_po_item-ebeln
              AND ebelp = it_po_item-ebelp
              AND vgabe = '1'.
          IF sy-subrc EQ 0.
            SORT it_ekbe BY ebeln ebelp zekkn.
          ENDIF.
          SELECT bukrs
                 belnr
                 gjahr
                 blart
                 bstat
                 xblnr
            FROM bkpf INTO TABLE it_bkpf
            FOR ALL ENTRIES IN it_rbkp
            WHERE bukrs EQ it_rbkp-bukrs
              AND bstat IN ('Z','V')
              AND xblnr EQ it_rbkp-xblnr
              AND blart IN s_blart1.
          IF sy-subrc EQ 0 .
*            DELETE it_bkpf WHERE blart EQ 'RE'.
            SORT it_bkpf BY bukrs xblnr ASCENDING gjahr DESCENDING belnr DESCENDING.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_DATA_FROM_DB
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_SALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_salv .

  DATA: lr_salv_tbl   TYPE REF TO cl_salv_table.

  IF it_output[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display   = if_salv_c_bool_sap=>false
*           r_container    =
*           container_name =
          IMPORTING
            r_salv_table   = lr_salv_tbl
          CHANGING
            t_table        = it_output.
      CATCH cx_salv_msg .                               "#EC NO_HANDLER
    ENDTRY.
    DATA: lv_ltext TYPE scrtext_l,                          "#EC NEEDED
          lv_stext TYPE scrtext_s,
          lv_mtext TYPE scrtext_m,
          lt_cols TYPE salv_t_column_ref,                   "#EC NEEDED
          ls_cols LIKE LINE OF lt_cols.                     "#EC NEEDED
    DATA: lr_columns TYPE REF TO cl_salv_columns_table,     "#EC NEEDED
          lr_cols    TYPE REF TO cl_salv_columns_list,      "#EC NEEDED
          lr_column TYPE REF TO cl_salv_column_table.       "#EC NEEDED

    lr_columns = lr_salv_tbl->get_columns( ).

    lr_cols = lr_salv_tbl->get_columns( ).
    lt_cols = lr_columns->get( ).

    LOOP AT lt_cols INTO ls_cols.
      TRY.
          lr_column ?= lr_columns->get_column( ls_cols-columnname ). "#EC NOTEXT
          IF ls_cols-columnname EQ 'GJAHR'.
            lr_column->set_technical( abap_true ).          "#EC NOTEXT
            CONTINUE.
          ENDIF.
          CLEAR: lv_ltext, lv_mtext, lv_stext.
          CASE ls_cols-columnname.
            WHEN 'GR_ERNAM'.
              lv_ltext = 'GR Processor'.                    "#EC NOTEXT
              lv_stext = 'GR Processor'.                    "#EC NOTEXT
              lv_mtext = 'GR Processor'.                    "#EC NOTEXT
            WHEN 'GR_MBLNR'.
              lv_ltext = 'GR Document #'.                   "#EC NOTEXT
              lv_stext = 'GR Doc #'.                        "#EC NOTEXT
              lv_mtext = 'GR Doc #'.                        "#EC NOTEXT
            WHEN 'ZZARIBA_APPROVER'.
              lv_ltext = 'SRC Approver'.                    "#EC NOTEXT
              lv_stext = 'SRC'.                             "#EC NOTEXT
              lv_mtext = 'SRC Approver'.                    "#EC NOTEXT
            WHEN 'BEDAT'.
              lv_ltext = 'PO/SRO Doc Date'.                 "#EC NOTEXT
              lv_stext = 'PO/SRO Dt'.                       "#EC NOTEXT
              lv_mtext = 'PO/SRO DocDt'.                    "#EC NOTEXT
            WHEN 'BEDNR'.
              lv_ltext = 'Shopping Cart#'.                  "#EC NOTEXT
              lv_stext = 'SC #'.                            "#EC NOTEXT
              lv_mtext = 'Shopping Cart#'.                  "#EC NOTEXT

            WHEN 'BELNR'.
              lv_ltext = 'Invoice #'.                       "#EC NOTEXT
              lv_stext = 'Invoice #'.                       "#EC NOTEXT
              lv_mtext = 'Invoice #'.                       "#EC NOTEXT
            WHEN 'ORIG_INV'.
              lv_ltext = 'Non-PO Inv#'.                     "#EC NOTEXT
              lv_stext = 'Non-PO Inv#'.                     "#EC NOTEXT
              lv_mtext = 'Non-PO Inv#'.                     "#EC NOTEXT
            WHEN 'STATUS'.
              lv_ltext = 'Status'.                          "#EC NOTEXT
              lv_stext = 'Status'.                          "#EC NOTEXT
              lv_mtext = 'Status'.                          "#EC NOTEXT

            WHEN 'BUZEI'.
              lv_ltext = 'Inv Item'.                        "#EC NOTEXT
              lv_stext = 'Inv Item'.                        "#EC NOTEXT
              lv_mtext = 'Inv Item'.                        "#EC NOTEXT
            WHEN 'EBELN'.
              lv_ltext = 'PO/SRO #'.                        "#EC NOTEXT
              lv_stext = 'PO/SRO #'.                        "#EC NOTEXT
              lv_mtext = 'PO/SRO #'.                        "#EC NOTEXT

            WHEN 'EBELP'.
              lv_ltext = 'PO/SRO Item'.                     "#EC NOTEXT
              lv_stext = 'PO/SRO Itm'.                      "#EC NOTEXT
              lv_mtext = 'PO/SRO Item'.                     "#EC NOTEXT
            WHEN 'TXZ01'.
              lv_ltext = 'PO/SRO Item Desc'.                "#EC NOTEXT
              lv_stext = 'Item Desc'.                       "#EC NOTEXT
              lv_mtext = 'PO/SRO Item Desc'.                "#EC NOTEXT

            WHEN 'WRBTR'.
              lv_ltext = 'Invoice Amount'.                  "#EC NOTEXT
              lv_stext = 'Inv Amt'.                         "#EC NOTEXT
              lv_mtext = 'Invoice Amt'.                     "#EC NOTEXT
              TRY.
                  lr_column->set_currency_column( 'WAERS' ).
                CATCH cx_salv_data_error.               "#EC NO_HANDLER
              ENDTRY.
            WHEN OTHERS.
              CONTINUE.
*              lv_mtext = lv_stext = lv_ltext = ls_cols-columnname.
          ENDCASE.
          lr_column->set_short_text( lv_stext ).            "#EC NOTEXT
          lr_column->set_medium_text( lv_mtext ).           "#EC NOTEXT
          lr_column->set_long_text( lv_ltext ).             "#EC NOTEXT

        CATCH cx_salv_not_found .                       "#EC NO_HANDLER
        CATCH cx_salv_data_error.                       "#EC NO_HANDLER
      ENDTRY.
    ENDLOOP.

    DATA: lr_layout       TYPE REF TO cl_salv_layout,       "#EC NEEDED
          lwa_key         TYPE salv_s_layout_key.           "#EC NEEDED

    DATA: lr_selections TYPE REF TO cl_salv_selections.     "#EC NEEDED

    lr_selections = lr_salv_tbl->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ). "row_column ).

    lwa_key-report = sy-repid.
    lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

    lr_layout = lr_salv_tbl->get_layout( ).
    lr_layout->set_key( lwa_key ).
    lr_layout->set_save_restriction( cl_salv_layout=>if_salv_c_layout~restrict_none )."
*    restrict_user_dependant )."cl_salv_layout=>restrict_none ).
    lr_layout->set_default( if_salv_c_bool_sap=>true ).

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.  "#EC NEEDED
*   Default Functions
    lo_functions = lr_salv_tbl->get_functions( ).
**Set all toolbar functions
    lo_functions->set_all( abap_true ).

    PERFORM f_display_progress_bar USING 99
                                         'Preparing Final ALV display...'(006).
    lr_salv_tbl->display( ).

  ELSE.
    MESSAGE 'No data found for the input criteria'(001)
          TYPE 'S' DISPLAY LIKE 'W'.

  ENDIF.

ENDFORM.                    " F_DISPLAY_SALV
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_SELSCR
*&---------------------------------------------------------------------*
*  Modify Selection screen - display or hide, disable for input
*----------------------------------------------------------------------*
FORM f_modify_selscr .

  DATA: lv_cccategory TYPE t000-cccategory,
        lv_cccopylock TYPE t000-cccopylock.

  CLEAR: lv_cccategory.
  SELECT SINGLE cccategory cccopylock FROM t000 CLIENT SPECIFIED
    INTO (lv_cccategory, lv_cccopylock)
    WHERE mandt = sy-mandt.

  LOOP AT SCREEN.
*- Disable Input for Header Text only for Production. It would be open
*- for input in DEV and QA systems.
    IF screen-group1 = 'M1' AND lv_cccategory CA 'P' AND lv_cccopylock NE space.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_MODIFY_SELSCR
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_PROGRESS_BAR
*&---------------------------------------------------------------------*
*    Display SAP GUI Progress Indicator on the task being done
*----------------------------------------------------------------------*
FORM f_display_progress_bar  USING    value(uv_percent) TYPE numc3
                                      value(uv_text)    TYPE char100.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = uv_percent
      text       = uv_text.
*  WAIT UP TO 1 SECONDS.
  IF uv_percent = 99.
    WAIT UP TO 1 SECONDS.
  ENDIF.

ENDFORM.                    " F_DISPLAY_PROGRESS_BAR
