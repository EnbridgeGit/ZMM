class ZCL_IM_ME_PROCESS_REQ_CUST definition
  public
  final
  create public .

*"* public components of class ZCL_IM_ME_PROCESS_REQ_CUST
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_ME_PROCESS_REQ_CUST .
*"* protected components of class ZCL_IM_ME_PROCESS_REQ_CUST
*"* do not include other source files here!!!
protected section.
private section.
*"* private components of class ZCL_IM_ME_PROCESS_REQ_CUST
*"* do not include other source files here!!!

  types:
    BEGIN OF ts_ekpo,
            ebeln TYPE ebeln,   "Agrement No.
            ebelp TYPE ebelp,   "Item No.
            txz01 TYPE txz01,   "Short Text
            matnr TYPE matnr,   "Material
            matkl TYPE matkl,   "Material Group
            meins TYPE bstme,   "Purchase Order Unit of Measure "(+)PANUSURI Ticket 71901
            bprme TYPE bbprm,   "Order Price Unit (Purchasing)  "(+)PANUSURI Ticket 71901
            netpr TYPE bprei,   "Price
            peinh TYPE epein,   "Price Unit                     "(+)PANUSURI Ticket 71901
            werks TYPE ewerk,   "Plant
            plifz TYPE eplif,   "Planned Delivery Time in Days 'Lead Time'
            lifnr TYPE elifn,   "Vendor No.
            ekorg TYPE ekorg,   "Purchasing Organization
          END OF ts_ekpo .
  types:
    tt_ekpo TYPE TABLE OF ts_ekpo .
  types:
    BEGIN OF ts_eina,
            matnr TYPE matnr,   "Material Number
            matkl TYPE matkl,   "Material Group
            lifnr TYPE elifn,   "Vendor Account No.
            meins TYPE bstme,   "Purchase Order Unit of Measure      "(+)PANUSURI Ticket 71901
            infnr TYPE infnr,   "No. of Purchasing Info records
            ekorg TYPE ekorg,   "Purchasing Org
            esokz TYPE esokz,   "Purchasing info record category
            werks TYPE ewerk,   "Plant
            netpr TYPE iprei,   "Net Price in Purchasing Info Record "(+)PANUSURI Ticket 71901
            peinh TYPE epein,   "Price Unit                          "(+)PANUSURI Ticket 71901
            bprme TYPE bbprm,   "Order Price Unit (Purchasing)       "(+)PANUSURI Ticket 71901
          END OF ts_eina .
  types:
    tt_eina TYPE TABLE OF ts_eina .

  methods AUTOSOURCING_PREFVENDOR
    importing
      !IM_ITEM_OLD type MEREQ_ITEM
      value(IM_OLA_UPDATE) type CHAR1 optional
      !IM_ITEM_PERSISTENT type EBAN optional
    changing
      !CH_ITEM type MEREQ_ITEM
      !CH_ITEMX type MEREQ_ITEMX .
ENDCLASS.



CLASS ZCL_IM_ME_PROCESS_REQ_CUST IMPLEMENTATION.


METHOD autosourcing_prefvendor.


*************
**** Assign prefered vendor
*************


* Define Structures
  DATA: ls_plantspc   TYPE ts_ekpo,
        ls_orgspc     TYPE ts_ekpo,
        ls_inforec    TYPE ts_eina,
        ls_ven_agre   TYPE ts_ekpo.

* Define Internal Tables
  DATA: lt_plantspc TYPE tt_ekpo, "Plant specific
        lt_orgspc   TYPE tt_ekpo, "Organization specific
        lt_inforec  TYPE tt_eina,
        lt_ven_agre TYPE tt_ekpo.

* Vendor Determination
  DATA: lv_minprice   TYPE bprei,   "Min Price
        lv_minledtime TYPE eplif,   "Min Lead Time
        lv_line       TYPE i.

  DATA: lv_mtart      TYPE mara-mtart.
* BOI by PANUSURI Ticket 71901
  DATA: lv_ebeln TYPE ebeln,
        lv_ebelp TYPE ebelp,
        lv_meins TYPE bstme,
        lv_bprme TYPE bbprm,
        lv_netpr TYPE bprei,
        lv_peinh TYPE epein.
* EOI by PANUSURI Ticket 71901
*- BOI by SKAPSE Ticket SDp 84234
  TYPES: BEGIN OF lty_zvar,
          programm  TYPE zvar-programm,
          varname   TYPE zvar-varname,
          varnum    TYPE zvar-varnum,
          value1    TYPE zvar-value1,
          value2    TYPE zvar-value2,
        END OF lty_zvar.
  DATA: lt_zvar  TYPE STANDARD TABLE OF lty_zvar,
        lt_ekgrp TYPE RANGE OF ekko-ekgrp,
        lv_skip  TYPE abap_bool.

  FIELD-SYMBOLS: <lfs_ekgrp>   LIKE LINE OF lt_ekgrp,
                 <lfs_zvar>    LIKE LINE OF lt_zvar.
*- EOI by SKAPSE Ticket SDp 84234
* Define Constants
  CONSTANTS : co_1       TYPE char1  VALUE '1',
              co_9       TYPE char1  VALUE '9',
              co_k       TYPE bstyp  VALUE 'K',
              co_x       TYPE char1  VALUE 'X',
              co_del     TYPE char1  VALUE ''.

*- BOI by SKAPSE Ticket SDp 84234
  lv_skip = abap_false.
  SELECT  programm
          varname
          varnum
          value1
          value2
    FROM zvar INTO TABLE lt_zvar
    WHERE programm = 'ZME_PROCESS_REQ_CUST'
      AND varname  = 'NO_UOM_CHECK_PGROUP'.
  IF sy-subrc EQ 0.
    LOOP AT lt_zvar ASSIGNING <lfs_zvar>.
      IF <lfs_zvar>-value1 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_ekgrp ASSIGNING <lfs_ekgrp>.
        <lfs_ekgrp>-sign = 'I'.
        <lfs_ekgrp>-option = 'EQ'.
        <lfs_ekgrp>-low = <lfs_zvar>-value1.
      ENDIF.
    ENDLOOP.
    IF lt_ekgrp[] IS NOT INITIAL  AND ch_item-ekgrp IN lt_ekgrp .
      lv_skip = abap_true.
    ENDIF.
  ENDIF.
*- EOI by SKAPSE Ticket SDP 84234

*      AND

  IF ch_item-loekz IS INITIAL AND ch_item-estkz = 'R' AND im_item_persistent-banfn IS INITIAL.  "(+)PANUSURI Ticket 71901
    IF ch_item-matnr <> im_item_old-matnr.
      im_ola_update = co_x.
    ELSEIF ch_item-matkl <> im_item_old-matkl.
      im_ola_update = co_x.
    ELSEIF ch_item-pstyp <> im_item_old-pstyp.
      im_ola_update = co_x.
    ELSEIF ch_item-werks <> im_item_old-werks.
      im_ola_update = co_x.
    ENDIF.

*    IF ch_item-matnr IS NOT INITIAL AND im_ola_update = co_x."(-)PANUSURI Ticket 71901
    IF ch_item-matnr IS NOT INITIAL."(+)PANUSURI Ticket 71901
*- Begin of Change dt 2/26/2015 SDP-82332
*      CLEAR : ch_item-ekorg,
*              ch_item-flief,
*              ch_item-lifnr,
*              ch_item-konnr,
*              ch_item-ktpnr,
*              ch_item-infnr,
*
*              ch_itemx-ekorg,
*              ch_itemx-flief,
*              ch_itemx-lifnr,
*              ch_itemx-konnr,
*              ch_itemx-ktpnr,
*              ch_itemx-infnr.
*- End of Change dt 2/26/2015 SDP-82332
      SELECT SINGLE mtart
        INTO lv_mtart
        FROM mara
        WHERE matnr = ch_item-matnr.
      .

      IF lv_mtart = 'DIEN'.
        "Do not source DIEN
        EXIT.
      ENDIF.

      "Fetch Agreement(EBELN) Vendor(LIFNR) using Material No.(MATNR)
      SELECT a~ebeln
           a~ebelp
           a~txz01
           a~matnr
           a~matkl
           a~meins  "(+)PANUSURI Ticket 71901
           a~bprme  "(+)PANUSURI Ticket 71901
           a~netpr
           a~peinh  "(+)PANUSURI Ticket 71901
           a~werks
           a~plifz
           b~lifnr
           b~ekorg
        INTO TABLE lt_ven_agre
        FROM ekpo AS a
        INNER JOIN ekko AS b
          ON a~ebeln = b~ebeln
        INNER JOIN lfa1 AS c
          ON b~lifnr = c~lifnr
        WHERE a~matnr = ch_item-matnr
          AND   a~ebeln >= '8100000000'
          AND   a~ebeln <  '8200000000'
          AND   a~pstyp = ch_item-pstyp
          AND   b~bstyp = co_k
          AND   a~loekz = ''
          AND   b~kdatb <= ch_item-lfdat
          AND   b~kdate >= ch_item-lfdat
          AND   c~sperm <> 'X'
          AND ( a~werks = ch_item-werks
              OR a~werks = space )
      .

      IF sy-subrc = 0.  " Agreement found
        CLEAR : ls_ven_agre, lt_plantspc, lt_orgspc.
*      "Check for multiple agreements Plant specific
*      DELETE lt_ven_agre
*        WHERE werks <> ch_item-werks
*          AND werks <> space
*      .

*      "Get the Active Vendors
*      CALL METHOD me->get_active_vendor
*        CHANGING
*          ch_tbl_ven_agree = lt_ven_agre.

        LOOP AT lt_ven_agre INTO ls_ven_agre.
*         BOI by PANUSURI Ticket 71901
*         Check Contract Pricing unit of measures is same as Purchase Requisition unit of measure
          IF ( ls_ven_agre-meins = ch_item-meins AND ls_ven_agre-bprme = ch_item-meins )
                          OR ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234.
          ELSE.
            MESSAGE e303(me) WITH text-006 text-007 text-008.
            RETURN.
          ENDIF.
*         Include Price Unit
          IF ls_ven_agre-netpr IS NOT INITIAL.
            ls_ven_agre-netpr = ls_ven_agre-netpr / ls_ven_agre-peinh.
          ENDIF.
*         EOI by PANUSURI Ticket 71901
          IF ls_ven_agre-werks IS NOT INITIAL.
            IF ls_ven_agre-werks EQ ch_item-werks.
              APPEND ls_ven_agre TO lt_plantspc.
            ENDIF.
          ELSE.
            APPEND ls_ven_agre TO lt_orgspc.
          ENDIF.
        ENDLOOP.

        CLEAR : lt_ven_agre, ls_ven_agre.
*     If multiple OLA then get the agreement with Lowest price
*     If same price then get the shortest lead time
*     if lowest price and lead time same then exit, source of supply will not be assigned.

        IF NOT lt_plantspc IS INITIAL. "Plant specific
          lt_ven_agre[] = lt_plantspc[].
        ELSEIF NOT lt_orgspc IS INITIAL. "Organization specific
          lt_ven_agre[] = lt_orgspc[].
        ENDIF.


        SORT lt_ven_agre BY netpr ASCENDING.   "Sort by Price

**Read 1st records for getting the lowest price
        CLEAR ls_ven_agre.
        READ TABLE lt_ven_agre INDEX  1
                                   INTO ls_ven_agre.

        lv_minprice = ls_ven_agre-netpr.

** Delete all other records not having lowest price
        DELETE lt_ven_agre WHERE netpr NE lv_minprice.

        DESCRIBE TABLE  lt_ven_agre LINES lv_line. "Count the number of records

**If more then one record found with same price then filter based on shortest lead time
        IF lv_line > 1.

          SORT lt_ven_agre BY plifz ASCENDING.   "Sort by Leadtime

**Read 1st records for getting the lowest price
          CLEAR ls_ven_agre.
          READ TABLE lt_ven_agre INDEX  1
                                     INTO ls_ven_agre.

          lv_minledtime = ls_ven_agre-plifz.

** Delete all other records not having lowest price
          DELETE lt_ven_agre WHERE plifz NE lv_minledtime.
          CLEAR lv_line.
          DESCRIBE TABLE  lt_ven_agre LINES lv_line. "Count the number of records

**If more then one record found with same price and same shortest lead time
          IF lv_line > 1.
            CLEAR ls_ven_agre.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_ven_agre IS NOT INITIAL. "Agreement found

        "Set the new values
        ch_item-konnr = ls_ven_agre-ebeln.
        ch_item-ktpnr = ls_ven_agre-ebelp.
        ch_item-flief = ls_ven_agre-lifnr.
        ch_item-lifnr = ls_ven_agre-lifnr.
        ch_item-ekorg = ls_ven_agre-ekorg.
        ch_itemx-konnr = co_x.
        ch_itemx-ktpnr = co_x.
        ch_itemx-flief = co_x.
        ch_itemx-lifnr = co_x.
        ch_itemx-ekorg = co_x.

      ELSE. "No Agreement found look for Source List
*        "Get Source List
*        BOC by PANUSURI Ticket 71901
*        SELECT SINGLE lifnr ekorg
*        INTO (ch_item-lifnr, ch_item-ekorg)
*        FROM eord
*        WHERE matnr = ch_item-matnr
*          AND werks = ch_item-werks
*          AND vdatu <= ch_item-lfdat
*          AND bdatu >= ch_item-lfdat
*          AND notkz = ''.
*        EOC by PANUSURI Ticket 71901
*       BOI by PANUSURI Ticket 71901
        SELECT SINGLE lifnr ebeln ebelp ekorg
          INTO (ch_item-lifnr, lv_ebeln, lv_ebelp, ch_item-ekorg)
          FROM eord
          WHERE matnr = ch_item-matnr
            AND werks = ch_item-werks
            AND vdatu <= ch_item-lfdat
            AND bdatu >= ch_item-lfdat
            AND notkz = ''.
        IF sy-subrc = 0 AND ch_item-lifnr IS NOT INITIAL."Source List found
          IF lv_ebeln IS NOT INITIAL AND lv_ebelp IS NOT INITIAL."Source List with Contract found
            SELECT SINGLE meins bprme netpr peinh
              INTO (lv_meins, lv_bprme, lv_netpr, lv_peinh)
              FROM ekpo
              WHERE ebeln = lv_ebeln
              AND   ebelp = lv_ebelp.
            IF sy-subrc = 0.
*             Check Purchase order Pricing unit of measures is same as Purchase Requisition unit of measure
              IF ( lv_meins = ch_item-meins AND lv_bprme = ch_item-meins )
                    OR ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234
              ELSE.
                MESSAGE e303(me) WITH text-006 text-007 text-008.
                RETURN.
              ENDIF.
              "Set the new values
              ch_item-konnr = lv_ebeln.
              ch_item-ktpnr = lv_ebelp.
              ch_item-flief = ch_item-lifnr.
              ch_item-lifnr = ch_item-lifnr.
              ch_item-ekorg = ch_item-ekorg.
              ch_itemx-konnr = co_x.
              ch_itemx-ktpnr = co_x.
              ch_itemx-flief = co_x.
              ch_itemx-lifnr = co_x.
              ch_itemx-ekorg = co_x.
            ENDIF.
          ELSE."If no Source List with Contract, look for Source List with Info Rec
            "Fetch Info Rec(INFNR) details
            SELECT SINGLE a~matnr
                    a~matkl
                    a~lifnr
                    a~meins
                    b~infnr
                    b~ekorg
                    b~esokz
                    b~werks
                    b~netpr
                    b~peinh
                    b~bprme
                    INTO ls_inforec
                    FROM eina AS a
                    INNER JOIN eine AS b
                    ON b~infnr = a~infnr
                    WHERE a~matnr = ch_item-matnr
                    AND a~lifnr = ch_item-lifnr
                    AND b~werks = ch_item-werks
                    AND a~loekz = ''.
            IF sy-subrc <> 0.
              "Check non plant specific
              SELECT SINGLE a~matnr
                      a~matkl
                      a~lifnr
                      a~meins
                      b~infnr
                      b~ekorg
                      b~esokz
                      b~werks
                      b~netpr
                      b~peinh
                      b~bprme
                      INTO ls_inforec
                      FROM eina AS a
                      INNER JOIN eine AS b
                      ON b~infnr = a~infnr
                      WHERE a~matnr = ch_item-matnr
                      AND a~lifnr = ch_item-lifnr
                      AND a~loekz = ''.
            ENDIF.
            IF sy-subrc = 0. "Info Rec Found
*             Check Info Rec Pricing unit of measures is same as Purchase Requisition unit of measure
              IF ( ls_inforec-meins = ch_item-meins AND ls_inforec-bprme = ch_item-meins )
                            OR ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234.
              ELSE.
                MESSAGE e303(me) WITH text-006 text-007 text-008.
                RETURN.
              ENDIF.
              ch_item-flief = ls_inforec-lifnr.
              ch_item-lifnr = ls_inforec-lifnr.
              ch_item-ekorg = ls_inforec-ekorg.
              ch_item-infnr = ls_inforec-infnr.

              ch_itemx-flief = co_x.
              ch_itemx-lifnr = co_x.
              ch_itemx-ekorg = co_x.
              ch_itemx-infnr = co_x.
            ENDIF.
          ENDIF.
*         EOI by PANUSURI Ticket 71901
        ELSE.
          "If no source list, look for Info Rec
          "Fetch Info Rec(INFNR) details
          IF NOT ch_item-lifnr IS INITIAL.
            SELECT  a~matnr
                    a~matkl
                    a~lifnr
                    a~meins "(+)PANUSURI Ticket 71901
                    b~infnr
                    b~ekorg
                    b~esokz
                    b~werks
                    b~netpr "(+)PANUSURI Ticket 71901
                    b~peinh "(+)PANUSURI Ticket 71901
                    b~bprme "(+)PANUSURI Ticket 71901
              INTO TABLE lt_inforec
              FROM eina AS a
              INNER JOIN eine AS b
                ON b~infnr = a~infnr
              WHERE a~matnr = ch_item-matnr
                AND a~lifnr = ch_item-lifnr
                AND b~werks = ch_item-werks
                AND a~loekz = ''.
          ELSE.
            SELECT a~matnr
                   a~matkl
                   a~lifnr
                   a~meins "(+)PANUSURI Ticket 71901
                   b~infnr
                   b~ekorg
                   b~esokz
                   b~werks
                   b~netpr "(+)PANUSURI Ticket 71901
                   b~peinh "(+)PANUSURI Ticket 71901
                   b~bprme "(+)PANUSURI Ticket 71901
              INTO TABLE lt_inforec
              FROM eina AS a
              INNER JOIN eine AS b
                ON b~infnr = a~infnr
              WHERE a~matnr = ch_item-matnr
                AND b~werks = ch_item-werks
                AND a~loekz = ''.

          ENDIF.

          IF sy-subrc = 0. "InfoRec Found
*           BOI by PANUSURI Ticket 71901
            LOOP AT lt_inforec INTO ls_inforec.
*             Check InforRec Pricing unit of measures is same as Purchase Requisition unit of measure
              IF ( ls_inforec-meins = ch_item-meins AND ls_inforec-bprme = ch_item-meins )
                                  OR ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234.
              ELSE.
                MESSAGE e303(me) WITH text-006 text-007 text-008.
                RETURN.
              ENDIF.
            ENDLOOP.
*           EOI by PANUSURI Ticket 71901
            SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

            "Get the most recent info record associated with the vendor
            CLEAR ls_inforec.
            READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

            IF sy-subrc = 0. "Recent Info record  found

              ch_item-flief = ls_inforec-lifnr.
              ch_item-lifnr = ls_inforec-lifnr.
              ch_item-ekorg = ls_inforec-ekorg.
              ch_item-infnr = ls_inforec-infnr.

              ch_itemx-flief = co_x.
              ch_itemx-lifnr = co_x.
              ch_itemx-ekorg = co_x.
              ch_itemx-infnr = co_x.
            ENDIF.
          ELSE. "Check non plant specific
            IF NOT ch_item-lifnr IS INITIAL.
              SELECT  a~matnr
                      a~matkl
                      a~lifnr
                      a~meins "(+)PANUSURI Ticket 71901
                      b~infnr
                      b~ekorg
                      b~esokz
                      b~werks
                      b~netpr "(+)PANUSURI Ticket 71901
                      b~peinh "(+)PANUSURI Ticket 71901
                      b~bprme "(+)PANUSURI Ticket 71901
                INTO TABLE lt_inforec
                FROM eina AS a
                INNER JOIN eine AS b
                  ON b~infnr = a~infnr
                WHERE a~matnr = ch_item-matnr
                  AND a~lifnr = ch_item-lifnr
                  AND a~loekz = ''.
            ELSE.
              SELECT a~matnr
                     a~matkl
                     a~lifnr
                     a~meins "(+)PANUSURI Ticket 71901
                     b~infnr
                     b~ekorg
                     b~esokz
                     b~werks
                     b~netpr "(+)PANUSURI Ticket 71901
                     b~peinh "(+)PANUSURI Ticket 71901
                     b~bprme "(+)PANUSURI Ticket 71901
                INTO TABLE lt_inforec
                FROM eina AS a
                INNER JOIN eine AS b
                  ON b~infnr = a~infnr
                WHERE a~matnr = ch_item-matnr
                  AND a~loekz = ''.
            ENDIF.
            IF sy-subrc = 0. "InfoRec Found
*             BOI by PANUSURI Ticket 71901
              LOOP AT lt_inforec INTO ls_inforec.
*               Check InforRec Pricing unit of measures is same as Purchase Requisition unit of measure
                IF ( ls_inforec-meins = ch_item-meins AND ls_inforec-bprme = ch_item-meins )
                              OR ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234.
                ELSE.
                  MESSAGE e303(me) WITH text-006 text-007 text-008.
                  RETURN.
                ENDIF.
              ENDLOOP.
*             EOI by PANUSURI Ticket 71901
              SORT lt_inforec BY infnr DESCENDING.  "Sort by Info record to get the last used

              "Get the most recent info record associated with the vendor
              CLEAR ls_inforec.
              READ TABLE lt_inforec INDEX 1 INTO ls_inforec.

              IF sy-subrc = 0. "Recent Info record  found

                ch_item-flief = ls_inforec-lifnr.
                ch_item-lifnr = ls_inforec-lifnr.
                ch_item-ekorg = ls_inforec-ekorg.
                ch_item-infnr = ls_inforec-infnr.

                ch_itemx-flief = co_x.
                ch_itemx-lifnr = co_x.
                ch_itemx-ekorg = co_x.
                ch_itemx-infnr = co_x.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

*    ELSEIF ch_item-matkl IS NOT INITIAL AND im_ola_update = co_x AND ( ch_item-pstyp = co_9 OR ch_item-pstyp = co_1 )."(-)PANUSURI Ticket 71901
    ELSEIF ch_item-matkl IS NOT INITIAL AND ( ch_item-pstyp = co_9 OR ch_item-pstyp = co_1 )."(+)PANUSURI Ticket 71901
      "Only on Services
*- Begin of Change dt 2/26/2015 SDP-82332
*      CLEAR : ch_item-ekorg,
*              ch_item-flief,
*              ch_item-lifnr,
*              ch_item-konnr,
*              ch_item-ktpnr,
*              ch_item-infnr,
*
*              ch_itemx-ekorg,
*              ch_itemx-flief,
*              ch_itemx-lifnr,
*              ch_itemx-konnr,
*              ch_itemx-ktpnr,
*              ch_itemx-infnr.
*- End of Change dt 2/26/2015 SDP-82332
      "Check for multiple agreements Plant specific (Field ?WERKS?)
      SELECT a~ebeln
             a~ebelp
             a~txz01
             a~matnr
             a~matkl
             a~meins  "(+)PANUSURI Ticket 71901
             a~bprme  "(+)PANUSURI Ticket 71901
             a~netpr
             a~peinh  "(+)PANUSURI Ticket 71901
             a~werks
             a~plifz
             b~lifnr
             b~ekorg
          INTO TABLE lt_ven_agre
          FROM ekpo AS a
          INNER JOIN ekko AS b
            ON b~ebeln = a~ebeln
          INNER JOIN lfa1 AS c
            ON b~lifnr = c~lifnr
          WHERE a~matkl = ch_item-matkl
            AND   a~ebeln >= '8100000000'
            AND   a~ebeln <  '8200000000'
            AND   a~pstyp = ch_item-pstyp
            AND   b~bstyp = co_k
            AND   a~loekz = ''
            AND   b~kdatb < ch_item-lfdat
            AND   b~kdate > ch_item-lfdat
            AND   c~sperm <> 'X'
            AND ( a~werks = ch_item-werks
                OR a~werks = space ).

      IF sy-subrc = 0.
        CLEAR : ls_ven_agre, lt_plantspc, lt_orgspc.
*      DELETE lt_ven_agre
*        WHERE werks <> ch_item-werks
*          AND werks <> space
*      .

*      DELETE lt_ven_agre
*        WHERE txz01 NS ch_item-txz01
*      .

*      "Get the Active Vendors
*      CALL METHOD me->get_active_vendor
*        CHANGING
*          ch_tbl_ven_agree = lt_ven_agre.

        LOOP AT lt_ven_agre INTO ls_ven_agre.
*         BOI by PANUSURI Ticket 71901
*         Check Contract Pricing unit of measures is same as Purchase Requisition unit of measure
          IF ( ls_ven_agre-meins = ch_item-meins AND ls_ven_agre-bprme = ch_item-meins )
                            OR ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234.
          ELSE.
            MESSAGE e303(me) WITH text-006 text-007 text-008.
            RETURN.
          ENDIF.
*         Include Price Unit
          IF ls_ven_agre-netpr IS NOT INITIAL.
            ls_ven_agre-netpr = ls_ven_agre-netpr / ls_ven_agre-peinh.
          ENDIF.
*         EOI by PANUSURI Ticket 71901
          IF ls_ven_agre-werks IS NOT INITIAL.
            IF ls_ven_agre-werks EQ ch_item-werks.
              APPEND ls_ven_agre TO lt_plantspc.
            ENDIF.
          ELSE.
            APPEND ls_ven_agre TO lt_orgspc.
          ENDIF.
        ENDLOOP.

        CLEAR : lt_ven_agre, ls_ven_agre.

        IF NOT lt_plantspc IS INITIAL. "Plant specific
          lt_ven_agre[] = lt_plantspc[].
        ELSEIF NOT lt_orgspc IS INITIAL. "Organization specific
          lt_ven_agre[] = lt_orgspc[].
        ENDIF.


        SORT lt_ven_agre BY netpr ASCENDING.   "Sort by Price

**Read 1st records for getting the lowest price
        CLEAR ls_ven_agre.
        READ TABLE lt_ven_agre INDEX  1
                                   INTO ls_ven_agre.

        lv_minprice = ls_ven_agre-netpr.

** Delete all other records not having lowest price
        DELETE lt_ven_agre WHERE netpr NE lv_minprice.

        DESCRIBE TABLE  lt_ven_agre LINES lv_line. "Count the number of records

**If more then one record found with same price then filter based on shortest lead time
        IF lv_line > 1.

          SORT lt_ven_agre BY plifz ASCENDING.   "Sort by Leadtime

**Read 1st records for getting the lowest price
          CLEAR ls_ven_agre.
          READ TABLE lt_ven_agre INDEX  1
                                     INTO ls_ven_agre.

          lv_minledtime = ls_ven_agre-plifz.

** Delete all other records not having lowest price
          DELETE lt_ven_agre WHERE plifz NE lv_minledtime.
          CLEAR lv_line.
          DESCRIBE TABLE  lt_ven_agre LINES lv_line. "Count the number of records

**If more then one record found with same price and same shortest lead time
          IF lv_line > 1.
            CLEAR ls_ven_agre.
          ENDIF.
        ENDIF.

        IF NOT ls_ven_agre IS INITIAL.
          "Assign New values
          ch_item-konnr = ls_ven_agre-ebeln.
          ch_item-ktpnr = ls_ven_agre-ebelp.
          ch_item-flief = ls_ven_agre-lifnr.
          ch_item-lifnr = ls_ven_agre-lifnr.
          ch_item-ekorg = ls_ven_agre-ekorg.

          ch_itemx-konnr = co_x.
          ch_itemx-ktpnr = co_x.
          ch_itemx-flief = co_x.
          ch_itemx-lifnr = co_x.
          ch_itemx-ekorg = co_x.
        ENDIF.
      ENDIF.
    ENDIF.

*************
**** Assign prefered vendor
*************
* BOC by PANUSURI Ticket 71901
*    DATA: lv_ebeln TYPE ebeln,
*          lv_ebelp TYPE ebelp,
*          lv_meins TYPE bstme,
*          lv_bprme TYPE bbprm,
*          lv_netpr TYPE bprei,
*          lv_peinh TYPE epein.
* EOC by PANUSURI Ticket 71901

    "Rest prefered vendor change flag if OLA fields have chagned
    IF ch_item-infnr <> im_item_old-infnr.
      ch_itemx-preis = ''.
    ELSEIF ch_item-konnr <> im_item_old-konnr.
      ch_itemx-preis = ''.
    ELSEIF ch_item-ktpnr <> im_item_old-ktpnr.
      ch_itemx-preis = ''.
    ENDIF.

    CLEAR: lv_ebeln, lv_ebelp, lv_meins, lv_bprme, lv_netpr, lv_peinh."(+)PANUSURI Ticket 71901
    "Check if prefered vendor has not been manually changed
    IF ch_itemx-preis = ''.
      "Check if Info rec is populated
      IF ch_item-infnr IS NOT INITIAL.
        SELECT SINGLE netpr peinh
          INTO (ch_item-preis, ch_item-peinh) "(+)PANUSURI Ticket 71901
          FROM eine
          WHERE infnr = ch_item-infnr
            AND ekorg = ch_item-ekorg
            AND esokz = 0
            AND werks = ch_item-werks
        .

        "Check for plant generic price
        IF sy-subrc <> 0.
          SELECT SINGLE netpr peinh
            INTO (ch_item-preis, ch_item-peinh) "(+)PANUSURI Ticket 71901
            FROM eine
            WHERE infnr = ch_item-infnr
              AND ekorg = ch_item-ekorg
              AND esokz = 0
              AND werks = ''
          .
        ENDIF.

        "If the price from the inforec is empty
        "Get the last PO price
        IF ch_item-preis = 0.
          CLEAR: lv_ebeln, lv_ebelp.
          "Search for plant specific last PO
          SELECT SINGLE ebeln ebelp
            INTO (lv_ebeln, lv_ebelp)
            FROM eine
            WHERE infnr = ch_item-infnr
              AND ekorg = ch_item-ekorg
              AND esokz = 0
              AND werks = ch_item-werks
          .

          IF sy-subrc <> 0.
            "Get plant independant last PO
            SELECT SINGLE ebeln ebelp
              INTO (lv_ebeln, lv_ebelp)
              FROM eine
              WHERE infnr = ch_item-infnr
                AND ekorg = ch_item-ekorg
                AND esokz = 0
                AND werks = ''
            .
          ENDIF.
          "Use the last PO to get the price
*         BOC by PANUSURI Ticket 71901
*          SELECT SINGLE netpr
*            INTO ch_item-preis
*            FROM ekpo
*            WHERE ebeln = lv_ebeln
*              AND ebelp = lv_ebelp.
*         EOC by PANUSURI Ticket 71901
*         BOI by PANUSURI Ticket 71901
          SELECT SINGLE meins bprme netpr peinh
            INTO (lv_meins, lv_bprme, lv_netpr, lv_peinh)
            FROM ekpo
            WHERE ebeln = lv_ebeln
            AND   ebelp = lv_ebelp.
          IF sy-subrc = 0.
*           Check Purchase order Pricing unit of measures is same as Purchase Requisition unit of measure
            IF lv_meins = ch_item-meins AND lv_bprme = ch_item-meins.
              ch_item-preis = lv_netpr.
              ch_item-peinh = lv_peinh.
            ELSEIF ( lv_skip = abap_true )  .     "(+) SKAPSE Ticket SDP 84234.

            ELSE.
              MESSAGE e303(me) WITH text-006 text-007 text-008.
              RETURN.
            ENDIF.
          ENDIF.
*         EOI by PANUSURI Ticket 71901
        ENDIF.

        "Check if agreement is populated
      ELSEIF ch_item-konnr IS NOT INITIAL AND ch_item-ktpnr IS NOT INITIAL.
        SELECT SINGLE netpr peinh
          INTO (ch_item-preis, ch_item-peinh) "(+)PANUSURI Ticket 71901
          FROM ekpo
          WHERE ebeln = ch_item-konnr
            AND ebelp = ch_item-ktpnr
        .
      ENDIF.
    ENDIF.
  ENDIF."(+)PANUSURI Ticket 71901

ENDMETHOD.


METHOD if_ex_me_process_req_cust~check.
  DATA: lt_pritems   TYPE mmpur_requisition_items,
        ls_pritems   TYPE mmpur_requisition_item,
        lc_pritem    TYPE REF TO if_purchase_requisition_item,
        ls_eban      TYPE mereq_item,
        ls_ebanold   TYPE mereq_item,
        ls_ebanx     TYPE mereq_itemx,

        ls_ebanoldpr      TYPE eban,
        lt_ebanoldpr      TYPE TABLE OF eban,
        lt_ebanoldpr_temp TYPE TABLE OF eban,

        lv_prefvend  TYPE char1,
        lv_isocode   TYPE t006-isocode,

        lv_zzariba_approver TYPE z_ariba_approver,
        lv_requestor        TYPE bname,

        lv_user             TYPE symsgv,
        lv_msgv             TYPE symsgv,
        lv_msg2             TYPE symsgv,

        lv_found(1)         TYPE c.

  DATA: lv_cl_message_mm TYPE REF TO cl_message_mm.

  CONSTANTS:  co_1        TYPE char1    VALUE '1',
              co_9        TYPE char1    VALUE '9',
              co_x        TYPE char1    VALUE 'X',
              co_blank    TYPE char1    VALUE ''.

  CALL METHOD im_header->get_items
    RECEIVING
      re_items = lt_pritems.

  LOOP AT lt_pritems INTO ls_pritems.
    lc_pritem = ls_pritems-item.

    CALL METHOD lc_pritem->get_data
      RECEIVING
        re_data = ls_eban.

    "Skip lines that are maked for deletion.
    IF ls_eban-loekz IS NOT INITIAL.
      CONTINUE.
    ENDIF.


    CLEAR ls_ebanold.
    CALL METHOD lc_pritem->get_previous_data
      RECEIVING
        re_data = ls_ebanold.

    CALL METHOD lc_pritem->get_datax
      RECEIVING
        re_datax = ls_ebanx.



****************
******Validations
****************
    IF ls_eban-eprofile IS NOT INITIAL.
      "Check that Split accounting is not by quantity.
      IF ls_eban-vrtkz = '1'.
        CALL METHOD cl_message_mm=>create
          EXPORTING
            im_msgid         = 'ZMM_MESSAGE'
            im_msgty         = 'E'
            im_msgno         = '018'
            im_force_collect = co_x
          IMPORTING
            ex_message       = lv_cl_message_mm.

        ch_failed = co_x.
      ENDIF.

      "Check that UOM has ISO code.
      IF ls_eban-meins IS NOT INITIAL.
        CLEAR lv_isocode.
        SELECT SINGLE isocode
          FROM t006
          INTO lv_isocode
          WHERE msehi = ls_eban-meins
        .

        IF sy-subrc <> 0 OR lv_isocode IS INITIAL.
          lv_msgv = ls_eban-meins.
          CALL METHOD cl_message_mm=>create
            EXPORTING
              im_msgid         = 'ZMM_MESSAGE'
              im_msgty         = 'E'
              im_msgno         = '019'
              im_msgv1         = lv_msgv
              im_force_collect = co_x
            IMPORTING
              ex_message       = lv_cl_message_mm.

          ch_failed = co_x.
        ENDIF.
      ENDIF.
    ENDIF.


    IF ls_eban-zzorigreq IS NOT INITIAL.
      "Do Material Check on previous PR
      IF ls_eban-matnr IS NOT INITIAL.

        "Fetch details of old purchase requisitio
        SELECT banfn
               bnfpo
               txz01
               matnr
               matkl
               zzorigreq
          FROM eban
          INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr
          WHERE banfn = ls_eban-zzorigreq
            AND loekz <> co_x.

        lv_found = ''.

        "loop through the ITAB of older PR.
        LOOP AT lt_ebanoldpr INTO ls_ebanoldpr.
          "Put a safety in for an infinite loop
          IF sy-tabix > 1000.
            EXIT.
          ENDIF.

          "If a match is found record it and exit
          IF ls_eban-matnr = ls_ebanoldpr-matnr.
            lv_found = 'X'.
            EXIT.
          ELSE.
            "Still no match
            "Find the PR's of this current check
            clear lt_ebanoldpr_temp.
            SELECT  banfn
                    bnfpo
                    txz01
                    matnr
                    matkl
                    zzorigreq
              FROM eban
              INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr_temp
              WHERE banfn = ls_ebanoldpr-zzorigreq
                AND loekz <> co_x.

            "Add these lines to the table that is looping through
            APPEND LINES OF lt_ebanoldpr_temp TO lt_ebanoldpr.
          ENDIF.
        ENDLOOP.

        "Check if no previous material was found
        IF lv_found = ''.
          lv_msgv = ls_eban-bnfpo.
          lv_msg2 = ls_eban-matnr.
          CALL METHOD cl_message_mm=>create
            EXPORTING
              im_msgid         = 'ZMM_MESSAGE'
              im_msgty         = 'E'
              im_msgno         = '000'
              im_msgv1         = lv_msgv
              im_msgv2         = lv_msg2
              im_force_collect = co_x
            IMPORTING
              ex_message       = lv_cl_message_mm.

          ch_failed = co_x.
        ENDIF. "lv_found = ''
      ELSE.
        "Fetch details of old purchase requisitio
        SELECT banfn
               bnfpo
               txz01
               matnr
               matkl
               zzorigreq
          FROM eban
          INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr
          WHERE banfn = ls_eban-zzorigreq
            AND loekz <> co_x.

        lv_found = ''.

        "loop through the ITAB of older PR.
        LOOP AT lt_ebanoldpr INTO ls_ebanoldpr.
          "Put a safety in for an infinite loop
          IF sy-tabix > 1000.
            EXIT.
          ENDIF.

          "If a match is found record it and exit
          IF ls_eban-matkl  = ls_ebanoldpr-matkl.
            lv_found = 'X'.
            EXIT.
          ELSE.
            "Still no match
            "Find the PR's of this current check
            clear lt_ebanoldpr_temp.
            SELECT  banfn
                    bnfpo
                    txz01
                    matnr
                    matkl
                    zzorigreq
              FROM eban
              INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr_temp
              WHERE banfn = ls_ebanoldpr-zzorigreq
                AND loekz <> co_x.

            "Add these lines to the table that is looping through
            APPEND LINES OF lt_ebanoldpr_temp TO lt_ebanoldpr.
          ENDIF.
        ENDLOOP.


        "Check if no previous service group was found
        IF lv_found = ''.
          lv_msgv = ls_eban-bnfpo.
          lv_msg2 = ls_eban-matkl.
          CALL METHOD cl_message_mm=>create
            EXPORTING
              im_msgid         = 'ZMM_MESSAGE'
              im_msgty         = 'E'
              im_msgno         = '001'
              im_msgv1         = lv_msgv
              im_msgv2         = lv_msg2
              im_force_collect = co_x
            IMPORTING
              ex_message       = lv_cl_message_mm.

          ch_failed = co_x.
        ENDIF. "lv_found = ''
      ENDIF." matnr is not initial
    ENDIF. "zzorigreq IS NOT INITIAL.



    "Force Requisitioner
    TRANSLATE ls_eban-afnam TO UPPER CASE.

    IF ls_eban-afnam = ''.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ME'
          im_msgty         = 'E'
          im_msgno         = '303'
          im_msgv1         = text-001
          im_force_collect = co_x
        IMPORTING
          ex_message       = lv_cl_message_mm.

      ch_failed = co_x.
    ELSE.
      "Validate Requisitioner
      SELECT SINGLE bname
        INTO lv_requestor
        FROM usr01
        WHERE bname = ls_eban-afnam
      .

      IF sy-subrc <> 0.
        lv_user = ls_eban-afnam.
        CALL METHOD cl_message_mm=>create
          EXPORTING
            im_msgid         = 'ME'
            im_msgty         = 'E'
            im_msgno         = '303'
            im_msgv1         = text-004
            im_msgv2         = lv_user
            im_msgv3         = text-005
            im_force_collect = co_x
          IMPORTING
            ex_message       = lv_cl_message_mm.

        ch_failed = co_x.
      ENDIF.
    ENDIF.


    IF ls_eban-pstyp = co_1 OR ls_eban-pstyp = co_9.
      IF ls_eban-zzariba_approver = ''.
        "Force Service Receipt Confirmer
        CALL METHOD cl_message_mm=>create
          EXPORTING
            im_msgid         = 'ME'
            im_msgty         = 'E'
            im_msgno         = '303'
            im_msgv1         = text-002
            im_force_collect = co_x
          IMPORTING
            ex_message       = lv_cl_message_mm.

        ch_failed = co_x.
      ELSE.
        SELECT SINGLE zariba_approver
          INTO lv_zzariba_approver
          FROM zaribaaprv
          WHERE zariba_approver = ls_eban-zzariba_approver.

        IF sy-subrc <> 0.
          lv_user = ls_eban-zzariba_approver.
          CALL METHOD cl_message_mm=>create
            EXPORTING
              im_msgid         = 'ME'
              im_msgty         = 'E'
              im_msgno         = '303'
              im_msgv1         = text-003
              im_msgv2         = lv_user
              im_msgv3         = text-005
              im_force_collect = co_x
            IMPORTING
              ex_message       = lv_cl_message_mm.

          ch_failed = co_x.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDMETHOD.


method IF_EX_ME_PROCESS_REQ_CUST~CLOSE.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~INITIALIZE.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~OPEN.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~POST.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ACCOUNT.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_HEADER.
endmethod.


METHOD if_ex_me_process_req_cust~process_item.
  DATA: ls_eban     TYPE  mereq_item,
        ls_ebanold  TYPE  mereq_item,
        ls_ebanx    TYPE  mereq_itemx,
        lv_doc_type TYPE z_varvaluel.   "(+)PANUSURI Ticket 49399

  CONSTANTS: co_x TYPE char1 VALUE 'X'.
*BOI by PANUSURI Ticket 49399
  CONSTANTS:   lc_zme_process_req_cust(20) TYPE c VALUE 'ZME_PROCESS_REQ_CUST',
*               lc_DOC_TYPE_GR(11) type c value 'DOC_TYPE_GR'.  "(-) PANUSURI Ticket 55438
*EOI by PANUSURI Ticket 49399
*BOI PANUSURI Ticket 55438
             lc_doc_type_gr_on(14)  TYPE c VALUE 'DOC_TYPE_GR_ON',
             lc_doc_type_gr_off(15) TYPE c VALUE 'DOC_TYPE_GR_OFF',
             lc_doc_type_gr_on_new(18)  TYPE c VALUE 'DOC_TYPE_GR_ON_NEW',
             lc_doc_type_gr_off_new(19) TYPE c VALUE 'DOC_TYPE_GR_OFF_NEW'.
  DATA : lt_zvar TYPE STANDARD TABLE OF zvar,
         lwa_zvar TYPE zvar.
  DATA : ls_eban_pers TYPE eban.
*EOI PANUSURI Ticket 55438

  IF im_count > 1. "Prevent infinite loop
    EXIT.
  ENDIF.

  CALL METHOD im_item->get_data
    RECEIVING
      re_data = ls_eban.

  CLEAR ls_ebanold.
  CALL METHOD im_item->get_previous_data
    RECEIVING
      re_data = ls_ebanold.

  CALL METHOD im_item->get_datax
    RECEIVING
      re_datax = ls_ebanx.

* BOI by PANUSURI Ticket 71901
* Get existing data from database
  CLEAR ls_eban_pers.
  CALL METHOD im_item->get_persistent_data
    RECEIVING
      re_data = ls_eban_pers.
* EOI by PANUSURI Ticket 71901

  "Run autosource to get vendor
  CALL METHOD me->autosourcing_prefvendor
    EXPORTING
      im_item_old        = ls_ebanold
      im_item_persistent = ls_eban_pers "(+)PANUSURI Ticket 71901
    CHANGING
      ch_item            = ls_eban
      ch_itemx           = ls_ebanx.

  "Uppercase requisitioner
  TRANSLATE ls_eban-afnam TO UPPER CASE.

  IF ls_eban-flief IS INITIAL.
    ls_eban-flief = ls_eban-lifnr.
    ls_ebanx-flief = co_x.
  ENDIF.


  "Update
  CALL METHOD im_item->set_data
    EXPORTING
      im_data = ls_eban.

  CALL METHOD im_item->set_datax
    EXPORTING
      im_datax = ls_ebanx.

*BOI by PANUSURI Ticket 49399
*BOC PANUSURI Ticket 55438
*  if ls_eban-wepos is INITIAL.
*    clear lv_doc_type.
**   Get document type from ZVAR table
*    select single value1
*           from zvar
*           into lv_doc_type
*           where programm = lc_ZME_PROCESS_REQ_CUST
*           and   varname = lc_DOC_TYPE_GR
*           and   value1 = ls_eban-bsart
*           and   value2 = ls_eban-pstyp.
*    if sy-subrc = 0.
**     Set GR flag.
*      ls_eban-wepos = 'X'.
*      CALL METHOD im_item->SET_DATA
*        EXPORTING
*          IM_DATA = ls_eban.
*    endif.
*  endif.
*EOC PANUSURI Ticket 55438
*EOI by PANUSURI Ticket 49399

*BOI PANUSURI Ticket 55438
* Get document type and Item category from ZVAR table
  SELECT *
         FROM zvar
         INTO TABLE lt_zvar
         WHERE programm = lc_zme_process_req_cust.

* Get existing data from database
  CLEAR ls_eban_pers.
  CALL METHOD im_item->get_persistent_data
    RECEIVING
      re_data = ls_eban_pers.

* If new item
  IF ls_eban_pers IS INITIAL.
    IF ls_eban-wepos IS INITIAL.
      READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_on_new
                                                value1 = ls_eban-bsart
                                                value2 = ls_eban-pstyp.
      IF sy-subrc = 0.
*       Set GR flag.
        ls_eban-wepos = 'X'.
        CALL METHOD im_item->set_data
          EXPORTING
            im_data = ls_eban.
      ENDIF.
    ELSE.
      READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_off_new
                                                value1 = ls_eban-bsart
                                                value2 = ls_eban-pstyp.
      IF sy-subrc = 0.
*       Clear GR flag.
        ls_eban-wepos = ' '.
        CALL METHOD im_item->set_data
          EXPORTING
            im_data = ls_eban.
      ENDIF.
    ENDIF.
  ELSE.
*   If existing item
    IF ls_eban-wepos IS INITIAL.
      READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_on
                                                value1 = ls_eban-bsart
                                                value2 = ls_eban-pstyp.
      IF sy-subrc = 0.
*       Set GR flag.
        ls_eban-wepos = 'X'.
        CALL METHOD im_item->set_data
          EXPORTING
            im_data = ls_eban.
      ENDIF.
    ELSE.
      READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_off
                                                value1 = ls_eban-bsart
                                                value2 = ls_eban-pstyp.
      IF sy-subrc = 0.
*       Clear GR flag.
        ls_eban-wepos = ' '.
        CALL METHOD im_item->set_data
          EXPORTING
            im_data = ls_eban.
      ENDIF.
    ENDIF.
  ENDIF.
*EOI PANUSURI Ticket 55438

ENDMETHOD.
ENDCLASS.
