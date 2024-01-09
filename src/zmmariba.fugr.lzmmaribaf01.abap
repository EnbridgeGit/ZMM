*----------------------------------------------------------------------*
***INCLUDE LZMMARIBAF01 .
*----------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   LZMMARIBAF01                                             *
*  Func-Grp:  ZMMARIBA                                                 *
*  Author:    John Hartung                                             *
*  Date:      February 25, 2011                                        *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*     - MM IDOC Input ARIBA Invoice - create Invoice via BAPI          *
*                                                                      *
*     This function module "Z_MM_IDOC_INPUT_ZARBINV" was copied from   *
*     generated function module "ZIDOC_INPUT_ZINVCRT".  After this     *
*     function module was created, it was enhanced with additional     *
*     requirements.                                                    *
*                                                                      *
*     If SAP requires that the original function be regenerated, then  *
*     this function may require changes.  An analysis/comparison will  *
*     need to be performed between the newly generated function and    *
*     this function to determine the necessary changes.                *
*                                                                      *
*     The original function was created via transaction "BDBG"         *
*     (Generate ALE Interface for BAPI) for Object "BUS2081" and       *
*     Method "CREATEFROMDATA".                                         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 02/25/11 0872 JRHARTU Initial program development                    *
* 10/26/12 EHP5 BTBOUNDY Remove INCLUDES to SAP Standard               *
*----------------------------------------------------------------------*
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  f_retrieve_codes
*&---------------------------------------------------------------------*
*       Retrieve the company code, currency key, and tolerance values
*----------------------------------------------------------------------*
  FORM f_retrieve_codes
    TABLES it_edidd STRUCTURE edidd.

    DATA: ls_edidd       TYPE edidd,
          ls_z1bp_incinv_create_item
                         TYPE z1bp_incinv_create_item.

    DATA: lv_inv_item    TYPE char6,
          lv_buzei       TYPE rblgp,
          lv_po_item     TYPE char5,
          lv_ebelp       TYPE ebelp,
          lv_ebeln       TYPE ebeln,
          lv_tol_perc    TYPE wkgxxx,
          lv_amnt_ac_c18 TYPE char18,
          lv_amnt_ac_c21 TYPE char21,
          lv_flag_first  TYPE flag.

    DATA: lt_zvar      LIKE STANDARD TABLE OF zvar,
          ls_zvar      LIKE LINE OF lt_zvar.

    CLEAR    gt_ekpo_key[].
    CLEAR    gt_ekpo_ekko[].

    lv_flag_first = gc_x.

* Loop through the IDoc looking for invoice items
    CLEAR                  ls_edidd.
    LOOP AT  it_edidd INTO ls_edidd.

      IF   ( ls_edidd-segnam        EQ 'Z1BP_INCINV_CREATE_ITEM' ).

        CLEAR                          ls_z1bp_incinv_create_item.
        MOVE   ls_edidd-sdata       TO ls_z1bp_incinv_create_item.

* Convert the invoice item number
        CLEAR                          lv_inv_item.
        MOVE   ls_z1bp_incinv_create_item-invoice_doc_item
                                    TO lv_inv_item.
        SHIFT  lv_inv_item LEFT DELETING LEADING space.

        CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
          MOVE   lv_inv_item        TO lv_buzei.
        ENDCATCH.
        IF ( sy-subrc EQ 1 ).
          gv_subrc_conv = sy-subrc.
          CLEAR                        gv_segnam_error.
          MOVE   ls_edidd-segnam    TO gv_segnam_error.
          RETURN.
        ENDIF.

*eject
* Convert the PO item number
        CLEAR                          lv_po_item.
        MOVE   ls_z1bp_incinv_create_item-po_item
                                    TO lv_po_item.
        SHIFT  lv_po_item LEFT DELETING LEADING space.

        CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
          MOVE   lv_po_item         TO lv_ebelp.
        ENDCATCH.
        IF ( sy-subrc EQ 1 ).
          gv_subrc_conv = sy-subrc.
          CLEAR                        gv_segnam_error.
          MOVE   ls_edidd-segnam    TO gv_segnam_error.
          RETURN.
        ENDIF.

* Convert the PO number
        CLEAR        lv_ebeln.

        IF ( ls_z1bp_incinv_create_item-po_number
                                    IS INITIAL ).
          gv_subrc_conv = 1.
          CLEAR                        gv_segnam_error.
          MOVE   ls_edidd-segnam    TO gv_segnam_error.
          RETURN.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_z1bp_incinv_create_item-po_number
          IMPORTING
            output = lv_ebeln.

* Append the PO item key values into an internal table
        IF   ( ( lv_ebeln IS NOT INITIAL ) AND
               ( lv_ebelp IS NOT INITIAL )     ).
          CLEAR                        gs_ekpo_key.
          MOVE   lv_ebeln           TO gs_ekpo_key-ebeln.
          MOVE   lv_ebelp           TO gs_ekpo_key-ebelp.
          MOVE   lv_buzei           TO gs_ekpo_key-buzei.
          APPEND gs_ekpo_key        TO gt_ekpo_key.
        ENDIF.

      ENDIF.

      CLEAR  ls_edidd.
    ENDLOOP.

*eject
* Select the PO header and item data
    IF       ( gt_ekpo_key[] IS NOT INITIAL ).

      SORT     gt_ekpo_key ASCENDING BY ebeln ebelp buzei.

      SELECT   p~ebeln p~ebelp p~loekz p~mwskz
               k~bukrs k~loekz k~waers
        INTO   TABLE gt_ekpo_ekko
        FROM   ekpo AS p INNER JOIN ekko AS k
               ON  p~ebeln = k~ebeln
               FOR ALL ENTRIES IN gt_ekpo_key
       WHERE   p~ebeln = gt_ekpo_key-ebeln
         AND   p~ebelp = gt_ekpo_key-ebelp.
      IF ( sy-subrc EQ 0 ).
        SORT   gt_ekpo_ekko ASCENDING BY ebeln ebelp.
        DELETE ADJACENT DUPLICATES  FROM gt_ekpo_ekko
                               COMPARING ebeln ebelp.
      ELSE.
        CLEAR  gt_ekpo_ekko[].
      ENDIF.

    ENDIF.

* Find the company code and currency keys
    CLEAR    gv_bukrs.
    CLEAR    gv_waers.

    SORT     gt_ekpo_key ASCENDING BY buzei.
    CLEAR                     gs_ekpo_key.
    LOOP AT  gt_ekpo_key INTO gs_ekpo_key.

      CLEAR          gs_ekpo_ekko.
      READ     TABLE gt_ekpo_ekko
                INTO gs_ekpo_ekko
                WITH KEY ebeln = gs_ekpo_key-ebeln
                         ebelp = gs_ekpo_key-ebelp.
      IF ( sy-subrc NE 0 ).
        CLEAR        gs_ekpo_ekko.
        CONTINUE.
      ENDIF.

      IF     ( lv_flag_first    IS NOT INITIAL ).
        CLEAR  lv_flag_first.
        CLEAR                          gv_bukrs.
        MOVE   gs_ekpo_ekko-bukrs   TO gv_bukrs.
        CLEAR                          gv_waers.
        MOVE   gs_ekpo_ekko-waers   TO gv_waers.
        CLEAR                          gv_ebeln.
        MOVE   gs_ekpo_ekko-ebeln   TO gv_ebeln.
      ENDIF.

*eject
      IF     ( gs_ekpo_ekko-bukrs   NE gv_bukrs ).
        CLEAR                          gs_flags_invalid-bukrs.
        MOVE   gc_x                 TO gs_flags_invalid-bukrs.
      ENDIF.

      IF     ( gs_ekpo_ekko-waers   NE gv_waers ).
        CLEAR                          gs_flags_invalid-waers.
        MOVE   gc_x                 TO gs_flags_invalid-waers.
      ENDIF.

      CLEAR  gs_ekpo_key.
    ENDLOOP.

* Check the company code
    IF   ( ( gv_bukrs               IS INITIAL ) OR
           ( gs_flags_invalid-bukrs EQ gc_x    )    ).

* Error - Unable to determine the company code
      CLEAR                            gs_text_lines_errors.
      MOVE   'HDR'(011)             TO gs_text_lines_errors-levl.
      MOVE   text-311               TO gs_text_lines_errors-tdline.
      APPEND gs_text_lines_errors   TO gt_text_lines_errors.

      gs_flags_invalid-bukrs = gc_x.

      IF   ( gv_bukrs               IS INITIAL ).
        CLEAR                          gs_ekpo_ekko.
        LOOP AT  gt_ekpo_ekko     INTO gs_ekpo_ekko
                                 WHERE bukrs NE space.
          CLEAR                        gv_bukrs.
          MOVE   gs_ekpo_ekko-bukrs TO gv_bukrs.
          EXIT.
          CLEAR  gs_ekpo_ekko.
        ENDLOOP.
      ENDIF.

    ENDIF.

*eject
* Check the currency key
    IF   ( ( gv_waers               IS INITIAL ) OR
           ( gs_flags_invalid-waers EQ gc_x    )    ).

* Error - Unable to determine the currency key
      CLEAR                            gs_text_lines_errors.
      MOVE   'HDR'(011)             TO gs_text_lines_errors-levl.
      MOVE   text-312               TO gs_text_lines_errors-tdline.
      APPEND gs_text_lines_errors   TO gt_text_lines_errors.

      gs_flags_invalid-waers = gc_x.

      IF   ( gv_waers               IS INITIAL ).
        CLEAR                          gs_ekpo_ekko.
        LOOP AT  gt_ekpo_ekko     INTO gs_ekpo_ekko
                                 WHERE waers NE space.
          CLEAR                        gv_waers.
          MOVE   gs_ekpo_ekko-waers TO gv_waers.
          EXIT.
          CLEAR  gs_ekpo_ekko.
        ENDLOOP.
      ENDIF.

    ENDIF.

* Determine the tax procedure
    CLEAR                              gv_kalsm.
    MOVE     gc_kalsm_canada        TO gv_kalsm.

    IF     ( gv_kalsm               IS INITIAL ).

* Error - Unable to determine the tax procedure
      CLEAR                            gs_text_lines_errors.
      MOVE   'HDR'(011)             TO gs_text_lines_errors-levl.
      MOVE   text-313               TO gs_text_lines_errors-tdline.
      APPEND gs_text_lines_errors   TO gt_text_lines_errors.

      gs_flags_invalid-kalsm = gc_x.

    ENDIF.

*eject
* Retrieve tolerance amount and percent
    CLEAR    gv_tol_amt.
    CLEAR    gv_tol_rate.

    CLEAR    lt_zvar[].
    SELECT   *
      INTO   TABLE lt_zvar
      FROM   zvar
     WHERE   programm = 'LZMMARIBAF01'
    .
    IF ( sy-subrc NE 0 ).
      CLEAR  lt_zvar[].
    ENDIF.

    CLEAR                   ls_zvar.
    LOOP AT  lt_zvar INTO ls_zvar.
      CASE   ls_zvar-varname.

* Retrieve the tolerance amount
        WHEN     gc_vnam_tol_amt.
          CLEAR                             lv_amnt_ac_c18.
          MOVE     ls_zvar-value1         TO lv_amnt_ac_c18.

          PERFORM  f_convert_amount_idoc_to_sap
                                   USING    gv_waers
                                            lv_amnt_ac_c18
                                   CHANGING gv_tol_amt
                                            lv_amnt_ac_c21
                                            gv_subrc_conv.

          IF ( gv_subrc_conv EQ 1 ).
            CLEAR                           gv_segnam_error.
            MOVE   gc_vnam_tol_amt       TO gv_segnam_error.
            RETURN.
          ENDIF.

* Retrieve the tolerance percentage
        WHEN     gc_vnam_tol_perc.
          CLEAR                             lv_amnt_ac_c18.
          MOVE     ls_zvar-value1         TO lv_amnt_ac_c18.

          PERFORM  f_convert_amount_idoc_to_sap
                                   USING    gv_waers
                                            lv_amnt_ac_c18
                                   CHANGING lv_tol_perc
                                            lv_amnt_ac_c21
                                            gv_subrc_conv.

          IF ( gv_subrc_conv EQ 1 ).
            CLEAR                           gv_segnam_error.
            MOVE   gc_vnam_tol_perc      TO gv_segnam_error.
            RETURN.
          ENDIF.

*eject
* Calculate the tolerance rate
          IF ( lv_tol_perc GT 0 ).
            gv_tol_rate = lv_tol_perc / 100.
          ENDIF.

      ENDCASE.

      CLEAR  ls_zvar.
    ENDLOOP.
*{   INSERT         S01K900044                                        1

* Begin changes - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

* Select the special charge and tolerance check data
    DATA:    lv_tabix TYPE sytabix.

    CLEAR    gt_zaribaspec_chrgs[].
    SELECT   *
      INTO   TABLE gt_zaribaspec_chrgs
      FROM   zaribaspec_chrgs.
    IF ( sy-subrc NE 0 ).
      CLEAR  gt_zaribaspec_chrgs[].
    ENDIF.

    CLEAR                              gs_zaribaspec_chrgs.
    LOOP AT  gt_zaribaspec_chrgs  INTO gs_zaribaspec_chrgs.
      lv_tabix = sy-tabix.
      TRANSLATE                        gs_zaribaspec_chrgs-zspecial_chrg
                                    TO UPPER CASE.
      SHIFT                            gs_zaribaspec_chrgs-zspecial_chrg
                                  LEFT DELETING LEADING space.
      MODIFY                           gt_zaribaspec_chrgs
                                  FROM gs_zaribaspec_chrgs
                                 INDEX lv_tabix.
      CLEAR  gs_zaribaspec_chrgs.
    ENDLOOP.

    SORT     gt_zaribaspec_chrgs ASCENDING BY zspecial_chrg.
    DELETE   ADJACENT DUPLICATES         FROM gt_zaribaspec_chrgs
                                    COMPARING zspecial_chrg.

* End changes   - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

*}   INSERT

  ENDFORM.                    " f_retrieve_codes
*eject
*&---------------------------------------------------------------------*
*&      Form  f_pre_process_idoc
*&---------------------------------------------------------------------*
*       Total the additional header and line item charges
*----------------------------------------------------------------------*
  FORM f_pre_process_idoc
    TABLES   it_edidd               STRUCTURE edidd
             ct_text_lines_addchrgs STRUCTURE gs_text_lines_addchrgs
    CHANGING cv_gross_amount        TYPE wkgxxx
             cv_amount_addchrgs     TYPE wkgxxx
*{   REPLACE        S01K900044                                        1
*\             cv_amount_addchrgs_c   TYPE char25.
             cv_amount_addchrgs_c   TYPE char25             "S01K900044
             cv_amount_addchrgs_tol TYPE wkgxxx.            "S01K900044
*}   REPLACE

    DATA: ls_edidd       TYPE edidd,
          ls_z1bp_incinv_create_header
                         TYPE z1bp_incinv_create_header,
          ls_z1bp_incinv_addchrgs_hdr
                         TYPE z1bp_incinv_addchrgs_hdr,
          ls_z1bp_incinv_create_item
                         TYPE z1bp_incinv_create_item,
          ls_z1bp_incinv_addchrgs_item
                         TYPE z1bp_incinv_addchrgs_item,
          ls_z1bp_incinv_create_tax_item
                         TYPE z1bp_incinv_create_tax_item,
          ls_z1bp_incinv_create_tax
                         TYPE z1bp_incinv_create_tax,
          ls_line_addchrgs
                         TYPE ty_gs_line_addchrgs.

    DATA: lv_subrc       TYPE sysubrc,
          lv_inv_item    TYPE char6,
          lv_po_item     TYPE char5,
          lv_levl_ac     TYPE char12,
          lv_catg_ac     TYPE char25,
          lv_desc_ac     TYPE z_char240,
          lv_amnt_ac_c18 TYPE char18,
          lv_amnt_ac_c21 TYPE char21,
          lv_amnt_ac_c25 TYPE char25,
          lv_amount      TYPE wkgxxx.
*{   INSERT         S01K900044                                        2
    DATA: lv_zspecial_chrg                                  "S01K900044
                         TYPE z_special_chrg.               "S01K900044
*}   INSERT

    CLEAR    ct_text_lines_addchrgs[].
    CLEAR    cv_gross_amount.
    CLEAR    cv_amount_addchrgs.
    CLEAR    cv_amount_addchrgs_c.
*{   INSERT         S01K900044                                        3
    CLEAR    cv_amount_addchrgs_tol.                        "S01K900044
*}   INSERT

    CHECK  ( gv_subrc_conv EQ 0 ).

    CLEAR                  ls_edidd.
    LOOP AT  it_edidd INTO ls_edidd.

*eject
* Process the header
      CASE     ls_edidd-segnam.

        WHEN     'Z1BP_INCINV_CREATE_HEADER'.

          CLEAR                        ls_z1bp_incinv_create_header.
          MOVE   ls_edidd-sdata     TO ls_z1bp_incinv_create_header.
          CLEAR                        lv_levl_ac.
          MOVE   'HDR'(011)         TO lv_levl_ac.
          CLEAR                        lv_catg_ac.
          MOVE   'DELIVERY COST'(001)
                                    TO lv_catg_ac.
          CLEAR                        lv_desc_ac.
          MOVE   'Delivery Cost'(002)
                                    TO lv_desc_ac.
          CLEAR                        lv_amnt_ac_c25.
          MOVE   ls_z1bp_incinv_create_header-del_costs
                                    TO lv_amnt_ac_c25.

          IF     ( lv_amnt_ac_c25 IS NOT INITIAL ).

            CLEAR    lv_amount.
            CLEAR    lv_amnt_ac_c21.

            PERFORM  f_convert_amount_idoc_to_sap
                              USING    gv_waers
                                       lv_amnt_ac_c25
                              CHANGING lv_amount
                                       lv_amnt_ac_c21
                                       gv_subrc_conv.

            IF ( gv_subrc_conv EQ 1 ).
              CLEAR                    gv_segnam_error.
              MOVE   ls_edidd-segnam
                                    TO gv_segnam_error.
              RETURN.
            ENDIF.

            IF     ( lv_amount      NE 0 ).

              ADD    lv_amount      TO cv_amount_addchrgs.

              CLEAR                    ls_line_addchrgs.
              MOVE   lv_levl_ac     TO ls_line_addchrgs-levl.
              MOVE   lv_catg_ac     TO ls_line_addchrgs-catg.
              MOVE   lv_amnt_ac_c21 TO ls_line_addchrgs-amnt.
              MOVE   gv_waers       TO ls_line_addchrgs-waers.
              MOVE   lv_desc_ac     TO ls_line_addchrgs-desc.

              PERFORM  f_append_text_line_addchrgs
                                 USING ls_line_addchrgs.

            ENDIF.

          ENDIF.

* Convert the gross amount
          CLEAR                        lv_amnt_ac_c25.
          MOVE   ls_z1bp_incinv_create_header-gross_amount
                                    TO lv_amnt_ac_c25.

          IF     ( lv_amnt_ac_c25 IS NOT INITIAL ).

            CLEAR    lv_amount.
            CLEAR    lv_amnt_ac_c21.

            PERFORM  f_convert_amount_idoc_to_sap
                              USING    gv_waers
                                       lv_amnt_ac_c25
                              CHANGING lv_amount
                                       lv_amnt_ac_c21
                                       gv_subrc_conv.

            IF ( gv_subrc_conv EQ 1 ).
              CLEAR                    gv_segnam_error.
              MOVE   ls_edidd-segnam
                                    TO gv_segnam_error.
              RETURN.
            ENDIF.

            IF     ( lv_amount      NE 0 ).

              MOVE   lv_amount      TO cv_gross_amount.

            ENDIF.

          ENDIF.

*eject
* Process header additional charges
        WHEN     'Z1BP_INCINV_ADDCHRGS_HDR'.

          CLEAR                        ls_z1bp_incinv_addchrgs_hdr.
          MOVE   ls_edidd-sdata     TO ls_z1bp_incinv_addchrgs_hdr.
          CLEAR                        lv_catg_ac.
          MOVE   ls_z1bp_incinv_addchrgs_hdr-category
                                    TO lv_catg_ac.
          CLEAR                        lv_desc_ac.
          MOVE   ls_z1bp_incinv_addchrgs_hdr-description
                                    TO lv_desc_ac.
          CLEAR                        lv_amnt_ac_c18.
          MOVE   ls_z1bp_incinv_addchrgs_hdr-amount
                                    TO lv_amnt_ac_c18.

          CLEAR    lv_amount.
          CLEAR    lv_amnt_ac_c21.

          PERFORM  f_convert_amount_idoc_to_sap
                              USING    gv_waers
                                       lv_amnt_ac_c18
                              CHANGING lv_amount
                                       lv_amnt_ac_c21
                                       gv_subrc_conv.

          IF ( gv_subrc_conv EQ 1 ).
            CLEAR                      gv_segnam_error.
            MOVE   ls_edidd-segnam  TO gv_segnam_error.
            RETURN.
          ENDIF.

          ADD      lv_amount        TO cv_amount_addchrgs.

          CLEAR                        ls_line_addchrgs.
          MOVE     lv_levl_ac       TO ls_line_addchrgs-levl.
          MOVE     lv_catg_ac       TO ls_line_addchrgs-catg.
          MOVE     lv_amnt_ac_c21   TO ls_line_addchrgs-amnt.
          MOVE     gv_waers         TO ls_line_addchrgs-waers.
          MOVE     lv_desc_ac       TO ls_line_addchrgs-desc.

          PERFORM  f_append_text_line_addchrgs
                                 USING ls_line_addchrgs.
*{   INSERT         S01K900044                                        4

* Begin changes - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

          CLEAR                        lv_zspecial_chrg.
          MOVE     ls_z1bp_incinv_addchrgs_hdr-description+00(35)
                                    TO lv_zspecial_chrg.

          CLEAR    lv_subrc.

          PERFORM  f_check_special_charge
                              USING    lv_levl_ac
                                       lv_zspecial_chrg
                              CHANGING lv_subrc.

          IF ( lv_subrc EQ 0 ).
            ADD    lv_amount        TO cv_amount_addchrgs_tol.
          ENDIF.

* End changes   - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

*}   INSERT

*eject
* Process items
        WHEN     'Z1BP_INCINV_CREATE_ITEM'.

          CLEAR                        ls_z1bp_incinv_create_item.
          MOVE   ls_edidd-sdata     TO ls_z1bp_incinv_create_item.
          CLEAR                        lv_inv_item.
          MOVE   ls_z1bp_incinv_create_item-invoice_doc_item
                                    TO lv_inv_item.
          CLEAR                        lv_po_item.
          MOVE   ls_z1bp_incinv_create_item-po_item
                                    TO lv_po_item.

          CLEAR                        lv_levl_ac.

          PERFORM  f_format_level
                              USING    lv_inv_item
                                       lv_po_item
                              CHANGING lv_levl_ac
                                       gv_subrc_conv.

          IF ( gv_subrc_conv EQ 1 ).
            CLEAR                      gv_segnam_error.
            MOVE   ls_edidd-segnam  TO gv_segnam_error.
            RETURN.
          ENDIF.

          PERFORM  f_check_po_item
                              USING    ls_z1bp_incinv_create_item
                                       lv_levl_ac.

*eject
* Process item additional charges
        WHEN     'Z1BP_INCINV_ADDCHRGS_ITEM'.

          CLEAR                        ls_z1bp_incinv_addchrgs_item.
          MOVE   ls_edidd-sdata     TO ls_z1bp_incinv_addchrgs_item.
          CLEAR                        lv_catg_ac.
          MOVE   ls_z1bp_incinv_addchrgs_item-category
                                    TO lv_catg_ac.
          CLEAR                        lv_desc_ac.
          MOVE   ls_z1bp_incinv_addchrgs_item-description
                                    TO lv_desc_ac.
          CLEAR                        lv_amnt_ac_c18.
          MOVE   ls_z1bp_incinv_addchrgs_item-amount
                                    TO lv_amnt_ac_c18.

          CLEAR    lv_amount.
          CLEAR    lv_amnt_ac_c21.

          PERFORM  f_convert_amount_idoc_to_sap
                              USING    gv_waers
                                       lv_amnt_ac_c18
                              CHANGING lv_amount
                                       lv_amnt_ac_c21
                                       gv_subrc_conv.

          IF ( gv_subrc_conv EQ 1 ).
            CLEAR                      gv_segnam_error.
            MOVE   ls_edidd-segnam  TO gv_segnam_error.
            RETURN.
          ENDIF.

          ADD      lv_amount        TO cv_amount_addchrgs.

          CLEAR                        ls_line_addchrgs.
          MOVE     lv_levl_ac       TO ls_line_addchrgs-levl.
          MOVE     lv_catg_ac       TO ls_line_addchrgs-catg.
          MOVE     lv_amnt_ac_c21   TO ls_line_addchrgs-amnt.
          MOVE     gv_waers         TO ls_line_addchrgs-waers.
          MOVE     lv_desc_ac       TO ls_line_addchrgs-desc.

          PERFORM  f_append_text_line_addchrgs
                                 USING ls_line_addchrgs.
*{   INSERT         S01K900044                                        5

* Begin changes - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

          CLEAR                        lv_zspecial_chrg.
          MOVE     ls_z1bp_incinv_addchrgs_item-description+00(35)
                                    TO lv_zspecial_chrg.

          CLEAR    lv_subrc.

          PERFORM  f_check_special_charge
                              USING    lv_levl_ac
                                       lv_zspecial_chrg
                              CHANGING lv_subrc.

          IF ( lv_subrc EQ 0 ).
            ADD    lv_amount        TO cv_amount_addchrgs_tol.
          ENDIF.

* End changes   - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

*}   INSERT

*eject
* Process item taxes
        WHEN     'Z1BP_INCINV_CREATE_TAX_ITEM'.

          CLEAR                        ls_z1bp_incinv_create_tax_item.
          MOVE   ls_edidd-sdata     TO ls_z1bp_incinv_create_tax_item.

          PERFORM  f_check_tax_item
                              USING    ls_z1bp_incinv_create_item
                                       ls_z1bp_incinv_create_tax_item
                                       lv_levl_ac.

* Process header taxes
        WHEN     'Z1BP_INCINV_CREATE_TAX'.

          CLEAR                        ls_z1bp_incinv_create_tax.
          MOVE   ls_edidd-sdata     TO ls_z1bp_incinv_create_tax.
          CLEAR                        lv_levl_ac.
          MOVE   'HDR'(011)         TO lv_levl_ac.

          PERFORM  f_check_tax_header
                              USING    ls_z1bp_incinv_create_header
                                       ls_z1bp_incinv_create_tax
                                       lv_levl_ac.

      ENDCASE.

      CLEAR  ls_edidd.
    ENDLOOP.

* Convert the additional charges to character format
    IF   ( cv_amount_addchrgs   IS NOT INITIAL ).

      PERFORM  f_convert_amount_sap_to_idoc
                          USING    gv_waers
                                   cv_amount_addchrgs
                          CHANGING cv_amount_addchrgs_c
                                   lv_subrc.

    ENDIF.

  ENDFORM.                    " f_pre_process_idoc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_text_language
*&---------------------------------------------------------------------*
*       Set the text language from the IDOC segment
*----------------------------------------------------------------------*
  FORM f_set_text_language
    USING    iv_tsspras    TYPE char3
    CHANGING cv_spras      TYPE spras
             cv_subrc_conv TYPE sysubrc.

    DATA:    lv_laiso      TYPE laiso.

    CLEAR    cv_spras.

    IF     ( iv_tsspras             IS INITIAL ).
      MOVE   gc_spras_dflt          TO cv_spras.
      RETURN.
    ENDIF.

    CLEAR                              lv_laiso.
    MOVE     iv_tsspras             TO lv_laiso.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = lv_laiso
      IMPORTING
        output           = cv_spras
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.

    IF ( sy-subrc NE 0 ).
      cv_subrc_conv = 1.
    ENDIF.

  ENDFORM.                    " f_set_text_language
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_level
*&---------------------------------------------------------------------*
*       Format the level number for the text lines
*----------------------------------------------------------------------*
  FORM f_format_level
    USING    iv_item_inv       TYPE char6
             iv_item_po        TYPE char5
    CHANGING cv_levl           TYPE char12
             cv_subrc_conv     TYPE sysubrc.

    DATA:    lv_item_inv_c     TYPE char6,
             lv_item_inv_n(6)  TYPE n,
             lv_item_po_c      TYPE char5,
             lv_item_po_n(5)   TYPE n.

    CLEAR    cv_levl.

* Format the invoice item number
    CLEAR    lv_item_inv_c.
    CLEAR    lv_item_inv_n.

    IF ( iv_item_inv IS NOT INITIAL ).

      MOVE     iv_item_inv          TO lv_item_inv_c.
      SHIFT    lv_item_inv_c LEFT DELETING LEADING space.

      CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
        MOVE   lv_item_inv_c        TO lv_item_inv_n.
      ENDCATCH.
      IF ( sy-subrc EQ 1 ).
        cv_subrc_conv = sy-subrc.
        RETURN.
      ENDIF.

      CLEAR                            lv_item_inv_c.
      MOVE     lv_item_inv_n        TO lv_item_inv_c.
      SHIFT    lv_item_inv_c LEFT DELETING LEADING '0'.

      IF     ( lv_item_inv_n        IS INITIAL ).
        MOVE   '0'                  TO lv_item_inv_c+0(1).
      ENDIF.

    ENDIF.

*eject
* Format the po item number
    CLEAR    lv_item_po_c.
    CLEAR    lv_item_po_n.

    IF ( iv_item_po  IS NOT INITIAL ).

      MOVE     iv_item_po           TO lv_item_po_c.
      SHIFT    lv_item_po_c  LEFT DELETING LEADING space.

      CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
        MOVE   lv_item_po_c         TO lv_item_po_n.
      ENDCATCH.
      IF ( sy-subrc EQ 1 ).
        cv_subrc_conv = sy-subrc.
        RETURN.
      ENDIF.

      CLEAR                            lv_item_po_c.
      MOVE     lv_item_po_n         TO lv_item_po_c.
      SHIFT    lv_item_po_c  LEFT DELETING LEADING '0'.

      IF     ( lv_item_po_n         IS INITIAL ).
        MOVE   '0'                  TO lv_item_po_c+0(1).
      ENDIF.

    ENDIF.

* Format the level number
    IF       ( iv_item_po           IS INITIAL ).
      MOVE     lv_item_inv_c        TO cv_levl.
    ELSE.
      CONCATENATE                      lv_item_inv_c '.'
                                       lv_item_po_c
                                  INTO cv_levl.
    ENDIF.

  ENDFORM.                    " f_format_level
*eject
*&---------------------------------------------------------------------*
*&      Form  f_convert_amount_idoc_to_sap
*&---------------------------------------------------------------------*
*       Convert IDOC amount to SAP internal amount
*----------------------------------------------------------------------*
  FORM f_convert_amount_idoc_to_sap
    USING    iv_waers       TYPE waers
             iv_idoc_amount
    CHANGING cv_sap_amount
             cv_amount_c21  TYPE char21
             cv_subrc_conv  TYPE sysubrc.

    DATA:    lv_chstring    TYPE char25.

    CLEAR    cv_sap_amount.
    CLEAR    cv_amount_c21.

    CLEAR                              lv_chstring.
    MOVE     iv_idoc_amount         TO lv_chstring.
    SHIFT    lv_chstring LEFT DELETING LEADING space.

    CALL FUNCTION 'OIK_IDOC_FIELD_CHECK_DECIMAL'
      EXPORTING
        i_chstring                = lv_chstring
        i_decimal_point           = 'X'
        i_minus_sign              = 'X'
      EXCEPTIONS
        first_byte_wrong          = 1
        sign_not_allowed          = 2
        multiple_decimal_point    = 3
        right_no_spaces           = 4
        decimal_point_not_allowed = 5
        minus_sign_not_allowed    = 6
        OTHERS                    = 7.

    IF ( sy-subrc NE 0 ).
      cv_subrc_conv = 1.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
      EXPORTING
        currency    = iv_waers
        idoc_amount = iv_idoc_amount
      IMPORTING
        sap_amount  = cv_sap_amount
      EXCEPTIONS
        OTHERS      = 1.

    IF ( sy-subrc NE 0 ).
      cv_subrc_conv = 1.
      RETURN.
    ENDIF.

*eject

    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_IDOC'
      EXPORTING
        currency    = iv_waers
        sap_amount  = cv_sap_amount
      IMPORTING
        idoc_amount = cv_amount_c21
      EXCEPTIONS
        OTHERS      = 1.

    IF ( sy-subrc NE 0 ).
      cv_subrc_conv = 1.
    ENDIF.

  ENDFORM.                    " f_convert_amount_idoc_to_sap
*eject
*&---------------------------------------------------------------------*
*&      Form  f_convert_amount_sap_to_idoc
*&---------------------------------------------------------------------*
*       Convert SAP internal amount to IDOC amount
*----------------------------------------------------------------------*
  FORM f_convert_amount_sap_to_idoc
    USING    iv_waers       TYPE waers
             iv_sap_amount
    CHANGING cv_idoc_amount
             cv_subrc       TYPE sysubrc.

    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_IDOC'
      EXPORTING
        currency    = iv_waers
        sap_amount  = iv_sap_amount
      IMPORTING
        idoc_amount = cv_idoc_amount
      EXCEPTIONS
        OTHERS      = 1.

    IF ( sy-subrc NE 0 ).
      cv_subrc = 1.
    ENDIF.

  ENDFORM.                    " f_convert_amount_sap_to_idoc
*{   INSERT         S01K900044                                        1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_special_charge                        "S01K900044
*&---------------------------------------------------------------------*
*       Validate the header and item special charge
*----------------------------------------------------------------------*
  FORM f_check_special_charge                               "S01K900044
    USING    iv_levl          TYPE char12
             iv_zspecial_chrg TYPE z_special_chrg
    CHANGING cv_subrc         TYPE sysubrc.

    DATA:    lv_zspecial_chrg TYPE z_special_chrg.

    DATA:    lv_tdline        TYPE tdline.

    CLEAR                              lv_zspecial_chrg.
    MOVE     iv_zspecial_chrg       TO lv_zspecial_chrg.
    TRANSLATE                          lv_zspecial_chrg TO UPPER CASE.
    SHIFT                              lv_zspecial_chrg LEFT
                                       DELETING LEADING space.

    CLEAR          gs_zaribaspec_chrgs.
    READ     TABLE gt_zaribaspec_chrgs
              INTO gs_zaribaspec_chrgs
          WITH KEY zspecial_chrg = lv_zspecial_chrg
                   BINARY SEARCH.
    cv_subrc = sy-subrc.

    IF     ( cv_subrc NE 0 ).

* Error - Invoice header tax code is invalid
      CLEAR                            lv_tdline.
      CONCATENATE                      text-351
                                       iv_zspecial_chrg
                                       text-352
                                  INTO lv_tdline
                                       SEPARATED BY space.
      CLEAR                            gs_text_lines_errors.
      MOVE     iv_levl              TO gs_text_lines_errors-levl.
      MOVE     lv_tdline            TO gs_text_lines_errors-tdline.
      APPEND   gs_text_lines_errors TO gt_text_lines_errors.

      gs_flags_invalid-spec_chrgs = gc_x.

    ELSE.
      "Check for an x.
      CLEAR          gs_zaribaspec_chrgs.
      READ     TABLE gt_zaribaspec_chrgs
                INTO gs_zaribaspec_chrgs
            WITH KEY zspecial_chrg = lv_zspecial_chrg
                     ztol_check    = 'X'
                     BINARY SEARCH.
      cv_subrc = sy-subrc.
    ENDIF.
  ENDFORM.                    " f_check_special_charge      "S01K900044
*}   INSERT
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_del_costs_tol
*&---------------------------------------------------------------------*
*       Perform the delivery costs tolerance check
*----------------------------------------------------------------------*
  FORM f_check_del_costs_tol
    USING iv_gross_amount TYPE wkgxxx
          iv_del_costs    TYPE wkgxxx.

    DATA: lv_tol_amt      TYPE wkgxxx,
          lv_amount       TYPE wkgxxx.

    IF   ( gv_tol_amt      IS NOT INITIAL ).

      lv_tol_amt = gv_tol_amt.

      IF ( iv_del_costs        GT lv_tol_amt ).

* Error - Total additional charges exceeds tolerance
        CLEAR                          gs_text_lines_errors.
        MOVE     'HDR'(011)         TO gs_text_lines_errors-levl.
        MOVE     text-321           TO gs_text_lines_errors-tdline.
        INSERT   gs_text_lines_errors
                                  INTO gt_text_lines_errors
                                 INDEX 1.

        gs_flags_invalid-del_costs = gc_x.

        RETURN.

      ENDIF.

    ENDIF.

*    IF   ( gv_tol_rate     IS NOT INITIAL ).
*
*      lv_amount = iv_gross_amount * gv_tol_rate.
*
*      IF ( iv_del_costs        GE lv_amount ).
*
** Error - Total additional charges exceeds tolerance
*        CLEAR                          gs_text_lines_errors.
*        MOVE     'HDR'(011)         TO gs_text_lines_errors-levl.
*        MOVE     text-321           TO gs_text_lines_errors-tdline.
*        INSERT   gs_text_lines_errors
*                                  INTO gt_text_lines_errors
*                                 INDEX 1.
*
*        gs_flags_invalid-del_costs = gc_x.
*
*      ENDIF.
*
*    ENDIF.

  ENDFORM.                    " f_check_del_costs_tol
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_po_item
*&---------------------------------------------------------------------*
*       Perform the PO item check at the invoice item
*----------------------------------------------------------------------*
  FORM f_check_po_item
    USING is_z1bp_incinv_create_item
                         TYPE z1bp_incinv_create_item
          iv_levl        TYPE char12.

    DATA: lv_subrc       TYPE sysubrc,
          lv_inv_item    TYPE char6,
          lv_buzei       TYPE rblgp.

* Validate that the tax code matches the tax code on the PO
    CLEAR                              lv_inv_item.
    MOVE   is_z1bp_incinv_create_item-invoice_doc_item
                                    TO lv_inv_item.
    SHIFT  lv_inv_item LEFT DELETING LEADING space.
    MOVE   lv_inv_item              TO lv_buzei.

    CLEAR          gs_ekpo_ekko.
    CLEAR          gs_ekpo_key.
    READ     TABLE gt_ekpo_key
              INTO gs_ekpo_key
              WITH KEY buzei = lv_buzei.
    IF ( sy-subrc EQ 0 ).
      READ     TABLE gt_ekpo_ekko
                INTO gs_ekpo_ekko
                WITH KEY ebeln   = gs_ekpo_key-ebeln
                         ebelp   = gs_ekpo_key-ebelp
                         loekz_p = space
                         loekz_k = space.
      IF ( sy-subrc NE 0 ).
        CLEAR        gs_ekpo_ekko.
      ENDIF.
    ENDIF.

    IF       ( gs_ekpo_ekko-ebeln   IS INITIAL ).

* Error - Invoice line PO item is invalid
      CLEAR                            gs_text_lines_errors.
      MOVE     iv_levl              TO gs_text_lines_errors-levl.
      MOVE     text-331             TO gs_text_lines_errors-tdline.
      APPEND   gs_text_lines_errors TO gt_text_lines_errors.

      gs_flags_invalid-po_item = gc_x.

    ENDIF.

  ENDFORM.                    " f_check_po_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_tax_item
*&---------------------------------------------------------------------*
*       Perform the tax check at the invoice item
*----------------------------------------------------------------------*
  FORM f_check_tax_item
    USING is_z1bp_incinv_create_item
                         TYPE z1bp_incinv_create_item
          is_z1bp_incinv_create_tax_item
                         TYPE z1bp_incinv_create_tax_item
          iv_levl        TYPE char12.

    DATA: lv_subrc       TYPE sysubrc,
          lv_mwskz       TYPE mwskz,
          lv_tdline      TYPE tdline,
          lv_inv_item    TYPE char6,
          lv_buzei       TYPE rblgp,
          lv_ebelp       TYPE ebelp,
          lv_ebeln       TYPE ebeln.

* Check the tax code
    CLEAR                              lv_mwskz.
    MOVE     is_z1bp_incinv_create_tax_item-tax_code
                                    TO lv_mwskz.
    CLEAR                              lv_subrc.

    PERFORM  f_check_tax_code USING    lv_mwskz
                              CHANGING lv_subrc.

    IF     ( lv_subrc NE 0 ).

* Error - Invoice item tax code is invalid
      CLEAR                            lv_tdline.
      MOVE     text-341             TO lv_tdline+00(16).
      MOVE     lv_mwskz+00(02)      TO lv_tdline+17(02).
      MOVE     text-342             TO lv_tdline+20(12).
      CLEAR                            gs_text_lines_errors.
      MOVE     iv_levl              TO gs_text_lines_errors-levl.
      MOVE     lv_tdline            TO gs_text_lines_errors-tdline.
      APPEND   gs_text_lines_errors TO gt_text_lines_errors.

      gs_flags_invalid-itm_taxes = gc_x.

      RETURN.

    ENDIF.

*eject
* Validate that the tax code matches the tax code on the PO
    CLEAR                              lv_inv_item.
    MOVE   is_z1bp_incinv_create_item-invoice_doc_item
                                    TO lv_inv_item.
    SHIFT  lv_inv_item LEFT DELETING LEADING space.
    MOVE   lv_inv_item              TO lv_buzei.

    CLEAR          gs_ekpo_ekko.
    CLEAR          gs_ekpo_key.
    READ     TABLE gt_ekpo_key
              INTO gs_ekpo_key
              WITH KEY buzei = lv_buzei.
    IF ( sy-subrc EQ 0 ).
      READ     TABLE gt_ekpo_ekko
                INTO gs_ekpo_ekko
                WITH KEY ebeln = gs_ekpo_key-ebeln
                         ebelp = gs_ekpo_key-ebelp.
      IF ( sy-subrc NE 0 ).
        CLEAR        gs_ekpo_ekko.
      ENDIF.
    ENDIF.

    IF       ( lv_mwskz             NE gs_ekpo_ekko-mwskz ).

* Error - Invoice item tax code is invalid
      CLEAR                            lv_tdline.
      MOVE     text-341             TO lv_tdline+00(16).
      MOVE     lv_mwskz+00(02)      TO lv_tdline+17(02).
      MOVE     text-342             TO lv_tdline+20(12).
      CLEAR                            gs_text_lines_errors.
      MOVE     iv_levl              TO gs_text_lines_errors-levl.
      MOVE     lv_tdline            TO gs_text_lines_errors-tdline.
      APPEND   gs_text_lines_errors TO gt_text_lines_errors.

      gs_flags_invalid-itm_taxes = gc_x.

    ENDIF.

  ENDFORM.                    " f_check_tax_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_tax_header
*&---------------------------------------------------------------------*
*       Perform the tax check at the invoice header
*----------------------------------------------------------------------*
  FORM f_check_tax_header
    USING is_z1bp_incinv_create_header
                         TYPE z1bp_incinv_create_header
          is_z1bp_incinv_create_tax
                         TYPE z1bp_incinv_create_tax
          iv_levl        TYPE char12.

    DATA: lv_subrc       TYPE sysubrc,
          lv_mwskz       TYPE mwskz,
          lv_tdline      TYPE tdline.

* Check the tax code
    CLEAR                              lv_mwskz.
    MOVE     is_z1bp_incinv_create_tax-tax_code
                                    TO lv_mwskz.
    CLEAR                              lv_subrc.

    PERFORM  f_check_tax_code USING    lv_mwskz
                              CHANGING lv_subrc.

    IF     ( lv_subrc NE 0 ).

* Error - Invoice header tax code is invalid
      CLEAR                            lv_tdline.
      MOVE     text-341             TO lv_tdline+00(16).
      MOVE     lv_mwskz+00(02)      TO lv_tdline+17(02).
      MOVE     text-342             TO lv_tdline+20(12).
      CLEAR                            gs_text_lines_errors.
      MOVE     iv_levl              TO gs_text_lines_errors-levl.
      MOVE     lv_tdline            TO gs_text_lines_errors-tdline.
      APPEND   gs_text_lines_errors TO gt_text_lines_errors.

      gs_flags_invalid-itm_taxes = gc_x.

      RETURN.

    ENDIF.

*eject
* Validate that the tax code matches the tax code on the PO
    CLEAR          gs_ekpo_ekko.
    READ     TABLE gt_ekpo_ekko
              INTO gs_ekpo_ekko
              WITH KEY ebeln = gv_ebeln
                       mwskz = lv_mwskz.
    IF ( sy-subrc NE 0 ).

* Error - Invoice item tax code is invalid
      CLEAR                            lv_tdline.
      MOVE     text-341             TO lv_tdline+00(16).
      MOVE     lv_mwskz+00(02)      TO lv_tdline+17(02).
      MOVE     text-342             TO lv_tdline+20(12).
      CLEAR                            gs_text_lines_errors.
      MOVE     iv_levl              TO gs_text_lines_errors-levl.
      MOVE     lv_tdline            TO gs_text_lines_errors-tdline.
      APPEND   gs_text_lines_errors TO gt_text_lines_errors.

      gs_flags_invalid-itm_taxes = gc_x.

    ENDIF.

  ENDFORM.                    " f_check_tax_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_tax_code
*&---------------------------------------------------------------------*
*       Check the tax code
*----------------------------------------------------------------------*
  FORM f_check_tax_code
    USING    iv_mwskz TYPE mwskz
    CHANGING cv_subrc TYPE sysubrc.

    DATA:    lv_mwskz TYPE mwskz.

    CLEAR    cv_subrc.

* Validate the tax code
    IF       ( iv_mwskz   EQ space    ).
      cv_subrc = 1.
    ELSEIF   ( iv_mwskz   EQ gv_mwskz ).
      cv_subrc = 0.
    ELSE.

      CLEAR    lv_mwskz.
      SELECT   SINGLE mwskz
        INTO   lv_mwskz
        FROM   t007a
       WHERE   kalsm = gv_kalsm
         AND   mwskz = iv_mwskz.
      IF ( sy-subrc EQ 0 ).
        CLEAR                gv_mwskz.
        MOVE   lv_mwskz   TO gv_mwskz.
        cv_subrc = 0.
      ELSE.
        CLEAR                lv_mwskz.
        cv_subrc = 1.
      ENDIF.

    ENDIF.

  ENDFORM.                    " f_check_tax_code
*eject
*&---------------------------------------------------------------------*
*&      Form  f_append_text_line_addchrgs
*&---------------------------------------------------------------------*
*       Append a text line of additional charges
*----------------------------------------------------------------------*
  FORM f_append_text_line_addchrgs
    USING    is_line_addchrgs TYPE ty_gs_line_addchrgs.

    DATA:    lv_amnt_c   TYPE char21,
             lv_text     TYPE string,
             lv_tdline   TYPE tdline.

    DATA:    ls_tline    TYPE tline,
             lt_tlines   TYPE tlinetab.

    CHECK  ( is_line_addchrgs   IS NOT INITIAL ).

* Format the amount value
    CLEAR                              lv_amnt_c.
    MOVE     is_line_addchrgs-amnt  TO lv_amnt_c.
    SHIFT    lv_amnt_c           RIGHT DELETING TRAILING space.
    IF     ( lv_amnt_c              NA '-' ).
      SHIFT  lv_amnt_c            LEFT BY 1 PLACES.
    ENDIF.

* Format the description text lines
    CLEAR                              lv_text.
    MOVE     is_line_addchrgs-desc  TO lv_text.

    CLEAR                              lt_tlines[].

    PERFORM  f_format_text_lines
                                TABLES lt_tlines
                                USING  lv_text
                                       gc_linelen_max
                                       gc_linecnt_max.

* Append the level, category, amount, and currency
    CLEAR                              lv_tdline.
    MOVE     is_line_addchrgs-levl+00(12)
                                    TO lv_tdline+00(12).
    MOVE     is_line_addchrgs-catg+00(25)
                                    TO lv_tdline+13(25).
    MOVE     lv_amnt_c              TO lv_tdline+39(21).
    MOVE     is_line_addchrgs-waers+00(03)
                                    TO lv_tdline+61(03).
    CLEAR                              gs_text_lines_addchrgs.
    MOVE     is_line_addchrgs-levl  TO gs_text_lines_addchrgs-levl.
    MOVE     lv_tdline              TO gs_text_lines_addchrgs-tdline.
    APPEND   gs_text_lines_addchrgs TO gt_text_lines_addchrgs.

*eject
* Append the description
    CLEAR                   ls_tline.
    LOOP AT  lt_tlines INTO ls_tline.
      CLEAR                            lv_tdline.
      MOVE   ls_tline+00(50)        TO lv_tdline+13(50).
      CLEAR                            gs_text_lines_addchrgs.
      MOVE   is_line_addchrgs-levl  TO gs_text_lines_addchrgs-levl.
      MOVE   lv_tdline              TO gs_text_lines_addchrgs-tdline.
      APPEND gs_text_lines_addchrgs TO gt_text_lines_addchrgs.
      CLEAR  ls_tline.
    ENDLOOP.

  ENDFORM.                    " f_append_text_line_addchrgs
*eject
*&---------------------------------------------------------------------*
*&      Form  f_attach_note
*&---------------------------------------------------------------------*
*       Attach the note to the invoice if the document was created
*----------------------------------------------------------------------*
  FORM f_attach_note
    TABLES ct_return     STRUCTURE bapiret2
    USING  iv_tdobject   TYPE tdobject
           iv_tdname     TYPE tdobname
           iv_tdid       TYPE tdid
           iv_tdspras    TYPE spras
           iv_tdtitle    TYPE tdtitle
           iv_tdversion  TYPE tdversion
           iv_tdfuser    TYPE tdfuser.

    DATA:  lv_levl       TYPE char12,
           lv_text       TYPE string.

    DATA:  ls_text_line  TYPE tline,
           lt_text_lines TYPE tlinetab.

    DATA:  ls_header     TYPE thead.

    CLEAR    ct_return[].

    CLEAR    lt_text_lines[].

* Append the text line for the supplier id
    CLEAR                                   ls_text_line.
    MOVE     '*'                         TO ls_text_line-tdformat.
    MOVE     'SUPPLIER ID -'(131)        TO ls_text_line-tdline+00(13).
    MOVE     gv_ref_doc_no               TO ls_text_line-tdline+14(16).
    APPEND   ls_text_line                TO lt_text_lines.

*eject
* Append the text lines for the additional charges
    IF ( LINES( gt_text_lines_addchrgs ) GT 0 ).

      CLEAR                                 ls_text_line.
      MOVE     '*'                       TO ls_text_line-tdformat.
      APPEND   ls_text_line              TO lt_text_lines.
      MOVE     'ADDITIONAL CHARGES'(101) TO ls_text_line-tdline+00(18).
      APPEND   ls_text_line              TO lt_text_lines.
      MOVE     '------------------'(102) TO ls_text_line-tdline+00(18).
      APPEND   ls_text_line              TO lt_text_lines.
      CLEAR                                 ls_text_line.
      MOVE     '*'                       TO ls_text_line-tdformat.
      MOVE     'LEVEL'(111)              TO ls_text_line-tdline+00(05).
      MOVE     'CATEGORY'(112)           TO ls_text_line-tdline+13(08).
      MOVE     'AMOUNT'(113)             TO ls_text_line-tdline+53(06).
      APPEND   ls_text_line              TO lt_text_lines.

      CLEAR                                 gs_text_lines_addchrgs.
      LOOP AT  gt_text_lines_addchrgs  INTO gs_text_lines_addchrgs.
        CLEAR                               ls_text_line.
        MOVE   '*'                       TO ls_text_line-tdformat.
        MOVE   gs_text_lines_addchrgs-tdline+00(70)
                                         TO ls_text_line-tdline+00(70).
        APPEND ls_text_line              TO lt_text_lines.
        CLEAR  gs_text_lines_addchrgs.
      ENDLOOP.

    ENDIF.

*eject
* Process the text lines for the header text
    IF ( ( LINES( gt_text_lines_header ) GT 0 ) OR
         ( LINES( gt_text_lines_items  ) GT 0 )    ).

      CLEAR                                 ls_text_line.
      MOVE     '*'                       TO ls_text_line-tdformat.
      APPEND   ls_text_line              TO lt_text_lines.
      MOVE     'COMMENTS'(121)           TO ls_text_line-tdline+00(08).
      APPEND   ls_text_line              TO lt_text_lines.
      MOVE     '--------'(122)           TO ls_text_line-tdline+00(08).
      APPEND   ls_text_line              TO lt_text_lines.
      CLEAR                                 ls_text_line.
      MOVE     '*'                       TO ls_text_line-tdformat.
      MOVE     'LEVEL'(111)              TO ls_text_line-tdline+00(05).
      MOVE     'DESCRIPTION'(114)        TO ls_text_line-tdline+13(11).
      APPEND   ls_text_line              TO lt_text_lines.

      CLEAR                                 gs_text_lines_header.
      LOOP AT  gt_text_lines_header    INTO gs_text_lines_header.

        AT NEW levl.
          MOVE gs_text_lines_header-levl TO lv_levl.
        ENDAT.

        CLEAR                               lv_text.
        MOVE   gs_text_lines_header-tdline
                                         TO lv_text.

* Append the text line
        PERFORM  f_append_text_line
                                   TABLES   lt_text_lines
                                   USING    lv_text
                                   CHANGING lv_levl.

        CLEAR  gs_text_lines_header.
      ENDLOOP.

*eject
* Process the text lines for the item text
      CLEAR                                 gs_text_lines_items.
      LOOP AT  gt_text_lines_items     INTO gs_text_lines_items.

        AT NEW levl.
          MOVE gs_text_lines_items-levl  TO lv_levl.
        ENDAT.

        CLEAR                               lv_text.
        MOVE   gs_text_lines_items-tdline
                                         TO lv_text.

* Append the text line
        PERFORM  f_append_text_line
                                   TABLES   lt_text_lines
                                   USING    lv_text
                                   CHANGING lv_levl.

        CLEAR  gs_text_lines_items.
      ENDLOOP.

    ENDIF.

*eject
* Process the text lines for the error text
    IF   ( LINES( gt_text_lines_errors ) GT 0 ).

      CLEAR                                 ls_text_line.
      MOVE     '*'                       TO ls_text_line-tdformat.
      APPEND   ls_text_line              TO lt_text_lines.
      MOVE     'VALIDATION WARNINGS'(141)
                                         TO ls_text_line-tdline+00(19).
      APPEND   ls_text_line              TO lt_text_lines.
      MOVE     '-------------------'(142)
                                         TO ls_text_line-tdline+00(19).
      APPEND   ls_text_line              TO lt_text_lines.
      CLEAR                                 ls_text_line.
      MOVE     '*'                       TO ls_text_line-tdformat.
      MOVE     'LEVEL'(111)              TO ls_text_line-tdline+00(05).
      MOVE     'DESCRIPTION'(114)        TO ls_text_line-tdline+13(11).
      APPEND   ls_text_line              TO lt_text_lines.

      CLEAR                                 gs_text_lines_errors.
      LOOP AT  gt_text_lines_errors    INTO gs_text_lines_errors.

        AT NEW levl.
          MOVE gs_text_lines_errors-levl TO lv_levl.
        ENDAT.

        CLEAR                               lv_text.
        MOVE   gs_text_lines_errors-tdline
                                         TO lv_text.

* Append the text line
        PERFORM  f_append_text_line
                                   TABLES   lt_text_lines
                                   USING    lv_text
                                   CHANGING lv_levl.

        CLEAR  gs_text_lines_errors.
      ENDLOOP.

    ENDIF.

*eject
* Assign values to the text header structure and save the text lines
    CLEAR                                   ls_header.
    MOVE   iv_tdobject                   TO ls_header-tdobject.
    MOVE   iv_tdname                     TO ls_header-tdname.
    MOVE   iv_tdid                       TO ls_header-tdid.
    MOVE   iv_tdspras                    TO ls_header-tdspras.
    MOVE   iv_tdtitle                    TO ls_header-tdtitle.
    MOVE   iv_tdversion                  TO ls_header-tdversion.
    MOVE   iv_tdfuser                    TO ls_header-tdfuser.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client                = sy-mandt
        header                = ls_header
*       INSERT                = ' '
        savemode_direct       = 'X'
*       OWNER_SPECIFIED       = ' '
*       LOCAL_CAT             = ' '
*     IMPORTING
*       FUNCTION              =
*       NEWHEADER             =
      TABLES
        lines                 = lt_text_lines
      EXCEPTIONS
        id                    = 1
        language              = 2
        name                  = 3
        object                = 4
        OTHERS                = 5.

    IF ( sy-subrc NE 0 ).
      CLEAR                                 ct_return.
      MOVE     'E'                       TO ct_return-type.
      MOVE     'B1'                      TO ct_return-id.
      MOVE     '099'                     TO ct_return-number.
      MOVE     text-201                  TO ct_return-message_v1.
      APPEND                                ct_return.
    ENDIF.

  ENDFORM.                    " f_attach_note
*eject
*&---------------------------------------------------------------------*
*&      Form  f_append_text_line
*&---------------------------------------------------------------------*
*       Append the text line
*----------------------------------------------------------------------*
  FORM f_append_text_line
    TABLES   ct_text_lines   STRUCTURE tline
    USING    iv_text         TYPE string
    CHANGING cv_levl         TYPE char12.

    DATA:    ls_text_lines   TYPE tline.

    DATA:    ls_tline        TYPE tline,
             lt_tlines       TYPE tlinetab.

* Format the text lines
    CLEAR                              lt_tlines[].

    PERFORM  f_format_text_lines
                                TABLES lt_tlines
                                USING  iv_text
                                       gc_linelen_max
                                       gc_linecnt_max.

* Append the formatted text lines
    CLEAR                   ls_tline.
    LOOP AT  lt_tlines INTO ls_tline.

      CLEAR                            ls_text_lines.
      MOVE   '*'                    TO ls_text_lines-tdformat.

      AT FIRST.
        MOVE cv_levl                TO ls_text_lines-tdline+00(12).
        CLEAR                          cv_levl.
      ENDAT.

      MOVE   ls_tline+00(50)        TO ls_text_lines-tdline+13(50).
      APPEND ls_text_lines          TO ct_text_lines.

      CLEAR  ls_tline.
    ENDLOOP.

  ENDFORM.                    " f_append_text_line
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_text_lines
*&---------------------------------------------------------------------*
*       Format an input string into text lines
*----------------------------------------------------------------------*
  FORM f_format_text_lines
    TABLES   ct_tlines   STRUCTURE tline
    USING    iv_text     TYPE string
             iv_txtlen   TYPE syindex
             iv_maxlns   TYPE syindex.

    DATA:    lv_ptr1     TYPE syindex,
             lv_ptr2     TYPE syindex,
             lv_ptr3     TYPE syindex,
             lv_strlen   TYPE syindex,
             lv_index1   TYPE syindex,
             lv_index2   TYPE syindex,
             lv_i        TYPE syindex,
             lv_j        TYPE syindex,
             lv_char1    TYPE char1.

    DATA:    ls_tline    TYPE tline.

    CLEAR    ct_tlines[].

    CLEAR    lv_ptr1.
    CLEAR    lv_ptr2.
    CLEAR    lv_ptr3.

* Determine the length of the text message
    lv_strlen = STRLEN( iv_text ).

    lv_ptr1   = 1.

* Perform the loop while the starting position (pointer 1) of a
* text segment does not exceed the length of the text message
    WHILE ( lv_ptr1 LE lv_strlen ).
      lv_index1 = sy-index.
      IF ( lv_index1 GT iv_maxlns ). "Exit when max lines exceeded
        EXIT.
      ENDIF.

      lv_ptr2 = 0.
      lv_ptr3 = lv_ptr1.

* Set the length of the character segment - cannot exceed line length
      lv_i = lv_strlen - lv_ptr1 + 1.

      IF ( lv_i GE iv_txtlen ).

        lv_i = iv_txtlen.

*eject
* Determine the position (pointer 2) of the last space in the segment
        DO lv_i TIMES.
          lv_index2 = sy-index.

          lv_ptr3 = lv_ptr1 + lv_index2 - 1."  "absolute position     "

          lv_j    = lv_ptr3 - 1."              "zero relative position"

          CLEAR                            lv_char1.
          MOVE     iv_text+lv_j(1)      TO lv_char1.

          IF ( lv_char1 EQ space ).
            lv_ptr2 = lv_ptr3.
          ENDIF.

        ENDDO.

        IF ( lv_ptr2 EQ 0 ).
          lv_ptr2 = lv_ptr3.
        ENDIF.

      ELSE.

        lv_ptr2 = lv_strlen.

      ENDIF.

      lv_i = lv_ptr1 - 1.
      lv_j = lv_ptr2 - lv_ptr1 + 1.

* Append the text segments to the text lines table
      CLEAR                            ls_tline.
      MOVE     iv_text+lv_i(lv_j)   TO ls_tline.
      APPEND   ls_tline             TO ct_tlines.

      lv_ptr1 = lv_ptr2 + 1.

* Trim leading spaces from beginning of split line
      WHILE ( lv_ptr1 LE lv_strlen ).
        lv_j    = lv_ptr1 - 1.
        CLEAR                          lv_char1.
        MOVE     iv_text+lv_j(1)    TO lv_char1.
        IF ( lv_char1 NE space ).
          EXIT.
        ENDIF.
        lv_ptr1 = lv_ptr1 + 1.
      ENDWHILE.

    ENDWHILE.

  ENDFORM.                    " f_format_text_lines
*&---------------------------------------------------------------------*
*&      Form  UPDATE_NOTE_TAB
*&---------------------------------------------------------------------*
*      Update Invoice text at Note Tab
*----------------------------------------------------------------------*
FORM update_note_tab  USING p_invoicedocnumber
                              TYPE bapi_incinv_fld-inv_doc_no
                              p_fiscalyear
                              TYPE bapi_incinv_fld-fisc_year
                              p_po_number TYPE ekko-ebeln.
    DATA: lv_id TYPE thead-tdid,
          lv_language TYPE thead-tdspras,
          lv_name TYPE thead-tdname,
          lv_object TYPE thead-tdobject,
          ls_lines TYPE tline,
          lt_lines TYPE TABLE OF tline.

    lv_id = '0001'.
    lv_language = 'EN'.
    CONCATENATE p_invoicedocnumber p_fiscalyear INTO lv_name.
    lv_object = 'RBKP'.
    break sahmad.
*Read Text and add new line to existing data
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = lv_id
        language                = lv_language
        name                    = lv_name
        object                  = lv_object
*      IMPORTING
*        header                  = ls_header1
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
    ENDIF.
    ls_lines-tdformat = '*'.
    APPEND ls_lines TO lt_lines. "one line space
    ls_lines-tdline = 'VALIDATION WARNINGS'.
    APPEND ls_lines TO lt_lines.
    ls_lines-tdline = '-------------------'.
    APPEND ls_lines TO lt_lines.
    CONCATENATE 'PO' p_po_number
     'has split accounting, only header data'
      'parked. Please complete Item details.'
      INTO ls_lines-tdline separated by space.
    APPEND ls_lines TO lt_lines.
*Create Text
    CALL FUNCTION 'CREATE_TEXT'
      EXPORTING
        fid       = lv_id
        flanguage = lv_language
        fname     = lv_name
        fobject   = lv_object
      TABLES
        flines    = lt_lines
      EXCEPTIONS
        no_init   = 1
        no_save   = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
    ENDIF.

endform.                    " UPDATE_NOTE_TAB
