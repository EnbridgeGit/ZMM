*-------------------------------------------------------------------
*  INCLUDE LMGD1I11 .
*    PAI-Module für Verbrauchswerte
*-------------------------------------------------------------------

  INCLUDE lmgd1i5m .  " GET_VERB_SUB


  INCLUDE lmgd1i5l .  " SET_VERB_SUB

  INCLUDE lmgd1i5k .  " VERBR_UEBERNEHMEN

  INCLUDE lmgd1i5j .  " OKCODE_VERBRAUCH

  INCLUDE lmgd1i5i .  " ANZAHL_EINTRAEGE




*&---------------------------------------------------------------------*
*&      Module  ahd_interface  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE ahd_site_interface INPUT.

    DATA:
      l_call_from      TYPE c,
      l_ahd_active     TYPE c.


*   Check if MRP AREA is active ..
    CALL FUNCTION 'DRDC_CHECK_MRP_AREA'
      IMPORTING
        e_disfg = g_disfg.

    IF g_disfg <> c_yes.

      IF ex_badi_wahd IS INITIAL.
        CALL METHOD cl_exithandler=>get_instance
          EXPORTING
            exit_name              = 'BADI_WAHD_INTF'
            null_instance_accepted = space
          IMPORTING
            act_imp_existing       = imp_badi_wahd
          CHANGING
            instance               = ex_badi_wahd.
      ENDIF.

      CHECK NOT imp_badi_wahd IS INITIAL.

    ELSE.

      IF ex_badi_mahd IS INITIAL.
        CALL METHOD cl_exithandler=>get_instance
          EXPORTING
            exit_name              = 'BADI_MAHD_INTF'
            null_instance_accepted = space
          IMPORTING
            act_imp_existing       = imp_badi_mahd
          CHANGING
            instance               = ex_badi_mahd.
      ENDIF.

      CHECK NOT imp_badi_mahd IS INITIAL.

    ENDIF.

    IF  rmmzu-okcode = 'PB09'.
*     Special handling in case of create or change article
      IF t130m-aktyp EQ aktypn OR sy-tcode = 'MM01'.
        l_call_from = '1'.
      ELSEIF t130m-aktyp NE aktypa OR sy-tcode = 'MM02'.
        l_call_from = '2'.
      ELSE.
        l_call_from = '3'.
      ENDIF.

*     BAdI for display/change of AHD
      IF g_disfg <> c_yes.

        CALL METHOD ex_badi_wahd->ex_badi_wahd_intf_disp_chng
          EXPORTING
            i_matnr      = rmmg1-matnr
            i_werks      = rmmg1-werks
            i_dismm      = marc-dismm
            i_meins      = mara-meins
            i_call_from  = l_call_from
          IMPORTING
            e_ahd_active = l_ahd_active.

      ELSE.

*       Check if MRP AREA is active ..
        CALL FUNCTION 'DRDC_GET_BERID_OF_WERKS'
          EXPORTING
            i_werks = rmmg1-werks
          IMPORTING
            e_berid = rmmg1-berid
            e_berty = rmmg1-berty
          EXCEPTIONS
            error   = 1
            OTHERS  = 99.

*       sy-subrc = 2 is not possible, because FM verbrauch_lesen_db is
*       called in that case
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CALL METHOD ex_badi_mahd->ex_badi_mahd_intf_disp_chng
          EXPORTING
            i_matnr      = rmmg1-matnr
            i_werks      = rmmg1-werks
            i_berid      = rmmg1-berid
            i_dismm      = marc-dismm
            i_meins      = mara-meins
            i_call_from  = l_call_from
          IMPORTING
            e_ahd_active = l_ahd_active.

      ENDIF.

*     Return to last screen, if AHD is active
      IF NOT l_ahd_active IS INITIAL.
        CLEAR rmmzu-okcode.
        bildflag = x.
      ENDIF.

    ENDIF.

  ENDMODULE.                 " ahd_interface  INPUT
