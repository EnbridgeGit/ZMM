"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(XEKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(XLFA1) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(XLFB1) LIKE  LFB1 STRUCTURE  LFB1
*"     VALUE(DOBJECT) LIKE  NAST STRUCTURE  NAST OPTIONAL
*"  TABLES
*"      INT_EDIDD STRUCTURE  EDIDD
*"      XEKPO STRUCTURE  UEKPO OPTIONAL
*"      XEKET STRUCTURE  UEKET OPTIONAL
*"      DEKEK_X STRUCTURE  EKEK_X OPTIONAL
*"      DEKEH STRUCTURE  IEKEH OPTIONAL
*"      DSADR STRUCTURE  SADR OPTIONAL
*"      DVBAK STRUCTURE  MMVBAK OPTIONAL
*"      DVBAP STRUCTURE  MMVBAP OPTIONAL
*"      DVBKD STRUCTURE  MMVBKD OPTIONAL
*"  CHANGING
*"     VALUE(ISC_ENHANCEMENT) TYPE  ISC_EXIT_SAPLEINM_002 OPTIONAL
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED
*"      DATA_NOT_RELEVANT_FOR_SENDING
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXM06U02
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM06U02                                                 *
*  Function:  EXIT_SAPLEINM_002                                        *
*  Author:    Brian Boundy                                             *
*  Date:      December 01, 2010                                        *
*  Track #:   TR872 Release 1                                          *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MM06E001 - user exits for EDI purchasing docs      *
*                                                                      *
*     This is a user exit within the framework of outbound IDoc        *
*     processing for purchasing documents.                             *
*                                                                      *
************************************************************************
*------------------------ CHANGE LOG ----------------------------------*
*  Date     TR # By      Description                                   *
* --------- ---- ------- --------------------------------------------- *
* 01-Dec-10 0872 BTBOUND D30K915866 - ARIBA R1 - Initial development   *
* 21-Feb-11 0872 JRHARTU D30K916275 - ARIBA R2 - Development           *
* 28-Jun-11 0872 BTBOUND D30K917172 - ARIBA R2.5 - fix empty vprei     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    :  002                                                 *
* Date          :  08-Feb-2013                                         *
* Modified By   :  BMOHAMMAD                                           *
* Correction No :  D30K921319                                          *
* Description   :  Added Tax Code ID                                   *
*----------------------------------------------------------------------*
* Version No    :  003                                                 *
* Date          :  26-Jun-2013                                         *
* Modified By   :  PANUSURI                                            *
* Correction No :  D30K922091                                          *
* Description   :  Modified Logic Mapping for Company Codes            *
*----------------------------------------------------------------------*
* Version No    :  004                                                 *
* Date          :  04-Jun-2014                                         *
* Modified By   :  JRHARTUNG                                           *
* Correction No :  D30K923669                                          *
* Description   :  Create segment Z1ARBA1 and populate with            *
*                  references to PO item attachments                   *
*----------------------------------------------------------------------*
* Version No    :  005                                                 *
* Date          :  18-Nov-2015                                         *
* Modified By   :  PANUSURI                                            *
* Correction No :  D30K926383                                          *
* Description   :  Added Goods Recipient to Item data.                 *
*----------------------------------------------------------------------*
* Version No    :  006                                                 *
* Date          :  01-Dec-2015                                         *
* Modified By   :  PANUSURI                                            *
* Correction No :  D30K926403                                          *
* Description   :  Create segment Z1ARBH2 and populate with Ariba/CLM  *
*                  contract number.                                    *
*----------------------------------------------------------------------*
* Version No    :  007                                                 *
* Date          :  05-March-2021                                       *
* Modified By   :  WAYCHALJ                                            *
* Correction No :  S01K900829                                          *
* Description   :  Create segments Z1OPNLNK3 and Z1ITMCOND to Send SA  *
*                  Details from SAP to Openlink.                       *
*----------------------------------------------------------------------*
* Version No    :  007A                                                *
* Date          :  16-March-2021                                       *
* Modified By   :  NAGIRIR                                             *
* Correction No :                                                      *
* Description   :  Create segments Z1OPNLNK3 and Z1ITMCOND Send Stock  *
*                  and Transportation Details from SAP to Openlink.    *
*----------------------------------------------------------------------*
************************************************************************

TYPES: BEGIN OF ty_wa_poaction,
        aende            TYPE na_aende,
        erdat            TYPE na_erdat,
        eruhr            TYPE na_eruhr,
       END   OF ty_wa_poaction,
*      BOI by PANUSURI Ticket 91113
       BEGIN OF ty_ekpo,
         ebeln           TYPE ebeln,
         ebelp           TYPE ebelp,
       END OF ty_ekpo.
*      EOI PANUSURI Ticket 91113


DATA:   lwa_edidd        TYPE edidd,
        lwa_edidd_p      TYPE edidd,
        lwa_e1edk01      TYPE e1edk01,
        lwa_e1edk01_p    TYPE e1edk01,
        lwa_z1arbh1      TYPE z1arbh1,
        lwa_e1edp01      TYPE e1edp01,
        lwa_z1arbi1      TYPE z1arbi1,
        lwa_z1opnlnk3    TYPE z1opnlnk3,  "+++WAYCHALJ COG Changes
        lwa_z1itmcond    TYPE z1itmcond,  "+++WAYCHALJ COG Changes
        lwa_z1edccond    TYPE z1edccond,  "+++NAGIRIR For COG Changes FSD07
        lwa_e1edc20      TYPE e1edc20,    "+++NAGIRIR For COG Changes FSD07
        lwa_e1edka1      TYPE e1edka1,
*       BOI by PANUSURI Ticket 91113
        lwa_z1arbh2      TYPE z1arbh2,
        lta_ekpo         TYPE STANDARD TABLE OF ty_ekpo,
        lwa_ekpo         TYPE ty_ekpo,
        lta_zqualf       TYPE STANDARD TABLE OF dd07v,
        lwa_zqualf       TYPE dd07v,
        ltp_konnr        TYPE konnr,
        ltp_ktpnr        TYPE ktpnr,
        ltp_clm_number   TYPE char255,
        ltp_temp1        TYPE string,
        ltp_temp2        TYPE string,
        ltp_name         TYPE thead-tdname,
        lta_text         TYPE STANDARD TABLE OF tline,
        lwa_text         TYPE tline.
*       EOI by PANUSURI Ticket 91113


DATA:   lwa_z1arba1      TYPE ty_wa_z1arba1,
        lit_z1arba1      TYPE ty_it_z1arba1.

DATA:   lv_msgtyp        TYPE edi_mestyp,
        lv_exttyp        TYPE edi_cimtyp,
        lv_segnam        TYPE edilsegtyp.

DATA:   lv_tabix         TYPE sytabix,
        lv_count         TYPE i,
        lv_zariba_email  TYPE z_ariba_email,
        lv_zporg         TYPE ad_smtpadr,
        lv_zporg_phone   TYPE char12,
        lv_zinco         TYPE inc01,
        lv_zinctxt       TYPE bezei30,
        lv_zevtxt        TYPE evtxt,
        lv_slfdt         TYPE slfdt,
        ltp_wempf        TYPE wempf,  "(+)PANUSURI Ticket 91084
        lv_ztax_code     TYPE text1_007s,
        lv_bukrs         TYPE bukrs,
        lv_butxt         TYPE butxt.

*Begin of insert COGs S01K900829 +++WAYCHALJ
DATA:   lv_zzmsa         TYPE zzmsa,
        lv_zzmsatype     TYPE zagreetype.

TYPES: BEGIN OF ty_konv,
         kposn TYPE kposn,
         kschl TYPE kschl,
         kbetr TYPE kbetr,
         waers TYPE waers,
         kpein TYPE kpein,
         kmein TYPE kmein,
       END OF ty_konv.

DATA: lta_konv TYPE STANDARD TABLE OF ty_konv,
      lwa_konv TYPE ty_konv.

CONSTANTS: lc_pbxx(4) TYPE c VALUE 'PBXX',
           lc_zc00(4) TYPE c VALUE 'ZC00',
           lc_zc01(4) TYPE c VALUE 'ZC01'.
*End of insert COGs
*Begin of insert for COG FSD07
TYPES: BEGIN OF ty_a081,
  kschl TYPE kscha,
  kont_pack  TYPE kont_pack,
  kont_zeile TYPE kont_zeile,
  datbi TYPE kodatbi,
  datab TYPE kodatab,
  knumh TYPE knumh,
  END OF ty_a081.

TYPES: BEGIN OF ty_konp,
  knumh TYPE knumh,
  kschl TYPE kscha,
  kbetr TYPE kbetr_kond,
  konwa TYPE konwa,
  kpein TYPE kpein,
  kmein TYPE kmein,
  END OF ty_konp.
DATA: lt_a081 TYPE STANDARD TABLE OF ty_a081,
      lw_a081 TYPE ty_a081,
      lt_konp TYPE STANDARD TABLE OF ty_konp,
      lw_konp TYPE ty_konp.
*End of insert for COG fsD07
CONSTANTS: lc_tot_perc_init TYPE uebto
                            VALUE '0.00'.

CLEAR                                       lv_msgtyp.
MOVE     control_record_out-mestyp       TO lv_msgtyp.
CLEAR                                       lv_exttyp.
MOVE     control_record_out-cimtyp       TO lv_exttyp.

IF     ( lv_msgtyp                       EQ 'ORDERS' ).

  IF     ( lv_exttyp                     EQ 'ZARIBA' ).

* Select the program parameters
    IF       ( git_zvar[]                IS INITIAL ).
      CLEAR    git_zvar[].
      SELECT   *
        INTO   TABLE git_zvar
        FROM   zvar
       WHERE   programm = 'ZXM06U02'.
      IF     ( sy-subrc NE 0 ).
        CLEAR  git_zvar[].
      ENDIF.
    ENDIF.

    CLEAR                                   lv_segnam.
    MOVE     int_edidd-segnam            TO lv_segnam.

    CASE     lv_segnam.

      WHEN     'E1EDK01'.

        CLEAR                               lwa_e1edk01.
        MOVE     int_edidd-sdata         TO lwa_e1edk01.

        CLEAR                               lwa_z1arbh1.
        MOVE     xekko-ernam             TO lwa_z1arbh1-zpo_crt_hdr.
        MOVE     xlfa1-emnfr             TO lwa_z1arbh1-zarib_com_sup.
        MOVE     xekko-zbd1t             TO lwa_z1arbh1-zpymt_days_1.
        MOVE     xekko-zbd2t             TO lwa_z1arbh1-zpymt_days_2.
        MOVE     xekko-zbd3t             TO lwa_z1arbh1-zpymt_days_3.
        MOVE     xekko-zbd1p             TO lwa_z1arbh1-zpymt_perc_1.
        MOVE     xekko-zbd2p             TO lwa_z1arbh1-zpymt_perc_2.

        IF     ( xekko-bsart             EQ 'ZF' ).

          IF   ( xekko-zzariba_approver  IS NOT INITIAL ).

            MOVE xekko-zzariba_approver  TO lwa_z1arbh1-zarib_apprvr.

            CLEAR    lv_zariba_email.
            SELECT   SINGLE zariba_email
              INTO   lv_zariba_email
              FROM   zaribaaprv
             WHERE   zariba_approver = xekko-zzariba_approver.
            IF ( sy-subrc EQ 0 ).
              MOVE   lv_zariba_email TO lwa_z1arbh1-zarib_approvr_email.
            ENDIF.

          ENDIF.

        ENDIF.

        IF       ( git_zvar[]            IS NOT INITIAL ).

          CLEAR                             gwa_zvar.
          LOOP AT      git_zvar        INTO gwa_zvar.
            CASE       gwa_zvar-varname.
              WHEN     'ARIBA_STREET'.
                MOVE   gwa_zvar-value1   TO lwa_z1arbh1-zstreet.
              WHEN     'ARIBA_CITY'.
                MOVE   gwa_zvar-value1   TO lwa_z1arbh1-zcity.
              WHEN     'ARIBA_POSTAL'.
                MOVE   gwa_zvar-value1   TO lwa_z1arbh1-zpostal_code.
              WHEN     'ARIBA_PROV'.
                MOVE   gwa_zvar-value1   TO lwa_z1arbh1-zprov.
              WHEN     'ARIBA_COUNTRY'.
                MOVE   gwa_zvar-value1   TO lwa_z1arbh1-zcountry.
            ENDCASE.
            CLEAR  gwa_zvar.
          ENDLOOP.

        ENDIF.

* Create PO action
        MOVE     '2'                     TO lwa_z1arbh1-zpo_action.

        CLEAR    lv_count.
        SELECT   COUNT( * )
          INTO   lv_count
          FROM   nast
         WHERE   objky = xekko-ebeln
           AND   parnr = xekko-lifnr
           AND   kschl = 'ZARI'.
        IF     ( sy-subrc NE 0 ).
          CLEAR  lv_count.
        ENDIF.

        IF     ( lv_count                EQ 0 ).
          CLEAR                             lwa_z1arbh1-zpo_action.
          MOVE   '1'                     TO lwa_z1arbh1-zpo_action.
        ENDIF.

* Update PO action
        CLEAR    lv_count.
        SELECT   COUNT( * )
          INTO   lv_count
          FROM   nast
         WHERE   objky  = xekko-ebeln
           AND   parnr  = xekko-lifnr
           AND   kschl  = 'ZARI'
           AND   vstat  = '0'
           AND   aende <> 'X'.
        IF     ( sy-subrc NE 0 ).
          CLEAR  lv_count.
        ENDIF.

        IF     ( lv_count                GT 0 ).
          CLEAR                             lwa_z1arbh1-zpo_action.
          MOVE   '1'                     TO lwa_z1arbh1-zpo_action.
        ENDIF.

* Delete PO action
        CLEAR    lv_count.
        SELECT   COUNT( * )
          INTO   lv_count
          FROM   ekpo
         WHERE   ebeln  = lwa_e1edk01-belnr
           AND   loekz <> 'L'.
        IF     ( sy-subrc NE 0 ).
          CLEAR  lv_count.
        ENDIF.

        IF     ( lv_count                EQ 0 ).
          CLEAR                             lwa_z1arbh1-zpo_action.
          MOVE   '3'                     TO lwa_z1arbh1-zpo_action.
        ENDIF.

        CLEAR    lv_zporg.
        CLEAR    lv_zporg_phone.
        SELECT   SINGLE smtp_addr  ektel
          INTO  (lv_zporg, lv_zporg_phone)
          FROM   t024
         WHERE   ekgrp = xekko-ekgrp.
        IF     ( sy-subrc EQ 0 ).
          MOVE   lv_zporg                TO lwa_z1arbh1-zporg.
          MOVE   lv_zporg_phone          TO lwa_z1arbh1-zporg_phone.
        ELSE.
          CLEAR                             lwa_z1arbh1-zporg.
          CLEAR                             lwa_z1arbh1-zporg_phone.
        ENDIF.

        CLEAR    xekpo.
        LOOP AT  xekpo.
          IF   ( xekpo-xersy             EQ 'X' ).
            MOVE 'X'                     TO lwa_z1arbh1-zers.
            EXIT.
          ENDIF.
          CLEAR  xekpo.
        ENDLOOP.

        CLEAR    lv_zinco.
        CLEAR    lv_zinctxt.
        SELECT   SINGLE inco1  bezei
          INTO  (lv_zinco, lv_zinctxt)
          FROM   tinct
         WHERE   inco1 = xekko-inco1
           AND   spras = 'E'.
        IF     ( sy-subrc EQ 0 ).
          MOVE   lv_zinco                TO lwa_z1arbh1-zinco.
          MOVE   lv_zinctxt              TO lwa_z1arbh1-zinctxt.
        ELSE.
          CLEAR                             lwa_z1arbh1-zinco.
          CLEAR                             lwa_z1arbh1-zinctxt.
        ENDIF.
*       BOI by PANUSURI Ticket 91113
        IF xekko-konnr IS NOT INITIAL.
          MOVE     xekko-konnr           TO lwa_z1arbh1-zsapcontract.
          SELECT ebeln
                 ebelp
                 FROM ekpo
                 INTO TABLE lta_ekpo
                 WHERE ebeln = xekko-konnr
                 AND   loekz = space.
          IF sy-subrc = 0.
            SORT lta_ekpo BY ebeln ebelp.
          ENDIF.
        ENDIF.
        CLEAR: lwa_ekpo,
               ltp_name.
        READ TABLE lta_ekpo INTO lwa_ekpo INDEX 1.
        IF sy-subrc = 0.
          ltp_konnr = lwa_ekpo-ebeln.
          ltp_ktpnr = lwa_ekpo-ebelp.
        ELSE.
          CLEAR: ltp_konnr,
                 ltp_ktpnr.
        ENDIF.
        CONCATENATE ltp_konnr ltp_ktpnr INTO ltp_name.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = 'K06'
            language                = 'E'
            name                    = ltp_name
            object                  = 'EKPO'
          TABLES
            lines                   = lta_text
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF sy-subrc = 0.
          CLEAR lwa_text.
          READ TABLE lta_text INTO lwa_text INDEX 1.
          IF sy-subrc = 0.
            SHIFT lwa_text-tdline LEFT DELETING LEADING space.
            IF lwa_text-tdline = '0000000000 / 00000'. "CLM Contract Number
              READ TABLE lta_text INTO lwa_text INDEX 2.
              IF sy-subrc = 0.
                CLEAR : ltp_temp1,
                        ltp_temp2,
                        ltp_clm_number.
                SPLIT lwa_text-tdline AT ':' INTO ltp_temp1 ltp_temp2.
                CLEAR ltp_temp1.
                SPLIT ltp_temp2 AT '/' INTO ltp_clm_number ltp_temp1.
                SHIFT ltp_clm_number LEFT DELETING LEADING space.
              ENDIF.
            ELSE.
              ltp_clm_number = lwa_text-tdline. "Ariba Contract Number
            ENDIF.
            lwa_z1arbh2-zextcontract = ltp_clm_number.
          ENDIF.
        ENDIF.

        CLEAR: ltp_konnr,
               ltp_ktpnr,
               ltp_temp1,
               ltp_temp2,
               ltp_clm_number.
        REFRESH: lta_text.
*       EOI by PANUSURI Ticket 91113

        CLEAR                               int_edidd.
        MOVE     'Z1ARBH1'               TO int_edidd-segnam.
        MOVE     lwa_z1arbh1             TO int_edidd-sdata.
        APPEND                              int_edidd.

*       BOI by PANUSURI Ticket 91113
        CLEAR                               int_edidd.
        MOVE     'Z1ARBH2'               TO int_edidd-segnam.
        MOVE     lwa_z1arbh2             TO int_edidd-sdata.
        APPEND                              int_edidd.
*       EOI by PANUSURI Ticket 91113

      WHEN     'E1EDP01' .

        CLEAR                               lwa_e1edp01.
        MOVE     int_edidd-sdata         TO lwa_e1edp01.

* Modify the E1EDP01 segment
        IF     ( lwa_e1edp01-vprei       IS INITIAL ).
          CLEAR                             lwa_e1edp01-vprei.
          MOVE     '0.00'                TO lwa_e1edp01-vprei.
          CLEAR                             int_edidd-sdata.
          MOVE     lwa_e1edp01           TO int_edidd-sdata.
          DESCRIBE TABLE int_edidd    LINES lv_tabix.
          MODIFY                            int_edidd
                                      INDEX lv_tabix.
        ENDIF.

* Create the Z1ARBI1 segment
        CLEAR                               lwa_z1arbi1.
        MOVE     xekpo-zzariba_approver  TO lwa_z1arbi1-zapp_id_itm.

        IF     ( xekpo-uebto             IS INITIAL ).
          MOVE   lc_tot_perc_init        TO lwa_z1arbi1-zover_tol_perc.
        ELSE.
          MOVE   xekpo-uebto             TO lwa_z1arbi1-zover_tol_perc.
        ENDIF.

        IF     ( xekpo-untto             IS INITIAL ).
          MOVE   lc_tot_perc_init        TO lwa_z1arbi1-zunder_tol_perc.
        ELSE.
          MOVE   xekpo-untto             TO lwa_z1arbi1-zunder_tol_perc.
        ENDIF.

        IF     ( xekpo-evers             IS NOT INITIAL ).
          MOVE   xekpo-evers             TO lwa_z1arbi1-zevers.

          CLEAR    lv_zevtxt.
          SELECT   SINGLE evtxt
            INTO   lv_zevtxt
            FROM   t027b
           WHERE   evers = xekpo-evers
             AND   spras = 'E'.
          IF     ( sy-subrc EQ 0 ).
            MOVE   lv_zevtxt             TO lwa_z1arbi1-zevtxt.
          ELSE.
            CLEAR                           lwa_z1arbi1-zevtxt.
          ENDIF.

        ENDIF.

        CLEAR    lv_slfdt.
        SELECT   SINGLE slfdt
          INTO   lv_slfdt
          FROM   eket
         WHERE   ebeln = xekpo-ebeln
           AND   ebelp = xekpo-ebelp.
        IF     ( sy-subrc EQ 0 ).
          MOVE   lv_slfdt                TO lwa_z1arbi1-zstdte.
        ELSE.
          CLEAR                             lwa_z1arbi1-zstdte.
        ENDIF.

        IF     ( xekko-bsart             EQ 'ZF' ).
          MOVE   xekpo-netpr             TO lwa_z1arbi1-zexpected_limit.
          MOVE   xekpo-webre             TO lwa_z1arbi1-zgr_flag.
        ENDIF.

        CLEAR    lv_ztax_code.
        SELECT   SINGLE text1
          INTO   lv_ztax_code
          FROM   t007s
         WHERE   spras = 'E'
           AND   kalsm = 'TAXCA'
           AND   mwskz = xekpo-mwskz.
        IF     ( sy-subrc EQ 0 ).
          MOVE   lv_ztax_code            TO lwa_z1arbi1-ztax_code.
        ELSE.
          CLEAR                             lwa_z1arbi1-ztax_code.
        ENDIF.

        MOVE     xekpo-mwskz             TO lwa_z1arbi1-ztax_code_id.

*       BOI by PANUSURI Ticket 91084
        CLEAR    ltp_wempf.
        SELECT   SINGLE wempf
          INTO   ltp_wempf
          FROM   ekkn
         WHERE   ebeln = xekpo-ebeln
           AND   ebelp = xekpo-ebelp
           AND   loekz = space.
        IF     ( sy-subrc EQ 0 ).
          MOVE   ltp_wempf                TO lwa_z1arbi1-zrecipient.
        ELSE.
          CLEAR                             lwa_z1arbi1-zrecipient.
        ENDIF.
*       EOI by PANUSURI Ticket 91084

        CLEAR                               int_edidd.
        MOVE     'Z1ARBI1'               TO int_edidd-segnam.
        MOVE     lwa_z1arbi1             TO int_edidd-sdata.
        APPEND                              int_edidd.

* Create the Z1ARBA1 segments
        CLEAR    lit_z1arba1[].

        PERFORM  f_get_po_attachments
                                   TABLES   lit_z1arba1
                                   USING    xekpo-ebeln
                                            xekpo-ebelp.

        IF     ( lit_z1arba1[]           IS NOT INITIAL ).

          CLEAR                             lwa_z1arba1.
          LOOP AT  lit_z1arba1         INTO lwa_z1arba1.

            CLEAR                           int_edidd.
            MOVE   'Z1ARBA1'             TO int_edidd-segnam.
            MOVE   lwa_z1arba1           TO int_edidd-sdata.
            APPEND                          int_edidd.

            CLEAR  lwa_z1arba1.
          ENDLOOP.

        ENDIF.

      WHEN     'E1EDKA1'.

        DESCRIBE TABLE int_edidd      LINES lv_tabix.
        CLEAR                               lwa_edidd.
        MOVE     int_edidd               TO lwa_edidd.
        CLEAR                               lwa_e1edka1.
        MOVE     int_edidd-sdata         TO lwa_e1edka1.

        IF     ( lwa_e1edka1-parvw       EQ 'AG' ).

          CLEAR                             lwa_edidd_p.
          READ     TABLE int_edidd     INTO lwa_edidd_p
                                   WITH KEY segnam = 'E1EDK01'.
          IF     ( sy-subrc EQ 0 ).

            CLEAR                           lwa_e1edk01_p.
            MOVE     lwa_edidd_p-sdata   TO lwa_e1edk01_p.

            CLEAR    lv_bukrs.
            SELECT   SINGLE bukrs
              INTO   lv_bukrs
              FROM   ekko
             WHERE   ebeln = lwa_e1edk01_p-belnr.
            IF     ( sy-subrc EQ 0 ).

              CLEAR    lv_butxt.
              SELECT   SINGLE butxt
                INTO   lv_butxt
                FROM   t001
               WHERE   bukrs = lv_bukrs.
              IF     ( sy-subrc EQ 0 ).

                CLEAR                       lwa_e1edka1-name1.
                MOVE   lv_butxt          TO lwa_e1edka1-name1.
                CLEAR                       lwa_edidd-sdata.
                MOVE   lwa_e1edka1       TO lwa_edidd-sdata.
                MODIFY                      int_edidd
                                       FROM lwa_edidd
                                      INDEX lv_tabix
                               TRANSPORTING sdata.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

    ENDCASE.

  ENDIF.


*Begin of insert COGs S01K900829 +++WAYCHALJ
  IF     ( lv_exttyp   EQ 'ZOPENLINK' ).

    CLEAR                                   lv_segnam.
    MOVE     int_edidd-segnam            TO lv_segnam.
    CASE     lv_segnam.
      WHEN     'E1EDK01'.

        CLEAR                               lwa_z1opnlnk3.
        MOVE     xekko-zztrloc1          TO lwa_z1opnlnk3-zztrloc1.
        MOVE     xekko-zztrloc2          TO lwa_z1opnlnk3-zztrloc2.
        MOVE     xekko-zztrloc3          TO lwa_z1opnlnk3-zztrloc3.
        MOVE     xekko-zztrloc4          TO lwa_z1opnlnk3-zztrloc4.
        MOVE     xekko-zzparty           TO lwa_z1opnlnk3-zzparty.
        MOVE     xekko-zzekgrp           TO lwa_z1opnlnk3-zztrbuyer.
        MOVE     xekko-zzparty_agmt_id   TO lwa_z1opnlnk3-zzparty_agmt_id.
        MOVE     xekko-zzoldealid        TO lwa_z1opnlnk3-zzoldealid.

        SELECT SINGLE zzmsa zzmsatype
          FROM zmmt_mastagree
          INTO (lv_zzmsa, lv_zzmsatype )
         WHERE zzparty_agmt_id = xekko-zzparty_agmt_id.
        IF ( sy-subrc EQ 0 ).
          MOVE   lv_zzmsa                TO lwa_z1opnlnk3-zzmsa.
          MOVE   lv_zzmsatype            TO lwa_z1opnlnk3-zzagreetype.
        ENDIF.
*** Append SA Openlink segment
        CLEAR                              int_edidd.
        MOVE     'Z1OPNLNK3'            TO int_edidd-segnam.
        MOVE     lwa_z1opnlnk3          TO int_edidd-sdata.
        APPEND                             int_edidd.

      WHEN     'E1EDP01' .


***Fill up Item level details
        SELECT kposn kschl kbetr waers  kpein kmein
        FROM konv
        INTO TABLE lta_konv
        WHERE knumv EQ xekko-knumv
*            AND EBELN EQ xekpo-EBELN
          AND kposn EQ xekpo-ebelp
          AND ( ( kschl EQ  'PBXX')
           OR ( kschl EQ  'ZC01')
           OR ( kschl EQ  'ZC00') )
          AND kbetr NE '0'.
        IF ( sy-subrc EQ 0 ).
          CALL FUNCTION 'GET_DOMAIN_VALUES'
            EXPORTING
              domname         = 'ZQUALF'
            TABLES
              values_tab      = lta_zqualf
            EXCEPTIONS
              no_values_found = 1
              OTHERS          = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
          CLEAR: lwa_konv.
          LOOP AT lta_konv INTO lwa_konv.
            CASE lwa_konv-kschl.

              WHEN lc_pbxx.
                CLEAR            lwa_z1itmcond.
                MOVE lwa_konv-kposn TO lwa_z1itmcond-kposn.
                MOVE lwa_konv-kbetr TO lwa_z1itmcond-kbetr.
                MOVE lwa_konv-waers TO lwa_z1itmcond-konwa.
                MOVE lwa_konv-kpein TO lwa_z1itmcond-kpein.
                MOVE lwa_konv-kmein TO lwa_z1itmcond-kmein.
                LOOP AT lta_zqualf INTO lwa_zqualf WHERE ddtext = lc_pbxx.
                  MOVE lwa_zqualf-domvalue_l  TO lwa_z1itmcond-qualf.
                ENDLOOP.
                CLEAR                       int_edidd.
                MOVE     'Z1ITMCOND'        TO int_edidd-segnam.
                MOVE     lwa_z1itmcond      TO int_edidd-sdata.
                APPEND                      int_edidd.

              WHEN lc_zc00.
                CLEAR            lwa_z1itmcond.
                IF ( lwa_konv-kbetr NE '' OR lwa_konv-kbetr NE '0' ).
                  MOVE lwa_konv-kposn TO lwa_z1itmcond-kposn.
                  LOOP AT lta_zqualf INTO lwa_zqualf WHERE ddtext = lc_zc00.
                    MOVE lwa_zqualf-domvalue_l  TO lwa_z1itmcond-qualf.
                  ENDLOOP.
                  MOVE lwa_konv-kbetr TO lwa_z1itmcond-kbetr.
                  MOVE lwa_konv-waers TO lwa_z1itmcond-konwa.
                  MOVE lwa_konv-kpein TO lwa_z1itmcond-kpein.
                  MOVE lwa_konv-kmein TO lwa_z1itmcond-kmein.

                  CLEAR                       int_edidd.
                  MOVE     'Z1ITMCOND'        TO int_edidd-segnam.
                  MOVE     lwa_z1itmcond      TO int_edidd-sdata.
                  APPEND                      int_edidd.
                ENDIF.

              WHEN lc_zc01.
                CLEAR            lwa_z1itmcond.
                IF ( lwa_konv-kbetr NE '' OR lwa_konv-kbetr NE '0' ).
                  MOVE lwa_konv-kposn TO lwa_z1itmcond-kposn.
                  LOOP AT lta_zqualf INTO lwa_zqualf WHERE ddtext = lc_zc01.
                    MOVE lwa_zqualf-domvalue_l  TO lwa_z1itmcond-qualf.
                  ENDLOOP.
                  MOVE lwa_konv-kbetr TO lwa_z1itmcond-kbetr.
                  MOVE lwa_konv-waers TO lwa_z1itmcond-konwa.
                  MOVE lwa_konv-kpein TO lwa_z1itmcond-kpein.
                  MOVE lwa_konv-kmein TO lwa_z1itmcond-kmein.

                  CLEAR                       int_edidd.
                  MOVE     'Z1ITMCOND'        TO int_edidd-segnam.
                  MOVE     lwa_z1itmcond      TO int_edidd-sdata.
                  APPEND                      int_edidd.
                ENDIF.
            ENDCASE.
          ENDLOOP.
        ENDIF.
    ENDCASE.
  ENDIF.
endif.
*Start of Changes for COG FSD07
*   IF     ( lv_exttyp   EQ 'ZOPENLINK' ).

IF ( lv_msgtyp EQ 'BLAORD' ) AND ( lv_exttyp EQ 'ZTSOPNLNK' ).
  CLEAR lv_segnam.
  MOVE int_edidd-segnam TO lv_segnam.
  CASE lv_segnam.
    WHEN     'E1EDK01'. " Transport and Stock Deals HEADER ITEM.
      CLEAR                               lwa_e1edk01.
      MOVE     int_edidd-sdata         TO lwa_e1edk01.
      CLEAR lwa_z1opnlnk3.
        MOVE     xekko-zzekgrp           TO lwa_z1opnlnk3-zztrbuyer.
        MOVE     xekko-zzparty_agmt_id   TO lwa_z1opnlnk3-zzparty_agmt_id.
        MOVE     xekko-zzoldealid        TO lwa_z1opnlnk3-zzoldealid.
      SELECT SINGLE zzmsa zzmsatype
        FROM zmmt_mastagree
        INTO (lv_zzmsa, lv_zzmsatype )
       WHERE zzparty_agmt_id = xekko-zzparty_agmt_id.
      IF ( sy-subrc EQ 0 ).
        MOVE   lv_zzmsa                TO lwa_z1opnlnk3-zzmsa.        " Master Service agreement in Purchase Order
        MOVE   lv_zzmsatype            TO lwa_z1opnlnk3-zzagreetype.  " Master Service agreeemnt type
      ENDIF.

*      SELECT SINGLE zzcareconid
*      FROM zcarereference
*        INTO lwa_z1opnlnk3-zzcareconid                                " CARE COntract ID
*        WHERE zzsapcontract = lwa_e1edk01-belnr.
*** Append Z1OPNLK3 segment
      if not lwa_z1opnlnk3 is INITIAL.
        CLEAR                              int_edidd.
        MOVE     'Z1OPNLNK3'            TO int_edidd-segnam.
        MOVE     lwa_z1opnlnk3          TO int_edidd-sdata.
        APPEND                             int_edidd.
      endif.
*    WHEN     'E1EDC20'. " Transport and Stock Deals ITEM.
*      CLEAR: lwa_e1edc20, lt_a081, lw_a081.
*      MOVE  int_edidd-sdata TO lwa_e1edc20.
*      MOVE: lwa_e1edc20-sgtyp TO lwa_e1edc20-sgtyp, " IDOC Service specs segment type
*            lwa_e1edc20-packno TO lwa_z1edccond-packno, " Pack Number
*            lwa_e1edc20-introw TO lwa_z1edccond-introw. " Internal Number
*
*      SELECT kschl kont_pack kont_zeile datbi datab knumh
*        INTO TABLE lt_a081
*        FROM a081
*        WHERE kschl = 'PRS'
*          AND kont_pack = lwa_z1edccond-packno
*          AND kont_zeile = lwa_z1edccond-introw.
*      IF lt_a081[] IS NOT INITIAL.
*        SELECT knumh kschl kbetr konwa kpein kmein
*          INTO TABLE lt_konp
*          FROM konp
*          FOR ALL ENTRIES IN lt_a081
*          WHERE knumh = lt_a081-knumh
*          AND   kschl = lt_a081-kschl.
*        IF sy-subrc = 0.
*          SORT lt_konp BY knumh kschl.
*        ENDIF.
*      ENDIF.
*      CLEAR: lw_a081.
*      LOOP AT lt_a081 INTO lw_a081.
*        lwa_z1edccond-kschl = lw_a081-kschl.  " Condition Type
*        lwa_z1edccond-datab = lw_a081-datab.  " Validity Start Date
*        lwa_z1edccond-datbi = lw_a081-datbi.  " Validity End Date
*        CLEAR: lw_konp.
*        READ TABLE lt_konp INTO lw_konp WITH KEY knumh = lw_a081-knumh
*                                                 kschl = lw_a081-kschl
*                                                 BINARY SEARCH.
*        IF sy-subrc = 0.
*          lwa_z1edccond-kbetr = lw_konp-kbetr.  " Amount
*          lwa_z1edccond-konwa = lw_konp-konwa.  " Currency
*          lwa_z1edccond-kpein = lw_konp-kpein.  " Pricing Unit
*          lwa_z1edccond-kmein = lw_konp-kmein.  " Pricing UOM
*        ENDIF.
*        CLEAR                       int_edidd.
*        MOVE     'Z1EDCCOND'        TO int_edidd-segnam.
*        MOVE     lwa_z1edccond      TO int_edidd-sdata.
*        APPEND                      int_edidd.
*      ENDLOOP.
  ENDCASE.
* endif.
*End of Changes for COG FSD07
***End of insert COGs
ENDIF.
