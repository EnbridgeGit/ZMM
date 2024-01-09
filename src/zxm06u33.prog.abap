*&---------------------------------------------------------------------*
*&  Include           ZXM06U33
*&---------------------------------------------------------------------*
*-------------------------MODIFICATION LOG-----------------------------*
* DATE      PROGRAMMER  TR#         DESCRIPTION                        *
*---------- ----------  ----------  -----------------------------------*
*05/27/2021 NAGIRIR     S01K900829  For Project COG FSD07, Transport & *
*                                   Storage deals from SAP to OpenLink *
*03/29/2022 NAGIRIR     D30K932120  Add Deletion Flag to KONP SELECT to*
*                                   get accurate/valid condition types *
*&---------------------------------------------------------------------*
*Begin of insert for COG FSD07
DATA:lwa_z1edccond    TYPE z1edccond,
     lwa_e1edc20      TYPE e1edc20.

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

DATA:   lv_msgtyp        TYPE edi_mestyp,
        lv_exttyp        TYPE edi_cimtyp,
        lv_segnam        TYPE edilsegtyp,
        lv_edidd TYPE  edidd,
        v_tabix TYPE sy-tabix.

CLEAR                                       lv_msgtyp.
MOVE     control_record_out-mestyp       TO lv_msgtyp.
CLEAR                                       lv_exttyp.
MOVE     control_record_out-cimtyp       TO lv_exttyp.
IF ( lv_msgtyp EQ 'BLAORD' ) AND ( lv_exttyp EQ 'ZTSOPNLNK' ).
  CLEAR: lv_segnam,v_tabix,lv_edidd,lwa_z1edccond,lwa_e1edc20.
  MOVE dint_edidd-segnam TO lv_segnam.
  LOOP AT dint_edidd INTO lv_edidd WHERE segnam = 'E1EDC20'.
    v_tabix = sy-tabix + 1.
    CLEAR: lwa_e1edc20, lt_a081, lw_a081.
    MOVE  lv_edidd-sdata TO lwa_e1edc20.
    MOVE: lwa_e1edc20-sgtyp TO lwa_e1edc20-sgtyp, " IDOC Service specs segment type
          lwa_e1edc20-packno TO lwa_z1edccond-packno, " Pack Number
          lwa_e1edc20-introw TO lwa_z1edccond-introw. " Internal Number

    SELECT kschl kont_pack kont_zeile datbi datab knumh
      INTO TABLE lt_a081
      FROM a081
      WHERE kschl = 'PRS'
        AND kont_pack = lwa_z1edccond-packno
        AND kont_zeile = lwa_z1edccond-introw.
    IF lt_a081[] IS NOT INITIAL.
      SELECT knumh kschl kbetr konwa kpein kmein
        INTO TABLE lt_konp
        FROM konp
        FOR ALL ENTRIES IN lt_a081
        WHERE knumh = lt_a081-knumh
        AND   kschl = lt_a081-kschl
        AND   loevm_ko = space. " Added for Change D30K932120
      IF sy-subrc = 0.
        SORT lt_konp BY knumh kschl.
      ENDIF.
    ENDIF.
    CLEAR: lw_a081.
    LOOP AT lt_a081 INTO lw_a081.
      lwa_z1edccond-packno = lw_a081-kont_pack.
      lwa_z1edccond-introw = lw_a081-kont_zeile.
      lwa_z1edccond-kschl = lw_a081-kschl.  " Condition Type
      lwa_z1edccond-datab = lw_a081-datab.  " Validity Start Date
      lwa_z1edccond-datbi = lw_a081-datbi.  " Validity End Date
      CLEAR: lw_konp.
      READ TABLE lt_konp INTO lw_konp WITH KEY knumh = lw_a081-knumh
                                               kschl = lw_a081-kschl
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_z1edccond-kbetr = lw_konp-kbetr.  " Amount
        lwa_z1edccond-konwa = lw_konp-konwa.  " Currency
        lwa_z1edccond-kpein = lw_konp-kpein.  " Pricing Unit
        lwa_z1edccond-kmein = lw_konp-kmein.  " Pricing UOM
      ENDIF.
      CLEAR                       dint_edidd.
      MOVE     'Z1EDCCOND'        TO dint_edidd-segnam.
      MOVE     lwa_z1edccond      TO dint_edidd-sdata.
      INSERT dint_edidd INDEX v_tabix." into
      v_tabix = v_tabix + 1.
      CLEAR lwa_z1edccond.
    ENDLOOP.
    CLEAR v_tabix.
  ENDLOOP.
ENDIF.
*End of Changes for COG FSD07
