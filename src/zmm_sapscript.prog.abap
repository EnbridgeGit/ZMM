REPORT zmm_sascript.
************************************************************************
*    Program       :  ZMM_SAPSCRIPT
*    Programmer    :  M. DeMeester
*    Date          :  September 2004.
*
* Contains all routines for Materials Management SAPSCRIPT
************************************************************************
*  Modification History                                                *
*  Date        Name                                                    *
* 2011/06/01   BTBOUNDY               - add get_isnstatus for S2C      *
************************************************************************


FORM get_storage_location TABLES in_par STRUCTURE itcsy
                                 out_par STRUCTURE itcsy.

  DATA:  l_werks          LIKE t001l-werks,
         l_lgort          LIKE t001l-lgort,
         l_text1          LIKE t001l-lgobe,
         l_subrc          LIKE sy-subrc.

  READ TABLE in_par INDEX 1.
  l_werks       = in_par-value.

  READ TABLE in_par INDEX 2.
  l_lgort       = in_par-value.

  SELECT SINGLE lgobe INTO l_text1 FROM t001l
       WHERE werks = l_werks
         AND lgort = l_lgort.

  READ TABLE out_par INDEX 1.
  IF l_subrc <> 0 OR l_text1 IS INITIAL.
    out_par-value = 'NOT AVAILABLE'.
  ELSE.
    out_par-value = l_text1.
  ENDIF.

  MODIFY out_par INDEX sy-tabix.

ENDFORM.                 "End of GET_STORAGE_LOCATION
************************************************************************
* get_isnstatus: Returns maximum 2 lines of isn vendor information
* from a specific PO/Vendor
************************************************************************
FORM  get_isnstatus TABLES in_variables STRUCTURE itcsy
                           out_variables STRUCTURE itcsy.


  DATA: lv_lifnr    TYPE lifnr,
        lv_ebeln    TYPE ebeln,
        lv_bstyp    TYPE bstyp,
        lv_bsart    TYPE bsart,
        lv_isnvdr   LIKE lfa1-zzisnvdr,
        ls_zvar     TYPE zvar,
        lt_zvar     LIKE TABLE OF ls_zvar,
        ls_ekpo     TYPE ekpo,
        lt_ekpo     LIKE TABLE OF ls_ekpo,
        ls_adrc     TYPE adrc,
        ls_zisnqual TYPE zisnqual,
        lt_zisnqual LIKE TABLE OF ls_zisnqual,
        lv_int      TYPE integer.

  DATA: BEGIN OF ls_isnplants,
          land1   LIKE zisnqual-land1,
          regio   LIKE zisnqual-regio,
          isnqual LIKE zisnqual-isnqual,
        END OF ls_isnplants,
        lt_isnplants LIKE TABLE OF ls_isnplants.

  READ TABLE in_variables WITH KEY 'EKKO-LIFNR'.
  lv_lifnr = in_variables-value.

  SHIFT lv_lifnr RIGHT DELETING TRAILING space.
  TRANSLATE lv_lifnr USING ' 0'.

  READ TABLE in_variables WITH KEY 'EKKO-EBELN'.
  lv_ebeln = in_variables-value.

  SHIFT lv_ebeln RIGHT DELETING TRAILING space.
  TRANSLATE lv_ebeln USING ' 0'.

  READ TABLE in_variables WITH KEY 'EKKO-BSTYP'.
  lv_bstyp = in_variables-value.

  READ TABLE in_variables WITH KEY 'EKKO-BSART'.
  lv_bsart = in_variables-value.

  "Default the output to not display anything.
  out_variables-name = 'ISNPROV1'.
  out_variables-value = ''.
  MODIFY out_variables INDEX 1.

  out_variables-name = 'ISNPROV2'.
  out_variables-value = ''.
  MODIFY out_variables INDEX 2.


  "Get ZVAR variables.
  SELECT *
    FROM zvar INTO CORRESPONDING FIELDS OF TABLE lt_zvar
    WHERE programm = 'ZMM_SAPSCRIPT'
  .

  "Check if the current BSTYP and BSART are valid.
  READ TABLE lt_zvar
         WITH KEY varname = 'BSTYP'
                  value1  = lv_bstyp
         INTO ls_zisnqual.

  IF sy-subrc <> 0.
    "Value not found in table.
    EXIT.
  ENDIF.

  READ TABLE lt_zvar
         WITH KEY varname = 'BSART'
                  value1  = lv_bsart
         INTO ls_zisnqual.

  IF sy-subrc <> 0.
    "Value not found in table.
    EXIT.
  ENDIF.

  "Check if the vendor has isn information
  SELECT zisnqual~isnvdr zisnqual~land1 zisnqual~regio
         zisnqual~isnqual
    INTO CORRESPONDING FIELDS OF TABLE lt_zisnqual
    FROM zisnqual INNER JOIN lfa1
      ON lfa1~zzisnvdr = zisnqual~isnvdr
    WHERE lfa1~lifnr = lv_lifnr
  .

  IF sy-subrc <> 0.
    "No ISNVendor information so do not display.
    EXIT.
  ENDIF.

  "Get the list of plants on this PO.
  SELECT werks adrn2 lgort
    FROM ekpo
    INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
    WHERE ebeln = lv_ebeln
      AND loekz = ''
  .


  "Create a list of locations from the plant.
  LOOP AT lt_ekpo INTO ls_ekpo.


    IF ls_ekpo-adrn2 IS NOT INITIAL.
      "Number of Deliver Adress
      "Do nothing adrn2 is already populated

    ELSEIF ls_ekpo-lgort IS NOT INITIAL.
      "Check Storage location
      SELECT SINGLE adrnr
        INTO ls_ekpo-adrn2
        FROM twlad
        WHERE lgort = ls_ekpo-lgort
          AND werks = ls_ekpo-werks
      .

      IF sy-subrc <> 0.
        "Use Plant
        SELECT SINGLE adrnr
          INTO ls_ekpo-adrn2
          FROM t001w
          WHERE werks = ls_ekpo-werks
        .
      ENDIF.

    ELSE.
      "Use Plant
      SELECT SINGLE adrnr
        INTO ls_ekpo-adrn2
        FROM t001w
        WHERE werks = ls_ekpo-werks
      .

    ENDIF.

    SELECT SINGLE country region
      FROM adrc
      INTO CORRESPONDING FIELDS OF ls_adrc
      WHERE addrnumber = ls_ekpo-adrn2
        AND langu      = 'E'
    .

    IF sy-subrc = 0.
      ls_isnplants-land1 = ls_adrc-country.
      ls_isnplants-regio = ls_adrc-region.
      APPEND ls_isnplants TO lt_isnplants.
    ENDIF.
  ENDLOOP.

  "Get rid of duplicates
  SORT lt_isnplants BY land1 regio.
  DELETE ADJACENT DUPLICATES FROM lt_isnplants COMPARING land1 regio.




  CLEAR lv_int.

  LOOP AT lt_isnplants INTO ls_isnplants.
    READ TABLE lt_zisnqual
        WITH KEY land1 = ls_isnplants-land1 regio = ls_isnplants-regio
        INTO ls_zisnqual.

    IF sy-subrc = 0.
      IF lv_int = 0.
        out_variables-name = 'ISNPROV1'.
        IF ls_zisnqual-isnqual = 'P'.
          CONCATENATE ls_zisnqual-regio ' - Pass' INTO out_variables-value.
        ELSEIF ls_zisnqual-isnqual = 'F'.
          CONCATENATE ls_zisnqual-regio ' - Fail' INTO out_variables-value.
        ELSE.
          CONCATENATE ls_zisnqual-regio ' - ' ls_zisnqual-isnqual INTO out_variables-value.
        ENDIF.
        MODIFY out_variables INDEX 1.
        lv_int = lv_int + 1.
      ELSEIF lv_int = 1.
        out_variables-name = 'ISNPROV2'.
        IF ls_zisnqual-isnqual = 'P'.
          CONCATENATE ls_zisnqual-regio ' - Pass' INTO out_variables-value.
        ELSEIF ls_zisnqual-isnqual = 'F'.
          CONCATENATE ls_zisnqual-regio ' - Fail' INTO out_variables-value.
        ELSE.
          CONCATENATE ls_zisnqual-regio ' - ' ls_zisnqual-isnqual INTO out_variables-value.
        ENDIF.
        MODIFY out_variables INDEX 2.
        lv_int = lv_int + 1.
        "This is the second one, so exit
        EXIT.
      ELSE.
        "Something weird happend with counter.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "GET_ISNSTATUS
*&---------------------------------------------------------------------*
*&      Form  get_MPARTNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM  get_mpartno TABLES in_par STRUCTURE itcsy
                         out_par STRUCTURE itcsy.

  DATA: lv_matnr TYPE mara-bmatn,
        lv_mtart TYPE mara-mtart, " VALUE 'HERS',
        lt_mara TYPE TABLE OF mara,
        ls_mara TYPE mara.


  CLEAR out_par-value.
  READ TABLE in_par INDEX 1.
  lv_matnr = in_par-value.
  lv_mtart = 'HERS'.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input              = lv_matnr
   IMPORTING
     output             = lv_matnr
*  EXCEPTIONS
*    LENGTH_ERROR       = 1
*    OTHERS             = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  SELECT * FROM mara INTO TABLE lt_mara WHERE bmatn = lv_matnr
                                          AND mtart = lv_mtart.
  LOOP AT lt_mara INTO ls_mara.
    CHECK ls_mara-mfrpn IS NOT INITIAL.
    IF out_par-value IS NOT INITIAL.
      CONCATENATE out_par-value ls_mara-mfrpn INTO out_par-value SEPARATED BY ','.
    ELSE.
      out_par-value = ls_mara-mfrpn.
    ENDIF.
  ENDLOOP.
  IF out_par-value IS NOT INITIAL.
    CONCATENATE 'PN:' out_par-value INTO out_par-value. " separated by space.
    MODIFY out_par INDEX 1 TRANSPORTING value.
  ENDIF.

ENDFORM.                    "GET_MPARTNO
