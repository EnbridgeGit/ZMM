"Name: \PR:SAPLVKOI\FO:CONV_UNIT_ISO_TO_SAP\SE:BEGIN\EI
ENHANCEMENT 0 ZLVKOIF01.
*BTBOUNDY
*P2P UOM Ratification
  "If UOM is already in SAP UNIT, do not attempt to convert from ISO
  DATA: ls_t006 TYPE T006.
  SELECT SINGLE MSEHI
    FROM T006
    INTO ls_t006
    WHERE MSEHI = isocode
  .

  IF sy-subrc = 0.
    "The ISOCODE is already in SAP format, do not convert.
    sapcode = isocode.
    EXIT.
  ENDIF.
ENDENHANCEMENT.
