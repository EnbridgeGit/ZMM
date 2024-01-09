*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(MESSAGE_TYPE) LIKE  EDIDC-MESTYP
*"             VALUE(SEGMENT_NAME) LIKE  EDIDD-SEGNAM
*"             VALUE(F_BDIEINA) LIKE  BDIEINA STRUCTURE  BDIEINA
*"                             OPTIONAL
*"             VALUE(F_BDIEINE) LIKE  BDIEINE STRUCTURE  BDIEINE
*"                             OPTIONAL
*"       EXPORTING
*"             VALUE(IDOC_CIMTYPE) LIKE  EDIDC-CIMTYP
*"       TABLES
*"              IDOC_DATA STRUCTURE  EDIDD
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXMALU03
*&---------------------------------------------------------------------*

TABLES: e1einam, z1mara.

CASE message_type.
  WHEN 'ZZINFREC'.
    idoc_cimtype = 'ZEINA01'.
    CASE segment_name.
      WHEN 'E1EINAM'.
        READ TABLE idoc_data WITH KEY segnam = 'E1EINAM'.
        e1einam = idoc_data-sdata.
        CLEAR z1mara.
        SELECT SINGLE zzmdmid FROM mara INTO CORRESPONDING FIELDS OF z1mara WHERE matnr EQ e1einam-matnr.
        IF sy-subrc EQ 0.
          idoc_data-segnam = 'Z1MARA'.
          idoc_data-sdata = z1mara.
          APPEND idoc_data.
        ENDIF.
    ENDCASE.
ENDCASE.
