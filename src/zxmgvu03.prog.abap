*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(MESSAGE_TYPE) LIKE  EDMSG-MSGTYP
*"             VALUE(SEGMENT_NAME) LIKE  EDIDD-SEGNAM
*"             VALUE(F_MARA) LIKE  MARA STRUCTURE  MARA OPTIONAL
*"             VALUE(F_MAKT) LIKE  MAKT STRUCTURE  MAKT OPTIONAL
*"             VALUE(F_MARC) LIKE  MARC STRUCTURE  MARC OPTIONAL
*"             VALUE(F_MARD) LIKE  MARD STRUCTURE  MARD OPTIONAL
*"             VALUE(F_MFHM) LIKE  MFHM STRUCTURE  MFHM OPTIONAL
*"             VALUE(F_MPGD) LIKE  MPGD STRUCTURE  MPGD OPTIONAL
*"             VALUE(F_MPOP) LIKE  MPOP STRUCTURE  MPOP OPTIONAL
*"             VALUE(F_MPRW) LIKE  MPRW STRUCTURE  MPRW OPTIONAL
*"             VALUE(F_MVEG) LIKE  MVEG STRUCTURE  MVEG OPTIONAL
*"             VALUE(F_MVEU) LIKE  MVEU STRUCTURE  MVEU OPTIONAL
*"             VALUE(F_MKAL) LIKE  MKAL STRUCTURE  MKAL OPTIONAL
*"             VALUE(F_MARM) LIKE  MARM STRUCTURE  MARM OPTIONAL
*"             VALUE(F_MEAN) LIKE  MEAN STRUCTURE  MEAN OPTIONAL
*"             VALUE(F_MBEW) LIKE  MBEW STRUCTURE  MBEW OPTIONAL
*"             VALUE(F_MLGN) LIKE  MLGN STRUCTURE  MLGN OPTIONAL
*"             VALUE(F_MVKE) LIKE  MVKE STRUCTURE  MVKE OPTIONAL
*"             VALUE(F_MLAN) LIKE  MLAN STRUCTURE  MLAN OPTIONAL
*"             VALUE(F_MLGT) LIKE  MLGT STRUCTURE  MLGT OPTIONAL
*"       EXPORTING
*"             VALUE(IDOC_CIMTYPE) LIKE  EDIDC-CIMTYP
*"       TABLES
*"              IDOC_DATA STRUCTURE  EDIDD
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXMGVU03
*&---------------------------------------------------------------------*

TABLES: e1maram, z1mara.
DATA: zbmatn LIKE mara-bmatn.

CASE message_type.
  WHEN 'MATMAS' or 'ZZMATMASCONF'.
    idoc_cimtype = 'ZMARA01'.
    CASE segment_name.
      WHEN 'E1MARAM'.

        READ TABLE idoc_data WITH KEY segnam = 'E1MARAM'.
        e1maram = idoc_data-sdata.
        clear z1mara.
        SELECT SINGLE zzmdmid FROM mara INTO CORRESPONDING FIELDS OF z1mara WHERE matnr EQ e1maram-matnr.
        IF sy-subrc EQ 0.
          idoc_data-segnam = 'Z1MARA'.
          idoc_data-sdata = z1mara.
          APPEND idoc_data.
        ENDIF.
    ENDCASE.
  WHEN 'ZZMATMAS'.
    idoc_cimtype = 'ZMARA01'.
    CASE segment_name.
      WHEN 'E1MARAM'.

        READ TABLE idoc_data WITH KEY segnam = 'E1MARAM'.
        e1maram = idoc_data-sdata.
        SELECT SINGLE bmatn FROM mara INTO zbmatn WHERE matnr EQ e1maram-matnr.

        IF sy-subrc EQ 0.
          SELECT SINGLE zzmdmid FROM mara INTO CORRESPONDING FIELDS OF z1mara WHERE matnr EQ zbmatn.
        ENDIF.

        IF sy-subrc EQ 0.
          idoc_data-segnam = 'Z1MARA'.
          idoc_data-sdata = z1mara.
          APPEND idoc_data.
        ENDIF.

      WHEN OTHERS.
        EXIT.
    ENDCASE.


  WHEN OTHERS.
    EXIT.
ENDCASE.
