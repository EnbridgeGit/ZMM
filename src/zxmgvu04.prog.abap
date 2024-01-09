*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(MESSAGE_TYPE) LIKE  EDIDC-MESTYP
*"             VALUE(F_CUST_SEGMENT) LIKE  EDIDD STRUCTURE  EDIDD
*"       TABLES
*"              RES_FIELDS STRUCTURE  DELFIELDS
*"       CHANGING
*"             VALUE(F_MARA_UEB) LIKE  MARA_UEB STRUCTURE  MARA_UEB
*"                             OPTIONAL
*"             VALUE(F_MAKT_UEB) LIKE  MAKT_UEB STRUCTURE  MAKT_UEB
*"                             OPTIONAL
*"             VALUE(F_MARC_UEB) LIKE  MARC_UEB STRUCTURE  MARC_UEB
*"                             OPTIONAL
*"             VALUE(F_MARD_UEB) LIKE  MARD_UEB STRUCTURE  MARD_UEB
*"                             OPTIONAL
*"             VALUE(F_MFHM_UEB) LIKE  MFHM_UEB STRUCTURE  MFHM_UEB
*"                             OPTIONAL
*"             VALUE(F_MPGD_UEB) LIKE  MPGD_UEB STRUCTURE  MPGD_UEB
*"                             OPTIONAL
*"             VALUE(F_MPOP_UEB) LIKE  MPOP_UEB STRUCTURE  MPOP_UEB
*"                             OPTIONAL
*"             VALUE(F_MPRW_UEB) LIKE  MPRW_UEB STRUCTURE  MPRW_UEB
*"                             OPTIONAL
*"             VALUE(F_MVEG_UEB) LIKE  MVEG_UEB STRUCTURE  MVEG_UEB
*"                             OPTIONAL
*"             VALUE(F_MVEU_UEB) LIKE  MVEU_UEB STRUCTURE  MVEU_UEB
*"                             OPTIONAL
*"             VALUE(F_MARM_UEB) LIKE  MARM_UEB STRUCTURE  MARM_UEB
*"                             OPTIONAL
*"             VALUE(F_MEAN_UEB) LIKE  MEA1_UEB STRUCTURE  MEA1_UEB
*"                             OPTIONAL
*"             VALUE(F_MBEW_UEB) LIKE  MBEW_UEB STRUCTURE  MBEW_UEB
*"                             OPTIONAL
*"             VALUE(F_MLGN_UEB) LIKE  MLGN_UEB STRUCTURE  MLGN_UEB
*"                             OPTIONAL
*"             VALUE(F_MVKE_UEB) LIKE  MVKE_UEB STRUCTURE  MVKE_UEB
*"                             OPTIONAL
*"             VALUE(F_MLGT_UEB) LIKE  MLGT_UEB STRUCTURE  MLGT_UEB
*"                             OPTIONAL
*"       EXCEPTIONS
*"              APPLICATION_ERROR
*"----------------------------------------------------------------------


*&---------------------------------------------------------------------*
*&  Include           ZXMGVU04
*&---------------------------------------------------------------------*
DATA: wa_z1mara TYPE z1mara.
IF message_type EQ 'MATMAS'.
  IF f_cust_segment-segnam EQ 'Z1MARA'.
    wa_z1mara = f_cust_segment-sdata.
    MOVE-CORRESPONDING wa_z1mara TO f_mara_ueb.
  ENDIF.
ENDIF.
