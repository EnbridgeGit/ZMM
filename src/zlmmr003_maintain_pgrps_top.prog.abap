*&---------------------------------------------------------------------*
*&  Include           ZLMMR003_MAINTAIN_PGRPS_TOP
*&---------------------------------------------------------------------*
TABLES: zmms_maintain_pgrps.
CONTROLS: tc_pgrps      TYPE TABLEVIEW USING SCREEN '0100',
          tc_pgrps_new  TYPE TABLEVIEW USING SCREEN '0101',
          tc_pgrps_cpy  TYPE TABLEVIEW USING SCREEN '0102',
          tc_pgrps_nerr TYPE TABLEVIEW USING SCREEN '0103'.
TYPES: BEGIN OF ty_t024,
         ekgrp       TYPE ekgrp,
         eknam       TYPE eknam,
         ektel       TYPE ektel,
         telfx       TYPE ektfx,
         smtp_addr   TYPE ad_smtpadr,
       END OF ty_t024,
       BEGIN OF ty_t160ex,
         ekgrp     TYPE ekgrp,
       END OF ty_t160ex.
DATA: ta_pgrps        TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_pgrps        TYPE zmms_maintain_pgrps,
      ta_pgrps_tmp    TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_pgrps_tmp    TYPE zmms_maintain_pgrps,
      ta_pgrps_del    TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_pgrps_del    TYPE zmms_maintain_pgrps,
      ta_pgrps_ins    TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_pgrps_ins    TYPE zmms_maintain_pgrps,
      ta_new_pgrps    TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_new_pgrps    TYPE zmms_maintain_pgrps,
      ta_cpy_pgrps     TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_cpy_pgrps     TYPE zmms_maintain_pgrps,
      ta_err_pgrps     TYPE STANDARD TABLE OF zmms_maintain_pgrps,
      wa_err_pgrps    TYPE zmms_maintain_pgrps,
      ta_t024         TYPE STANDARD TABLE OF ty_t024,
      wa_t024         TYPE ty_t024,
      ta_t024_tmp     TYPE STANDARD TABLE OF t024,
      wa_t024_tmp     TYPE t024,
      ta_del_t024     TYPE STANDARD TABLE OF t024,
      ta_t160ex       TYPE STANDARD TABLE OF ty_t160ex,
      wa_t160ex       TYPE ty_t160ex,
      ta_t160ex_tmp   TYPE STANDARD TABLE OF t160ex,
      wa_t160ex_tmp   TYPE t160ex,
      tp_okcode      TYPE sy-ucomm,
      tp_new_okcode  TYPE sy-ucomm,
      tp_cpy_okcode  TYPE sy-ucomm,
      tp_err_okcode  TYPE sy-ucomm,
      tp_ekgrp       TYPE ekgrp,
      tp_answer      TYPE c,
      tp_disp        TYPE c,
      tp_chng        TYPE c,
      tp_msg         TYPE c,
      tp_lock        TYPE c,
      tp_error       TYPE c,
      tp_modify      TYPE c.
