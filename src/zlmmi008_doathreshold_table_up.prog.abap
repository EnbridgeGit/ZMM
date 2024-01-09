*&---------------------------------------------------------------------*
*& Report ZMMR_THRESHOLD_TABLE_UPDATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zlmmi008_doathreshold_table_up.

TYPES : BEGIN OF ty_sc_details,
  object_id      TYPE  char10,
  description    TYPE  char40,
  created_at     TYPE  char15,
  changed_at     TYPE  char15,
  zzcumulval     TYPE  ekpo-netwr,
  be_object_id   TYPE  ekko-ebeln,
  doa_approver   TYPE  char12,
    END OF ty_sc_details.

TYPES : BEGIN OF ty_ekko,
        ebeln TYPE ekko-ebeln,
        aedat TYPE ekko-aedat,
        ekgrp TYPE ekko-ekgrp,
        END OF ty_ekko,

        BEGIN OF ty_ekpo,
        ebeln  TYPE ekpo-ebeln,
        ebelp  TYPE ekpo-ebelp,
        afnam  TYPE ekpo-afnam,
        netwr  TYPE ekpo-netwr,
        packno TYPE ekpo-packno,
        END OF ty_ekpo,

        BEGIN OF ty_zmmt_thresh_rep,
          mandt           TYPE  zmmt_thresh_rep-mandt,
          aedat           TYPE  zmmt_thresh_rep-aedat,
          ebeln           TYPE  zmmt_thresh_rep-ebeln,
          shopng_crt_no   TYPE  zmmt_thresh_rep-shopng_crt_no,
          shopng_decrip   TYPE  zmmt_thresh_rep-shopng_decrip,
          netpr           TYPE  zmmt_thresh_rep-netpr,
          net_price       TYPE  zmmt_thresh_rep-net_price,
          afnam           TYPE  zmmt_thresh_rep-afnam,
          ekgrp           TYPE  zmmt_thresh_rep-ekgrp,
          doa_approver    TYPE  zmmt_thresh_rep-doa_approver,
          apr_confirm     TYPE  zmmt_thresh_rep-apr_confirm,
          confrim_date    TYPE  zmmt_thresh_rep-confrim_date,
          confrim_time    TYPE  zmmt_thresh_rep-confrim_time,
          ernam           TYPE  zmmt_thresh_rep-ernam,
          comments        TYPE  zmmt_thresh_rep-comments,
          END OF ty_zmmt_thresh_rep,

*BOI PANUSURI Ticket 34265
        BEGIN OF ty_threshold,
        aedat            TYPE  zmmt_thresh_rep-aedat,
        ebeln            TYPE  zmmt_thresh_rep-ebeln,
        shopng_crt_no    TYPE  zmmt_thresh_rep-shopng_crt_no,
        END OF ty_threshold.
*EOI PANUSURI Ticket 34265

DATA : it_sc_details      TYPE STANDARD TABLE OF ty_sc_details,
       it_ekko            TYPE STANDARD TABLE OF ty_ekko,
       it_zmmt_thresh_rep TYPE STANDARD TABLE OF ty_zmmt_thresh_rep,
       it_ekpo            TYPE STANDARD TABLE OF ty_ekpo,
       wa_zmmt_thresh_rep TYPE ty_zmmt_thresh_rep,
       wa_ekko            TYPE ty_ekko,
       wa_ekpo            TYPE ty_ekpo,
       wa_sc_details      TYPE ty_sc_details,

*BOI PANUSURI Ticket 34265
      git_threshold       TYPE TABLE OF ty_threshold,
      gwa_threshold       TYPE ty_threshold,
      git_threshold_final TYPE TABLE OF ty_zmmt_thresh_rep,
      it_po               TYPE zmmt_purchase_order,
      wa_po               TYPE zmms_purchase_order.
*EOI PANUSURI Ticket 34265

DATA : lv_netwr           TYPE ekpo-netpr,
       lv_afnam           TYPE ekpo-afnam,
       lv_sumlimit        TYPE esuh-sumlimit,
       lv_date            TYPE sy-datum.

CONSTANTS :
            co_i          TYPE char1            VALUE 'I',
            co_varsrm     TYPE zvarsys-varname  VALUE 'SRM_RFC',
            co_prosrm     TYPE zvarsys-programm VALUE 'LOGSYS',
            co_varnum     TYPE zvarsys-varnum   VALUE '1'.
* Define varaibles
DATA : ltp_rfcdest       TYPE rfcdisplay-rfcdest.

PARAMETERS: p_dates TYPE sy-datum OBLIGATORY,
            p_datee TYPE sy-datum.

* Select RFC destination for SRM system
SELECT SINGLE  value1
         FROM  zvarsys
         INTO  ltp_rfcdest
         WHERE programm = co_prosrm
         AND   varname  = co_varsrm
         AND   varnum   = co_varnum.
IF sy-subrc <> 0.
  MESSAGE e009(zmm_message)  .
  EXIT.
ENDIF.

IF p_datee IS INITIAL.
  p_datee = p_dates.
ENDIF.

lv_date = p_dates.

*WHILE lv_date <= p_datee."(-)PANUSURI Ticket 34265

*BOI PANUSURI Ticket 34265
*Get purchase orders based on date range.
SELECT ebeln
       aedat
       ekgrp
  INTO TABLE it_ekko
  FROM ekko
  WHERE aedat >= p_dates
  AND   aedat <= p_datee.

IF it_ekko IS NOT INITIAL.
  SELECT ebeln
         ebelp
         afnam
         netwr
         packno
    FROM ekpo
    INTO TABLE it_ekpo
    FOR ALL ENTRIES IN it_ekko
    WHERE ebeln = it_ekko-ebeln.
ENDIF.

LOOP AT it_ekko INTO wa_ekko.
  wa_po-be_object_id = wa_ekko-ebeln.
  APPEND wa_po TO it_po.
  CLEAR wa_po.
ENDLOOP.
*EOI PANUSURI Ticket 34265
if it_po is not initial."(+)PANUSURI Ticket 34265
  CALL FUNCTION 'Z_GET_SC_DETAILS'
    DESTINATION ltp_rfcdest
    EXPORTING
*     imp_datum      = lv_date "(-)PANUSURI Ticket 34265
      imp_po         = it_po[] "(+)PANUSURI Ticket 34265
    IMPORTING
      exp_sc_details = it_sc_details.

  IF it_sc_details IS NOT INITIAL.
*BOC PANUSURI Ticket 34265
*    SELECT ebeln
*           aedat
*           ekgrp
*      INTO TABLE it_ekko
*      FROM ekko
*      FOR ALL ENTRIES IN it_sc_details
*      WHERE ebeln = it_sc_details-be_object_id
*    .
*
*    IF it_ekko IS NOT INITIAL.
*      SELECT ebeln
*             ebelp
*             afnam
*             netwr
*             packno
*        FROM ekpo
*        INTO TABLE it_ekpo
*        FOR ALL ENTRIES IN it_ekko
*        WHERE ebeln = it_ekko-ebeln.
*    ENDIF.
*EOC PANUSURI Ticket 34265
    LOOP AT it_sc_details INTO wa_sc_details.

      wa_zmmt_thresh_rep-mandt         = sy-mandt.
      wa_zmmt_thresh_rep-ebeln         = wa_sc_details-be_object_id .
      wa_zmmt_thresh_rep-shopng_crt_no = wa_sc_details-object_id  .
      wa_zmmt_thresh_rep-shopng_decrip = wa_sc_details-description .
      wa_zmmt_thresh_rep-netpr         = wa_sc_details-zzcumulval .

      READ TABLE it_ekko INTO wa_ekko
        WITH KEY ebeln = wa_sc_details-be_object_id.

      IF sy-subrc = 0.
        wa_zmmt_thresh_rep-aedat = wa_ekko-aedat .
        wa_zmmt_thresh_rep-ekgrp = wa_ekko-ekgrp .

        CLEAR: lv_netwr,lv_afnam.
        LOOP AT it_ekpo INTO wa_ekpo
          WHERE ebeln = wa_ekko-ebeln.

          IF lv_afnam IS INITIAL.
            lv_afnam = wa_ekpo-afnam.
          ENDIF.

          SELECT SINGLE sumlimit
                 FROM esuh
                 INTO lv_sumlimit
                  WHERE packno = wa_ekpo-packno.
          IF sy-subrc = 0.
            lv_netwr = lv_netwr + lv_sumlimit.
          ELSE.
            lv_netwr = lv_netwr + wa_ekpo-netwr.
          ENDIF.

        ENDLOOP.

        wa_zmmt_thresh_rep-afnam = lv_afnam .
        IF wa_sc_details-doa_approver IS NOT INITIAL.
          wa_zmmt_thresh_rep-doa_approver  = wa_sc_details-doa_approver .
        ELSE.
          wa_zmmt_thresh_rep-doa_approver = lv_afnam.
        ENDIF.

        wa_zmmt_thresh_rep-net_price = lv_netwr .

        APPEND wa_zmmt_thresh_rep TO it_zmmt_thresh_rep.
      ENDIF.
    ENDLOOP.
*BOI PANUSURI Ticket 34265
*Check for duplicate records
    IF it_zmmt_thresh_rep IS NOT INITIAL.
      REFRESH git_threshold.
      SELECT aedat ebeln shopng_crt_no
             FROM zmmt_thresh_rep
             INTO TABLE git_threshold
             FOR ALL ENTRIES IN it_zmmt_thresh_rep
             WHERE aedat = it_zmmt_thresh_rep-aedat AND
                   ebeln = it_zmmt_thresh_rep-ebeln AND
                   shopng_crt_no = it_zmmt_thresh_rep-shopng_crt_no.

      CLEAR wa_zmmt_thresh_rep.
      LOOP AT it_zmmt_thresh_rep INTO wa_zmmt_thresh_rep.
        READ TABLE git_threshold
                   WITH KEY aedat = wa_zmmt_thresh_rep-aedat
                            ebeln = wa_zmmt_thresh_rep-ebeln
                            shopng_crt_no = wa_zmmt_thresh_rep-shopng_crt_no
                            TRANSPORTING NO FIELDS.

        IF sy-subrc <> 0.
          APPEND wa_zmmt_thresh_rep TO git_threshold_final.
        ENDIF.
      ENDLOOP.
    ENDIF.
*EOI PANUSURI Ticket 34265
*  MODIFY zmmt_thresh_rep FROM TABLE it_zmmt_thresh_rep."(-)PANUSURI Ticket 34265
*BOI PANUSURI Ticket 34265
    IF git_threshold_final IS NOT INITIAL.
      INSERT zmmt_thresh_rep FROM TABLE git_threshold_final.
      COMMIT WORK.
*EOI PANUSURI Ticket 34265
      IF sy-subrc = 0.
        WRITE:/ lv_date, text-001.
      ELSE.
        WRITE:/ lv_date, text-002.
      ENDIF.
*BOI PANUSURI Ticket 34265
      REFRESH git_threshold_final.
    ELSE.
      WRITE:/ text-005.
    endif.
*EOI PANUSURI Ticket 34265
  ELSE.
    WRITE:/ lv_date, text-003.
  ENDIF.
*BOI PANUSURI Ticket 34265
ELSE.
  WRITE:/ text-004.
endif.
*EOI PANUSURI Ticket 34265
*lv_date = lv_date + 1."(-)PANUSURI Ticket 34265
*ENDWHILE."(-)PANUSURI Ticket 34265
