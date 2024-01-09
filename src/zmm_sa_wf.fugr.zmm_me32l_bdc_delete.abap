FUNCTION ZMM_ME32L_BDC_DELETE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_EBELN) TYPE  EKPO-EBELN
*"  TABLES
*"      BDC_RELEASE_MSG STRUCTURE  TLINE
*"      ERROR_MSG STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
DATA: lv_group TYPE apqi-groupid,
      lv_keep  TYPE apqi-qerase VALUE 'X',
      lv_user  TYPE apqi-userid,
      ls_msg TYPE BAPIRET2,
      lv_error TYPE xfeld,
      lv_tcode TYPE tstc-tcode VALUE 'ME32L',
      lv_group1 type D0100-MAPN.
data: Bdc_rel_msg TYPE TLINE OCCURS 0,
      ls_text TYPE TLINE,
      lt_text type TABLE OF TLINE.
DATA: lt_seltab  TYPE TABLE OF rsparams,
      ls_seltab  LIKE LINE OF lt_seltab,
      lt_abaplist TYPE TABLE OF abaplist,
      lt_list TYPE list_string_table,
      ls_list TYPE LINE OF list_string_table.

clear: gt_bdc_rel_msg,
       gt_BDC_error_msg.
refresh: gt_bdc_rel_msg.

PERFORM clear_delivery_ME38 USING iv_ebeln.
IF gt_bdc_error_msg[] is INITIAL.
   PERFORM Delete_ME32L USING iv_ebeln.
endif.
BDC_RELEASE_MSG[] = gt_bdc_rel_msg[].
error_msg[] = gt_bdc_error_msg[].

ENDFUNCTION.
