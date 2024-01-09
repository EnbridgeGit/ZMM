************************************************************************
* Z Copy of BBP_EXTREQ_TRANSFER                                        *
* - This version will only transfer new items, not changed             *
************************************************************************


REPORT zbbp_extreq_transfer.

TABLES: eprtrans.

DATA:
  lv_maxrows  TYPE int4,
  lv_pksize   TYPE int4,


  ls_eprtrans TYPE eprtrans,
  lt_eprtrans TYPE SORTED TABLE OF eprtrans WITH UNIQUE KEY mandt banfn bnfpo,
  lv_lines    TYPE i,

  lt_banfsel  TYPE bbpt_trans_section,
  ls_banfsel  LIKE LINE OF lt_banfsel,
  ls_result   TYPE bbs_er_result_from_transfer.



SELECT-OPTIONS:
  so_banfn    FOR ls_eprtrans-banfn NO-EXTENSION OBLIGATORY MEMORY ID ban.

PARAMETERS:
  lp_tradt    TYPE bbp_er_transdate DEFAULT sy-datum,
  lp_ofset    TYPE char4,
  lp_pksiz(8) TYPE n DEFAULT 100,
  lp_maxr(8)  TYPE n DEFAULT 100000,
  lp_qname    TYPE char22 DEFAULT 'BBP_EXTREQ_TRANS',
  lp_split    TYPE xfeld.


START-OF-SELECTION.
  MOVE lp_maxr  TO lv_maxrows.
  MOVE lp_pksiz TO lv_pksize.


  "Get all entries from the eprtrans table
  SELECT *
    FROM eprtrans
    INTO TABLE lt_eprtrans
  .

  IF so_banfn-high IS INITIAL.
    so_banfn-high = so_banfn-low.
  ENDIF.

  "Delete entries that are not within the date range
  DELETE lt_eprtrans
    WHERE banfn < so_banfn-low
      OR  banfn > so_banfn-high
  .

  "Delete entries that are changes (transstat = 2).
  DELETE lt_eprtrans
    WHERE transstat = '2'
  .

  "Delete FROM the DATABASE table to cleanup
  "If in date range, and are changes, remove
  DELETE FROM eprtrans
    WHERE banfn >= so_banfn-low
      AND banfn <= so_banfn-high
      AND transstat = '2'
  .

  "Build the select items list.
  LOOP AT lt_eprtrans INTO ls_eprtrans.
    CLEAR ls_banfsel.
    "List every item.
    ls_banfsel-sign   = 'I'.
    ls_banfsel-option = 'EQ'.
    ls_banfsel-low    = ls_eprtrans-banfn.
    APPEND ls_banfsel TO lt_banfsel.
  ENDLOOP.

  "Check if we have any items to be transfered
  DESCRIBE TABLE lt_banfsel LINES lv_lines.

  IF lv_lines > 0.
    CALL FUNCTION 'BBP_EXTREQ_TRANSFER'
      EXPORTING
        iv_trans_date   = lp_tradt
        iv_trans_offset = lp_ofset
        iv_packagesize  = lv_pksize
        iv_queuename    = lp_qname
        it_banfsel      = lt_banfsel
        iv_maxrows      = lv_maxrows
        iv_split        = lp_split
      IMPORTING
        ev_result       = ls_result.
    WRITE: text-001, ls_result-total_transfered_pr.
    WRITE: text-002, ls_result-total_transfered_items.
  ELSE.
    WRITE: text-003.
  ENDIF.
