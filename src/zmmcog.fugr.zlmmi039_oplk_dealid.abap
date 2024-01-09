FUNCTION ZLMMI039_OPLK_DEALID.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TAB_DEALID STRUCTURE  ZMMS_OPLK_DEALID
*"      TAB_NODEAL STRUCTURE  ZMMS_OPLK_DEALID_RETURN
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Author             :  Durgaprakash Biruduraju                       *
*& Creation Date      :  19-Feb-2021                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  MM                                            *
*& Description        :  Update Openlink Deal ID in EKKO through BODS  *
*&---------------------------------------------------------------------*
  TABLES:ekko.
  DATA: "lt_ekko type STANDARD TABLE OF ebeln,
        ls_dealid type ZMMS_OPLK_DEALID,
        ls_dealid_return type ZMMS_OPLK_DEALID_return,
        v_ebeln type ekko-ebeln.

*  *custom Fields Extension in Scheduling Agreement
  DATA: bapi_te_purcon  TYPE bapi_te_meoutheader,                 "Extension Tables
        bapi_te_purconx TYPE bapi_te_meoutheaderx,                "Extension Tables
        str             type string,
        lta_return      TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        lwa_return      TYPE bapiret2,
        ext_container   TYPE TABLE OF bapiparex WITH HEADER LINE. "Extension Container

*  REFRESH lt_ekko.
  clear: v_ebeln,ls_dealid,ls_dealid_return.
  LOOP AT tab_dealid INTO ls_dealid.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = ls_dealid-ebeln
      IMPORTING
        OUTPUT = v_ebeln.

    SELECT SINGLE * FROM ekko
      WHERE ebeln = v_ebeln.
    IF sy-subrc = 0.
      ekko-zzoldealid = ls_dealid-zzoldealid.
      UPDATE ekko.
      COMMIT WORK.
    ELSE.
      ls_dealid_return = ls_dealid.
      ls_dealid_return-zmsg = text-041.
      APPEND ls_dealid_return to tab_nodeal.
    ENDIF.
    CLEAR: ls_dealid, v_ebeln,ls_dealid_return.

  ENDLOOP.




ENDFUNCTION.
