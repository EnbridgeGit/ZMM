FUNCTION ZLMMI036_CARE_LOCMAST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TAB_LOCMAST STRUCTURE  ZMMT_LOCMAST
*"  EXCEPTIONS
*"      UPDATE_FAILED
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Author             :  Durgaprakash Biruduraju                       *
*& Creation Date      :  19-Feb-2021                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  MM                                            *
*& Description        :  Update Care Location Master data through      *
*                        BODS                                          *
*&---------------------------------------------------------------------*

  DATA: lt_locmast type STANDARD TABLE OF zmmt_locmast,
        ls_locmast type zmmt_locmast,
        lt_locmast_v type STANDARD TABLE OF zmmt_locmast,
        ls_locmast_v type zmmt_locmast.

  CLEAR: ls_locmast, ls_locmast_v.
  REFRESH:lt_locmast, lt_locmast_v.
* To check if table entry exits to mark as modified.
  IF not tab_locmast[] is INITIAL.
    SELECT * FROM zmmt_locmast
    INTO TABLE lt_locmast_v
    FOR ALL ENTRIES IN tab_locmast
    where zztrloc = tab_locmast-zztrloc
      AND zzparty = tab_locmast-zzparty.
    IF sy-subrc = 0.
      sort lt_locmast_v by zztrloc zzparty.
    ENDIF.
  ENDIF.

  LOOP AT tab_locmast INTO ls_locmast.
    READ TABLE lt_locmast_v into ls_locmast_v with key
                            zztrloc = ls_locmast-zztrloc
                            zzparty = ls_locmast-zzparty.
*                            BINARY SEARCH.
    IF sy-subrc = 0.
      ls_locmast-zzfrequsdflg = ls_locmast_v-zzfrequsdflg.
      ls_locmast-aedat = sy-datum.
      ls_locmast-cputm = sy-uzeit.
      ls_locmast-usnam = sy-uname.
      ls_locmast-erdat = ls_locmast_v-erdat.
      ls_locmast-erzet = ls_locmast_v-erzet.
      ls_locmast-ernam = ls_locmast_v-ernam.
    ELSE.
      ls_locmast-erdat = sy-datum.
      ls_locmast-erzet = sy-uzeit.
      ls_locmast-ernam = sy-uname.
    ENDIF.

    APPEND ls_locmast to lt_locmast.
    clear ls_locmast.
  ENDLOOP.

  MODIFY zmmt_locmast FROM table lt_locmast.
  IF sy-subrc ne 0.
    RAISE update_failed.
  ENDIF.

ENDFUNCTION.
