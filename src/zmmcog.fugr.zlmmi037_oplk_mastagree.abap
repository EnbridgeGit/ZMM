FUNCTION ZLMMI037_OPLK_MASTAGREE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TAB_MASTAGREE STRUCTURE  ZMMT_MASTAGREE
*"      TAB_DUPLICATE STRUCTURE  ZMMT_MASTAGREE
*"  EXCEPTIONS
*"      UPDATE_FAILED
*"      DUPLICATE_MSA
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Author             :  Durgaprakash Biruduraju                       *
*& Creation Date      :  19-Feb-2021                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  MM                                            *
*& Description        :  Update Openlink Master Service Agreement data *
*                        through BODS                                  *
*&---------------------------------------------------------------------*

  DATA: lt_mastagree type STANDARD TABLE OF zmmt_mastagree,
        ls_mastagree type zmmt_mastagree,
        lt_mastagree_v type STANDARD TABLE OF zmmt_mastagree,
        ls_mastagree_v type zmmt_mastagree.
*        lt_mastagree_d type STANDARD TABLE OF zmmt_mastagree. "##NEEDED
*        ls_mastagree_d type zmmt_mastagree,
*        lt_mastagree_dv TYPE STANDARD TABLE OF zmmt_mastagree.

  CLEAR : ls_mastagree, ls_mastagree_v.
  REFRESH:lt_mastagree,lt_mastagree_v.
* To check if table entry exits to mark as modified.
  IF not tab_mastagree[] is INITIAL.
    SELECT * FROM zmmt_mastagree
    INTO TABLE lt_mastagree_v
    FOR ALL ENTRIES IN tab_mastagree
    WHERE zzparty_agmt_id = tab_mastagree-zzparty_agmt_id.
    IF sy-subrc = 0.
      SORT lt_mastagree_v BY zzparty_agmt_id.
    ENDIF.
  ENDIF.

* To update MSA data to ZMMT_MASTAGREE
  sort tab_mastagree by zzparty_agmt_id.
  LOOP AT tab_mastagree INTO ls_mastagree.

* Check if existing record to update change log
    READ TABLE lt_mastagree_v INTO ls_mastagree_v WITH KEY
            zzparty_agmt_id = ls_mastagree-zzparty_agmt_id.
*                            BINARY SEARCH.
    IF sy-subrc = 0.
      ls_mastagree-aedat = sy-datum.
      ls_mastagree-cputm = sy-uzeit.
      ls_mastagree-usnam = sy-uname.
      ls_mastagree-erdat = ls_mastagree_v-erdat.
      ls_mastagree-erzet = ls_mastagree_v-erzet.
      ls_mastagree-ernam = ls_mastagree_v-ernam.
    ELSE.
      ls_mastagree-erdat = sy-datum.
      ls_mastagree-erzet = sy-uzeit.
      ls_mastagree-ernam = sy-uname.
    ENDIF.

    APPEND ls_mastagree to lt_mastagree.
    CLEAR ls_mastagree.
  ENDLOOP.
  IF NOT lt_mastagree IS INITIAL.
    MODIFY zmmt_mastagree FROM TABLE lt_mastagree.
    IF sy-subrc ne 0.
      RAISE update_failed.
    ENDIF.
  ENDIF.


ENDFUNCTION.
