************************************************************************
*                                                                      *
*   PROGRAM: ZMMI_LAST_MSEG_TABLE_UPDATE                               *
*   AUTHOR:  Larry Ritchie                                             *
*   CREATED: 2009/12/30                                                *
*                                                                      *
*   DESCRIPTION: This program captures the last material document from *
*                MSEG and saves it on table ZMM_LAST_MSEG.  The last   *
*                material document is based on material, plant,        *
*                storage location, movement type & special stock       *
*                indicator.                                            *
************************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G              *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                             *
* -------------------------------------------------------------------- *
* 2009/11/30 LRITCHIE TR774 New program for DBossy                     *
************************************************************************

report  zmmi_last_mseg_table_update no standard page heading
                                       line-size 132.

************************************************************************
*    TABLES                                                            *
************************************************************************

tables: mkpf,                          "Header: Material Document
        mseg,                          "Document Segment: Material
        zmm_last_mseg.                 "Most Recent Material Document

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************

selection-screen skip 2.
select-options s_budat for mkpf-budat.

************************************************************************
*    INTERNAL TABLES                                                   *
************************************************************************

data: begin of tbl_mkpf_mseg occurs 0,
        mblnr   like mkpf-mblnr,
        mjahr   like mkpf-mjahr,
        budat   like mkpf-budat,
        cpudt   like mkpf-cpudt,
        bwart   like mseg-bwart,
        matnr   like mseg-matnr,
        werks   like mseg-werks,
        lgort   like mseg-lgort,
        sobkz   like mseg-sobkz,
      end of tbl_mkpf_mseg.

data: begin of tbl_recent occurs 0,
        mandt   like mseg-mandt,
        matnr   like mseg-matnr,
        werks   like mseg-werks,
        lgort   like mseg-lgort,
        bwart   like mseg-bwart,
        sobkz   like mseg-sobkz,
        mblnr   like mkpf-mblnr,
        mjahr   like mkpf-mjahr,
        cpudt   like mkpf-cpudt,
        budat   like mkpf-budat,
      end of tbl_recent.

data: begin of tbl_last_mseg occurs 0,
        mandt   like mseg-mandt,
        matnr   like mseg-matnr,
        werks   like mseg-werks,
        lgort   like mseg-lgort,
        bwart   like mseg-bwart,
        sobkz   like mseg-sobkz,
        mblnr   like mkpf-mblnr,
        mjahr   like mkpf-mjahr,
        cpudt   like mkpf-cpudt,
        budat   like mkpf-budat,
      end of tbl_last_mseg.

************************************************************************
*    VARAIBLES                                                         *
************************************************************************

data: v_new_count(13) type p,
      v_old_count(13) type p.

************************************************************************
*    START OF SELECTION                                                *
************************************************************************

start-of-selection.

  perform read_mkpf_mseg.

  perform read_last_mseg.

  perform find_most_recent.

  perform update.

end-of-selection.

************************************************************************
*    FORMS                                                             *
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  read_mkpf_mseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form read_mkpf_mseg .

  refresh tbl_mkpf_mseg.

  select mkpf~mblnr mkpf~mjahr mkpf~budat mkpf~cpudt
         mseg~bwart mseg~matnr mseg~werks mseg~lgort mseg~sobkz
    into table tbl_mkpf_mseg
    from mkpf inner join mseg
         on mkpf~mblnr = mseg~mblnr and
            mkpf~mjahr = mseg~mjahr
    where mkpf~budat in s_budat.

  delete tbl_mkpf_mseg where matnr is initial.
  delete tbl_mkpf_mseg where werks is initial.
  delete tbl_mkpf_mseg where lgort is initial.

  sort tbl_mkpf_mseg by matnr werks lgort bwart sobkz.

endform.                    " read_mkpf_mseg
*
*&---------------------------------------------------------------------*
*&      Form  find_most_recent
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form find_most_recent .

  tbl_recent[] = tbl_last_mseg[].

  loop at tbl_mkpf_mseg.
    read table tbl_recent with key matnr = tbl_mkpf_mseg-matnr
                                   werks = tbl_mkpf_mseg-werks
                                   lgort = tbl_mkpf_mseg-lgort
                                   bwart = tbl_mkpf_mseg-bwart
                                   sobkz = tbl_mkpf_mseg-sobkz
                                   binary search.
    if sy-subrc <> 0.
      move-corresponding tbl_mkpf_mseg to tbl_recent.
      tbl_recent-mandt = sy-mandt.
      insert tbl_recent index sy-tabix.
    else.
      if tbl_mkpf_mseg-cpudt > tbl_recent-cpudt or
         ( tbl_mkpf_mseg-cpudt = tbl_recent-cpudt and
           tbl_mkpf_mseg-mblnr > tbl_recent-mblnr and
           tbl_mkpf_mseg-mjahr >= tbl_recent-mjahr ).
        tbl_recent-cpudt = tbl_mkpf_mseg-cpudt.
        tbl_recent-mblnr = tbl_mkpf_mseg-mblnr.
        tbl_recent-mjahr = tbl_mkpf_mseg-mjahr.
        tbl_recent-budat = tbl_mkpf_mseg-budat.
        modify tbl_recent index sy-tabix.
      endif.
    endif.

  endloop.

  describe table tbl_recent lines v_new_count.

* remove entries that are not changing
  loop at tbl_last_mseg.
    read table tbl_recent with key matnr = tbl_last_mseg-matnr
                                    werks = tbl_last_mseg-werks
                                    lgort = tbl_last_mseg-lgort
                                    bwart = tbl_last_mseg-bwart
                                    sobkz = tbl_last_mseg-sobkz
                                    binary search.
    if sy-subrc = 0 and
       tbl_last_mseg-mblnr = tbl_recent-mblnr and
       tbl_last_mseg-mjahr = tbl_recent-mjahr.
      delete tbl_recent index sy-tabix.
    endif.

  endloop.

endform.                    " find_most_recent
*&---------------------------------------------------------------------*
*&      Form  update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form update.

  if not tbl_recent[] is initial.
    modify zmm_last_mseg from table tbl_recent.
  endif.

  if sy-subrc <> 0.
    write:/ 'Mass update/insert to ZMM_LAST_MSEG failed - return code',
            sy-subrc.
  else.
    write:/ 'Mass update/insert to ZMM_LAST_MSEG successful'.
    skip 1.
    write:/ 'Total original entries:', v_old_count.
    write:/ 'Total new entries:     ', v_new_count.
  endif.

endform.                    " update
*&---------------------------------------------------------------------*
*&      Form  read_last_mseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form read_last_mseg .

  select mandt matnr werks lgort bwart sobkz mblnr mjahr cpudt budat
         into table tbl_last_mseg
         from zmm_last_mseg.

  sort tbl_last_mseg by mandt matnr werks lgort bwart sobkz.

  describe table tbl_last_mseg lines v_old_count.

endform.                    " read_last_mseg
