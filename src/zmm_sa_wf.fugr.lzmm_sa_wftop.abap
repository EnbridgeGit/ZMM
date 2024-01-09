FUNCTION-POOL zmm_sa_wf.                    "MESSAGE-ID ..

* INCLUDE LZMM_SA_WFD...                     " Local class definition
DATA: gv_ebeln TYPE ekko-ebeln,
      ok_code TYPE sy-ucomm,
      gv_okcode TYPE sy-ucomm,
      gv_decision type char01,
      gv_releasecode type t16fc-frgco,
      gv_comments TYPE char100,
      gv_schedule_updated type EKKO-LOEKZ.
DATA: gs_ekko    TYPE ekko,
      gs_ekpo    TYPE ekpo,
      gs_eket    TYPE eket,
      gs_lfa1    TYPE lfa1,
      gs_lfm1    TYPE lfm1,
      gs_t001    TYPE t001,
      gs_t880    TYPE t880,
      gs_t024    TYPE t024,
      gs_rm06e   TYPE rm06e,
      fs_t001w   TYPE t001w, "COG
      fs_konv    TYPE konv,  "COG
      fs_t006a   TYPE t006a, "COG
      v_kbetr    TYPE char15, " COG
      v_cur      TYPE char15, " COG.
      w_comm_method(7) TYPE c,
      w_tdname type stxh-tdname,
      dummy1(10),
      dummy2(10),
      dummy3(10).

DATA: BEGIN OF fs_stxh OCCURS 100,
        tdobject LIKE stxh-tdobject,
        tdname   LIKE stxh-tdname,
        tdid     LIKE stxh-tdid,
        tdspras  LIKE stxh-tdspras,
      END OF fs_stxh.

DATA: BEGIN OF w_tlinetab OCCURS 100.   "Internal table for long text.
        INCLUDE STRUCTURE tline.
DATA: END OF w_tlinetab.
DATA: w_potexttab LIKE w_tlinetab OCCURS 100,
      w_sahdrtexttab LIKE zsatext OCCURS 0 WITH HEADER LINE,
      w_remainder          LIKE tline-tdline.
DATA: BEGIN OF bdcdata OCCURS 0.               "batch input data
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
data: gt_Bdc_rel_msg TYPE TLINE OCCURS 0,
      gt_BDC_error_msg TYPE TABLE OF BAPIRET2.
