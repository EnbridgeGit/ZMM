FUNCTION-POOL ZMM_CAP_PO_WF.                "MESSAGE-ID ..

* INCLUDE LZMM_CAP_PO_WFD...                 " Local class definition
DATA: gv_ebeln TYPE ekko-ebeln,
      ok_code TYPE sy-ucomm,
      gv_okcode TYPE sy-ucomm,
      gv_decision type char01,
      gv_releasecode type t16fc-frgco,
      gv_comments TYPE char100.
