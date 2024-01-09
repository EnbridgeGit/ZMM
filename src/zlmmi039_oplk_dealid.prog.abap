*&---------------------------------------------------------------------*
*& Report  ZLMMI039_OPLK_DEALID
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlmmi039_oplk_dealid.

TABLES: ekko .           " Purchasing Document Header

TYPES: BEGIN OF ty_po_list,
         ebeln      TYPE ebeln,
         zzoldealid TYPE zoldealid,
       END OF ty_po_list .

DATA: gt_po_list TYPE STANDARD TABLE OF ty_po_list,
      gw_po_list TYPE ty_po_list .

DATA: fcode   TYPE syucomm,
      gv_col  TYPE lvc_colpos,
      gs_fcat TYPE lvc_s_fcat .

DATA: gt_fcat TYPE lvc_t_fcat,
      gs_lout TYPE lvc_s_layo,
      g_cntrl TYPE scrfname VALUE 'POLST',
      g_grid  TYPE REF TO cl_gui_alv_grid,
      g_ccntr TYPE REF TO cl_gui_custom_container .


*CONSTANTS:

SELECT-OPTIONS: so_ebeln FOR ekko-ebeln .  " Purchasing Document Number


START-OF-SELECTION .

  SELECT ebeln zzoldealid
    INTO TABLE gt_po_list
    FROM ekko
   WHERE ebeln IN so_ebeln .



END-OF-SELECTION .

  CLEAR: gv_col, gs_fcat .

  ADD 1 TO gv_col .

  gs_fcat-col_pos   = gv_col .
  gs_fcat-fieldname = 'EBELN' .
  gs_fcat-tabname   = 'EKKO' .

  CLEAR: gs_fcat .

  ADD 1 TO gv_col .

  gs_fcat-col_pos   = gv_col .
  gs_fcat-fieldname = 'ZZOLDEALID' .
  gs_fcat-tabname   = 'EKKO' .
