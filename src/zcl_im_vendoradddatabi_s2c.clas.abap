class ZCL_IM_VENDORADDDATABI_S2C definition
  public
  final
  create public .

*"* public components of class ZCL_IM_VENDORADDDATABI_S2C
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_BI .
*"* protected components of class ZCL_IM_VENDORADDDATABI_S2C
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_VENDORADDDATABI_S2C
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_VENDORADDDATABI_S2C IMPLEMENTATION.


method IF_EX_VENDOR_ADD_DATA_BI~CHECK_DATA_ROW.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~FILL_ALE_SEGMENTS_OWN_DATA.
endmethod.


METHOD if_ex_vendor_add_data_bi~fill_bi_table_with_own_segment.

  DATA: ls_lfa1_o     TYPE lfa1,
        ls_lfa1_n     TYPE lfa1,
        lv_lifnr      TYPE lfa1-lifnr,
        lv_object     TYPE cdobjectv,
        lv_tcode      TYPE cdtcode,
        lv_chg        TYPE cdchngind,
        lv_z1lfa1m(1) TYPE c.

  lv_tcode  = i_trans_data-tcode.
  lv_lifnr  = i_trans_data-lifnr.
  lv_object = lv_lifnr.

  GET PARAMETER ID 'S2C-LFA1-Z1LFA1M'
             FIELD lv_z1lfa1m.

  "Skip if Z1LFA1M Segment was never hit.
  IF lv_z1lfa1m IS INITIAL.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM lfa1
    INTO ls_lfa1_o
    WHERE lifnr = lv_lifnr.

  IF sy-subrc = 0.
    ls_lfa1_n = ls_lfa1_o.

    GET PARAMETER ID 'S2C-LFA1-ZZISNVDR'
           FIELD ls_lfa1_n-zzisnvdr.

    "forward slash is used to signify no change.
    IF ls_lfa1_n-zzisnvdr <> '/'.

      UPDATE lfa1 FROM ls_lfa1_n.

      CALL FUNCTION 'BANK_API_CHDOC_WRITE'
        EXPORTING
          i_objectclas               = 'KRED'
          i_objectid                 = lv_object
          i_udate                    = sy-datum
          i_utime                    = sy-uzeit
          i_username                 = sy-uname
          i_tcode                    = lv_tcode
          i_object_change_indicator  = 'U'
          i_tabname0                 = 'LFA1'
          i_workarea_old0            = ls_lfa1_o
          i_workarea_new0            = ls_lfa1_n
          i_change_indicator0        = 'U'
        EXCEPTIONS
          objectclass_not_defined    = 1
          table_not_defined          = 2
          wrong_work_area_type       = 3
          internal_error             = 4
          no_document_inserted       = 5
          time_zone_conversion_error = 6
          OTHERS                     = 7.
    ENDIF.

  ENDIF.

ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_BI~FILL_FT_TABLE_USING_DATA_ROWS.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~MODIFY_BI_STRUCT_FROM_STD_SEG.
endmethod.


METHOD if_ex_vendor_add_data_bi~pass_non_standard_segment.

  DATA: ls_z1lfa1m TYPE z1lfa1m.

  IF i_segment_name = 'Z1LFA1M'.
    SET PARAMETER   ID 'S2C-LFA1-Z1LFA1M'
                 FIELD 'X'.

    ls_z1lfa1m = i_segment_data.
    SET PARAMETER   ID 'S2C-LFA1-ZZISNVDR'
                 FIELD ls_z1lfa1m-zzisnvdr.
  ENDIF.


ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_BI~PROCESS_ALE_OWN_CHANGE_POINTER.
endmethod.
ENDCLASS.
