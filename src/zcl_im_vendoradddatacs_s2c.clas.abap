class ZCL_IM_VENDORADDDATACS_S2C definition
  public
  final
  create public .

*"* public components of class ZCL_IM_VENDORADDDATACS_S2C
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_CS .
*"* protected components of class ZCL_IM_VENDORADDDATACS_S2C
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_VENDORADDDATACS_S2C
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_VENDORADDDATACS_S2C IMPLEMENTATION.


METHOD if_ex_vendor_add_data_cs~get_data.
  DATA: lv_zzisnvdr TYPE z_isnvdr.
  IMPORT act TO lv_zzisnvdr FROM MEMORY ID 'VENDORADDDATACS_S2C-ZZISNVDR'.

  IF lv_zzisnvdr NE s_lfa1-zzisnvdr.
    s_lfa1-zzisnvdr = lv_zzisnvdr.
  ENDIF.
ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
endmethod.


METHOD if_ex_vendor_add_data_cs~get_taxi_screen.
  IF i_taxi_fcode = 'ZISN'.
    e_program = 'SAPLZMMS2C'.
    e_screen = '9001'.
  ENDIF.
ENDMETHOD.


METHOD if_ex_vendor_add_data_cs~set_data.
  EXPORT act FROM i_activity      TO MEMORY ID 'VENDORADDDATACS_S2C-ZZACT'.
  EXPORT act FROM i_lfa1-zzisnvdr TO MEMORY ID 'VENDORADDDATACS_S2C-ZZISNVDR'.
ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_CS~SET_FCODE.
endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
endmethod.
ENDCLASS.
