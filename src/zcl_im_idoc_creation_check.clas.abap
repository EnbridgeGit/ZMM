class ZCL_IM_IDOC_CREATION_CHECK definition
  public
  final
  create public .

*"* public components of class ZCL_IM_IDOC_CREATION_CHECK
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_IDOC_CREATION_CHECK .
*"* protected components of class ZCL_IM_IDOC_CREATION_CHECK
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_IDOC_CREATION_CHECK
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_IDOC_CREATION_CHECK IMPLEMENTATION.


METHOD if_ex_idoc_creation_check~idoc_data_check.
  DATA: ls_z1mara TYPE z1mara,
        lv_mdmid  TYPE z_mdmid,
        lv_msgtyp TYPE edi_mestyp,
        lv_segnam TYPE edilsegtyp,
        ls_edidd  TYPE edidd.


  "Set default for the output variable create_idoc.
  create_idoc = 'X'.

  lv_msgtyp = idoc_control-mestyp.

  CASE lv_msgtyp.
    WHEN 'ZZMATMASCONF'.
      LOOP AT idoc_data INTO ls_edidd.
        lv_segnam = ls_edidd-segnam.
        CASE lv_segnam.
          WHEN 'Z1MARA'.
            ls_z1mara = ls_edidd-sdata.
            lv_mdmid = ls_z1mara-zzmdmid.
            IF lv_mdmid IS INITIAL.
              create_idoc = ''.
            ENDIF.
        ENDCASE.
      ENDLOOP.
  ENDCASE.



ENDMETHOD.
ENDCLASS.
