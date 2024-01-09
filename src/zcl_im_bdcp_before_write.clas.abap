class ZCL_IM_BDCP_BEFORE_WRITE definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BDCP_BEFORE_WRITE
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BDCP_BEFORE_WRITE .
*"* protected components of class ZCL_IM_BDCP_BEFORE_WRITE
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_BDCP_BEFORE_WRITE
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_BDCP_BEFORE_WRITE IMPLEMENTATION.


METHOD if_ex_bdcp_before_write~filter_bdcpv_before_write.


  "Used by all
  DATA: ls_bdcpv TYPE bdcpv,   "Local Change Pointer
        lv_tabix  TYPE sytabix.

  DATA: t_lfa1    TYPE TABLE OF lfa1,
        t_lfm1    TYPE TABLE OF lfm1,
        lv_ktokk  TYPE ktokk,
        lv_ekorg  TYPE ekorg,
        lv_matnr  TYPE matnr,
        lv_mdmid  TYPE z_mdmid.



  LOOP AT change_pointers INTO ls_bdcpv.
    lv_tabix = sy-tabix.      "Remeber table index

    CASE ls_bdcpv-mestype.

      WHEN 'ZZCREMAS'.
        CLEAR: lv_ktokk, lv_ekorg, t_lfa1, t_lfm1.
        "Get KTOKK value from variable table.
        SELECT SINGLE value1
          FROM zvar INTO lv_ktokk
          WHERE programm  = 'ZCL_IM_BDCP_BEFORE_WRITE'
            AND varname   = 'KTOKK'
            AND varnum    = 1
        .
        "Get EKORG value from variable table.
        SELECT SINGLE value1
          FROM zvar INTO lv_ekorg
          WHERE programm  = 'ZCL_IM_BDCP_BEFORE_WRITE'
            AND varname   = 'EKORG'
            AND varnum    = 1
        .

        SELECT  lifnr ktokk
          FROM lfa1
          INTO CORRESPONDING FIELDS OF TABLE t_lfa1
          WHERE lifnr = ls_bdcpv-cdobjid
            AND ktokk = lv_ktokk
        .

        "If it exists check second condition
        IF sy-subrc = 0.
          SELECT lifnr ekorg
            FROM lfm1
            INTO CORRESPONDING FIELDS OF TABLE t_lfm1
            WHERE lifnr = ls_bdcpv-cdobjid
              AND ekorg = lv_ekorg
          .

          "If this does not exist, remove the change pointer
          IF sy-subrc NE 0.
            DELETE change_pointers INDEX lv_tabix.
          ENDIF.
        ENDIF.



      WHEN 'ZZINFREC'.
        CLEAR: lv_matnr, lv_mdmid.
        SELECT SINGLE matnr
          FROM eina
          INTO lv_matnr
          WHERE infnr = ls_bdcpv-cdobjid.

        IF sy-subrc = 0.
          SELECT SINGLE zzmdmid
            FROM mara
            INTO lv_mdmid
            WHERE matnr = lv_matnr.

          IF lv_mdmid = ''.
            DELETE change_pointers INDEX lv_tabix.
          ENDIF.
        ELSE.
          DELETE change_pointers INDEX lv_tabix.
        ENDIF.



      WHEN 'ZZMATMAS'.
        CLEAR: lv_matnr, lv_mdmid.
        SELECT SINGLE bmatn
          FROM mara
          INTO lv_matnr
          WHERE matnr = ls_bdcpv-cdobjid.

        IF sy-subrc = 0.
          SELECT SINGLE zzmdmid
            FROM mara
            INTO lv_mdmid
            WHERE matnr = lv_matnr.

          IF lv_mdmid = ''.
            DELETE change_pointers INDEX lv_tabix.
          ENDIF.
        ENDIF.


***Removed because MARA is not populated when this
***change doc is being run.
*      WHEN 'ZZMATMASCONF'.
*        CLEAR: lv_mdmid.
*        SELECT SINGLE zzmdmid
*          FROM mara
*          INTO lv_mdmid
*          WHERE matnr = ls_bdcpv-cdobjid.
*
*        IF lv_mdmid = ''.
*          DELETE change_pointers INDEX lv_tabix.
*        ENDIF.



    ENDCASE.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
