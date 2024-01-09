class ZCL_IM_BDCP_BEFORE_WRITE2 definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BDCP_BEFORE_WRITE2
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BDCP_BEFORE_WRITE .
*"* protected components of class ZCL_IM_BDCP_BEFORE_WRITE2
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_BDCP_BEFORE_WRITE2
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_BDCP_BEFORE_WRITE2 IMPLEMENTATION.


METHOD if_ex_bdcp_before_write~filter_bdcpv_before_write.


  "Used by all
  DATA: ls_bdcpv  TYPE bdcpv,   "Local Change Pointer
        ls_bdcpv2 TYPE bdcpv,
        ls_bdcpvn TYPE bdcpv,   "New Change Pointer
        lv_tabix  TYPE sytabix,
        lv_int    TYPE integer,
        true      TYPE integer VALUE 1,
        false     TYPE integer VALUE 0.

  "Cremas Sourcing
  DATA: t_lfa1    TYPE TABLE OF lfa1,
        t_lfm1    TYPE TABLE OF lfm1,
        lv_ktokk  TYPE ktokk,
        lv_ekorg  TYPE ekorg.



  "Matmas Sourcing
  DATA: lv_mstae  TYPE mstae.

  DATA: BEGIN OF ls_plantlist,
          plant TYPE werks_d,
        END OF ls_plantlist.

  DATA: lt_plantlist  LIKE TABLE OF ls_plantlist,
        ls_zvar       TYPE zvar,
        lv_plantix    TYPE sytabix,
        lv_found      TYPE integer.

 DATA: lt_change_pointers TYPE bdi_bdcpvt,
       itab_bdcpv TYPE TABLE OF bdcpv,
       itab_stxh TYPE TABLE OF stxh,
       fl_tabkey TYPE bdcpv-tabkey.

 CONSTANTS: cons_tdobject TYPE stxh-tdobject VALUE 'MATERIAL',
            cons_tdid TYPE stxh-tdid VALUE 'GRUN',
            cons_tdspras TYPE stxh-tdspras VALUE 'E',
            cons_tabname TYPE bdcpv-tabname VALUE 'MATERIAL',
            cons_fldname TYPE bdcpv-fldname VALUE 'GRUNE'.



  LOOP AT change_pointers INTO ls_bdcpv.
    lv_tabix = sy-tabix.      "Remeber table index

    CASE ls_bdcpv-mestype.

      WHEN 'ZZCREMAS_SOURCING'.
        CLEAR: lv_ktokk, lv_ekorg.
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
            IF sy-subrc = 0.
              "Good, duplicate the record.
            ELSE.
              "LFM1 Doesn't Exist
              DELETE change_pointers INDEX lv_tabix.
              CONTINUE.
            ENDIF.

          ELSE.
            "LFA1 Doesn't Exist
            DELETE change_pointers INDEX lv_tabix.
            CONTINUE.
          ENDIF.



        "If here, the record should be duplicated.
        IF ls_bdcpv-tabname = 'LFA1'.
          "Skip this, we only care about LFA1 changes LFM1 will be dup

          CLEAR ls_bdcpvn.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ALE_CP'
              quantity    = '1'
            IMPORTING
              number      = lv_int.
          .

          ls_bdcpvn-cpident = lv_int.
          ls_bdcpvn-mestype = 'ZZCREMAS_SOURCING'.
          ls_bdcpvn-process = ''.
          ls_bdcpvn-tabname = 'LFM1'.
          CONCATENATE ls_bdcpv-tabkey lv_ekorg INTO ls_bdcpvn-tabkey.
          ls_bdcpvn-fldname = 'KEY'.
          ls_bdcpvn-cretime = ls_bdcpv-cretime.
          ls_bdcpvn-acttime = ls_bdcpv-acttime.
          ls_bdcpvn-usrname = ls_bdcpv-usrname.
          ls_bdcpvn-cdobjcl = ls_bdcpv-cdobjcl.
          ls_bdcpvn-cdobjid = ls_bdcpv-cdobjid.
          ls_bdcpvn-cdchgno = ls_bdcpv-cdchgno.
          ls_bdcpvn-cdchgid = ls_bdcpv-cdchgid.

          "Add it to the change pointer list.
          APPEND ls_bdcpvn TO change_pointers.

        ENDIF.







      WHEN 'ZZMATMAS_SOURCING'.

        SELECT SINGLE mstae
          INTO lv_mstae
          FROM mara
          WHERE matnr = ls_bdcpv-cdobjid
        .

        "Check cross plant material status is set.
        IF lv_mstae = '00'.
          DELETE change_pointers INDEX lv_tabix.
          CONTINUE.
        ENDIF.

        "Get joining records where statm = 'E' and value WERKS
        " is the same as statm = 'B' and value of BWKEY
        CLEAR lt_plantlist.
        SELECT msta1~werks AS plant
          INTO CORRESPONDING FIELDS OF TABLE lt_plantlist
          FROM msta AS msta1
            INNER JOIN msta AS msta2
            ON msta1~werks = msta2~bwkey
          WHERE msta1~matnr = ls_bdcpv-cdobjid
            AND msta2~matnr = ls_bdcpv-cdobjid
            AND msta1~statm = 'E'
            AND msta2~statm = 'B'
            AND msta1~werks <> ''
        .

        "Delete the change pointer.
        IF sy-subrc <> 0 OR lt_plantlist IS INITIAL.
          DELETE change_pointers INDEX lv_tabix.
          CONTINUE.
        ENDIF.


        "Reduce the list of plants if not in config.
        LOOP AT lt_plantlist INTO ls_plantlist.
          lv_plantix = sy-tabix.

          CLEAR ls_zvar.
          SELECT SINGLE *
            INTO ls_zvar
            FROM zvar
            WHERE programm  = 'ZBDCP_BEFORE_WRITE'
              AND varname   = 'ZZMATMAS_SOURCING_PLANT'
              AND value1    = ls_plantlist-plant
          .

          "If no records found delete this plant.
          IF sy-subrc <> 0 OR ls_zvar IS INITIAL.
            DELETE lt_plantlist INDEX lv_plantix.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        "Check if plant list is now empty.
        CLEAR lv_int.
        DESCRIBE TABLE lt_plantlist LINES lv_int.
        IF lv_int = 0.
          DELETE change_pointers INDEX lv_tabix.
          CONTINUE.
        ENDIF.

        lv_found = true.

        "Check MARC table name
        IF ls_bdcpv-tabname = 'MARC'.
          lv_int = STRLEN( ls_bdcpv-tabkey ).
          lv_int = lv_int - 4.
          CLEAR ls_plantlist.
          lv_found = false.
          LOOP AT lt_plantlist INTO ls_plantlist.
            lv_plantix = sy-tabix.
            "Check if current plant is the same as the table key plant
            IF ls_plantlist-plant = ls_bdcpv-tabkey+lv_int(4).
              lv_found = true.
              DELETE lt_plantlist INDEX lv_plantix.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF lv_found = false.
          DELETE change_pointers INDEX lv_tabix.
          CONTINUE.
        ENDIF.


        "Remaining plants need duplicate records.
        LOOP AT lt_plantlist INTO ls_plantlist.

          CLEAR ls_bdcpvn.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ALE_CP'
              quantity    = '1'
            IMPORTING
              number      = lv_int.
          .

          ls_bdcpvn-cpident = lv_int.
          ls_bdcpvn-mestype = 'ZZMATMAS_SOURCING'.
          ls_bdcpvn-process = ''.
          ls_bdcpvn-tabname = 'MARC'.
          CONCATENATE sy-mandt ls_bdcpv-cdobjid ls_plantlist-plant INTO ls_bdcpvn-tabkey.
          ls_bdcpvn-fldname = 'KEY'.
          ls_bdcpvn-cretime = ls_bdcpv-cretime.
          ls_bdcpvn-acttime = ls_bdcpv-acttime.
          ls_bdcpvn-usrname = ls_bdcpv-usrname.
          ls_bdcpvn-cdobjcl = ls_bdcpv-cdobjcl.
          ls_bdcpvn-cdobjid = ls_bdcpv-cdobjid.
          ls_bdcpvn-cdchgno = ls_bdcpv-cdchgno.
          ls_bdcpvn-cdchgid = ls_bdcpv-cdchgid.


          "Check if the change pointer exists.
          lv_found = false.
          LOOP AT change_pointers INTO ls_bdcpv2.
            IF ls_bdcpv2-tabkey = ls_bdcpvn-tabkey and ls_bdcpv2-tabname = 'MARC'.
              lv_found = true.
            ENDIF.
          ENDLOOP.

          "Add it to the change pointer list.
          IF lv_found = false.
            APPEND ls_bdcpvn TO change_pointers.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDLOOP.
**************Additional logic by SAHMAD for BASIC DATA Long Text (GRUNE)
*  lt_change_pointers[] = change_pointers[].
  break sahmad.
  IF change_pointers IS NOT INITIAL and
     FLT_VAL = 'ZZMATMAS_SOURCING'.

    CLEAR: itab_bdcpv,
           itab_stxh.
    SELECT * INTO TABLE itab_bdcpv FROM bdcpv
        WHERE tabname = cons_tabname
          AND fldname = cons_fldname
          AND cdobjid = change_document_header-objectid
          AND process <> 'X'.
    CHECK sy-subrc <> 0.    "No BDCPV data
    SELECT * INTO TABLE itab_stxh FROM stxh
         WHERE tdobject = cons_tdobject
           AND tdid = cons_tdid
           AND tdspras = cons_tdspras
           AND tdname = change_document_header-objectid.
    CHECK sy-subrc = 0. " text data then proceed.
*Check Pointers internal table. No duplicate for GRUNE field.
    read table change_pointers with key mestype = 'ZZMATMAS_SOURCING'
                                        tabname = 'MATERIAL'
                                        fldname = 'GRUNE' transporting no fields.
    check sy-subrc <> 0. "Do not create new one, if GRUNE record already exist.
*     "Add it to the change pointer list.
    CLEAR: ls_bdcpvn,
           lv_int,
           ls_bdcpv.
    READ TABLE change_pointers INTO ls_bdcpv INDEX 1.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ALE_CP'
        quantity    = '1'
      IMPORTING
        number      = lv_int.
    CONCATENATE sy-mandt ls_bdcpv-cdobjid INTO fl_tabkey.
    ls_bdcpvn-cpident = lv_int.
    ls_bdcpvn-mestype = 'ZZMATMAS_SOURCING'.
    ls_bdcpvn-process = ''.
    ls_bdcpvn-tabname = 'MATERIAL'.
    ls_bdcpvn-tabkey = fl_tabkey(21). "ls_bdcpv-tabkey(21).
    ls_bdcpvn-fldname = 'GRUNE'.
    ls_bdcpvn-cretime = ls_bdcpv-cretime.
    ls_bdcpvn-acttime = ls_bdcpv-acttime.
    ls_bdcpvn-usrname = ls_bdcpv-usrname.
    ls_bdcpvn-cdobjcl = ls_bdcpv-cdobjcl.
    ls_bdcpvn-cdobjid = ls_bdcpv-cdobjid.
    ls_bdcpvn-cdchgno = ls_bdcpv-cdchgno.
    ls_bdcpvn-cdchgid = ls_bdcpv-cdchgid.
    "Add it to the change pointer list.
    break sahmad.
    APPEND ls_bdcpvn TO change_pointers.
*endloop.
  ENDIF.
ENDMETHOD.
ENDCLASS.
