*&---------------------------------------------------------------------*
*&      Form  FILL_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*      -->P_VALUE  text
*      <--P_<XML_ATTR>  text
*----------------------------------------------------------------------*
FORM fill_values  USING    p_name  TYPE any
                           p_value TYPE any
                  CHANGING ps_data TYPE ty_xml_attr.

  IF p_name EQ 'APPLICATION'.
    ps_data-application = p_value.
  ELSEIF p_name EQ 'PIDX Attributes'.
  ELSEIF p_name EQ   'MATERIAL'.
    ps_data-material = p_value.
  ELSEIF p_name EQ 'Deletion Flag'.
    ps_data-delete_flag = p_value.
  ELSEIF p_name EQ   'SIZE' .
    ps_data-size = p_value.
  ELSEIF p_name EQ   'TYPE'.
    ps_data-type = p_value.
  ELSEIF p_name EQ 'BALL MATERIAL'.
    ps_data-ball_material = p_value.
  ELSEIF p_name EQ 'BODY MATERIAL'.
    ps_data-body_material = p_value.
  ELSEIF p_name EQ 'END CONNECTION'.
    ps_data-end_connection = p_value.
  ELSEIF p_name EQ 'MANUFACTURING STANDARD'.
    ps_data-manufacturing_standard = p_value.
  ELSEIF p_name EQ 'METHOD OF OPERATION'.
    ps_data-method_of_operation = p_value.
  ELSEIF p_name EQ 'NOMINAL PIPE SIZE'.
    ps_data-nominal_pipe_size = p_value.
  ELSEIF p_name EQ 'OPENING'.
    ps_data-opening = p_value.
  ELSEIF p_name EQ 'PRESSURE RATING'.
    ps_data-pressure_rating = p_value.
  ELSEIF p_name EQ 'SEAT MATERIAL'.
    ps_data-seat_material = p_value.
  ELSEIF p_name EQ 'Long Description'.
    ps_data-long_description = p_value.
  ELSEIF p_name EQ 'Active'.
    ps_data-active = p_value.
  ELSEIF p_name EQ 'Comments'.
    ps_data-comments = p_value.
  ELSEIF p_name EQ 'Business Unit'.
    ps_data-business_unit = p_value.
  ELSEIF p_name EQ 'Part Number Driven'.
    ps_data-part_number_driven = p_value.
  ELSEIF p_name EQ 'External System ID'.
    ps_data-external_system_id = p_value.
  ELSEIF p_name EQ 'Manufacturer'.
    ps_data-manufacturer = p_value.
  ELSEIF p_name EQ 'Manufacturer Name'.
    ps_data-manufacturer_name = p_value.
  ELSEIF p_name EQ 'Manufacturer Part Number'.
    ps_data-manufacturer_part_number = p_value.
  ELSEIF p_name EQ 'Model Number'.
    ps_data-model_number = p_value.
*  ELSEIF P_NAME EQ 'Vendor'.
*    PS_DATA-VENDOR = P_VALUE.
*  ELSEIF P_NAME EQ 'Vendor Name'.
*    PS_DATA-VENDOR_NAME = P_VALUE.
*  ELSEIF P_NAME EQ 'Vendor Part Number'.
*    PS_DATA-VENDOR_PART_NUMBER = P_VALUE.
  ELSEIF p_name EQ 'Part_Info_In_LD'.
    ps_data-part_info_in_ld = p_value.
  ELSEIF p_name EQ 'Base Unit of Measure'.
    ps_data-base_unit_of_measure = p_value.
  ELSEIF p_name EQ 'Exists in UG'.
    ps_data-exists_in_ug = p_value.
  ELSEIF p_name EQ 'Material Group'.
    ps_data-material_group = p_value.
  ELSEIF p_name EQ 'Material Type'.
    ps_data-material_type = p_value.
  ELSEIF p_name EQ 'SAP Material Number'.
    ps_data-sap_material_number = p_value.
  ELSEIF p_name EQ 'SAP Short Description'.
    ps_data-sap_short_description = p_value.
  ELSEIF p_name EQ 'X Plant Matl Status'.
    ps_data-x_plant_matl_status = p_value.
  ENDIF.

ENDFORM.                    "FILL_VALUES
*&---------------------------------------------------------------------*
*&      Form  GET_NODE_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<STR>  text
*      <--P_NAME  text
*      <--P_VALUE  text
*----------------------------------------------------------------------*
FORM get_node_attribute  USING    p_data     TYPE string
                                  p_attr     TYPE any
                                  p_attrname TYPE string
                         CHANGING p_value    TYPE any.

  DATA: node       TYPE REF TO if_ixml_node,
        lo_xml_doc TYPE REF TO cl_xml_document,
        counter    TYPE i,
        value      TYPE string,
        name       TYPE string.

  CLEAR: p_value.

  CREATE OBJECT lo_xml_doc.
  lo_xml_doc->parse_string( stream = p_data ).
  node = lo_xml_doc->find_node( name = p_attr ).
  IF node IS BOUND.
    DO.
      CALL METHOD lo_xml_doc->get_node_attr
        EXPORTING
          node  = node
          index = counter
        IMPORTING
          name  = name
          value = value.

      IF name EQ p_attrname.
        p_value = value.
      ENDIF.

      IF p_value IS NOT INITIAL OR name IS INITIAL.
        EXIT.
      ENDIF.
      counter = counter + 1.
    ENDDO.
  ENDIF.

ENDFORM.                    "GET_NODE_ATTRIBUTE

*&---------------------------------------------------------------------*
*&      Form  CREATE_CHANGE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_change_material .

  LOOP AT lt_xml_attr ASSIGNING <xml_attr>.

    CLEAR: gv_matnr,      gv_externalid, gv_mfrnr, gv_fixed,
           gv_bapi_error, gv_xml_flag,   gt_applog, gt_xml_data.

    CALL FUNCTION 'BUFFER_REFRESH_ALL'.

* format material number to internal
    IF <xml_attr>-sap_material_number IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <xml_attr>-sap_material_number
        IMPORTING
          output       = gv_matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF s_matnr[] IS NOT INITIAL AND gv_matnr IS NOT INITIAL.
        CHECK gv_matnr IN s_matnr.
      ENDIF.
    ENDIF.

* Format external system id to internal material number
    PERFORM convert_to_internal USING <xml_attr>-external_system_id CHANGING gv_externalid.
    IF s_extid[] IS NOT INITIAL AND gv_externalid IS NOT INITIAL.
      CHECK gv_externalid IN s_extid.
    ENDIF.

* Format manufacturer number to internal
    PERFORM convert_to_internal USING <xml_attr>-manufacturer CHANGING gv_mfrnr.

    IF <xml_attr>-delete_flag EQ 'True'.
      PERFORM delete_material.
      PERFORM commit_work.
      PERFORM save_application_log USING gt_applog[].
    ELSEIF <xml_attr>-action EQ 'Add'.  " Create
      PERFORM create_material.
      PERFORM create_material_mpn.
      PERFORM create_mat_classification.
      PERFORM commit_work.
      PERFORM save_application_log USING gt_applog[].
    ELSE. " Update
      PERFORM change_material.
      PERFORM change_material_mpn.
      PERFORM change_mat_classification.
      PERFORM commit_work.
      PERFORM save_application_log USING gt_applog[].
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_CHANGE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CREATE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_material .

  DATA: ls_headdata            TYPE bapimathead,
        ls_clientdata          TYPE  bapi_mara,
        ls_clientdatax         TYPE  bapi_marax,
        ls_return              TYPE bapiret2,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        ls_materialdescription LIKE LINE OF lt_materialdescription,
        lt_materiallongtext    TYPE STANDARD TABLE OF bapi_mltx,
        ls_materiallongtext    LIKE LINE OF lt_materiallongtext,
        lt_returnmessages      TYPE STANDARD TABLE OF bapi_matreturn2,
        ls_returnmessage       LIKE LINE OF lt_returnmessages,
        lt_extensionin         TYPE STANDARD TABLE OF  bapiparex,
        ls_extensionin         LIKE LINE OF lt_extensionin,
        lt_extensioninx        TYPE STANDARD TABLE OF  bapiparexx,
        ls_extensioninx        LIKE LINE OF lt_extensioninx.


  DATA: bapi_te_mara  TYPE bapi_te_mara,
        bapi_te_marax TYPE bapi_te_marax,
        lv_char(960)  TYPE c,
        text_tab      TYPE STANDARD TABLE OF ty_tdline,
        lv_matnr      TYPE mara-matnr.

  FIELD-SYMBOLS: <text> LIKE LINE OF text_tab,
                 <val>  TYPE any.

  CHECK gv_matnr IS NOT INITIAL.

  SELECT SINGLE matnr FROM mara
                      INTO lv_matnr
                      WHERE matnr EQ gv_matnr.

  IF lv_matnr IS INITIAL.
    ls_headdata-material    = gv_matnr.
    ls_headdata-matl_type = <xml_attr>-material_type.
    ls_headdata-ind_sector    = 'U'.

    ls_headdata-purchase_view = 'X'.

    IF <xml_attr>-material_type EQ 'NLAG'.
*      ls_e1maram-pstat = 'KCE'.
      ls_headdata-basic_view    = 'X'. " Pstat value K
      ls_headdata-purchase_view = 'X'. " Pstat value E
    ELSE.
*      ls_e1maram-pstat = 'KCEL'.
      ls_headdata-basic_view    = 'X'. " Pstat value K
      ls_headdata-purchase_view = 'X'. " Pstat value E
      ls_headdata-storage_view  = 'X'. " Pstat value L
    ENDIF.

    ls_clientdata-matl_group     = <xml_attr>-material_group.
    ls_clientdata-base_uom       = <xml_attr>-base_unit_of_measure.
    ls_clientdata-division       = '00'.
    ls_clientdata-pur_status     = <xml_attr>-x_plant_matl_status.
    ls_clientdata-manuf_prof     = 'MP01'.
    ls_clientdata-item_cat       = 'NORM'.
    ls_clientdata-material_fixed = 'X'.

    ls_clientdatax-matl_group     = 'X'.
    ls_clientdatax-base_uom       = 'X'.
    ls_clientdatax-division       = 'X'.
    ls_clientdatax-pur_status     = 'X'.
    ls_clientdatax-manuf_prof     = 'X'.
    ls_clientdatax-item_cat       = 'X'.
    ls_clientdatax-material_fixed = 'X'.

    ls_materialdescription-langu     = sy-langu.
    ls_materialdescription-matl_desc = <xml_attr>-sap_short_description.
    APPEND ls_materialdescription TO lt_materialdescription.

    IF <xml_attr>-long_description NE ''.
      ls_materiallongtext-applobject  = 'MATERIAL'.
      ls_materiallongtext-text_name   = gv_matnr.
      ls_materiallongtext-text_id     = 'GRUN'.
      ls_materiallongtext-langu       = 'E'.
*    ls_e1mtxhm-tdtexttype = 'ASCII'.
      ls_materiallongtext-langu_iso  = 'EN'.

      CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
        EXPORTING
          text        = <xml_attr>-long_description
          line_length = 132
          langu       = sy-langu
        TABLES
          text_tab    = text_tab.

      LOOP AT text_tab ASSIGNING <text>.
        ls_materiallongtext-format_col   = '*'.
        ls_materiallongtext-text_line     = <text>-tdline.
        APPEND ls_materiallongtext TO lt_materiallongtext.
      ENDLOOP.
    ENDIF.

    CLEAR: ls_extensionin, bapi_te_mara.
*Fill in custom fields
    bapi_te_mara-material = gv_matnr.
    bapi_te_mara-zzmdmid  = <xml_attr>-externalid.
    ASSIGN lv_char TO <val> CASTING TYPE bapi_te_mara.
*Use the following if you only want fill specific valuepart
*  ASSIGN ls_extensionin-valuepart1 TO <wa> CASTING TYPE zbapi_test.
    <val> = bapi_te_mara.
    ls_extensionin-structure = 'BAPI_TE_MARA'.
    ls_extensionin-valuepart1 = lv_char(240).
    ls_extensionin-valuepart2 = lv_char+240(240).
    ls_extensionin-valuepart3 = lv_char+480(240).
    ls_extensionin-valuepart4 = lv_char+720(240).
    APPEND ls_extensionin TO lt_extensionin.

    CLEAR: lv_char, ls_extensioninx, bapi_te_marax.
    UNASSIGN: <val>.
    bapi_te_marax-material     = gv_matnr.
    bapi_te_marax-zzmdmid      = 'X'.
    ASSIGN lv_char TO <val> CASTING TYPE bapi_te_marax.
    <val> = bapi_te_marax.
    ls_extensioninx-structure  = 'BAPI_TE_MARAX'.
    ls_extensioninx-valuepart1 = lv_char(240).
    ls_extensioninx-valuepart2 = lv_char+240(240).
    ls_extensioninx-valuepart3 = lv_char+480(240).
    ls_extensioninx-valuepart4 = lv_char+720(240).
    APPEND ls_extensioninx TO lt_extensioninx.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata            = ls_headdata
        clientdata          = ls_clientdata
        clientdatax         = ls_clientdatax
      IMPORTING
        return              = ls_return
      TABLES
        materialdescription = lt_materialdescription
        materiallongtext    = lt_materiallongtext
        returnmessages      = lt_returnmessages
        extensionin         = lt_extensionin
        extensioninx        = lt_extensioninx.

    IF ls_return-type EQ 'E'.
      gv_bapi_error = 'x'.
    ENDIF.
    PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
  ELSE.
    ls_returnmessage-type   = 'E'.
    ls_returnmessage-id     = '00'.
    ls_returnmessage-number = '001'.
    CONCATENATE 'Material' lv_matnr 'already exists'
    INTO ls_returnmessage-message_v1 SEPARATED BY space.
    APPEND ls_returnmessage TO lt_returnmessages.
    PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
  ENDIF.

ENDFORM.                    " CREATE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CHANGE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_material .
  DATA: ls_headdata            TYPE bapimathead,
        ls_clientdata          TYPE  bapi_mara,
        ls_clientdatax         TYPE  bapi_marax,
        ls_return              TYPE bapiret2,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        ls_materialdescription LIKE LINE OF lt_materialdescription,
        lt_materiallongtext    TYPE STANDARD TABLE OF bapi_mltx,
        ls_materiallongtext    LIKE LINE OF lt_materiallongtext,
        lt_returnmessages      TYPE STANDARD TABLE OF bapi_matreturn2,
        ls_returnmessage       LIKE LINE OF lt_returnmessages,
        lt_extensionin         TYPE STANDARD TABLE OF  bapiparex,
        ls_extensionin         LIKE LINE OF lt_extensionin,
        lt_extensioninx        TYPE STANDARD TABLE OF  bapiparexx,
        ls_extensioninx        LIKE LINE OF lt_extensioninx,
        ls_mara                TYPE mara.

  DATA: bapi_te_mara  TYPE bapi_te_mara,
        bapi_te_marax TYPE bapi_te_marax,
        lv_char(960)  TYPE c,
        text_tab      TYPE STANDARD TABLE OF ty_tdline.

  FIELD-SYMBOLS: <text> LIKE LINE OF text_tab,
                 <val>  TYPE any.

  CHECK gv_matnr IS NOT INITIAL.

  SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr EQ gv_matnr.
  IF sy-subrc EQ 0.

    CHECK <xml_attr>-material_group        IS NOT INITIAL OR
          <xml_attr>-base_unit_of_measure  IS NOT INITIAL OR
          <xml_attr>-x_plant_matl_status   IS NOT INITIAL OR
          <xml_attr>-sap_short_description IS NOT INITIAL OR
          <xml_attr>-long_description      IS NOT INITIAL OR
          <xml_attr>-externalid            IS NOT INITIAL..

    IF <xml_attr>-material_type IS NOT INITIAL AND
      <xml_attr>-material_type NE ls_mara-mtart.
      ls_returnmessage-type   = 'E'.
      ls_returnmessage-id     = '00'.
      ls_returnmessage-number = '001'.
      CONCATENATE 'Material type cannot be changed for material' gv_matnr
      INTO ls_returnmessage-message_v1 SEPARATED BY space.
      APPEND ls_returnmessage TO lt_returnmessages.
*      PERFORM create_log USING lt_returnmessages[].
      PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
      EXIT.
    ENDIF.

    ls_headdata-basic_view    = 'X'.
    ls_headdata-material = gv_matnr.

* Execute BAPI to open up locked fields to update
    ls_clientdata-material_fixed  = ''.
    ls_clientdatax-material_fixed = 'X'.
    gv_fixed = 'X'.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata    = ls_headdata
        clientdata  = ls_clientdata
        clientdatax = ls_clientdatax
      IMPORTING
        return      = ls_return.

    IF ls_return-type NE 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      ls_returnmessage-type = 'E'.            "MESSAGE TYPE
      CONCATENATE gv_matnr ' - Could not lock the material to EDIT'
       INTO ls_returnmessage-message_v1 SEPARATED BY space.
      APPEND ls_returnmessage TO lt_returnmessages.
      PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
      EXIT.
    ENDIF.

    ls_clientdata-material_fixed  = 'X'.
    ls_clientdatax-material_fixed = 'X'.

    IF <xml_attr>-material_group IS NOT INITIAL.
      ls_clientdata-matl_group = <xml_attr>-material_group.
      ls_clientdatax-matl_group     = 'X'.
    ENDIF.

    IF <xml_attr>-base_unit_of_measure IS NOT INITIAL.
      ls_clientdata-base_uom     = <xml_attr>-base_unit_of_measure.
      ls_clientdatax-base_uom       = 'X'.
    ENDIF.

    IF <xml_attr>-x_plant_matl_status IS NOT INITIAL.
      ls_clientdata-pur_status   = <xml_attr>-x_plant_matl_status.
      ls_clientdatax-pur_status     = 'X'.
    ENDIF.

    IF <xml_attr>-sap_short_description IS NOT INITIAL.
      ls_materialdescription-langu     = sy-langu.
      ls_materialdescription-matl_desc = <xml_attr>-sap_short_description.
      APPEND ls_materialdescription TO lt_materialdescription.
    ENDIF.

    IF <xml_attr>-long_description NE ''.
      ls_materiallongtext-applobject  = 'MATERIAL'.
      ls_materiallongtext-text_name   = gv_matnr.
      ls_materiallongtext-text_id     = 'GRUN'.
      ls_materiallongtext-langu       = 'E'.
*    ls_e1mtxhm-tdtexttype = 'ASCII'.
      ls_materiallongtext-langu_iso  = 'EN'.

      CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
        EXPORTING
          text        = <xml_attr>-long_description
          line_length = 132
          langu       = sy-langu
        TABLES
          text_tab    = text_tab.

      LOOP AT text_tab ASSIGNING <text>.
        ls_materiallongtext-format_col   = '*'.
        ls_materiallongtext-text_line     = <text>-tdline.
        APPEND ls_materiallongtext TO lt_materiallongtext.
      ENDLOOP.
    ENDIF.

    IF <xml_attr>-externalid IS NOT INITIAL AND <xml_attr>-externalid NE ls_mara-zzmdmid.
*      and <xml_attr>-externalid ne ls_mara-zzmdmid.
      CLEAR: ls_extensionin, bapi_te_mara.
*Fill in custom fields
      bapi_te_mara-material = gv_matnr.
      bapi_te_mara-zzmdmid  = <xml_attr>-externalid.
      ASSIGN lv_char TO <val> CASTING TYPE bapi_te_mara.
*Use the following if you only want fill specific valuepart
*  ASSIGN ls_extensionin-valuepart1 TO <wa> CASTING TYPE zbapi_test.
      <val> = bapi_te_mara.
      ls_extensionin-structure = 'BAPI_TE_MARA'.
      ls_extensionin-valuepart1 = lv_char(240).
      ls_extensionin-valuepart2 = lv_char+240(240).
      ls_extensionin-valuepart3 = lv_char+480(240).
      ls_extensionin-valuepart4 = lv_char+720(240).
      APPEND ls_extensionin TO lt_extensionin.

      CLEAR: lv_char, ls_extensioninx, bapi_te_marax.
      UNASSIGN: <val>.
      bapi_te_marax-material     = gv_matnr.
      bapi_te_marax-zzmdmid      = 'X'.
      ASSIGN lv_char TO <val> CASTING TYPE bapi_te_marax.
      <val> = bapi_te_marax.
      ls_extensioninx-structure  = 'BAPI_TE_MARAX'.
      ls_extensioninx-valuepart1 = lv_char(240).
      ls_extensioninx-valuepart2 = lv_char+240(240).
      ls_extensioninx-valuepart3 = lv_char+480(240).
      ls_extensioninx-valuepart4 = lv_char+720(240).
      APPEND ls_extensioninx TO lt_extensioninx.
    ENDIF.

    REFRESH lt_returnmessages.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata            = ls_headdata
        clientdata          = ls_clientdata
        clientdatax         = ls_clientdatax
      IMPORTING
        return              = ls_return
      TABLES
        materialdescription = lt_materialdescription
        materiallongtext    = lt_materiallongtext
        returnmessages      = lt_returnmessages
        extensionin         = lt_extensionin
        extensioninx        = lt_extensioninx.

    IF ls_return-type EQ 'E'.
      gv_bapi_error = 'x'.
    ENDIF.
    PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
  ELSE. "Material doesn't exist but XML came with an "Update" action
    ls_returnmessage-type = 'E'.            "MESSAGE TYPE
    ls_returnmessage-id     = '00'.
    ls_returnmessage-number = '001'.

    ls_returnmessage-message_v1 = gv_matnr.
    ls_returnmessage-message_v2 = ' - Material NOT exists but XML with'.
    ls_returnmessage-message_v3 = ' Update" action'.
    APPEND ls_returnmessage TO lt_returnmessages.
    PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
  ENDIF.

ENDFORM.                    " CHANGE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CREATE_MATERIAL_MPN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_material_mpn .

  DATA: ls_headdata            TYPE bapimathead,
        ls_clientdata          TYPE  bapi_mara,
        ls_clientdatax         TYPE  bapi_marax,
        ls_return              TYPE bapiret2,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        ls_materialdescription LIKE LINE OF lt_materialdescription,
        lt_returnmessages      TYPE STANDARD TABLE OF bapi_matreturn2,
        ls_returnmessage       LIKE LINE OF lt_returnmessages,
        lv_matnr               TYPE matnr.

  FIELD-SYMBOLS: <hers> TYPE LINE OF ty_t_hers.

  CHECK gv_bapi_error IS INITIAL.

  LOOP AT <xml_attr>-t_hers ASSIGNING <hers> WHERE manufacturer NE ''.
    PERFORM convert_to_internal USING <hers>-external_system_id CHANGING gv_externalid.
    CHECK gv_externalid IS NOT INITIAL.
    SELECT SINGLE matnr FROM mara
                        INTO lv_matnr
                        WHERE matnr EQ gv_externalid.

    IF lv_matnr IS INITIAL.
      PERFORM convert_to_internal USING <hers>-manufacturer CHANGING gv_mfrnr.

      CHECK <hers>-manufacturer_part_number NE ''.

      ls_headdata-material    = gv_externalid.
      ls_headdata-matl_type     = 'HERS'.
      ls_headdata-ind_sector    = 'U'.
      ls_headdata-basic_view    = 'X'.
      ls_headdata-purchase_view = 'X'.

      ls_clientdata-manu_mat = <hers>-manufacturer_part_number.
      ls_clientdatax-manu_mat       = 'X'.

      ls_clientdata-mfr_no         = gv_mfrnr.
      ls_clientdatax-mfr_no         = 'X'.

      ls_clientdata-authoritygroup = 'MDM'.
      ls_clientdatax-authoritygroup = 'X'.

      ls_clientdata-matl_group     = <xml_attr>-material_group.
      ls_clientdatax-matl_group     = 'X'.

      ls_clientdata-inv_mat_no     = gv_matnr.
      ls_clientdatax-inv_mat_no     = 'X'.

      ls_clientdata-base_uom       = <xml_attr>-base_unit_of_measure.
      ls_clientdatax-base_uom       = 'X'.

      ls_clientdata-division       = '00'.
      ls_clientdatax-division       = 'X'.

      ls_clientdata-pur_status     = 'PN'.
      ls_clientdatax-pur_status     = 'X'.

      ls_clientdata-manuf_prof     = 'MP01'.
      ls_clientdatax-manuf_prof     = 'X'.

      ls_clientdata-item_cat       = 'NORM'.
      ls_clientdatax-item_cat       = 'X'.

      ls_clientdata-material_fixed = 'X'.
      ls_clientdatax-material_fixed = 'X'.

      ls_materialdescription-langu     = sy-langu.
      ls_materialdescription-matl_desc = <xml_attr>-sap_short_description.
      APPEND ls_materialdescription TO lt_materialdescription.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata            = ls_headdata
          clientdata          = ls_clientdata
          clientdatax         = ls_clientdatax
        IMPORTING
          return              = ls_return
        TABLES
          materialdescription = lt_materialdescription
          returnmessages      = lt_returnmessages.

      IF ls_return-type EQ 'E'.
        gv_bapi_error = 'x'.
      ENDIF.
      PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
    ELSE.
      ls_returnmessage-type   = 'E'.
      ls_returnmessage-id     = '00'.
      ls_returnmessage-number = '001'.
      CONCATENATE 'Material' lv_matnr 'already exists'
      INTO ls_returnmessage-message_v1 SEPARATED BY space.
      APPEND ls_returnmessage TO lt_returnmessages.
      PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_MATERIAL_MPN
*&---------------------------------------------------------------------*
*&      Form  CREATE_MAT_CLASSIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_mat_classification .

  DATA: lt_allocvaluesnum  TYPE STANDARD TABLE OF  bapi1003_alloc_values_num,
        ls_allocvaluesnum  LIKE LINE OF lt_allocvaluesnum,
        lt_allocvalueschar TYPE STANDARD TABLE OF  bapi1003_alloc_values_char,
        ls_allocvalueschar LIKE LINE OF lt_allocvalueschar,
        lt_allocvaluescurr TYPE STANDARD TABLE OF  bapi1003_alloc_values_curr,
        ls_allocvaluescurr LIKE LINE OF lt_allocvaluescurr,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        ls_return          LIKE LINE OF lt_return.

  DATA: classnumnew  TYPE bapi1003_key-classnum,
        objectkeynew TYPE bapi1003_key-object,
        lv_klart     TYPE kssk-klart.

  FIELD-SYMBOLS: <class> TYPE LINE OF ty_t_class.

  CHECK gv_bapi_error IS INITIAL.

  CHECK gv_matnr IS NOT INITIAL.
  CHECK <xml_attr>-t_class[] IS NOT INITIAL.
  SELECT SINGLE klart FROM kssk
                      INTO lv_klart
                      WHERE objek EQ gv_matnr.

  IF lv_klart IS INITIAL.
    LOOP AT <xml_attr>-t_class ASSIGNING <class>.
      CONCATENATE 'MDM' <class>-atnam INTO ls_allocvalueschar-charact.
      ls_allocvalueschar-value_char    = <class>-atwrt.
      ls_allocvalueschar-charact_descr = <class>-charac_name.
      APPEND ls_allocvalueschar TO lt_allocvalueschar.
    ENDLOOP.

    classnumnew  = <class>-class.
    objectkeynew = gv_matnr.

    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew    = objectkeynew
        objecttablenew  = 'MARA'
        classnumnew     = classnumnew
        classtypenew    = '001' " mat class
        status          = '1'
        standardclass   = 'X'
      TABLES
        allocvaluesnum  = lt_allocvaluesnum
        allocvalueschar = lt_allocvalueschar
        allocvaluescurr = lt_allocvaluescurr
        return          = lt_return.

    LOOP AT lt_return INTO ls_return WHERE type EQ 'E'. ENDLOOP.
    IF ls_return-type EQ 'E'.
      gv_bapi_error = 'x'.
    ENDIF.
    PERFORM fill_log_with_xmldata USING 'CLASS' lt_return[] CHANGING gt_applog[].
  ELSE.
    ls_return-type   = 'E'.
    ls_return-id     = '00'.
    ls_return-number = '001'.
    CONCATENATE 'Material classification already exists for material ' gv_matnr
    INTO ls_return-message_v1 SEPARATED BY space.
    APPEND ls_return TO lt_return.
    PERFORM fill_log_with_xmldata USING 'CLASS' lt_return[] CHANGING gt_applog[].
  ENDIF.

ENDFORM.                    " CREATE_MAT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  FORMAT_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RETURNMESSAGE  text
*----------------------------------------------------------------------*
FORM format_message  USING ps_msg TYPE bapi_matreturn2.

  CALL FUNCTION 'FORMAT_MESSAGE'
    EXPORTING
      id        = ps_msg-id
      lang      = 'EN'
      no        = ps_msg-number
      v1        = ps_msg-message_v1
      v2        = ps_msg-message_v2
      v3        = ps_msg-message_v3
      v4        = ps_msg-message_v4
    IMPORTING
      msg       = gs_msg-msg
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  APPEND gs_msg TO gt_msg.
  CLEAR gs_msg.

ENDFORM.                    "FORMAT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CREATE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_log USING pt_msgs TYPE ty_t_bapi_matreturn2.

  DATA: ls_log        TYPE bal_s_log,
        ls_log_handle TYPE balloghndl,
        lt_msg        TYPE STANDARD TABLE OF bal_s_msg,
        ls_msg        LIKE LINE OF lt_msg,
        lc_set        TYPE boolean,
        lt_log_handle TYPE bal_t_logh,
        lt_context    TYPE STANDARD TABLE OF bal_s_cont,
        ls_params     TYPE bal_s_parm,
        lt_param      TYPE bal_t_par.

  DATA: l_ref   TYPE REF TO cl_abap_tabledescr,
        l_dref  TYPE REF TO cl_abap_structdescr,
        text    TYPE string,
        lo_cast TYPE REF TO cx_sy_move_cast_error.

  FIELD-SYMBOLS: <msg>     LIKE LINE OF pt_msgs,
                 <context> LIKE LINE OF lt_context,
                 <val>     TYPE any,
                 <cont>    LIKE LINE OF lt_context,
                 <comp>    TYPE abap_compdescr.

  ls_log-extnumber = <xml_attr>-entity_id.
  ls_log-object    = 'ZRIVERSAND'.     " OBJECT NAME
*  CONCATENATE gv_matnr gv_externalid INTO ls_log-subobject SEPARATED BY '/'.
  IF <xml_attr>-action EQ 'Add'.
    ls_log-subobject = 'CREATE'.
  ELSE.
    ls_log-subobject = 'UPDATE'.
  ENDIF.
*  LS_LOG-SUBOBJECT = <XML_ATTR>-ENTITY_ID.

  ls_log-aluser = sy-uname.        " USERNAME
  ls_log-alprog = sy-repid.        " REPORT NAME
  ls_log-aldate = sy-datum.
  ls_log-altime = sy-uzeit.
  ls_log-aluser = sy-uname.

***Open Log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ls_log
    IMPORTING
      e_log_handle            = ls_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  IF sy-subrc EQ 0.
    LOOP AT pt_msgs ASSIGNING <msg> WHERE type EQ 'E' OR type EQ 'S'.
***Create message
      ls_msg-msgty = <msg>-type.            "MESSAGE TYPE
      ls_msg-msgid = <msg>-id.              "MESSAGE ID
      ls_msg-msgno = <msg>-number.          "MESSAGE NUMBER
      ls_msg-msgv1 = <msg>-message_v1.      "TEXT THAT YOU WANT TO PASS AS MESSAGE
      ls_msg-msgv2 = <msg>-message_v2.
      ls_msg-msgv3 = <msg>-message_v3.
      ls_msg-msgv4 = <msg>-message_v4.
      APPEND ls_msg TO lt_msg.
    ENDLOOP.


    PERFORM fill_log_with_xmldata USING 'MAT' pt_msgs CHANGING gt_applog[].
    LOOP AT lt_msg INTO ls_msg.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = ls_log_handle
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      APPEND ls_log_handle TO lt_log_handle.
    ENDLOOP.

***Save message
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_save_all       = ' '
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc EQ 0.
      REFRESH: lt_log_handle.
    ENDIF.
  ENDIF.

ENDFORM.                    "CREATE_LOG
*&---------------------------------------------------------------------*
*&      Form  FILL_PARAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PARAM[]  text
*----------------------------------------------------------------------*
FORM fill_params USING pt_params TYPE bal_t_par.

  DATA: l_ref    TYPE REF TO cl_abap_tabledescr,
        l_dref   TYPE REF TO cl_abap_structdescr,
        text     TYPE string,
        lo_cast  TYPE REF TO cx_sy_move_cast_error,
        lv_val   TYPE string,
        lv_lines TYPE i,
        lv_tabix TYPE sytabix,
        text_tab TYPE STANDARD TABLE OF ty_tdline.

  FIELD-SYMBOLS: <comp>  TYPE abap_compdescr,
                 <val>   TYPE any,
                 <param> LIKE LINE OF pt_params,
                 <class> LIKE LINE OF lt_class,
                 <text>  LIKE LINE OF text_tab.
  TRY.
      l_ref ?= cl_abap_typedescr=>describe_by_data( lt_xml_attr ).
      l_dref ?= l_ref->get_table_line_type( ).
      LOOP AT l_dref->components ASSIGNING <comp>.
        IF <comp>-name NE 'T_CLASS' AND <comp>-name NE 'T_ATTR' AND <comp>-name NE 'LONG_DESCRIPTION'.
          ASSIGN COMPONENT <comp>-name OF STRUCTURE <xml_attr> TO <val>.
          APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
          lv_val = <val>.
          <param>-parname = <comp>-name.
          CONCATENATE <comp>-name lv_val INTO <param>-parvalue SEPARATED BY '/'.
        ELSE.
          IF <comp>-name EQ 'LONG_DESCRIPTION'.
            IF <xml_attr>-long_description NE ''.
              CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
                EXPORTING
                  text        = <xml_attr>-long_description
                  line_length = 75
                  langu       = sy-langu
                TABLES
                  text_tab    = text_tab.
              lv_lines = lines( text_tab ).
              LOOP AT text_tab ASSIGNING <text>.
                lv_tabix = sy-tabix.

                IF lv_tabix EQ '1'.
                  APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
                  <param>-parname = <comp>-name.
                  <param>-parvalue = '---Long Description START---'.
                ENDIF.
                APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
                <param>-parname = <comp>-name.
                <param>-parvalue = <text>-tdline.
                IF lv_tabix EQ lv_lines.
                  APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
                  <param>-parname = <comp>-name.
                  <param>-parvalue = '---Long Description END---'.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    CATCH cx_sy_move_cast_error INTO lo_cast.
      text = lo_cast->get_text( ).
  ENDTRY.

  lv_lines = lines( <xml_attr>-t_class ).
  LOOP AT <xml_attr>-t_class ASSIGNING <class>.
    lv_tabix = sy-tabix.
    IF lv_tabix EQ '1'.
      APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
      <param>-parvalue = '---Classification Data START---'.
    ENDIF.
    APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
    CONCATENATE <class>-class <class>-class_desc <class>-charac_name
                <class>-atnam <class>-atwrt INTO <param>-parvalue
                SEPARATED BY '/'.
    IF lv_tabix EQ lv_lines.
      APPEND INITIAL LINE TO pt_params ASSIGNING <param>.
      <param>-parvalue = '---Classification Data END---'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "FILL_PARAMS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_MATERIAL_MPN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_material_mpn .

  DATA: ls_headdata            TYPE bapimathead,
        ls_clientdata          TYPE  bapi_mara,
        ls_clientdatax         TYPE  bapi_marax,
        ls_return              TYPE bapiret2,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        ls_materialdescription LIKE LINE OF lt_materialdescription,
        lt_returnmessages      TYPE STANDARD TABLE OF bapi_matreturn2,
        ls_returnmessage       LIKE LINE OF lt_returnmessages,
        ls_mara                TYPE mara.

  FIELD-SYMBOLS: <hers> TYPE LINE OF ty_t_hers.

  CHECK <xml_attr>-manufacturer_part_number IS NOT INITIAL OR
        <xml_attr>-external_system_id       IS NOT INITIAL OR
        <xml_attr>-manufacturer             IS NOT INITIAL OR
        <xml_attr>-material_group           IS NOT INITIAL OR
        <xml_attr>-base_unit_of_measure     IS NOT INITIAL OR
        <xml_attr>-sap_short_description    IS NOT INITIAL.

  CHECK gv_bapi_error IS INITIAL.
  LOOP AT <xml_attr>-t_hers ASSIGNING <hers> WHERE manufacturer NE ''.
    PERFORM convert_to_internal USING <hers>-external_system_id
                 CHANGING gv_externalid.
    CHECK gv_externalid IS NOT INITIAL.
    SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr EQ gv_externalid.
    IF sy-subrc EQ 0.

      ls_headdata-basic_view    = 'X'.
      ls_headdata-purchase_view = 'X'.
      ls_headdata-material   = ls_mara-matnr.
* Execute BAPI to open up locked fields to update
*      IF gv_fixed EQ ''.
      ls_clientdata-material_fixed  = ''.
      ls_clientdatax-material_fixed = 'X'.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata    = ls_headdata
          clientdata  = ls_clientdata
          clientdatax = ls_clientdatax
        IMPORTING
          return      = ls_return.

      IF ls_return-type NE 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        ls_returnmessage-type = 'E'.
        CONCATENATE gv_matnr ' - Could not lock the material to EDIT'
         INTO ls_returnmessage-message_v1 SEPARATED BY space.
        APPEND ls_returnmessage TO lt_returnmessages.
        PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
        EXIT.
      ENDIF.

      ls_headdata-basic_view = 'X'.
      ls_headdata-material   = gv_externalid.

      IF <hers>-manufacturer_part_number IS NOT INITIAL AND
         <hers>-manufacturer_part_number NE ls_mara-mfrpn.
        ls_clientdata-manu_mat = <hers>-manufacturer_part_number.
        ls_clientdata-manu_mat = 'x'.
      ENDIF.

      PERFORM convert_to_internal USING <hers>-manufacturer
                                  CHANGING gv_mfrnr.

      IF gv_mfrnr IS NOT INITIAL AND gv_mfrnr NE ls_mara-mfrnr.
        ls_clientdata-mfr_no         = gv_mfrnr.
        ls_clientdatax-mfr_no         = 'X'.
      ENDIF.

      IF <xml_attr>-material_group IS NOT INITIAL AND
         <xml_attr>-material_group NE ls_mara-matkl.
        ls_clientdata-matl_group     = <xml_attr>-material_group.
        ls_clientdatax-matl_group    = 'X'.
      ENDIF.

      IF <xml_attr>-base_unit_of_measure IS NOT INITIAL AND
        <xml_attr>-base_unit_of_measure NE ls_mara-meins.
        ls_clientdata-base_uom       = <xml_attr>-base_unit_of_measure.
        ls_clientdatax-base_uom      = 'X'.
      ENDIF.

      IF <xml_attr>-sap_short_description IS NOT INITIAL.
        ls_materialdescription-langu     = sy-langu.
        ls_materialdescription-matl_desc = <xml_attr>-sap_short_description.
        APPEND ls_materialdescription TO lt_materialdescription.
      ENDIF.

      ls_clientdata-material_fixed  = 'X'.
      ls_clientdatax-material_fixed = 'X'.

      ls_headdata-matl_type     = 'HERS'.
      ls_headdata-ind_sector    = 'U'.

      ls_clientdata-division       = '00'.
      ls_clientdatax-division      = 'X'.
      ls_clientdata-pur_status     = 'PN'.
      ls_clientdatax-pur_status    = 'X'.
      ls_clientdata-manuf_prof     = 'MP01'.
      ls_clientdatax-manuf_prof    = 'X'.
      ls_clientdata-item_cat       = 'NORM'.
      ls_clientdatax-item_cat      = 'X'.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata            = ls_headdata
          clientdata          = ls_clientdata
          clientdatax         = ls_clientdatax
        IMPORTING
          return              = ls_return
        TABLES
          materialdescription = lt_materialdescription
          returnmessages      = lt_returnmessages.

      IF ls_return-type EQ 'E'.
        gv_bapi_error = 'x'.
      ENDIF.
      PERFORM fill_log_with_xmldata USING 'MPN' lt_returnmessages[] CHANGING gt_applog[].
    ELSE.
      ls_returnmessage-id     = '00'.
      ls_returnmessage-number = '001'.
      ls_returnmessage-type = 'E'.            "MESSAGE TYPE
      ls_returnmessage-message_v1 = gv_matnr.
      ls_returnmessage-message_v2 = ' - HERS Material NOT exists but XML with '.
      ls_returnmessage-message_v3 = 'Update" action'.
      APPEND ls_returnmessage TO lt_returnmessages.
      PERFORM fill_log_with_xmldata USING 'MPN' lt_returnmessages[] CHANGING gt_applog[].
    ENDIF.
  ENDLOOP.

ENDFORM.                    "CHANGE_MATERIAL_MPN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_MAT_CLASSIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_mat_classification .

  DATA: lt_allocvaluesnum    TYPE STANDARD TABLE OF  bapi1003_alloc_values_num,
        ls_allocvaluesnum    LIKE LINE OF lt_allocvaluesnum,
        lt_allocvalueschar   TYPE STANDARD TABLE OF  bapi1003_alloc_values_char,
        lt_allocvalueschar_n TYPE STANDARD TABLE OF  bapi1003_alloc_values_char,
        ls_allocvalueschar   LIKE LINE OF lt_allocvalueschar,
        lt_allocvaluescurr   TYPE STANDARD TABLE OF  bapi1003_alloc_values_curr,
        ls_allocvaluescurr   LIKE LINE OF lt_allocvaluescurr,
        lt_return            TYPE STANDARD TABLE OF bapiret2,
        ls_return            LIKE LINE OF lt_return,
        lt_alloc_list        TYPE STANDARD TABLE OF bapi1003_alloc_list.

  DATA: classnumnew  TYPE bapi1003_key-classnum,
        objectkeynew TYPE bapi1003_key-object.

  FIELD-SYMBOLS: <class>        TYPE LINE OF ty_t_class,
                 <valueschar_n> LIKE LINE OF lt_allocvalueschar_n,
                 <valueschar>   LIKE LINE OF lt_allocvalueschar.

  DATA: lv_klart  TYPE kssk-klart,
        lv_clint  TYPE kssk-clint,
        lv_class  TYPE klah-class,
        lv_lines  TYPE i,
        lv_lines2 TYPE i,
        lv_upd    TYPE c.

  CHECK gv_bapi_error IS INITIAL.
  CHECK gv_matnr IS NOT INITIAL.
  SELECT SINGLE klart clint FROM kssk INTO (lv_klart, lv_clint)
                                      WHERE objek EQ gv_matnr.
  IF sy-subrc EQ 0.
    SELECT SINGLE class FROM klah INTO lv_class
                                  WHERE clint EQ lv_clint
                                  AND   klart EQ lv_klart.
    objectkeynew = gv_matnr.
    classnumnew = lv_class.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = objectkeynew
        objecttable     = 'MARA'
        classnum        = classnumnew
        classtype       = '001'
      TABLES
        allocvaluesnum  = lt_allocvaluesnum
        allocvalueschar = lt_allocvalueschar
        allocvaluescurr = lt_allocvaluescurr
        return          = lt_return.

* If the class has changed then delete the class
* first then we are going to create the new assignment
    IF lv_class NE <xml_attr>-class.
      lv_upd = 'X'.
      objectkeynew = gv_matnr.
      classnumnew  = lv_class.
      CALL FUNCTION 'BAPI_OBJCL_DELETE'
        EXPORTING
          objectkey   = objectkeynew
          objecttable = 'MARA'
          classnum    = classnumnew
          classtype   = '001'
        TABLES
          return      = lt_return.
      LOOP AT lt_return INTO ls_return WHERE type EQ 'E'. ENDLOOP.
      IF ls_return-type EQ 'E'.
        PERFORM fill_log_with_xmldata USING 'CLASS' lt_return[] CHANGING gt_applog[].
        EXIT. "Stop furtehr processing of this subroutine
      ENDIF.
    ENDIF.

    SORT lt_allocvalueschar.
    lv_lines = lines( lt_allocvalueschar ).

    CHECK <xml_attr>-t_class[] IS NOT INITIAL.
    LOOP AT <xml_attr>-t_class ASSIGNING <class>.
      CONCATENATE 'MDM' <class>-atnam INTO ls_allocvalueschar-charact.
      ls_allocvalueschar-value_char    = <class>-atwrt.
      ls_allocvalueschar-charact_descr = <class>-charac_name.
      APPEND ls_allocvalueschar TO lt_allocvalueschar_n.
    ENDLOOP.

    SORT lt_allocvalueschar_n.
    lv_lines2 = lines( lt_allocvalueschar_n ).

* Comapare existing and new characterristics to decide if
* characteristics needs to be updated or not
    IF lv_upd EQ ''.
*If lv_upd flag is not already set to an X before
* then check for characteristics change.
      IF lv_lines EQ lv_lines2 AND lv_upd EQ ''.
*  Same number of characteristics but check if any value changed
        LOOP AT lt_allocvalueschar_n ASSIGNING <valueschar_n>.
          LOOP AT lt_allocvalueschar ASSIGNING <valueschar>.
            IF <valueschar_n>-charact    EQ <valueschar>-charact AND
               <valueschar_n>-value_char EQ <valueschar>-value_char.
            ELSE.
              lv_upd = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        lv_upd = 'X'.
      ENDIF.
    ENDIF.

    CHECK lv_upd = 'X'. " Something changed so update the characteristics
    classnumnew  = <class>-class.
    objectkeynew = gv_matnr.
    REFRESH: lt_return,lt_allocvaluesnum, lt_allocvaluescurr.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objectkey          = objectkeynew
        objecttable        = 'MARA'
        classnum           = classnumnew
        classtype          = '001' " mat class
      TABLES
        allocvaluesnumnew  = lt_allocvaluesnum
        allocvaluescharnew = lt_allocvalueschar_n
        allocvaluescurrnew = lt_allocvaluescurr
        return             = lt_return.

    LOOP AT lt_return INTO ls_return WHERE type EQ 'E'. ENDLOOP.
    IF ls_return-type EQ 'E'.
      gv_bapi_error = 'x'.
    ENDIF.
    PERFORM fill_log_with_xmldata USING 'CLASS' lt_return[] CHANGING gt_applog[].
  ELSE.
    ls_return-id     = '00'.
    ls_return-number = '001'.
    ls_return-type = 'E'.            "MESSAGE TYPE
    ls_return-message_v1 = gv_matnr.
    ls_return-message_v2 = ' - XML with "Update" action but Material '.
    ls_return-message_v3 = 'Classification NOT exists'.
    APPEND ls_return TO lt_return.
    PERFORM fill_log_with_xmldata USING 'CLASS' lt_return[] CHANGING gt_applog[].
  ENDIF.

ENDFORM.                    "CHANGE_MAT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  MARK_X
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0703   text
*----------------------------------------------------------------------*
FORM mark_x USING ps_name TYPE any
                  ps_data TYPE any.

  DATA: lr_data TYPE REF TO data.
  DATA: l_ref  TYPE REF TO cl_abap_tabledescr,
        l_dref TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <comp>  TYPE abap_compdescr.

  FIELD-SYMBOLS: <struc> TYPE any.

  CREATE DATA lr_data TYPE (ps_name) .
* assign the data reference to a field symbol
  ASSIGN lr_data->* TO <struc>.

  l_ref ?= cl_abap_typedescr=>describe_by_data( <struc> ).
  l_dref ?= l_ref->get_table_line_type( ).

ENDFORM.                    "MARK_X
*&---------------------------------------------------------------------*
*&      Form  FILL_LOG_WITH_XMLDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MSG[]  text
*----------------------------------------------------------------------*
FORM fill_log_with_xmldata USING    p_context TYPE any
                                    pt_msgs   TYPE ty_t_bapi_matreturn2
                           CHANGING pt_applog TYPE ty_t_bal_s_msg.

  DATA: l_ref       TYPE REF TO cl_abap_tabledescr,
        l_dref      TYPE REF TO cl_abap_structdescr,
        text        TYPE string,
        lo_cast     TYPE REF TO cx_sy_move_cast_error,
        lv_val      TYPE string,
        lv_lines    TYPE i,
        lv_tabix    TYPE sytabix,
        text_tab    TYPE STANDARD TABLE OF ty_tdline,
        ls_msg      LIKE LINE OF pt_applog,
        l_s_context TYPE name2value.

  FIELD-SYMBOLS: <comp>  TYPE abap_compdescr,
                 <val>   TYPE any,
                 <msg>   LIKE LINE OF pt_msgs,
                 <attr>  TYPE LINE OF ty_t_attr,
                 <hers>  TYPE LINE OF ty_t_hers,
                 <class> LIKE LINE OF lt_class,
                 <text>  LIKE LINE OF text_tab.

* Fill application log table with messaged returned from BAPI
  CLEAR: ls_msg.
  ls_msg-msgty           = 'W'.
  ls_msg-msgid           = '00'.
  ls_msg-msgno           = '001'.
  IF p_context EQ 'CLASS'.
    ls_msg-msgv1 = '----------- Material Classification BAPI Log START -----------'.
    ls_msg-msgv1_src = p_context.
  ELSEIF p_context EQ 'MAT'.
    ls_msg-msgv1 = '----------- Material Creation BAPI Log START -----------'.
    ls_msg-msgv1_src = p_context.
  ELSEIF p_context EQ 'MPN'.
    ls_msg-msgv1 = '----------- Material(HERS) Creation BAPI Log START -----------'.
    ls_msg-msgv2 = gv_externalid.
    ls_msg-msgv1_src = p_context.
  ELSEIF p_context EQ 'DEL'.
    ls_msg-msgv1 = '----------- Material Deletion BAPI Log START -----------'.
    ls_msg-msgv1_src = p_context.
  ENDIF.
  APPEND ls_msg TO pt_applog.
  CLEAR: ls_msg.

  LOOP AT pt_msgs ASSIGNING <msg> WHERE type EQ 'E' OR type EQ 'S'.
***Create message
    IF <msg>-type EQ 'E'.
      gv_bapi_error = <msg>-type.
      ls_msg-probclass = '1'.
    ENDIF.
    ls_msg-msgty = <msg>-type.            "MESSAGE TYPE
    ls_msg-msgid = <msg>-id.              "MESSAGE ID
    ls_msg-msgno = <msg>-number.          "MESSAGE NUMBER
    ls_msg-msgv1 = <msg>-message_v1.      "TEXT THAT YOU WANT TO PASS AS MESSAGE
    ls_msg-msgv2 = <msg>-message_v2.
    ls_msg-msgv3 = <msg>-message_v3.
    ls_msg-msgv4 = <msg>-message_v4.
    ls_msg-msgv1_src = p_context.
    APPEND ls_msg TO pt_applog.
  ENDLOOP.

  CLEAR: ls_msg.
  ls_msg-msgty           = 'W'.
  ls_msg-msgid           = '00'.
  ls_msg-msgno           = '001'.
  IF p_context EQ 'CLASS'.
    ls_msg-msgv1 = '----------- Material Classification BAPI Log END -----------'.
    ls_msg-msgv1_src = p_context.
  ELSEIF p_context EQ 'MAT'.
    ls_msg-msgv1 = '----------- Material Creation BAPI Log END -----------'.
    ls_msg-msgv1_src = p_context.
  ELSEIF p_context EQ 'DEL'.
    ls_msg-msgv1 = '----------- Material Deletion BAPI Log END -----------'.
    ls_msg-msgv1_src = p_context.
  ELSEIF p_context EQ 'MPN'.
    ls_msg-msgv1 = '----------- Material(HERS) Creation BAPI Log END -----------'.
    ls_msg-msgv1_src = p_context.
  ENDIF.
  APPEND ls_msg TO pt_applog.
  CLEAR: ls_msg.

**********Fill application log table with XML data****************

* Check if GV_XML_FLAG is blank meaning xml data is already written for the entity.
* we only want to write XML data in the long for once
  IF gv_xml_flag = ''.
    ls_msg-msgty           = 'I'.
    ls_msg-msgid           = '00'.
    ls_msg-msgno           = '001'.
    ls_msg-msgv1 = '**********DATA FROM XML FILE BELOW**********'.
    ls_msg-probclass       = '4'.
    ls_msg-msgv1_src = p_context.
    APPEND ls_msg TO gt_xml_data.
    CLEAR: ls_msg.
    TRY.
        l_ref ?= cl_abap_typedescr=>describe_by_data( lt_xml_attr ).
        l_dref ?= l_ref->get_table_line_type( ).
        LOOP AT l_dref->components ASSIGNING <comp>.
          IF <comp>-name NE 'T_CLASS' AND <comp>-name NE 'T_ATTR' AND
             <comp>-name NE 'T_HERS' AND  <comp>-name NE 'LONG_DESCRIPTION'.
            ASSIGN COMPONENT <comp>-name OF STRUCTURE <xml_attr> TO <val>.
            ls_msg-msgty           = 'I'.
            ls_msg-msgid           = '00'.
            ls_msg-msgno           = '001'.
            CONCATENATE 'XML DATA::' <comp>-name '=' INTO ls_msg-msgv1.
            ls_msg-msgv2           = <val>.
            ls_msg-probclass       = '4'.
            APPEND ls_msg TO gt_xml_data.
            CLEAR: ls_msg.

          ELSE.
            IF <comp>-name EQ 'LONG_DESCRIPTION'.
              IF <xml_attr>-long_description NE ''.
                CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
                  EXPORTING
                    text        = <xml_attr>-long_description
                    line_length = 50
                    langu       = sy-langu
                  TABLES
                    text_tab    = text_tab.
                lv_lines = lines( text_tab ).
                LOOP AT text_tab ASSIGNING <text>.
                  lv_tabix = sy-tabix.
                  ls_msg-msgty           = 'I'.
                  ls_msg-msgid           = '00'.
                  ls_msg-msgno           = '001'.
                  CONCATENATE 'XML DATA::' <comp>-name '=' INTO ls_msg-msgv1.
                  ls_msg-probclass       = '4'.
                  ls_msg-msgv2 = <text>-tdline.
                  APPEND ls_msg TO gt_xml_data.
                  CLEAR: ls_msg.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH cx_sy_move_cast_error INTO lo_cast.
        text = lo_cast->get_text( ).
    ENDTRY.

* Write HERS material type data - MPN Data
    LOOP AT <xml_attr>-t_hers ASSIGNING <hers> WHERE manufacturer NE ''.
      ls_msg-msgty           = 'I'.
      ls_msg-msgid           = '00'.
      ls_msg-msgno           = '001'.
      CONCATENATE 'XML MPN(HERS) DATA::' <hers>-external_system_id '/' INTO ls_msg-msgv1.
      CONCATENATE <hers>-action '/' INTO ls_msg-msgv2.
      CONCATENATE <hers>-manufacturer <hers>-manufacturer_name <hers>-manufacturer_part_number
      INTO ls_msg-msgv3 SEPARATED BY  '/'.
      ls_msg-probclass       = '4'.
      APPEND ls_msg TO gt_xml_data.
      CLEAR: ls_msg.
    ENDLOOP.

* Write material characteristics data
    LOOP AT <xml_attr>-t_class ASSIGNING <class>.
      ls_msg-msgty           = 'I'.
      ls_msg-msgid           = '00'.
      ls_msg-msgno           = '001'.
      CONCATENATE 'XML Material Characteristics DATA::' <class>-class '/' INTO ls_msg-msgv1.
      CONCATENATE <class>-class_desc '/' INTO ls_msg-msgv2.
      CONCATENATE <class>-charac_name <class>-atnam <class>-atwrt INTO ls_msg-msgv3 SEPARATED BY  '/'.
      ls_msg-probclass       = '4'.
      APPEND ls_msg TO gt_xml_data.
      CLEAR: ls_msg.
    ENDLOOP.
    gv_xml_flag = 'X'.
  ENDIF.

ENDFORM.                    " FILL_LOG_WITH_XMLDATA

*--------------------------------------------------------------------
* FORM LOG_DISPLAY
*--------------------------------------------------------------------
FORM log_display.
  DATA:
    l_s_display_profile TYPE bal_s_prof,
    l_s_fcat            TYPE bal_s_fcat.

* get standard display profile
  CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
    IMPORTING
      e_s_display_profile = l_s_display_profile
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  l_s_display_profile-disvariant-report = sy-repid.
  l_s_display_profile-disvariant-handle = 'LOG'.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile = l_s_display_profile
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "log_display
*&---------------------------------------------------------------------*
*&      Form  PARSE_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM parse_xml .

  DATA: lv_pidx  TYPE string,
        lv_hers  TYPE string,
        lv_value TYPE string.

  FIELD-SYMBOLS: <str> LIKE LINE OF gt_str,
                 <hers> TYPE LINE OF  ty_t_hers.

  LOOP AT gt_str ASSIGNING <str>.
    SHIFT <str> LEFT DELETING LEADING space.
    IF <str>+0(6) EQ '<Entit' AND <str>+0(10) NE '<Entities>'. " <str> NE '<Entities>'.
      " Read the node '<Entity
      APPEND INITIAL LINE TO lt_xml_attr ASSIGNING <xml_attr>.
      PERFORM get_node_attribute USING <str> 'Entity' 'Id'
      CHANGING <xml_attr>-entity_id.
      PERFORM get_node_attribute USING <str> 'Entity' 'ExternalId'
      CHANGING <xml_attr>-externalid.
      PERFORM get_node_attribute USING <str> 'Entity' 'CategoryName'
      CHANGING <xml_attr>-class.
      IF <xml_attr>-class NE ''.
        ls_class-class = <xml_attr>-class.
      ENDIF.
      PERFORM get_node_attribute USING <str> 'Entity' 'CategoryLongName'
      CHANGING <xml_attr>-class_desc.
      IF <xml_attr>-class_desc NE ''.
        ls_class-class_desc = <xml_attr>-class_desc.
      ENDIF.

      PERFORM get_node_attribute USING <str> 'Entity' 'Action'
      CHANGING <xml_attr>-action.

    ELSEIF <str>+0(6) EQ '<Attri'.  " Read the node '<Attribute
      IF <str>+0(10) EQ '<Attribute' AND <str>+0(12) NE '<Attributes>'.
        PERFORM get_node_attribute USING <str> 'Attribute' 'Id'
        CHANGING ls_attr-attr_id.
        PERFORM get_node_attribute USING <str> 'Attribute' 'Name'
        CHANGING ls_attr-name.
        PERFORM get_node_attribute USING <str> 'Attribute' 'AttributeParentName'
     CHANGING lv_pidx.
        IF lv_pidx EQ 'PIDX Attributes'.
          ls_class-charac_name = ls_attr-name.
          ls_attr-name         = lv_pidx.
          ls_class-atnam       = ls_attr-attr_id.
        ENDIF.

        IF ls_attr-name EQ 'External System ID'.
*          CLEAR: gs_hers.
          UNASSIGN: <hers>.
          APPEND INITIAL LINE TO <xml_attr>-t_hers ASSIGNING <hers>.
          PERFORM get_node_attribute USING <str> 'Attribute' 'Action'
          CHANGING <hers>-action.
        ENDIF.
      ENDIF.
    ELSEIF <str>+0(6) EQ '<Value'.  " Read the node '<Value
      IF <str>+0(7)  NE '<Values'.
        PERFORM get_node_attribute USING <str> 'Value' 'Action' CHANGING
        ls_attr-action.
        CREATE OBJECT xml_doc.
        xml_doc->parse_string( stream = <str> ).
        DATA: lo_node TYPE REF TO if_ixml_node.
        xml_doc->find_node( EXPORTING name = 'Value'
                             RECEIVING node = lo_node ).
        IF lo_node IS BOUND.
          lv_value = lo_node->get_value( ).
          ls_attr-value  = lv_value.

          IF ls_attr-name EQ 'PIDX Attributes'.
            ls_class-atwrt  = lv_value.
            APPEND ls_class TO <xml_attr>-t_class.
          ENDIF.

          IF ls_attr-name EQ 'External System ID'.
            <hers>-external_system_id  = lv_value.
          ENDIF.

          IF ls_attr-name EQ 'Manufacturer'.
            <hers>-manufacturer = lv_value.
            PERFORM convert_to_internal USING <hers>-manufacturer
                  CHANGING <hers>-manufacturer.
          ELSEIF ls_attr-name EQ 'Manufacturer Name'.
            <hers>-manufacturer_name = lv_value.
          ELSEIF ls_attr-name EQ 'Manufacturer Part Number'.
            <hers>-manufacturer_part_number = lv_value.
            PERFORM convert_to_internal USING <hers>-manufacturer_part_number
                   CHANGING <hers>-manufacturer_part_number.
          ELSEIF ls_attr-name EQ 'Vendor'.
            <hers>-vendor = lv_value.
          ELSEIF ls_attr-name EQ 'Vendor Part Number'.
            <hers>-vendor_part_no = lv_value.
          ENDIF.

          IF <xml_attr>-action EQ 'Add'
             OR ( <xml_attr>-action EQ 'Update' AND ls_attr-action NE 'NoChange' )
             OR ( ls_attr-name EQ 'SAP Material Number' OR
                  ls_attr-name EQ 'External System ID'  OR
                  ls_attr-name EQ 'Manufacturer' ).
            PERFORM fill_values USING ls_attr-name lv_value CHANGING <xml_attr>.
          ENDIF.
        ENDIF.
        APPEND ls_attr TO <xml_attr>-t_attr.
      ENDIF.
    ELSEIF <str>+0(6) EQ '</Entit' AND <str> NE '<Entities>'.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " PARSE_XML
*&---------------------------------------------------------------------*
*&      Form  READ_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_xml .

  DATA: lv_message TYPE string,
        lv_str     TYPE string,
        lv_file    TYPE rlgrap-filename,
        ls_data    LIKE LINE OF gt_str.

  CLEAR: gv_dset_msg.
  REFRESH: gt_str.
  IF p_pc IS NOT INITIAL.
    lv_str = p_fname.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename      = lv_str
        filetype      = 'ASC'
        header_length = 0
      IMPORTING
        filelength    = gv_size
      TABLES
        data_tab      = gt_str
      EXCEPTIONS
        OTHERS        = 1.
  ELSE.
    lv_file = p_fname.
    OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE gv_dset_msg.
    IF sy-subrc NE 0.
      WRITE / gv_dset_msg.
      STOP. " stop the execution of the program in case of error
    ENDIF.
    DO.
      READ DATASET lv_file INTO ls_data.
      IF sy-subrc EQ 0.
        APPEND ls_data TO gt_str.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CLOSE DATASET lv_file.
  ENDIF.

ENDFORM.                    " READ_XML
*&---------------------------------------------------------------------*
*&      Form  COMMIT_WORK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM commit_work .

  IF gv_bapi_error IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.                    "COMMIT_WORK
*&---------------------------------------------------------------------*
*&      Form  SAVE_APPLICATION_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_application_log USING pt_applog TYPE ty_t_bal_s_msg.

  DATA: ls_log        TYPE bal_s_log,
        ls_log_handle TYPE balloghndl,
        lt_log_handle TYPE bal_t_logh,
        lt_lognumbers TYPE bal_t_lgnm.

  FIELD-SYMBOLS: <applog> LIKE LINE OF pt_applog.

  IF gt_xml_data[] IS NOT INITIAL.
    APPEND LINES OF gt_xml_data TO gt_applog.
  ENDIF.

  CONCATENATE <xml_attr>-entity_id gv_matnr
         INTO ls_log-extnumber SEPARATED BY '/'.
  ls_log-object    = 'ZRIVERSAND'.     " OBJECT NAME
  IF <xml_attr>-action EQ 'Add'.
    ls_log-subobject = 'CREATE'.
  ELSE.
    ls_log-subobject = 'UPDATE'.
  ENDIF.

  ls_log-aluser = sy-uname.
  ls_log-alprog = sy-repid.
  ls_log-aldate = sy-datum.
  ls_log-altime = sy-uzeit.
  ls_log-aluser = sy-uname.

***Open Log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ls_log
    IMPORTING
      e_log_handle            = ls_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  IF sy-subrc EQ 0.
    DATA: lv_src TYPE balmsgvsrc.

    LOOP AT pt_applog ASSIGNING <applog>.
      IF gv_bapi_error IS INITIAL.
*        No error so make log grren light
        IF <applog>-msgty NE 'I'.
          <applog>-msgty = 'S'.
        ENDIF.
      ELSE.
        IF <applog>-msgty NE 'I'.
          <applog>-probclass = '1'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = ls_log_handle
          i_s_msg          = <applog>
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      APPEND ls_log_handle TO lt_log_handle.
    ENDLOOP.

*    INSERT ls_log_handle INTO lt_log_handle INDEX 1.

***Save message
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_save_all       = ' '
        i_t_log_handle   = lt_log_handle
      IMPORTING
        e_new_lognumbers = lt_lognumbers
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

  ENDIF.

ENDFORM.                    "SAVE_APPLICATION_LOG
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_INTERNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<XML_ATTR>_EXTERNAL_SYSTEM_ID  text
*      <--P_GV_EXTERNALID  text
*----------------------------------------------------------------------*
FORM convert_to_internal  USING    p_input  TYPE  any
                          CHANGING p_output TYPE any.

  CHECK p_input IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input        = p_input
    IMPORTING
      output       = p_output
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  IF sy-subrc EQ 0.

  ENDIF.

ENDFORM.                    "CONVERT_TO_INTERNAL

*&---------------------------------------------------------------------*
*&      Form  BUILD_ATTACHEMNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_attachemnt .

  DATA: gv_document TYPE REF TO cl_document_bcs,
        gt_text     TYPE soli_tab,
        ls_text     LIKE LINE OF gt_text,
        lv_msg      TYPE string.

  FIELD-SYMBOLS: <applog> LIKE LINE OF gt_applog.

  LOOP AT gt_applog ASSIGNING <applog>.

    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = <applog>-msgid
        no        = <applog>-msgno
        v1        = <applog>-msgv1
        v2        = <applog>-msgv2
        v3        = <applog>-msgv3
        v4        = <applog>-msgv4
      IMPORTING
        msg       = lv_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    ls_text = lv_msg.
    APPEND ls_text TO gt_text.
  ENDLOOP.

  gv_document = cl_document_bcs=>create_document(
i_type = 'RAW'
i_text = gt_text
i_length = '12'
i_subject = 'Failed Precalculation Settings!' ).

ENDFORM.                    "BUILD_ATTACHEMNT
*&---------------------------------------------------------------------*
*&      Form  GENERATE_ERROR_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_error_email .

  DATA: lv_send_request  TYPE REF TO cl_bcs,
        lv_document      TYPE REF TO cl_document_bcs,
        lv_sender        TYPE REF TO cl_sapuser_bcs,
        lv_recipient     TYPE REF TO if_recipient_bcs,
        lv_email         TYPE adr6-smtp_addr,
        lv_bcs_exception TYPE REF TO cx_bcs,
        lv_sent_to_all   TYPE os_boolean,
        lv_msg           TYPE string,
        gt_text          TYPE soli_tab,
        ls_text          LIKE LINE OF gt_text,
        lt_idlient       TYPE STANDARD TABLE OF sodlienti1,
        ls_idlient       LIKE LINE OF lt_idlient.

  FIELD-SYMBOLS: <applog> LIKE LINE OF gt_applog.

  TRY.
*     -------- create persistent send request ------------------------
      lv_send_request = cl_bcs=>create_persistent( ).

      LOOP AT gt_applog ASSIGNING <applog>.
*                         WHERE msgty EQ 'E'.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = <applog>-msgid
            no        = <applog>-msgno
            v1        = <applog>-msgv1
            v2        = <applog>-msgv2
            v3        = <applog>-msgv3
            v4        = <applog>-msgv4
          IMPORTING
            msg       = lv_msg
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        ls_text = lv_msg.
        APPEND ls_text TO gt_text.
      ENDLOOP.

*add document to send request
      CHECK gt_text[] IS NOT INITIAL.
      lv_document = cl_document_bcs=>create_document(
                        i_type = 'RAW'
                        i_text = gt_text
                        i_length = '12'
                        i_subject = p_subj ).

*add document to send request
      CALL METHOD lv_send_request->set_document( lv_document ).

*set sender
      lv_sender = cl_sapuser_bcs=>create( sy-uname ).
      CALL METHOD lv_send_request->set_sender
        EXPORTING
          i_sender = lv_sender.

*add recipient (e-mail address)
      IF s_email[] IS NOT INITIAL.
        LOOP AT s_email.
          MOVE s_email-low TO lv_email.
          lv_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).
          CALL METHOD lv_send_request->add_recipient
            EXPORTING
              i_recipient = lv_recipient.
*            i_express   = 'X'.

        ENDLOOP.
      ENDIF.

* Read distribution list and get email address
      IF p_dli IS NOT INITIAL.
        CALL FUNCTION 'SO_DLI_READ_API1'
          EXPORTING
            dli_name                   = p_dli
            shared_dli                 = 'X'
          TABLES
            dli_entries                = lt_idlient
          EXCEPTIONS
            dli_not_exist              = 1
            operation_no_authorization = 2
            parameter_error            = 3
            x_error                    = 4
            OTHERS                     = 5.
        IF lt_idlient[] IS NOT INITIAL.
          LOOP AT lt_idlient INTO ls_idlient.
            MOVE ls_idlient-member_adr TO lv_email.
            lv_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).
            CALL METHOD lv_send_request->add_recipient
              EXPORTING
                i_recipient = lv_recipient.
          ENDLOOP.
        ENDIF.
      ENDIF.

*SET TO SEND IMMEDIATELY
      CALL METHOD lv_send_request->set_send_immediately( 'X' ).

*Send document
      CALL METHOD lv_send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = lv_sent_to_all ).

      IF lv_sent_to_all = 'X'.
        WRITE:/ 'email sent'.
      ENDIF.

      COMMIT WORK.

*Exception handling
    CATCH cx_bcs INTO lv_bcs_exception.
      WRITE:/ 'Error sending email - ', lv_bcs_exception->error_type.
  ENDTRY.

ENDFORM.                    "GENERATE_ERROR_EMAIL
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_screen .

*  IF s_email[] IS NOT INITIAL.
*    MESSAGE 'Enter email address' TYPE 'E'.
*  ENDIF.

ENDFORM.                    "VALIDATE_SCREEN
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_file .

  DATA: lv_src    TYPE sapb-sappfad,
        lv_target TYPE sapb-sappfad,
        ls_msg    LIKE LINE OF gt_applog,
        lv_replace TYPE text60.

  lv_src = p_fname.
  IF p_aname IS INITIAL.
    lv_target = lv_src.
    CONCATENATE '_' sy-datum '_' sy-uzeit '_arch.xml' INTO lv_replace.
    REPLACE ALL OCCURRENCES OF '.xml' IN lv_target WITH lv_replace.
    REPLACE ALL OCCURRENCES OF '.XML' IN lv_target WITH lv_replace.
  ELSE.
    lv_target = p_aname.
  ENDIF.

  CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
    EXPORTING
      sourcepath       = lv_src
      targetpath       = lv_target
    EXCEPTIONS
      error_file       = 1
      no_authorization = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.
    WRITE: /'File archive error'.
    ls_msg-msgty           = 'W'.
    ls_msg-msgid           = '00'.
    ls_msg-msgno           = '001'.
    ls_msg-msgv1 = 'File archive error'.
    ls_msg-probclass       = '2'.
    INSERT ls_msg INTO gt_applog INDEX 1.
  ELSE.
    OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      DELETE DATASET lv_src.
      WRITE:/'File archived successfully'.
      ls_msg-msgty           = 'W'.
      ls_msg-msgid           = '00'.
      ls_msg-msgno           = '001'.
      ls_msg-msgv1 = 'File archived successfully'.
      ls_msg-probclass       = '2'.
      INSERT ls_msg INTO gt_applog INDEX 1.
    ELSE.
      WRITE: /'File archive error'.
      ls_msg-msgty           = 'W'.
      ls_msg-msgid           = '00'.
      ls_msg-msgno           = '001'.
      ls_msg-msgv1 = 'File archive error'.
      ls_msg-probclass       = '2'.
      INSERT ls_msg INTO gt_applog INDEX 1.
    ENDIF.
  ENDIF.

ENDFORM.                    " ARCHIVE_FILE
*&---------------------------------------------------------------------*
*&      Form  FILE_ARCHIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_archive .

  DATA: gv_command         TYPE sxpgcolist-name VALUE 'ZFILE_MOVE',
        gv_file            TYPE sxpgcolist-parameters,
        gv_status          LIKE extcmdexex-status,
        gv_exitcode        LIKE extcmdexex-exitcode,
        lt_exec_protocol   TYPE STANDARD TABLE OF btcxpm,
        gwa_exec_protocol  TYPE btcxpm,
        ls_msg             LIKE LINE OF gt_applog,
        lv_src             TYPE sxpgcolist-parameters,
        lv_target          TYPE sxpgcolist-parameters,
        lv_replace         TYPE text60.

  CHECK p_app EQ 'X' AND p_arch EQ 'X' AND gv_dset_msg IS INITIAL.

  ls_msg-msgty           = 'W'.
  ls_msg-msgid           = '00'.
  ls_msg-msgno           = '001'.
  ls_msg-msgv1           = 'File archive error'.
  ls_msg-probclass       = '2'.
  INSERT ls_msg INTO gt_applog INDEX 1.

  lv_src = p_fname.
  IF p_aname IS INITIAL.
    lv_target = lv_src.
    CONCATENATE '_' sy-datum '_' sy-uzeit '_arch.xml' INTO lv_replace.
    REPLACE ALL OCCURRENCES OF '.xml' IN lv_target WITH lv_replace.
    REPLACE ALL OCCURRENCES OF '.XML' IN lv_target WITH lv_replace.
  ELSE.
    lv_target = p_aname.
  ENDIF.

  CONCATENATE lv_src lv_target INTO gv_file SEPARATED BY space.
  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname                   = gv_command
      additional_parameters         = gv_file
    IMPORTING
      status                        = gv_status
      exitcode                      = gv_exitcode
    TABLES
      exec_protocol                 = lt_exec_protocol
    EXCEPTIONS
      no_permission                 = 1
      command_not_found             = 2
      parameters_too_long           = 3
      security_risk                 = 4
      wrong_check_call_interface    = 5
      program_start_error           = 6
      program_termination_error     = 7
      x_error                       = 8
      parameter_expected            = 9
      too_many_parameters           = 10
      illegal_command               = 11
      wrong_asynchronous_parameters = 12
      cant_enq_tbtco_entry          = 13
      jobcount_generation_error     = 14
      OTHERS                        = 15.

  IF sy-subrc NE 0 OR gv_status EQ 'E'.
    ls_msg-msgty           = 'W'.
    ls_msg-msgid           = '00'.
    ls_msg-msgno           = '001'.
    ls_msg-probclass       = '2'.
    MOVE 'Unable to move the file from source to target' TO ls_msg-msgv1.
    CLEAR gwa_exec_protocol.
    READ TABLE lt_exec_protocol   INTO gwa_exec_protocol INDEX 1.
    IF ( sy-subrc EQ 0 ).
      MOVE  gwa_exec_protocol-message+00(50)  TO ls_msg-msgv2.
      MOVE  gwa_exec_protocol-message+50(50)  TO ls_msg-msgv3.
      MOVE  gwa_exec_protocol-message+100(28) TO ls_msg-msgv4.
    ELSE.
      CLEAR gwa_exec_protocol.
    ENDIF.
    INSERT ls_msg INTO gt_applog INDEX 2.
  ELSE.
    DELETE DATASET lv_src.
  ENDIF.

ENDFORM.                    " FILE_ARCHIVE
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_FILE_V2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_file_v2 .

  DATA: lv_src    TYPE string,
        lv_message TYPE string,
        lv_target TYPE string,
        ls_msg    LIKE LINE OF gt_applog,
        lv_replace TYPE text60.

  FIELD-SYMBOLS: <str> LIKE LINE OF gt_str.

  CHECK p_app EQ 'X' AND p_arch EQ 'X' AND gv_dset_msg IS INITIAL.

  lv_src = p_fname.
  IF p_aname IS INITIAL.
    lv_target = lv_src.
    CONCATENATE '_' sy-datum '_' sy-uzeit '_arch.xml' INTO lv_replace.
    REPLACE ALL OCCURRENCES OF '.xml' IN lv_target WITH lv_replace.
    REPLACE ALL OCCURRENCES OF '.XML' IN lv_target WITH lv_replace.
    REPLACE ALL OCCURRENCES OF 'MDM/' IN lv_target WITH 'MDM/ARCH/'.
  ELSE.
    lv_target = p_aname.
  ENDIF.

  OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_message.
  IF sy-subrc EQ 0.
    LOOP AT gt_str ASSIGNING <str>.
      TRANSFER <str> TO lv_target.
    ENDLOOP.

    DELETE DATASET lv_src.
    WRITE:/'File archived successfully'.
    ls_msg-msgty           = 'W'.
    ls_msg-msgid           = '00'.
    ls_msg-msgno           = '001'.
    ls_msg-msgv1 = 'File archived successfully'.
    ls_msg-probclass       = '2'.
    INSERT ls_msg INTO gt_applog INDEX 1.
    CLOSE DATASET lv_target.
  ELSE.
    WRITE: /'File archive open error'.
    ls_msg-msgty           = 'W'.
    ls_msg-msgid           = '00'.
    ls_msg-msgno           = '001'.
    ls_msg-msgv1 = lv_message.
    ls_msg-probclass       = '2'.
    INSERT ls_msg INTO gt_applog INDEX 1.
  ENDIF.

ENDFORM.                    " ARCHIVE_FILE_V2
*&---------------------------------------------------------------------*
*&      Form  DELETE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_material .

  DATA: ls_headdata            TYPE bapimathead,
        ls_clientdata          TYPE  bapi_mara,
        ls_clientdatax         TYPE  bapi_marax,
        ls_return              TYPE bapiret2,
        lt_returnmessages      TYPE STANDARD TABLE OF bapi_matreturn2,
        ls_returnmessage       LIKE LINE OF lt_returnmessages,
        ls_mara                TYPE mara.

  SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr EQ gv_matnr.
  IF sy-subrc EQ 0.

    ls_headdata-basic_view    = 'X'.
    ls_headdata-material      = gv_matnr.

    ls_clientdata-del_flag  = 'X'.
    ls_clientdatax-del_flag = 'X'.

    ls_clientdata-manuf_prof = ls_mara-mprof.
    ls_clientdata-manuf_prof = 'X'.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata       = ls_headdata
        clientdata     = ls_clientdata
        clientdatax    = ls_clientdatax
      IMPORTING
        return         = ls_return
      TABLES
        returnmessages = lt_returnmessages.

    IF ls_return-type EQ 'E'.
      gv_bapi_error = 'x'.
    ELSE.
      IF lt_returnmessages[] IS NOT INITIAL.
        ls_returnmessage-type   = 'S'.
        ls_returnmessage-id     = '00'.
        ls_returnmessage-number = '001'.
        ls_returnmessage-message_v1 = gv_matnr.
        ls_returnmessage-message_v2 = ' - Material deleted'.
        APPEND ls_returnmessage TO lt_returnmessages.
      ENDIF.
    ENDIF.
    PERFORM fill_log_with_xmldata USING 'DEL' lt_returnmessages[] CHANGING gt_applog[].
  ELSE.
    ls_returnmessage-type = 'E'.
    ls_returnmessage-id     = '00'.
    ls_returnmessage-number = '001'.
    ls_returnmessage-message_v1 = gv_matnr.
    ls_returnmessage-message_v2 = ' - Material NOT exists but XML with'.
    ls_returnmessage-message_v3 = ' Delete" action'.
    APPEND ls_returnmessage TO lt_returnmessages.
    PERFORM fill_log_with_xmldata USING 'MAT' lt_returnmessages[] CHANGING gt_applog[].
  ENDIF.

ENDFORM.                    " DELETE_MATERIAL
