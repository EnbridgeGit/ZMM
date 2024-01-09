TABLES: mara.

DATA: gcl_xml       TYPE REF TO cl_xml_document.
DATA: gv_filename   TYPE string.
DATA: gt_xml        TYPE swxmlcont.
DATA: gv_xml_string TYPE string.
DATA: gv_size       TYPE i.

TYPES: BEGIN OF ty_attr,
         attr_id TYPE text20,
         name    TYPE text50,
         value   TYPE string,
         action  TYPE text20,
       END OF ty_attr,

       BEGIN OF ty_msg,
         matnr TYPE matnr,
         msg   TYPE bapi_msg,
       END OF ty_msg,

       BEGIN OF ty_tdline,
         tdline(132) TYPE c,
       END OF ty_tdline,

       BEGIN OF ty_classification,
         class       TYPE string,
         class_desc  TYPE string,
         charac_name TYPE string,
         atnam       TYPE string,
         atwrt       TYPE string,
       END OF ty_classification,

       BEGIN OF ty_hers,
         action                   TYPE string,
         external_system_id       TYPE string,
         manufacturer             TYPE string,
         manufacturer_name        TYPE string,
         manufacturer_part_number TYPE string,
         vendor TYPE string,
         vendor_part_no TYPE string,
       END OF ty_hers,

       ty_t_class           TYPE STANDARD TABLE OF ty_classification  WITH NON-UNIQUE DEFAULT KEY,
       ty_t_hers            TYPE STANDARD TABLE OF ty_hers WITH NON-UNIQUE DEFAULT KEY,
       ty_t_attr            TYPE STANDARD TABLE OF ty_attr WITH NON-UNIQUE DEFAULT KEY,
       ty_t_bal_s_msg       TYPE STANDARD TABLE OF bal_s_msg,
       ty_t_bapi_matreturn2 TYPE STANDARD TABLE OF bapi_matreturn2.

TYPES: BEGIN OF ty_xml_attr,
         entity_id                TYPE text20,
         externalid               TYPE text20,
         action                   TYPE text10,
         delete_flag              type text10,
         class                    TYPE string,
         class_desc               TYPE string,
         t_hers                   TYPE ty_t_hers,
         t_class                  TYPE ty_t_class,
         t_attr                   TYPE ty_t_attr,
         application              TYPE string,
         material                 TYPE string,
         size                     TYPE string,
         type                     TYPE string,
         ball_material            TYPE string,
         body_material            TYPE string,
         end_connection           TYPE string,
         manufacturing_standard   TYPE string,
         method_of_operation      TYPE string,
         nominal_pipe_size        TYPE string,
         opening                  TYPE string,
         pressure_rating          TYPE string,
         seat_material            TYPE string,
         long_description         TYPE string,
         active                   TYPE string,
         comments                 TYPE string,
         business_unit            TYPE string,
         part_number_driven       TYPE string,
         external_system_id       TYPE string,
         manufacturer             TYPE string,
         manufacturer_name        TYPE string,
         manufacturer_part_number TYPE string,
         model_number             TYPE string,
*         VENDOR                   TYPE STRING,
*         VENDOR_NAME              TYPE STRING,
*         VENDOR_PART_NUMBER       TYPE STRING,
         part_info_in_ld          TYPE string,
         base_unit_of_measure     TYPE string,
         exists_in_ug             TYPE string,
         material_group           TYPE string,
         material_type            TYPE string,
         sap_material_number      TYPE string,
         sap_short_description    TYPE string,
         x_plant_matl_status      TYPE string,
       END OF ty_xml_attr,

       ty_t_edidd TYPE STANDARD TABLE OF edidd,
       ty_t_edidc TYPE STANDARD TABLE OF edidc.

DATA: lt_xml_attr TYPE STANDARD TABLE OF ty_xml_attr,
      lt_class    TYPE ty_t_class,
      lt_attr     TYPE ty_t_attr.

DATA: gt_str TYPE string_table.
DATA: xml_doc TYPE REF TO cl_xml_document.


DATA: lv_val        TYPE string,
      ls_attr       TYPE ty_attr,
      ls_class      TYPE ty_classification,
      gs_hers       TYPE LINE OF ty_t_hers,
      gv_msgfn      TYPE msgfn,
      gv_matnr      TYPE mara-matnr,
      gv_mfrnr      TYPE mara-mfrnr,
      gv_externalid TYPE mara-matnr,
      gt_msg        TYPE STANDARD TABLE OF ty_msg,
      gs_msg        LIKE LINE OF gt_msg,
      gt_msgs       TYPE STANDARD TABLE OF bapiret2,
      gt_applog     TYPE STANDARD TABLE OF bal_s_msg,
      gt_xml_data   TYPE STANDARD TABLE OF bal_s_msg,
      gv_bapi_error TYPE bapi_mtype,
      gv_xml_flag   TYPE c,
      gv_email      TYPE char50,
      gv_fixed      TYPE c,
      gv_dset_msg   TYPE string. "App server file message

FIELD-SYMBOLS: <xml_attr> LIKE LINE OF lt_xml_attr.
