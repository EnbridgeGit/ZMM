*&---------------------------------------------------------------------*
*& Report  ZZ_PROCESS_XML
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zz_process_xml.

PARAMETERS: p_filnam TYPE localfile OBLIGATORY

DEFAULT 'H:\Riversand\xmlfile.xml'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filnam.

  DATA: l_v_fieldname TYPE dynfnam.

  l_v_fieldname = p_filnam.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = l_v_fieldname
    IMPORTING
      file_name     = p_filnam.

START-OF-SELECTION.


  DATA xml_content_table TYPE TABLE OF string.
  DATA xml_content_line  TYPE string.

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename = 'H:\Riversand\xmlfile.xml'
*    TABLES
*      data_tab = xml_content_table.

  TYPES: BEGIN OF ty_tab,
          name TYPE string,
          value TYPE string,
        END OF ty_tab.

  DATA: lcl_xml_doc TYPE REF TO cl_xml_document,
  v_subrc TYPE sysubrc,
  v_node TYPE REF TO if_ixml_node,
  v_child_node TYPE REF TO if_ixml_node,
  v_root TYPE REF TO if_ixml_node,
  v_iterator TYPE REF TO if_ixml_node_iterator,
  v_nodemap TYPE REF TO if_ixml_named_node_map,
  v_count TYPE i,
  v_index TYPE i,
  v_attr TYPE REF TO if_ixml_node,
  v_name TYPE string,
  v_prefix TYPE string,
  v_value TYPE string,
  v_char TYPE char2.

  DATA: itab TYPE STANDARD TABLE OF ty_tab,
        wa TYPE ty_tab.

  CREATE OBJECT lcl_xml_doc.

  CALL METHOD lcl_xml_doc->import_from_file
    EXPORTING
      filename = p_filnam
    RECEIVING
      retcode  = v_subrc.

  CHECK v_subrc = 0.

  v_node = lcl_xml_doc->m_document.
  CHECK NOT v_node IS INITIAL.
  v_iterator = v_node->create_iterator( ).
  v_node = v_iterator->get_next( ).

  WHILE NOT v_node IS INITIAL.
    CASE v_node->get_type( ).
      WHEN if_ixml_node=>co_node_element.
        v_name = v_node->get_name( ).
        v_nodemap = v_node->get_attributes( ).

        IF NOT v_nodemap IS INITIAL.
* attributes
          v_count = v_nodemap->get_length( ).
          DO v_count TIMES.
            v_index = sy-index - 1.
            v_attr = v_nodemap->get_item( v_index ).
            v_name = v_attr->get_name( ).
            v_prefix = v_attr->get_namespace_prefix( ).
            v_value = v_attr->get_value( ).
          ENDDO.
        ENDIF.

      WHEN if_ixml_node=>co_node_text OR
      if_ixml_node=>co_node_cdata_section.
* text node
        v_value = v_node->get_value( ).
        MOVE v_value TO v_char.
        IF v_char <> cl_abap_char_utilities=>cr_lf.
          wa-name = v_name.
          wa-value = v_value.
          APPEND wa TO itab.
          CLEAR wa.
        ENDIF.
    ENDCASE.
* advance to next node
    v_node = v_iterator->get_next( ).
  ENDWHILE.

  LOOP AT itab INTO wa.

  ENDLOOP.

  DATA: gcl_xml       TYPE REF TO cl_xml_document.
  DATA: gv_filename   TYPE string.
  DATA: gt_xml        TYPE swxmlcont.
  DATA: gv_xml_string TYPE string.
  DATA: gv_size       TYPE i.

  DATA: gt_str TYPE string_table.

  CREATE OBJECT gcl_xml.

  gv_filename = 'H:\Riversand\xmlfile.xml'.

*Upload XML file
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename            = gv_filename
      filetype            = 'BIN'
      has_field_separator = ' '
      header_length       = 0
    IMPORTING
      filelength          = gv_size
    TABLES
      data_tab            = gt_xml
    EXCEPTIONS
      OTHERS              = 1.

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename                = gv_filename
*      filetype                = 'ASC'
**     has_field_separator     = 'X'
*    TABLES
*      data_tab                = gt_str
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      OTHERS                  = 17.

*Convert uploaded data to string
  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = gv_size
    IMPORTING
      text_buffer  = gv_xml_string
    TABLES
      binary_tab   = gt_xml
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

**Parses XML String to DOM
  CALL METHOD gcl_xml->parse_string
    EXPORTING
      stream = gv_xml_string.

  DATA: g_t_xml_info TYPE STANDARD TABLE OF smum_xmltb,
        g_t_return   TYPE STANDARD TABLE OF bapiret2.

  DATA:g_xmldata TYPE xstring.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = gv_xml_string
    IMPORTING
      buffer = g_xmldata
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

*  IF sy-subrc<> 0.
*
*    MESSAGE #error IN the XML file# TYPE #e#.
*
*  ENDIF.

  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = g_xmldata
    TABLES
      xml_table = g_t_xml_info
      return    = g_t_return
    EXCEPTIONS
      OTHERS    = 0.

  CHECK sy-uname EQ 'MZHOSSAIN'.

*  DATA : lif_ixml TYPE REF TO if_ixml ,
*       lif_ixml_document TYPE REF TO if_ixml_document ,
*       lif_ixml_stream_factory TYPE REF TO if_ixml_stream_factory ,
*       lif_ixml_istream TYPE REF TO if_ixml_istream ,
*       lif_ixml_parser TYPE REF TO if_ixml_parser ,
*       lif_ixml_parse_error TYPE REF TO if_ixml_parse_error ,
*       lif_ixml_node TYPE REF TO if_ixml_node,
*       node          TYPE REF TO if_ixml_node.
*
*  DATA : "lt_xml_error TYPE dts_tty_xml_error ,
*          "lt_xml_error_field TYPE dts_tty_xml_error ,
*          lv_error_count TYPE i ,
*          lv_index TYPE i.
*
*  DATA : lif_ixml_node_list TYPE REF TO if_ixml_node_list ,
*        lif_ixml_node_iterator TYPE REF TO if_ixml_node_iterator ,
*        lif_ixml_node1 TYPE REF TO if_ixml_node ,
*        lif_ixml_node_list1 TYPE REF TO if_ixml_node_list ,
*        lif_ixml_node_iterator1 TYPE REF TO if_ixml_node_iterator ,
*        lv_type TYPE i ,
*        lv_name TYPE string ,
*        lv_xml TYPE string,
*        lv_value TYPE string .
*
***-- create the main factory
*  lif_ixml = cl_ixml=>create( ).
*
***-- Create the Initial Document
*  lif_ixml_document = lif_ixml->create_document( ).
*
***-- Create a Stream Factory
*  lif_ixml_stream_factory = lif_ixml->create_stream_factory( ).
*
***-- Create an Input Stream
**  lif_ixml_istream       = lif_ixml_stream_factory->create_istream_string( string = gv_xml_stringgv_xml_string ).
*
***-- Create a Parser
*  lif_ixml_parser = lif_ixml->create_parser(
*      document       = lif_ixml_document
*      istream        = lif_ixml_istream
*      stream_factory = lif_ixml_stream_factory
*  ).
*
***-- check errors in parsing
*  IF lif_ixml_parser->parse( ) <> 0.
*    IF lif_ixml_parser->num_errors( ) <> 0.
*      lv_error_count = lif_ixml_parser->num_errors( ).
*      lv_index = 0.
*      WHILE lv_index < lv_error_count.
*        lif_ixml_parse_error = lif_ixml_parser->get_error( index = lv_index ).
*        APPEND INITIAL LINE TO lt_xml_error ASSIGNING <fs_xml_error>.
*        <fs_xml_error>-column_name = lif_ixml_parse_error->get_column( ).
*        <fs_xml_error>-line = lif_ixml_parse_error->get_line( ).
*        <fs_xml_error>-reason = lif_ixml_parse_error->get_reason( ).
*        lv_index = lv_index + 1.
*      ENDWHILE.
*    ENDIF.
*  ENDIF.
*
***-- Close the Input Stream after Parsing
*  lif_ixml_istream->close( ).
*
****Nevigate to the 'DATA' node of xml
*  lif_ixml_node = lif_ixml_document->find_from_name( name = 'DATA' ).
****get name of the node name will be data
*  lv_name = lif_ixml_node->get_name( ).
****in order to process childs of data node we need to create iterator using its childerns
*  lif_ixml_node_list = lif_ixml_node->get_children( ).
*  lif_ixml_node_iterator = lif_ixml_node_list->create_iterator( ).
*
****Loop on actions
*  WHILE lif_ixml_node IS NOT INITIAL.
*    lv_name = lif_ixml_node->get_name( ).
*    IF lif_ixml_node->get_name( ) = 'ACTION'.
*      lif_ixml_node1 = lif_ixml_node->get_first_child( ).
*      lif_ixml_node_list1 = lif_ixml_node->get_children( ).
*      lif_ixml_node_iterator1 = lif_ixml_node_list1->create_iterator( ).
****loop at fields
*      WHILE lif_ixml_node1 IS NOT INITIAL.
*        lv_name = lif_ixml_node1->get_name( ).
*****element node
*        node = lif_ixml_node1->get_first_child( ).
*
*        IF node->get_name( ) = 'NAME'.
*          IF node->get_value( ) = 'VBAK-AUART'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test1' ).
*          ENDIF.
*          IF node->get_value( ) = 'VBAK-VKORG'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test2' ).
*          ENDIF.
*          IF node->get_value( ) = 'VBAK-VTWEG'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test3' ).
*          ENDIF.
*          IF node->get_value( ) = 'VBAK-SPART'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test4' ).
*          ENDIF.
*          IF node->get_value( ) = 'VBAK-VKBUR'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test5' ).
*          ENDIF.
*          IF node->get_value( ) = 'VBAK-VKGRP'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test6' ).
*          ENDIF.
*          IF node->get_value( ) = 'VBAK-AUGRU'.
*            node = node->get_next( ).
*            node->set_value( value = 'Test7' ).
*          ENDIF.
*        ENDIF.
****Get the next field under action
*        lif_ixml_node1 = lif_ixml_node_iterator1->get_next( ).
*      ENDWHILE.
*    ENDIF.
****Set the node to next node
*    lif_ixml_node = lif_ixml_node_iterator->get_next( ).
*  ENDWHILE.
*
****To show the xml on selection screen
**call function 'SDIXML_DOM_TO_SCREEN'
**  exporting
**    document = lif_ixml_document.
*
*  DATA : xml TYPE xstring.
*  CALL FUNCTION 'SDIXML_DOM_TO_XML'
*    EXPORTING
*      document      = lif_ixml_document
*    IMPORTING
*      xml_as_string = xml.
*  cl_abap_browser=>show_xml(
*  EXPORTING
*    xml_xstring  = xml    " XML in XString
*  ).
**
***Display XML
*  CALL METHOD gcl_xml->display.
