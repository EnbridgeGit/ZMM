class ZCL_UTILITIES definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_UTILITIES
*"* do not include other source files here!!!

  interface IF_SALV_BS_XML load .
  class-methods CREATE_XLS_FROM_ITAB
    importing
      !I_FILENAME type RLGRAP-FILENAME
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_SORT type LVC_T_SORT optional
      !IT_FILT type LVC_T_FILT optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !I_FILETYPE type SALV_BS_CONSTANT default IF_SALV_BS_XML=>C_TYPE_EXCEL_XML
    exporting
      !E_XSTRING type XSTRING
    changing
      !CT_DATA type STANDARD TABLE .
protected section.
*"* protected components of class ZCL_UTILITIES
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_UTILITIES
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_UTILITIES IMPLEMENTATION.


METHOD create_xls_from_itab.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZCL_UTILITIES                                  *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 22-DEC-2013                                    *
*& Object ID          : R_PTP_MM_0001_SRO Status Report                *
*& Application Area   : MM                                             *
*& Description        : Download internal table data to Excel file     *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
*& Description  :                                                      *
*----------------------------------------------------------------------*
  DATA: mt_fcat        TYPE lvc_t_fcat,
        mt_data        TYPE REF TO data,
        m_flavour      TYPE string,
        m_version      TYPE string,
        mo_result_data TYPE REF TO cl_salv_ex_result_data_table,
        mo_columns     TYPE REF TO cl_salv_columns_table,
        mo_aggreg      TYPE REF TO cl_salv_aggregations,
        mo_salv_table  TYPE REF TO cl_salv_table,
        m_file_type    TYPE salv_bs_constant,
        gt_bintab      TYPE solix_tab,
        lv_file        TYPE string,
        lv_length      TYPE i.
  FIELD-SYMBOLS <tab>  TYPE ANY TABLE.

  GET REFERENCE OF ct_data INTO mt_data.

* if we didn't pass fieldcatalog we need to create it
  IF it_fieldcat[] IS INITIAL.
    ASSIGN mt_data->* TO <tab>.
    TRY .
        cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = mo_salv_table
        CHANGING
          t_table      = <tab> ).
      CATCH cx_salv_msg.

    ENDTRY.
    "get colums & aggregation infor to create fieldcat
    mo_columns  = mo_salv_table->get_columns( ).
    mo_aggreg   = mo_salv_table->get_aggregations( ).
    mt_fcat     =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                  r_columns      = mo_columns
                                  r_aggregations = mo_aggreg ).
  ELSE.
*   else we take the one we passed
    mt_fcat[] = it_fieldcat[].
  ENDIF.

  IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
     cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

    mo_result_data = cl_salv_ex_util=>factory_result_data_table(
        r_data                      = mt_data
        s_layout                    = is_layout
        t_fieldcatalog              = mt_fcat
        t_sort                      = it_sort
        t_filter                    = it_filt
    ).

    CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        m_version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        m_version = if_salv_bs_xml=>version_26.
    ENDCASE.

    m_file_type = i_filetype.

    m_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.
    "transformation of data to excel
    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = m_file_type
        xml_version   = m_version
        r_result_data = mo_result_data
        xml_flavour   = m_flavour
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = e_xstring.
  ENDIF.

  IF e_xstring IS NOT INITIAL.

    "save file
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer          = e_xstring
*       APPEND_TO_TABLE = ' '
      IMPORTING
        output_length   = lv_length
      TABLES
        binary_tab      = gt_bintab.

    lv_file = i_filename.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_length
        filename                  = lv_file
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = gt_bintab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = lv_file
        operation              = 'OPEN'
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
