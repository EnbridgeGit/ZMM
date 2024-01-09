*&---------------------------------------------------------------------*
*&  Include           ZLMMR003_DELETE_EKBE_RCDS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR003_DELETE_EKBE_RCDS_F01                  *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 08-Jun-2016                                    *
*& Object ID          : ACR-1001                                       *
*& Application Area   : MM                                             *
*& Description        : This report deletes parked invoices with zero  *
*&                      value from EKBE table.                         *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_RECORDS_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_records_delete .

* Get records from purchasing document history table
  CLEAR wa_ekbe.
  SELECT SINGLE *
         FROM ekbe
         INTO wa_ekbe
         WHERE ebeln = pa_ebeln
         AND   ebelp = pa_ebelp
         AND   vgabe = 'P'
         AND   belnr = pa_belnr
         AND   dmbtr = space
         AND   wrbtr = space.
  IF sy-subrc = 0.
    APPEND wa_ekbe TO ta_output.
    CLEAR wa_ekbe.
  ENDIF.

ENDFORM.                    " GET_RECORDS_DELETE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_output .
  DATA: lr_salv_table     TYPE REF TO cl_salv_table,
        lr_columns        TYPE REF TO cl_salv_columns_list,
        lr_functions_list TYPE REF TO cl_salv_functions_list,
        lr_display        TYPE REF TO cl_salv_display_settings,
        lr_events         TYPE REF TO cl_salv_events_table,
        ltp_title         TYPE lvc_title.
  CONSTANTS: lco_true     TYPE sap_bool VALUE 'X',
             lco_repid    TYPE syrepid VALUE 'ZLMMR003_DELETE_EKBE_RECORDS'.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lr_salv_table
        CHANGING
          t_table      = ta_output[].
    CATCH cx_salv_msg .
  ENDTRY.
  lr_columns = lr_salv_table->get_columns( ).
  TRY.
      lr_columns->set_optimize( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.

  lr_functions_list = lr_salv_table->get_functions( ).
  lr_functions_list->set_all( lco_true ).

  lr_salv_table->set_screen_status(
                 pfstatus      =  'ZDELETE'
                 report        =  lco_repid ).

  lr_display = lr_salv_table->get_display_settings( ).
  ltp_title = 'Delete EKBE record'(008).
  lr_display->set_list_header( ltp_title ).

  lr_events = lr_salv_table->get_event( ).
  CREATE OBJECT gr_events.
* Register to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command FOR lr_events.

* Display table
  lr_salv_table->display( ).

ENDFORM.                    " DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_record  USING  i_function TYPE salv_de_function.
  DATA: ltp_answer TYPE c.

  IF i_function = 'DELE'.
    AUTHORITY-CHECK OBJECT 'Z_EKBE'
        ID 'ACTVT' FIELD '06'.
    IF sy-subrc NE 0.
      MESSAGE 'No authorization to delete the record'(007) TYPE 'E'.
    ENDIF.
    CLEAR ltp_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation Message'(003)
        text_question         = 'The displayed record will be deleted, do you want to continue?'(004)
        text_button_1         = 'Yes'(005)
        text_button_2         = 'No'(006)
        default_button        = '2'
        display_cancel_button = ' '
      IMPORTING
        answer                = ltp_answer.
    IF ltp_answer = '1'.
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable = 'E'
          tabname      = 'EKBE'.
      DELETE ekbe FROM TABLE ta_output.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          mode_rstable = 'E'
          tabname      = 'EKBE'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " DELETE_RECORD
