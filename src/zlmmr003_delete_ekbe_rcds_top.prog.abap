*&---------------------------------------------------------------------*
*&  Include           ZLMMR003_DELETE_EKBE_RCDS_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR003_DELETE_EKBE_RCDS_TOP                  *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 10-Jun-2016                                    *
*& Object ID          : ACR-1001                                       *
*& Application Area   : MM                                             *
*& Description        : This report deletes parked invoices with zero  *
*&                      value from EKBE table.                         *
*&---------------------------------------------------------------------*

************************************************************************
* DATA DECLARATION
************************************************************************
DATA: wa_ekbe   TYPE ekbe,
      ta_output TYPE STANDARD TABLE OF ekbe.

CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.

************************************************************************
*SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: pa_ebeln TYPE ebeln OBLIGATORY,
            pa_ebelp TYPE ebelp OBLIGATORY,
            pa_belnr TYPE mblnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*Define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
                      IMPORTING e_salv_function.

ENDCLASS.                    "lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
*Implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM delete_record USING e_salv_function.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
