*&---------------------------------------------------------------------*
*& Report  ZLMMRCI_NOOUTPUT_POS
*&
*&---------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 11-Oct-2018                                          *
* Created By    : SKAKUMANU                                            *
* Correction No : D30K929174                                           *
* Object ID     : CI Item                                              *
* Description   : Report to display POs with no output triggered       *
*----------------------------------------------------------------------*

REPORT  zlmmrci_nooutput_pos.

* Global declarations and selection screen
INCLUDE zlmmrci_nooutput_pos_top.

* Subroutines
INCLUDE zlmmrci_nooutput_pos_f01.

INITIALIZATION.
s_aedat-high = sy-datum.
s_aedat-low = sy-datum - 30.
s_aedat-sign = 'I'.
s_aedat-option = 'BT'.
APPEND s_aedat.

START-OF-SELECTION.
* Get PO data
  PERFORM get_ekko.

* Get output details
  PERFORM get_nast.

END-OF-SELECTION.
* Process data and prepare final  table
  PERFORM process_data.

  IF gt_final IS NOT INITIAL.
*   Create ALV object
    PERFORM initialize_alv.

*   Prepare field catalog and column names
    PERFORM field_catalog.
*   Display data
    PERFORM display_data.
    ELSE.
      MESSAGE text-014 TYPE 'S'.
      LEAVE LIST-PROCESSING.
  ENDIF.
