*&---------------------------------------------------------------------*
*& Report  ZLMMR003_DELETE_EKBE_RECORDS
*&---------------------------------------------------------------------*
REPORT  zlmmr003_delete_ekbe_records NO STANDARD PAGE HEADING
                                     LINE-SIZE 130 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Program Name       : ZLMMR003_DELETE_EKBE_RECORDS                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 10-Jun-2016                                    *
*& Object ID          : ACR-1001                                       *
*& Application Area   : MM                                             *
*& Description        : This report deletes parked invoices with zero  *
*&                      value from EKBE table.                         *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                Modification Log(Latest Version on Top)               *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 10-Jun-2016                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K926922                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*

*Include for data declarations
INCLUDE zlmmr003_delete_ekbe_rcds_top.
*Include for subroutines
INCLUDE zlmmr003_delete_ekbe_rcds_f01.

**----------------------------------------------------------------------*
**START-OF-SELECTION
**----------------------------------------------------------------------*
START-OF-SELECTION.
* Get records for deletion
  PERFORM get_records_delete.

*----------------------------------------------------------------------*
*END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF ta_output IS NOT INITIAL.
    PERFORM display_output.
  ELSE.
    MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
