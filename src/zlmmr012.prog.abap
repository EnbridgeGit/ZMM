*&---------------------------------------------------------------------*
*& Report  ZLMMR012
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZLMMR012                                       *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 19-DEC-2013                                    *
*& Object ID          : FS59177 Report: Vendor Delivery Assessment     *
*& Application Area   : MM                                             *
*& Description        : This report provides both the PO Delivery date *
*                       with the Goods Receipt date for tracking Vendor*
*                       delivery performance.                          *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 04-SEP-2015                                          *
* Modified By   : Praveena Anusuri                                     *
*& Object ID    : SDP88758 - UG Modify ZLMMR012 to add Material Number *
*& Description  : Add Material Number to the report output.            *
*----------------------------------------------------------------------*
* 2021/02/17 birudurd COG Changes to include Plant in section Screen   *
*----------------------------------------------------------------------*
REPORT  zlmmr012 NO STANDARD PAGE HEADING LINE-SIZE 132.

*Include for data declarations
INCLUDE zlmmr012_top.
*Include for subroutines
INCLUDE zlmmr012_f01.

************************************************************************
*START-OF-SELECTION.
************************************************************************
START-OF-SELECTION.
  PERFORM get_details.
  PERFORM get_output_data.

************************************************************************
* END-OF SELECTION
************************************************************************
END-OF-SELECTION.
  IF lt_output IS NOT INITIAL.
    PERFORM display_report.
  ELSE.
    MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
