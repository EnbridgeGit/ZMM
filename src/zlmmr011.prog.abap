*&---------------------------------------------------------------------*
*& Report  ZLMMR011
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZLMMR011                                       *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 20-DEC-2013                                    *
*& Object ID          : 59211: Report Storage Location MRP Information *
*& Application Area   : MM                                             *
*& Description        : Maintain/monitor the MRP settings at the storage
*&                      location level                                 *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 1.1                                                  *
* Date          : 10-Feb-2013                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K923004                                           *
*& Description  : 1.Leading zeros removed.                             *
*&                2.UoM pulled from T006A table                        *
*----------------------------------------------------------------------*
REPORT  zlmmr011 NO STANDARD PAGE HEADING LINE-SIZE 132.

*Include for data declarations
INCLUDE zlmmr011_top.
*Include for subroutines
INCLUDE zlmmr011_f01.

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
