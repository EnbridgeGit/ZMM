*&---------------------------------------------------------------------*
*& Report  ZMMR_SRO_STATUS_REPORT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZMMR_SRO_STATUS_REPORT                         *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 20-Dec-2013                                    *
*& Object ID          : R_PTP_MM_0001_SRO Status Report                *
*& Application Area   : MM                                             *
*& Description        : Display SRO data retrieved from cross functional
*                       modules like PS,MM,AP in a single report.      *
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

REPORT  zmmr_sro_status_report NO STANDARD PAGE HEADING
                               LINE-SIZE 132.
*Include for data declarations
INCLUDE zmmr_sro_status_report_top.
*Include for subroutines
INCLUDE zmmr_sro_status_report_f01.

*----------------------------------------------------------------------*
*AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_f4_help_presentation.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_sro_details.
  PERFORM get_output_data.

*----------------------------------------------------------------------*
*END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF lt_output IS NOT INITIAL.
    PERFORM display_output.
  ELSE.
    MESSAGE 'No data selected'(030) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
