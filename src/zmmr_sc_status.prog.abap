*&---------------------------------------------------------------------*
*& Report  ZMMR_SC_STATUS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZMMR_SC_STATUS                                 *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 18-Sep-2014                                    *
*& Object ID          : SDP70149: PM-PR to SRM reference field         *
*& Application Area   : MM                                             *
*& Description        : Report displays Shopping cart status in ECC    *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 14-Oct-2014                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K924464                                           *
*& Description  : Report performance issue                             *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 1.1                                                  *
* Date          : 12-Nov-2014                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K924464                                           *
*& Description  : Add new fields on selection screen.                  *
*&                PR Created By,PR Requisitioner,PR Tracking No/Org.PR#*
*&                Add new fields on report layout.                     *
*&                PR Created By,PR Requisitioner,PR Tracking No/Org.PR#*
*&                and PR Delivery Date.                                *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 11-Mar-2016                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K926689                                           *
*& Description  : Add Vendor material number on report layout.         *
*&                Include deleted WO, PO and PR line items in the report.
*&                Add WO Status, PO Item Status and PR Status on report*
*&                layout.                                              *
*----------------------------------------------------------------------*
* Version No    : 3.0                                                  *
* Date          : 14-Sep-2018                                          *
* Modified By   : AKMADASU                                             *
* Correction No : D30K929117                                           *
*& Description  : Add Requisitioner#s full name and Email ID in report *
*                 layout.                                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
REPORT  zmmr_sc_status NO STANDARD PAGE HEADING LINE-SIZE 132.

*Include for data declarations
INCLUDE zmmr_sc_status_top.
*Include for subroutines
INCLUDE zmmr_sc_status_f01.

************************************************************************
* INITIALIZATION.
************************************************************************
INITIALIZATION.
  LOOP AT SCREEN.
    IF screen-group1 = 'M1'.
      CLEAR: lwa_ucomm.
      lwa_ucomm-sign = 'I'.
      lwa_ucomm-option = 'EQ'.
      CONCATENATE '%' screen-group4 INTO lwa_ucomm-low.
      APPEND lwa_ucomm TO lt_ucomm.
    ENDIF.
  ENDLOOP.

************************************************************************
*AT SELECTION SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  PERFORM field_validation.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_variant_inputhelp  USING p_layout.

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
    MESSAGE 'No data selected'(003) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
