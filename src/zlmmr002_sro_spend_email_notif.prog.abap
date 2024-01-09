*&---------------------------------------------------------------------*
*& Report  ZLMMR002_SRO_SPEND_EMAIL_NOTIF
*&---------------------------------------------------------------------*
REPORT  zlmmr002_sro_spend_email_notif NO STANDARD PAGE HEADING
                                       LINE-SIZE 130 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Program Name       : ZLMMR002_SRO_SPEND_EMAIL_NOTIF                 *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 08-Mar-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : MM                                             *
*& Description        : Send daily email notification at 80% SRO spend *
*&                      to Requestor, Service Confirmer and Buyer.     *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 08-Mar-2016                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K926687                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 06-Jun-2016                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K926910                                           *
* Description   : Restrict SRO's to get only last 3 years records.     *
*                 Do not send email notifications to closed SRO's with *
*                 'Final Invoice indicator' flag set.                  *
*                 Change in Invoice logic to get invoice amounts from  *
*                 EKBE table.                                          *
*----------------------------------------------------------------------*
* Version No    : 3.0                                                  *
* Date          : 22-May-2017                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K928236                                           *
* Description   : Fix is done for divide by zero issue.                *
*----------------------------------------------------------------------*
*Include for data declarations
INCLUDE zlmmr002_sro_spend_top.
*Include for subroutines
INCLUDE zlmmr002_sro_spend_f01.
*
**----------------------------------------------------------------------*
**START-OF-SELECTION
**----------------------------------------------------------------------*
START-OF-SELECTION.
* Get SRO Purchase Orders
  PERFORM get_sro_pos.

* Get SRO Spend data
  PERFORM get_sro_spend.

*----------------------------------------------------------------------*
*END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Send email
  IF ta_sro_final[] IS NOT INITIAL.
    PERFORM send_email.
  ELSE.
    WRITE:/ 'No email notifications sent'(004).
  ENDIF.
