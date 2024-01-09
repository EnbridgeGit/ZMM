*&---------------------------------------------------------------------*
*& Module Pool       ZLMMR003_MAINTAIN_PGRPS
*&---------------------------------------------------------------------*
PROGRAM  zlmmr003_maintain_pgrps NO STANDARD PAGE HEADING
                                 LINE-SIZE 130 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Program Name       : ZLMMR003_MAINTAIN_PGRPS                        *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 17-Jun-2016                                    *
*& Object ID          : ACR-53                                         *
*& Application Area   : MM                                             *
*& Description        : Maintain Purchasing Groups.                    *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 17-Jun-2016                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K926944                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*

*Include for data declarations
INCLUDE zlmmr003_maintain_pgrps_top.

*Include for subroutines
INCLUDE zlmmr003_maintain_pgrps_f01.

*Include for PBO modules
INCLUDE zlmmr003_maintain_pgrps_o01.

*Include for PAI modules
INCLUDE zlmmr003_maintain_pgrps_i01.
