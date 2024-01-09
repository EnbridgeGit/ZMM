*&---------------------------------------------------------------------*
*&  Include           ZXM08U16
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(E_TRBKPV) TYPE  MRM_RBKPV
*"  TABLES
*"      E_TDRSEG TYPE  MMCR_TDRSEG
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 01                                                   *
* Date          : April 08, 2019                                       *
* Modified By   : Ashok Kumar(MADASUAK)                                *
* Correction No : D30K929624 & D30K929754                              *
* Description   :Supress error message for RN Document type CHG0137057 *
*----------------------------------------------------------------------*

DATA: lv_bsart TYPE bsart.

DATA: ls_tdrseg LIKE LINE OF e_tdrseg,
      ls_trbkpv LIKE e_trbkpv.

DATA: lv_wfuser TYPE z_varvaluel.

break sahmad.

ls_trbkpv = e_trbkpv.

READ TABLE e_tdrseg INTO ls_tdrseg INDEX 1.

SELECT SINGLE bsart
  FROM ekko
  INTO lv_bsart
  WHERE ebeln = ls_tdrseg-ebeln
.

SELECT SINGLE value1
  FROM zvar
  INTO lv_wfuser
  WHERE programm = 'ALL'
    AND varname = 'WORKFLOWID'
.

IF lv_bsart = 'ZF' AND
**-- start of changes by akmadasu CHG0137057
*   ( ls_trbkpv-blart = 'RE' OR
*     ls_trbkpv-blart = 'RF') AND
**-- end of changes by akmadasu CHG0137057
   sy-uname <> lv_wfuser.
  "Ignore workflow user
  CASE sy-tcode.
    WHEN 'MRIS' OR 'MRRL' OR 'MRDC' OR 'MRKO' OR 'MRNB' OR 'FB02' OR 'MR8M'.
      "DO NOTHING
    WHEN OTHERS.
      "Ignore TCODES
      MESSAGE text-001 TYPE 'E'.
  ENDCASE.
ENDIF.
