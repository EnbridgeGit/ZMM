************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM02O01                                                 *
*  Func Grp:  XM02                                                     *
*  Author:    Brian Boundy                                             *
*  Date:      November 25, 2010                                        *
*  Track #:   TR872 Release 1                                          *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MEREQ001 - user exits to maintain customer         *
*                              fields in purchase requisitions         *
*                                                                      *
*     This enhancement is used to maintain data in the custom          *
*     subscreen - purchase requisition.                                *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 11/25/10 0872 BTBOUND C11K921911 - ARIBA R1 - Initial development    *
*----------------------------------------------------------------------*
*-----------------------------------------------------------------------
*Modification details                                                  *
*Version No    : 01                                                    *
*Date          : 22/09/2011                                            *
*Modified by   : RBHATT                                                *
*Correction No : EE004                                                 *
*Transport No  : D30K917670  SL:LMM:MM: EE004-MM_REQ_CUSTOM FIELDS_UG  *
*Description   : To add new custom field 'Service Confirmer'           *
*                [CI_EBANDB-ZZARIBA_APPROVER] & 'Original PR Reference'*
*                [CI_EBANDB-ZZORIGREQ]in the additional tab in the     *
*                PO requisition transaction ME51N, ME52N, ME53N        *
*-----------------------------------------------------------------------
************************************************************************
*&---------------------------------------------------------------------*
*&      Module  STATUS_0111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*


MODULE status_0111 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

* Define Constants
  CONSTANTS : co_a    TYPE char1  VALUE 'A'.

  LOOP AT SCREEN.
    IF gf_aktyp EQ co_a.
      screen-input = 0.
    ELSE.
      screen-input = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " STATUS_0111  OUTPUT
