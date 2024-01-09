*&---------------------------------------------------------------------*
*&  Include           ZXM02U01
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM02U01                                                 *
*  Function:  EXIT_SAPLMEREQ_001                                       *
*  Author:    Praveena Anusuri                                         *
*  Date:      September 12, 2011                                       *
*  Track #:   TR926 Release 1                                          *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MEREQ001 - user exits to maintain customer         *
*                              fields in purchase requisitions         *
*                                                                      *
*     This user exit is used to export data to the custom subscreen -  *
*     purchase requisition (PBO).                                      *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 09/12/11 0872 PANUSURI C11K921911 - ARIBA R1 - Initial development   *
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

**Begin of addition by RBHATT**
**Change log EE004**
*DATA: gwa_mereq_item TYPE mereq_item. "Commented as defined globaly
**Change log EE004**
**End of addition by RBHATT**

* clear eban values if no PReq item
IF im_req_item IS INITIAL.
  CLEAR: *eban,
         eban.
ELSE.

* read item data from system
  gwa_mereq_item = im_req_item->get_data( ).

* fill customer field with old values
  eban-zzariba_approver = gwa_mereq_item-zzariba_approver.

   *eban-zzariba_approver = gwa_mereq_item-zzariba_approver.

  eban-zzorigreq = gwa_mereq_item-zzorigreq.

   *eban-zzorigreq = gwa_mereq_item-zzorigreq.

ENDIF.

* Read and store activity type to chnge the field status
* of the subscreen
IF NOT im_req_item IS INITIAL.

  gf_aktyp = im_req_item->get_activity( ).

ENDIF.
