*&---------------------------------------------------------------------*
*&  Include           ZXM02U03
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM02U03                                                 *
*  Function:  EXIT_SAPLMEREQ_003                                       *
*  Author:    Praveena Anusuri                                         *
*  Date:      September 12, 2011                                       *
*  Track #:   TR926 Release 1                                          *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MEREQ001 - user exits to maintain customer         *
*                              fields in purchase requisitions         *
*                                                                      *
*     This user exit is used to import data from the custom subscreen  *
*     - purchase requisition (PAI).                                    *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 09/12/11 0872 PANUSURI C11K921911 - ARIBA R1 - Initial development    *
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

**Work area decleration
  DATA : lwa_eban TYPE ty_eban.
******Begin of addition by PANUSURI******
  DATA : lwa_mereq_item TYPE mereq_item,
         lt_rootPR TYPE REF TO IF_PURCHASE_REQUISITION,
         lwa_req_item TYPE REF TO IF_PURCHASE_REQUISITION_ITEM,
         lt_PRcont TYPE MMPUR_REQUISITION_ITEMS,
         lwa_PRline TYPE MMPUR_REQUISITION_ITEM.
******End of addition by PANUSURI******
* Define Constant
  CONSTANTS : co_i type char1 value 'I',
              co_x type char1 value 'X'.


**Begin of addition by RBHATT**
**Change log EE004**
*DATA: gwa_mereq_item TYPE mereq_item. "Commented as defined globaly
**Change log EE004**
**End of addition by RBHATT**

*get values if PReq item exists
  IF NOT im_req_item IS INITIAL.

*read item data from system
    gwa_mereq_item = im_req_item->get_data( ).

*if customer field changed
    IF eban-zzariba_approver NE *eban-zzariba_approver.

*fill field with new value
      gwa_mereq_item-zzariba_approver = eban-zzariba_approver.

**set new item data to system
*    CALL METHOD im_req_item->set_data( gwa_mereq_item ).
**tell the system that there has something changed on the customer tab
*    ex_changed = 'X'.

    ENDIF.
*if customer field changed
    IF eban-zzorigreq NE *eban-zzorigreq.

*fill field with new value
      gwa_mereq_item-zzorigreq = eban-zzorigreq.

**set new item data to system
*    CALL METHOD im_req_item->set_data( gwa_mereq_item ).
**tell the system that there has something changed on the customer tab
*    ex_changed = 'X'.
******Begin of addition by PANUSURI******
      lt_rootPR = im_req_item->get_requisition( ).
      lt_PRcont = lt_rootPR->get_items( ).
      LOOP AT lt_PRcont INTO lwa_PRLine.
        lwa_req_item = lwa_PRLine-item.
        lwa_mereq_item = lwa_req_item->get_data( ).
        lwa_mereq_item-zzorigreq = gwa_mereq_item-zzorigreq.
        CALL METHOD lwa_req_item->set_data( lwa_mereq_item ).
        ex_changed = co_x.
      ENDLOOP.
******End of addition by PANUSURI******

    ENDIF.


    IF   eban-zzorigreq NE *eban-zzorigreq
      OR eban-zzariba_approver NE *eban-zzariba_approver.
****Change log EE004****
**End of addition by RBHATT**


*set new item data to system
      CALL METHOD im_req_item->set_data( gwa_mereq_item ).
*tell the system that there has something changed on the customer tab
      ex_changed = co_x.

    ENDIF.  " added by RBHATT
  ENDIF.
