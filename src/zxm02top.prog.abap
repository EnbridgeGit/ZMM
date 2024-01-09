*&---------------------------------------------------------------------*
*&  Include           ZXM02TOP
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM02TOP                                                 *
*  Func Grp:  XM02                                                     *
*  Author:    Praveena Anusuri                                         *
*  Date:      September 12, 2011                                       *
*  Track #:   TR926 Release 1                                          *
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
*-----------------------------------------------------------------------
*  Include :  ZXM06U43                                                 *
*  Function:  EXIT_SAPMM06E_012                                        *
*  Author  :  Rahul Sagane                                             *
*  Date    :  October 05, 2011                                         *
*  Track # :  EE031                                                    *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MM06E005 - user exits to assign source of supply   *
*                                                                      *
*     This user exit is used to assign the source of supply, for the   *
*     material at the line item. The source of supply wil also be      *
*     assinged for the service line items at the time of PR creation   *
************************************************************************
************************************************************************


**Begin of addition by RABHATT**
**Change log EE004**
TABLES: eban.
* Define Types
TYPES : BEGIN OF ty_eban,
        banfn TYPE banfn, "Purchasing Document Number
        bnfpo TYPE bnfpo, "Item Number of Purchasing Document
        txz01 TYPE txz01, "Short Text
        matnr TYPE matnr, "Material Number
        matkl TYPE matkl, "Material Group
        END OF ty_eban.
**Change log EE004**
**End of addition by RABHATT**


**Begin of addition by RSAGANE**
**Change log EE031**
** Type decleration P.R header and item
TYPES : BEGIN OF ty_ekpo,
        ebeln TYPE ebeln,   "Agrement No.
        ebelp TYPE ebelp,   "Item No.
        txzo1 TYPE txz01,   "Short Text
        matnr TYPE matnr,   "Material
        matkl TYPE matkl,   "Material Group
        netpr TYPE bprei,   "Price
        werks TYPE ewerk,   "Plant
        plifz TYPE eplif,   "Planned Delivery Time in Days 'Lead Time'
        lifnr TYPE elifn,   "Vendor No.
        ekorg TYPE ekorg,   "Purchasing Organization
        END OF ty_ekpo.

DATA: gta_ven_agre TYPE STANDARD TABLE OF ty_ekpo.
**Change log EE031**
**End of addition by RSAGANE**

DATA: *eban     TYPE eban,
      gf_aktyp  TYPE aktyp.

**Begin of addition by RABHATT**
**Change log EE004**

DATA: gwa_mereq_item TYPE mereq_item.

**Change log EE004**
**End of addition by RABHATT**
