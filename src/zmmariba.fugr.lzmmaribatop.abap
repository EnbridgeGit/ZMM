FUNCTION-POOL zmmariba.                     "MESSAGE-ID ..
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Func-Pool: ZMMARIBA                                                 *
*  Include:   LZMMARIBATOP                                             *
*  Author:    John Hartung                                             *
*  Date:      February 25, 2011                                        *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*     - MM IDOC Input ARIBA Invoice - create Invoice via BAPI          *
*                                                                      *
*     This function module "Z_MM_IDOC_INPUT_ZARBINV" was copied from   *
*     generated function module "ZIDOC_INPUT_ZINVCRT".  After this     *
*     function module was created, it was enhanced with additional     *
*     requirements.                                                    *
*                                                                      *
*     If SAP requires that the original function be regenerated, then  *
*     this function may require changes.  An analysis/comparison will  *
*     need to be performed between the newly generated function and    *
*     this function to determine the necessary changes.                *
*                                                                      *
*     The original function was created via transaction "BDBG"         *
*     (Generate ALE Interface for BAPI) for Object "BUS2081" and       *
*     Method "CREATEFROMDATA".                                         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 02/25/11 0872 JRHARTU D30K916283 - Initial program development       *
* 10/26/12 EHP5 BTBOUNDY Remove INCLUDES to SAP Standard               *
*----------------------------------------------------------------------*
************************************************************************
* Begin changes - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044
TYPES: BEGIN OF ty_gs_zaribaspec_chrgs.
        INCLUDE STRUCTURE zaribaspec_chrgs.
TYPES: END   OF ty_gs_zaribaspec_chrgs.

TYPES: ty_gt_zaribaspec_chrgs TYPE STANDARD TABLE OF
                              ty_gs_zaribaspec_chrgs.

DATA:  gs_zaribaspec_chrgs    TYPE ty_gs_zaribaspec_chrgs,
       gt_zaribaspec_chrgs    TYPE ty_gt_zaribaspec_chrgs.

* End changes   - insert code   JRHARTUNG  05/06/11  TR0872  S01K900044

*}   INSERT

* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

TYPES: BEGIN OF ty_gs_flags_invalid,
        bukrs                 TYPE flag,
        waers                 TYPE flag,
        kalsm                 TYPE flag,
        del_costs             TYPE flag,
        po_item               TYPE flag,
        hdr_taxes             TYPE flag,
        itm_taxes             TYPE flag,
*{   INSERT         S01K900044                                        2
        spec_chrgs            TYPE flag,                    "S01K900044
*}   INSERT
       END   OF ty_gs_flags_invalid.

DATA:  gs_flags_invalid       TYPE ty_gs_flags_invalid.

TYPES: BEGIN OF ty_gs_ekpo_key,
        ebeln                 TYPE ebeln,
        ebelp                 TYPE ebelp,
        buzei                 TYPE rblgp,
       END   OF ty_gs_ekpo_key.

TYPES: ty_gt_ekpo_key         TYPE STANDARD TABLE OF ty_gs_ekpo_key.

DATA:  gs_ekpo_key            TYPE ty_gs_ekpo_key,
       gt_ekpo_key            TYPE ty_gt_ekpo_key.

TYPES: BEGIN OF ty_gs_ekpo_ekko,
        ebeln                 TYPE ebeln,
        ebelp                 TYPE ebelp,
        loekz_p               TYPE eloek,
        mwskz                 TYPE mwskz,
        bukrs                 TYPE bukrs,
        loekz_k               TYPE eloek,
        waers                 TYPE waers,
       END   OF ty_gs_ekpo_ekko.

TYPES: ty_gt_ekpo_ekko        TYPE STANDARD TABLE OF ty_gs_ekpo_ekko.

DATA:  gs_ekpo_ekko           TYPE ty_gs_ekpo_ekko,
       gt_ekpo_ekko           TYPE ty_gt_ekpo_ekko.

TYPES: BEGIN OF ty_gs_line_addchrgs,
        levl                  TYPE char12,
        catg                  TYPE char25,
        amnt                  TYPE char21,
        waers                 TYPE waers,
        desc                  TYPE z_char240,
       END   OF ty_gs_line_addchrgs.

TYPES: BEGIN OF ty_gs_text_lines,
        levl                  TYPE char12.
        INCLUDE STRUCTURE tline.
TYPES: END   OF ty_gs_text_lines.

TYPES: ty_gt_text_lines       TYPE STANDARD TABLE OF ty_gs_text_lines.

DATA:  gs_text_lines_header   TYPE ty_gs_text_lines,
       gt_text_lines_header   TYPE ty_gt_text_lines.

DATA:  gs_text_lines_items    TYPE ty_gs_text_lines,
       gt_text_lines_items    TYPE ty_gt_text_lines.

DATA:  gs_text_lines_addchrgs TYPE ty_gs_text_lines,
       gt_text_lines_addchrgs TYPE ty_gt_text_lines.

DATA:  gs_text_lines_errors   TYPE ty_gs_text_lines,
       gt_text_lines_errors   TYPE ty_gt_text_lines.

CONSTANTS:
       gc_x                   TYPE char1
                              VALUE 'X',
       gc_kalsm_canada        TYPE kalsm_d
                              VALUE 'TAXCA',
       gc_spras_dflt          TYPE spras
                              VALUE 'E',
       gc_tdobject_miro       TYPE tdobject
                              VALUE 'RBKP',
       gc_tdid_miro           TYPE tdid
                              VALUE '0001',
       gc_tdtitle_miro        TYPE tdtitle
                              VALUE 'NOTE',
       gc_tdversion_miro      TYPE tdversion
                              VALUE '00001',
       gc_vnam_tol_amt        TYPE rvari_vnam
                              VALUE 'ARIBA_TOL_AMT',
       gc_vnam_tol_perc       TYPE rvari_vnam
                              VALUE 'ARIBA_TOL_PERC',
       gc_linecnt_max         TYPE syindex
                              VALUE 5,
       gc_linelen_max         TYPE syindex
                              VALUE 50.

DATA:  gv_bukrs               TYPE bukrs,
       gv_waers               TYPE waers,
       gv_kalsm               TYPE kalsm_d,
       gv_ebeln               TYPE ebeln,
       gv_mwskz               TYPE mwskz,
       gv_ref_doc_no          TYPE char16,
       gv_gross_amount        TYPE wkgxxx,
       gv_amount_addchrgs     TYPE wkgxxx,
       gv_amount_addchrgs_c   TYPE char25,
*{   INSERT         S01K900044                                        3
       gv_amount_addchrgs_tol TYPE wkgxxx,                  "S01K900044
*}   INSERT
       gv_tol_amt             TYPE bapicurr_d,
       gv_tol_rate            TYPE pkuab.

DATA:  gv_tdname_miro         TYPE tdobname,
       gv_tdspras_miro        TYPE spras,
       gv_tdfuser_miro        TYPE tdfuser.

DATA:  gv_subrc_bapi          TYPE sysubrc,
       gv_subrc_conv          TYPE sysubrc,
       gv_flag_post           TYPE flag,
       gv_flag_park           TYPE flag,
       gv_segnam_error        TYPE edilsegtyp.

DATA:  gv_item_inv            TYPE char6,
       gv_item_po             TYPE char5,
       gv_levl                TYPE char12.

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283
