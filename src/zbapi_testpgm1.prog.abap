*
* BAPI TO Upload Inventory Data
*
* GMCODE Table T158G - 01 - MB01 - Goods Receipts for Purchase Order
*                      02 - MB31 - Goods Receipts for Prod Order
*                      03 - MB1A - Goods Issue
*                      04 - MB1B - Transfer Posting
*                      05 - MB1C - Enter Other Goods Receipt
*                      06 - MB11
*
* Domain: KZBEW - Movement Indicator
*      Goods movement w/o reference
*  B - Goods movement for purchase order
*  F - Goods movement for production order
*  L - Goods movement for delivery note
*  K - Goods movement for kanban requirement (WM - internal only)
*  O - Subsequent adjustment of "material-provided" consumption
*  W - Subsequent adjustment of proportion/product unit material
*
report zbapi_goodsmovement.

parameters: p-file like rlgrap-filename default
                                 'c:\sapdata\TEST.txt'.
parameters: e-file like rlgrap-filename default
                                 'c:\sapdata\gdsmvterror.txt'.

parameters: xpost like sy-datum default sy-datum.

data: begin of gmhead.
        include structure bapi2017_gm_head_01.
data: end of gmhead.

data: begin of gmcode.
        include structure bapi2017_gm_code.
data: end of gmcode.

data: begin of mthead.
        include structure bapi2017_gm_head_ret.
data: end of mthead.

data: begin of itab occurs 100.
        include structure bapi2017_gm_item_create.
data: end of itab.

data: begin of errmsg occurs 10.
        include structure bapiret2.
data: end of errmsg.

data: wmenge like iseg-menge,
      errflag.

data: begin of pcitab occurs 100,
        ext_doc(10),           "External Document Number
        mvt_type(3),           "Movement Type
        doc_date(8),           "Document Date
        post_date(8),          "Posting Date
        plant(4),              "Plant
        material(18),          "Material Number
        qty(13),               "Quantity
        recv_loc(4),           "Receiving Location
        issue_loc(4),          "Issuing Location
        pur_doc(10),           "Purchase Document No
        po_item(3),            "Purchase Document Item No
        del_no(10),            "Delivery Purchase Order Number
        del_item(3),           "Delivery Item
        prod_doc(10),          "Production Document No
        scrap_reason(10),      "Scrap Reason
        upd_sta(1),            "Update Status
      end of pcitab.

call function 'WS_UPLOAD'
  exporting
    filename                      = p-file
    filetype                      = 'DAT'
* IMPORTING
*   FILELENGTH                    =
  tables
    data_tab                      = pcitab
* EXCEPTIONS
*   FILE_OPEN_ERROR               = 1
*   FILE_READ_ERROR               = 2
*   NO_BATCH                      = 3
*   GUI_REFUSE_FILETRANSFER       = 4
*   INVALID_TYPE                  = 5
*   OTHERS                        = 6
          .
if sy-subrc <> 0.
  message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  exit.
endif.

gmhead-pstng_date = sy-datum.
gmhead-doc_date = sy-datum.
gmhead-pr_uname = sy-uname.
gmcode-gm_code = '01'.   "01 - MB01 - Goods Receipts for Purchase Order

loop at pcitab.
  itab-move_type  = pcitab-mvt_type.
  itab-mvt_ind    = 'B'.
  itab-plant      = pcitab-plant.
  itab-material   = pcitab-material.
  itab-entry_qnt  = pcitab-qty.
  itab-move_stloc = pcitab-recv_loc.
  itab-stge_loc   = pcitab-issue_loc.
  itab-po_number  = pcitab-pur_doc.
  itab-po_item    = pcitab-po_item.
  concatenate pcitab-del_no pcitab-del_item into itab-item_text.
  itab-move_reas  = pcitab-scrap_reason.

  append itab.
endloop.

loop at itab.
  write:/ itab-material, itab-plant, itab-stge_loc,
          itab-move_type, itab-entry_qnt, itab-entry_uom,
          itab-entry_uom_iso, itab-po_number, itab-po_item,
                                              pcitab-ext_doc.
endloop.

call function 'BAPI_GOODSMVT_CREATE'
  exporting
    goodsmvt_header             = gmhead
    goodsmvt_code               = gmcode
*   TESTRUN                     = ' '
* IMPORTING
    goodsmvt_headret            = mthead
*   MATERIALDOCUMENT            =
*   MATDOCUMENTYEAR             =
  tables
    goodsmvt_item               = itab
*   GOODSMVT_SERIALNUMBER       =
    return                      = errmsg
          .
clear errflag.
loop at errmsg.
  if errmsg-type eq 'E'.
    write:/'Error in function', errmsg-message.
    errflag = 'X'.
  else.
    write:/ errmsg-message.
  endif.
endloop.

if errflag is initial.
  commit work and wait.
  if sy-subrc ne 0.
    write:/ 'Error in updating'.
    exit.
  else.
    write:/ mthead-mat_doc, mthead-doc_year.
    perform upd_sta.
  endif.
endif.

*---------------------------------------------------------------------*
*       FORM UPD_STA                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form upd_sta.
  loop at pcitab.
    pcitab-upd_sta = 'X'.
    modify pcitab.
  endloop.

  call function 'WS_DOWNLOAD'
    exporting
      filename                      = p-file
      filetype                      = 'DAT'
* IMPORTING
*   FILELENGTH                    =
    tables
      data_tab                      = pcitab
* EXCEPTIONS
*   FILE_OPEN_ERROR               = 1
*   FILE_READ_ERROR               = 2
*   NO_BATCH                      = 3
*   GUI_REFUSE_FILETRANSFER       = 4
*   INVALID_TYPE                  = 5
*   OTHERS                        = 6
            .

endform.

*--- End of Program
