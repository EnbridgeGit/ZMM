REPORT ZMPUR005 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 65.
*-------------------------------------------------------------------*
* DESCRIPTION                                                       *
* Outstanding Commitments to Vendors                                *
* NOTE -------------------------------------------------------------*
* In the report, some fields have been combined into 1 columnn.     *
* eg. Vendor & Vendor Name; Material & Material Description,        *
*     PO & Item Number, Plant and Storage Location.                 *
* This is because the excel spreadsheet is limited to a maximum of  *
* 20 columns.                                                       *
*-------------------------------------------------------------------*
* CHANGES                                                           *
* 2008/07/03 mdemeest TR541 New Report                              *
*                                                                   *
* 2012/09/05 M Khan   TR995 Change C: drive to H: drive with direct-*
*                           ory file selection using F4 & move      *
*                           hard-coded file path/name to variant.   *
*                                                                   *
*-------------------------------------------------------------------*
 TABLES:
        EKKO,                       "Purchase Order
        EKPO,                       "Purchase Order Detail
        EBAN,                       "Purchase Requisition
        cdhdr,
        cdpos,
        t16fs,
        lfa1,


*        EKBE,                       "PO History
        EKET.                       "Scheduling Details
*        esuh.


 DATA:
     BEGIN OF wa OCCURS 0,
        bsart(5) type c,      "like ekko-bsart,       "PO Type EKKO
        lifnr_name(35) type c,                "Vendor# & name
        ebeln_ebelp(13) type c,               "PO # & Item #
        banfn(10),       "like eket-banfn,       "Purchase Req
        bnfpo(8) type c,   "like eket-bnfpo,     "Purchase Item#
        badat           like eban-badat,                "PR create date
        labnr_bednr(30) type c,          "Acknowledgement /Tracking no
        ekgrp_matkl(9) type c,                "Purch Group/Mat Group
        aedat           like ekko-aedat,      "PO create date
        udate           like ekko-aedat,      "PO release date,
        strat(6)        type c,               "Release strategy
        podays(7)       type c,               "PO Days
        eindt(10) type c,                     "Delivery date
        start(10) type c,                     "Start date
        releases(4) type c,                      "Number of releases
        werks_lgort(9) type c,                "Plant/Storage Location
        ematn_txz01(45) type c,               "Material Number&Desc
        po_pr_days(8)   type c,               "Days between po & pr
     END OF wa.

*-----------------------------------------------------------------------
data:  w_option(12)  type c  value 'START_EXCEL'.
data:  w_head01(132) type c.
data:  w_head02(120) type c.
data:  retcode       like sy-subrc.
data:  w_repttl      like sy-title.

data:  wa_ekgrp(4) type c.

data:  begin of prot_header occurs 1,
         spaltenname(20)    type c,
         ddic_table(5)      type c,
         ddic_field(5)      type c,
         key                type c,
       end of prot_header.

data:  errortab like hrerror   occurs 0 with header line.

*-----------------------------------------------------------------------

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
 select-options:
            s_lifnr for ekko-lifnr,                    "Vendor
            s_ekorg for ekko-ekorg  no intervals obligatory
                                   default 'MATL'.
 select-options:
            s_bsart for ekko-bsart obligatory,
            s_ekgrp for ekko-ekgrp,
            s_matkl for ekpo-matkl,                    "Material Group
            s_werks for ekpo-werks,                    "Plant
            s_ebelp for ekpo-ebelp,                    "Item Number
            s_knttp for ekpo-knttp,
            s_eindt for eket-eindt,
            s_kdate for ekko-kdate,
            s_ebeln for ekko-ebeln,
            s_ematn for ekpo-ematn,
            s_bedat for ekko-bedat,
            s_banfn for eket-banfn.                    "Purchase req
SELECTION-SCREEN END OF BLOCK BOX1.

* DELETED
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
selection-screen comment  1(15) text-210.
selection-screen comment 20(13) text-201 for field p_dltall.
parameter:       p_dltall  radiobutton                     group DELT.
selection-screen comment 45(22) text-211 for field p_delt.
parameter:       p_delt     radiobutton                    group DELT.
selection-screen comment 79(23) text-212 for field p_nondel.
parameter:       p_nondel  radiobutton                     group DELT.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.



selection-screen begin of block box2 with frame title text-002.
parameters:      p_rprt radiobutton group rbcr,     "Print report
                 p_excl radiobutton group rbcr.     "Excel spreadsheet
parameters       p_file like rlgrap-filename default 'H:\SAPTEMP'. "TR995
selection-screen end of block box2.


INITIALIZATION.
   move 'IBTFO  NB' to s_bsart.
   append s_bsart.


******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILE.
   PERFORM CHECK_FILE_PATH.
*End of TR995 changes
START-OF-SELECTION.
*-----------------------------------------------------------------------
* Select all PO's and the PO detail for report
*-----------------------------------------------------------------------
   SELECT * from ekko
       where ebeln in s_ebeln
         and bsart in s_bsart
         and ekgrp in s_ekgrp
         and ekorg in s_ekorg
         and lifnr in s_lifnr
         and bedat in s_bedat
         and loekz = space.                 "Non-deleted records
         clear wa.
*         if ekko-bsart = 'FO'.
*            if ekko-kdate < s_kdate+3(8).
*             exit.
*            endif.
*         endif.
*-----------------------------------------------------------------------
*  fields being moved: bsart,  waers
*-----------------------------------------------------------------------
data:  tmp_bsart(5) type c. "like ekko-bsart.
         move-corresponding ekko to wa.
         move ekko-bsart to tmp_bsart.
         move ekko-ekgrp to wa_ekgrp.
         concatenate ekko-kdatb(4) '/' ekko-kdatb+4(2) '/'
                                ekko-kdatb+6(2) into wa-start.

         perform move_other_fields.

         select * from ekpo
            where ebeln = ekko-ebeln
              and ebelp in s_ebelp
*              and loekz = space              "Non-deleted records
              and werks in s_werks
              and knttp in s_knttp
              and ematn in s_ematn
              and matkl in s_matkl.
              if  ( p_dltall = 'X' ) or
                  ( p_delt = 'X' and ekpo-loekz <> ' ' ) or
                  ( p_nondel = 'X' and ekpo-loekz = ' ' ).
              else.
                  continue.
              endif.
              move-corresponding ekpo to wa.
              if ekpo-loekz <> ' '.
                 concatenate tmp_bsart ekpo-loekz into wa-bsart
                                                separated by '_'.
              else.
                 move tmp_bsart to wa-bsart.
              endif.
              move ekko-aedat to wa-aedat. "aedat is in both ekko & ekpo

*-----------------------------------------------------------------------
*  fields being moved: labnr, matkl, meins, netpr, menge
*-----------------------------------------------------------------------
             clear wa-ekgrp_matkl.
             concatenate ekko-ekgrp ekpo-matkl into wa-ekgrp_matkl
                                                  separated by '/'.
             concatenate ekpo-ebeln '_' ekpo-ebelp+3(2)
                                                   into wa-ebeln_ebelp.
             concatenate ekpo-labnr '_' ekpo-bednr into wa-labnr_bednr.
             concatenate ekpo-werks '/' ekpo-lgort into wa-werks_lgort.
             concatenate ekpo-ematn+12(6) '_' ekpo-txz01(25)
                                                    into wa-ematn_txz01.

* Purchase Requisition Information.
             clear:  wa-eindt, wa-banfn, wa-bnfpo.
             select single * from eket
              where ebeln = ekpo-ebeln
                and ebelp = ekpo-ebelp
                and eindt in s_eindt.

              if sy-subrc = '0'.
              concatenate eket-eindt(4) '/' eket-eindt+4(2) '/'
                                 eket-eindt+6(2) into wa-eindt.
                 move eket-banfn to wa-banfn.
                 move eket-bnfpo to wa-bnfpo.
                 perform get_eban_purchase_req_info.
              else.
                 continue.
              endif.

              perform get_po_release_date.

              if wa-udate = '00000000' and wa-strat = space.
                 move wa-aedat  to wa-udate.
              endif.

              if wa-badat <> '00000000'.
                 if wa-udate <> '00000000'.
                    compute wa-po_pr_days = wa-udate - wa-badat.
                 else.
                    clear wa-po_pr_days.
                 endif.
              else.
                 clear wa-po_pr_days.
              endif.

              if wa-aedat <> '00000000'.
                 if wa-udate <> '00000000'.
                    compute wa-podays = wa-udate - wa-aedat.
                 else.
                    clear wa-podays.
                 endif.
              else.
                 clear wa-podays.
              endif.

            append wa.
     endselect.
  endselect.

* Retain only those items whose purchase req. # is in the variant.

  loop at wa.
    if wa-banfn in s_banfn.
       continue.
    else.
       delete wa.
    endif.
  endloop.

sort wa by bsart lifnr_name ebeln_ebelp.
if p_rprt = 'X'.
   perform create_output_report.
else.
   perform create_spreadsheet.
endif.

*--------------------------  MOVE_EKKO_TO_WA ---------------------------
*  See MOVE_EKPO_TO_WA regarding move-corresponding for LIFNR
*-----------------------------------------------------------------------
FORM MOVE_EKKO_TO_WA.
   move-corresponding ekko to wa.
   move ekko-ekgrp to wa-ekgrp_matkl.
   if ekpo-werks <> space and ekpo-lgort <> space.
      concatenate ekpo-werks '/' ekpo-lgort into wa-werks_lgort.
   else.
      move ekpo-werks to wa-werks_lgort.
   endif.
   if ekko-bstyp = 'B'.
      move wa-ebeln_ebelp(10) to wa-banfn.
      clear wa-ebeln_ebelp.
   endif.
ENDFORM.

*-------------------------- MOVE_EKPO_TO_WA ----------------------------
*  The field sizes of material number and item number have been reduced
*  to conserve space on the report.  In a move-corresponding field names
*  and attributes must be the same.  Therefore, these 2 fields must be
*  moved individually.
*-----------------------------------------------------------------------
FORM MOVE_EKPO_TO_WA.
   clear wa-ematn_txz01.
   move-corresponding ekpo to wa.
   if ekpo-ematn+12(6) <> 0.
    concatenate ekpo-ematn+12(6) '_' ekpo-txz01(25) into wa-ematn_txz01.
   else.
    move ekpo-txz01 to wa-ematn_txz01.
   endif.
   concatenate ekpo-ebeln '_' ekpo-ebelp+3(2) into wa-ebeln_ebelp.
   move ekpo-banfn+4(6)    to wa-banfn.
*   concatenate wa-ekgrp_matkl ekpo-matkl into wa-ekgrp_matkl
*                                                     separated by '_'.
ENDFORM.

form get_po_release_date.
data: changenr like cdhdr-changenr.
data: releases type i.

  clear: wa-udate, changenr, releases...
  select * from CDPOS
      where objectclas   = 'EINKBELEG'
        and objectid     = ekko-ebeln
        and fname        = 'FRGKE'
        and value_new    = 'Y'.
      if changenr is initial.
         move cdpos-changenr to changenr.
         add 1 to releases.
      else.
         if cdpos-changenr > changenr.
            add 1 to releases.
         else.
            move cdpos-changenr to changenr.
            add 1 to releases.
         endif.
      endif.
   endselect.

   select single * from CDHDR
        where objectclas = 'EINKBELEG'
          and objectid   = ekko-ebeln
          and changenr   = changenr.
     if sy-subrc = '0'.
        move cdhdr-udate  to wa-udate.
     endif.

   move releases to wa-releases.

endform.

form create_output_report.

   move sy-repid  to w_repttl.
   move '-'       to w_repttl+10(1).
   move text-ttl  to w_repttl+12(35).

   concatenate text-dte sy-datum text-clt sy-mandt sy-sysid
                into w_head01 separated by space.



perform prot_header.
if p_rprt = 'X'.
   clear w_option.
   if sy-batch = 'X'.
      w_option = 'LINESELMOD:1'.
   endif.
endif.

call function 'HR_DISPLAY_BASIC_LIST'
    exporting
       basic_list_title             = w_repttl
       file_name                    = sy-cprog
       head_line1                   = w_head01
       head_line2                   = w_head02
       additional_options           = w_option
    importing
       return_code                  = retcode
    tables
       data_tab                     = wa
       fieldname_tab                = prot_header
       error_tab                     = errortab
    exceptions
       download_problem             = 1
       no_data_tab_entries          = 2
       table_mismatch               = 3
       print_problems               = 4
       others                       = 5.

    if sy-subrc <> 0.
       write: /1 'table download unsuccessful - reason = ', sy-subrc.
       write: /1 'No data selected'.
    endif.
endform.

form prot_header.
    move text-100        to prot_header-spaltenname.
    append prot_header.
    move text-101        to prot_header-spaltenname.
    append prot_header.
*    move text-102        to prot_header-spaltenname.
*    append prot_header.
    move text-103        to prot_header-spaltenname.
    append prot_header.
    move text-104        to prot_header-spaltenname.
    append prot_header.
    move text-113        to prot_header-spaltenname.
    append prot_header.
    move text-114        to prot_header-spaltenname.
    append prot_header.
    move text-105        to prot_header-spaltenname.
    append prot_header.
    move text-106        to prot_header-spaltenname.
    append prot_header.
    move text-107        to prot_header-spaltenname.
    append prot_header.
    move text-108        to prot_header-spaltenname.
    append prot_header.
    move text-110        to prot_header-spaltenname.
    append prot_header.
    move text-116        to prot_header-spaltenname.
    append prot_header.


    move text-109        to prot_header-spaltenname.
    append prot_header.
    move text-111        to prot_header-spaltenname.
    append prot_header.
    move text-124        to prot_header-spaltenname.
    append prot_header.

    move text-112        to prot_header-spaltenname.
    append prot_header.


    move text-115        to prot_header-spaltenname.
    append prot_header.

    move text-117        to prot_header-spaltenname.
    append prot_header.
    move text-118        to prot_header-spaltenname.
    append prot_header.
    move text-119        to prot_header-spaltenname.
    append prot_header.
    move text-120        to prot_header-spaltenname.
    append prot_header.
    move text-121        to prot_header-spaltenname.
    append prot_header.
    move text-122        to prot_header-spaltenname.
    append prot_header.
    move text-123        to prot_header-spaltenname.
    append prot_header.
endform.

*-----------------------  MOVE_OTHER_FIELDS  ---------------------------
*  This routine is used to get the vendor's name from the master file
*  and the release strategy
*-----------------------------------------------------------------------
form move_other_fields.

   select single * from lfa1    "For each PO, get vendor name
      where lifnr = ekko-lifnr.
      if sy-subrc = '0'.
      concatenate ekko-lifnr+5(5) '_' lfa1-name1(29) into wa-lifnr_name.
      else.
         move ekko-lifnr+5(5) to wa-lifnr_name.
      endif.

    select single * from t16fs
       where frgsx = ekko-frgsx.
    if sy-subrc = '0'.
       concatenate t16fs-frgc1 t16fs-frgc2 t16fs-frgc3 into wa-strat.
    endif.
endform.

*

*---------------------------------------------------------------
* Purchase Requisition Create Date
*---------------------------------------------------------------
form get_eban_purchase_req_info.
  clear wa-badat.
  select single * from eban
      where banfn = eket-banfn
        and bnfpo = eket-bnfpo.
  if sy-subrc = '0'.
     move eban-badat to wa-badat.
*     move eban-frgdt to wa-frgdt.
  endif.
endform.


form create_spreadsheet.

data:  begin of lt_fnames occurs 0,
       text(60) type c,
       end of lt_fnames.

  lt_fnames-text = text-100.
  append lt_fnames.
  lt_fnames-text = text-101.
  append lt_fnames.
  lt_fnames-text = text-103.
  append lt_fnames.
  lt_fnames-text = text-104.
  append lt_fnames.
  lt_fnames-text = text-113.
  append lt_fnames.
  lt_fnames-text = text-114.
  append lt_fnames.
  lt_fnames-text = text-105.
  append lt_fnames.
  lt_fnames-text = text-106.
  append lt_fnames.
  lt_fnames-text = text-107.
  append lt_fnames.
  lt_fnames-text = text-108.
  append lt_fnames.
  lt_fnames-text = text-110.
  append lt_fnames.
  lt_fnames-text = text-116.  "PO Days
  append lt_fnames.
  lt_fnames-text = text-109.
  append lt_fnames.           "Start date
  lt_fnames-text = text-111.
  append lt_fnames.           "# of Releases
  lt_fnames-text = text-124.

  append lt_fnames.
  lt_fnames-text = text-112.
  append lt_fnames.


  lt_fnames-text = text-115.
  append lt_fnames.
  lt_fnames-text = text-117.
  append lt_fnames.
  lt_fnames-text = text-118.
  append lt_fnames.
  lt_fnames-text = text-119.
  append lt_fnames.
  lt_fnames-text = text-120.
  append lt_fnames.
  lt_fnames-text = text-121.
  append lt_fnames.
  lt_fnames-text = text-122.
  append lt_fnames.
  lt_fnames-text = text-123.
  append lt_fnames.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
    exporting
*           file_name                  = 'c:\saptemp' "TR995
            file_name                  = p_file       "TR995
            create_pivot               = '0'
    tables
            data_tab                   = wa
            fieldnames                 = lt_fnames
    exceptions
            file_not_exist             = 1
            filename_expected          = 2
            communication_error        = 3
            ole_object_method_error    = 4
            ole_object_property_error  = 5
            invalid_filename           = 6
            invalid_pivot_fields       = 7
            download_problem           = 8
            others                     = 9.
   if sy-subrc <> 0.
   endif.

endform.

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      LV_BOL TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = P_FILE
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

*lv_dir = sep_path.
IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
*Check if directory path exist or not.
CALL METHOD cl_gui_frontend_services=>directory_exist
  EXPORTING
    directory            = sep_path      "lv_dir
  RECEIVING
    result               = lv_bol
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    wrong_parameter      = 3
    not_supported_by_gui = 4
    OTHERS               = 5.
IF lv_bol IS INITIAL.
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.
