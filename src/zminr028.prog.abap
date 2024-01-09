REPORT zminr028 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR028
*    Programmer  :  M De Meester
*    Client      :  Union Gas Limited
*    Date        :  December 2003
*
* This report will list the information on all outstanding reservations
* by plant and storage locations.
*
************************************************************************
* 2006/06/21 mdemeest 4.7 Replaced CALL function for excel spreadsheet
*                         because columns were expanding to maximum
*                         width.  Changed all numeric fields in EXCLTAB
*                         to characters to stop abends caused by column
*                         headings in numeric fields (per Laurie Callow)
*
* 03/12/09 md7140 #--- copied zminr006 as a base for this report
*                      added excel spreadsheet
*
* 2012/08/30 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
* 2013/10/22 G Ymana   Corrected default path/filename
*           SDP#####
*
* 2013/12/16 PANUSURI  1.Fix the Excel column heading for 2 fields not
*                        populating currently
*                      2.Add 2 new fields
*                      3.Modify logic for the Recipient field
* 2014/12/09 MELDHOSE S01K900422
*                      1.Changes relevant for PM Reservations
*                      2.Performance tuning of Select Statements
* 2014/18/09 MELDHOSE S01K900455
*                      1.Change in logic for recipient
************************************************************************
TABLES  : resb,          "Reservations
          makt,          "Material Description
          marc,          "Plant Information
          mard,          "Storage Location Information
          ekpo,          "Purchase order detail
          ekbe,          "Purchase order history
          rkpf.          "Reservation Document Header

DATA    : qtyhand         TYPE i,
          qooamt          TYPE i,
          temp-bdmng      LIKE resb-bdmng.


*-------------------------- For Excel SpreadSheet ----------------------
DATA:  retcode         LIKE sy-subrc,
       w_option(11)    TYPE c         VALUE 'start_excel',
       w_head01(40)    TYPE c,
       w_head02(40)    TYPE c,
       w_repttl        LIKE sy-title.

DATA:  BEGIN OF prot_header  OCCURS 1,
       spaltenname(20) TYPE c,
       ddic_table(5)   TYPE c,
       ddic_field(5)   TYPE c,
       key             TYPE c,
       END OF prot_header.

DATA:  BEGIN OF exceltab     OCCURS 0,
       werks     LIKE mard-werks,
       lgort     LIKE mard-lgort,
       matnr     LIKE mard-matnr,
       maktx     LIKE makt-maktx,
       dismm     LIKE marc-dismm,
       bdter     LIKE resb-bdter,
       pkdat     TYPE sy-datum,                         " (+) S01K900422
*       rsnum     LIKE resb-rsnum,  "(-) PANUSURI Ticket 57435
       rsnum(10) TYPE c,            "(+) PANUSURI Ticket 57435
*       rspos     LIKE resb-rspos,  "(-) PANUSURI Ticket 57435
       rspos(4)  TYPE c,            "(+) PANUSURI Ticket 57435
*       BDMNG     TYPE I,                     "Requirements Qty
       bdmng(10) TYPE c,
*       enmng     type I,                     "Quantity Withdrawn
       enmng(10) TYPE c,
*       outstand  type i,
       outstand(10) TYPE c,
*       qtyhand   type i,
       qtyhand(10) TYPE c,
*       qooamt    type i,
       qooamt(10) TYPE c,
       meins     LIKE resb-meins,
       bwart     LIKE resb-bwart,
       accounting(20) TYPE c,
       wempf     LIKE resb-wempf,            "Recipient
       umlgo(9)  TYPE c,    "umlgo,  (+) PANUSURI Ticket 57435
       deltime   TYPE bdter,        "(+) PANUSURI Ticket 57435
       END OF exceltab.

*BOI by PANUSURI Ticket 57435
TYPES: BEGIN OF ty_twlad,
       werks      TYPE werks_d,
       lgort      TYPE lgort_d,
       adrnr      TYPE ad_addrnum,
       END OF ty_twlad,

       BEGIN OF ty_adrc,
       addrnumber TYPE ad_addrnum,
       name4      TYPE ad_name4,
       END OF ty_adrc.

DATA: lt_twlad   TYPE STANDARD TABLE OF ty_twlad,
      lwa_twlad  TYPE ty_twlad,
      lt_adrc    TYPE STANDARD TABLE OF ty_adrc,
      lwa_adrc   TYPE ty_adrc.
*EOI by PANUSURI Ticket 57435

DATA:  errortab LIKE hrerror OCCURS 0 WITH HEADER LINE.

DATA: gv_bdter TYPE bdter,                                      " (+) S01K900422
      gt_resb  TYPE STANDARD TABLE OF resb,                     " (+) S01K900422
      gt_rkpf  TYPE STANDARD TABLE OF rkpf,                     " (+) S01K900422
      gt_marc  TYPE STANDARD TABLE OF marc,                     " (+) S01K900422
      gt_mard  TYPE STANDARD TABLE OF mard,                     " (+) S01K900422
      gt_makt  TYPE STANDARD TABLE OF makt,                     " (+) S01K900422
      gt_ekpo  TYPE STANDARD TABLE OF ekpo,                     " (+) S01K900422
      gt_ekbe  TYPE STANDARD TABLE OF ekbe.                     " (+) S01K900422
*--------------------- End of EXCEL SpreadSheet ------------------------

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-001.


*SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.               " (-) S01K900422
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-102. " (+) S01K900422

SELECT-OPTIONS:
     s_plant         FOR   marc-werks,
     s_storlo        FOR   mard-lgort,
     s_matnum        FOR   marc-matnr.
SELECTION-SCREEN END OF BLOCK box1.

*SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.               " (-) S01K900422
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-103. " (+) S01K900422

PARAMETERS: p_date    LIKE sy-datum  OBLIGATORY.
PARAMETERS: p_pltm    TYPE char3.                               " (+) S01K900422
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-100. " (+) S01K900422
PARAMETERS: p_mr_al TYPE c RADIOBUTTON GROUP rvtp DEFAULT 'X',  " (+) S01K900422
            p_mr_mn TYPE c RADIOBUTTON GROUP rvtp,              " (+) S01K900422
            p_mr_pm TYPE c RADIOBUTTON GROUP rvtp.              " (+) S01K900422
SELECTION-SCREEN END OF BLOCK box3.                             " (+) S01K900422

*SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME.               " (-) S01K900422
SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-101. " (+) S01K900422
PARAMETERS:  p_excl  RADIOBUTTON GROUP rbcr,   "Excel Spreadsheet
             p_file LIKE rlgrap-filename
                    DEFAULT 'H:\SAPTEMP\ZMINR028.xlsx',     "TR995
             p_rprt  RADIOBUTTON GROUP rbcr.   "Print Report

SELECTION-SCREEN  END OF BLOCK box4.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  PERFORM check_file_path.
*End of TR995 changes
*******************************  MAIN  *********************************
TOP-OF-PAGE.
  WRITE: /1 text-rpt, sy-repid COLOR COL_NEGATIVE INVERSE ON,
         58 text-002 COLOR COL_HEADING,
        105 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE:  / text-clt UNDER text-rpt, sy-mandt,
         56 text-901 INTENSIFIED OFF,
             p_date COLOR COL_GROUP INVERSE ON, text-014,
             text-pge UNDER text-dte, sy-pagno.
  WRITE: /.
  ULINE.
  FORMAT COLOR COL_NORMAL.
*  WRITE: /91 text-030, text-032, text-034, text-036, text-038. (-) S01K900422
  WRITE: /101 text-030, text-032, text-034, text-036, text-038.
  WRITE: /1 text-020, text-028, text-004, text-029,
*            text-021, text-024, text-007, text-008,          " (-) S01K900422
            text-021, text-024, text-104, text-007, text-008, " (+) S01K900422
            text-031 UNDER text-030, text-033 UNDER text-032,
            text-035 UNDER text-034, text-037 UNDER text-036,
            text-039 UNDER text-038,
*            text-040, text-041, text-042, text-043,
            text-040, text-041, text-042 RIGHT-JUSTIFIED, text-043,
            text-044, text-045.  "(+) PANUSURI Ticket 57435

  ULINE.
  WRITE: /.

*-------------------------- START-OF-SELECTION -------------------------
START-OF-SELECTION.
  FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
  CLEAR exceltab.
  REFRESH exceltab.
* BOI by PANUSURI Ticket 57435
* Determination of Address from Plant and Storage Location
  SELECT werks
         lgort
         adrnr
         FROM twlad
         INTO TABLE lt_twlad
         WHERE lfdnr = '001'.

* Addresses (Business Address Services)
  SELECT addrnumber
         name4
         FROM adrc
         INTO TABLE lt_adrc.
* EOI by PANUSURI Ticket 57435

  gv_bdter = p_date + p_pltm.                                   " (+) S01K900422
  SELECT * FROM resb
    INTO TABLE gt_resb                                          " (+) S01K900422
    WHERE matnr IN s_matnum
      AND werks IN s_plant
      AND lgort IN s_storlo
      AND xloek NE 'X'
*{   INSERT         D30K925528                                        1
AND XWAOK EQ 'X'"SKAPSE TICKET #83358 D30K925528
*}   INSERT
      AND kzear NE 'X'
*      AND bdter < p_date.                .                     " (-) S01K900422
      AND bdter LE gv_bdter.                                    " (+) S01K900422

  IF sy-subrc EQ 0.                                             " (+) S01K900422
    SELECT * FROM rkpf                                          " (+) S01K900422
    INTO TABLE gt_rkpf                                          " (+) S01K900422
    FOR ALL ENTRIES IN gt_resb                                  " (+) S01K900422
    WHERE rsnum = gt_resb-rsnum.                                " (+) S01K900422

    IF sy-subrc EQ 0.                                           " (+) S01K900422
      SORT gt_rkpf BY rsnum.                                    " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422
  ENDIF.                                                        " (+) S01K900422

  LOOP AT gt_resb INTO resb.                                    " (+) S01K900422
    READ TABLE gt_rkpf INTO rkpf WITH KEY rsnum = resb-rsnum    " (+) S01K900422
                                 BINARY SEARCH.                 " (+) S01K900422
    IF sy-subrc NE 0.                                           " (+) S01K900422
      CONTINUE.                                                 " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422

    CASE rkpf-kzver.                                            " (+) S01K900422
      WHEN 'F'.                                                 " (+) S01K900422
        IF p_mr_mn EQ abap_true.                                " (+) S01K900422
          CONTINUE.                                             " (+) S01K900422
        ENDIF.                                                  " (+) S01K900422
      WHEN OTHERS.                                              " (+) S01K900422
        IF p_mr_pm EQ abap_true.                                " (+) S01K900422
          CONTINUE.                                             " (+) S01K900422
        ENDIF.                                                  " (+) S01K900422
    ENDCASE.                                                    " (+) S01K900422

* If Material number is blank, then do not populate Excel       " (+) S01K900422
    IF resb-matnr IS INITIAL.                                   " (+) S01K900422
      CONTINUE.                                                 " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422

* if storage location is empty, default it to Main Storage Location A001
    IF resb-lgort EQ space.
      MOVE 'A001' TO exceltab-lgort.
    ELSE.
      exceltab-lgort = resb-lgort.
    ENDIF.
* end of storage location default

    temp-bdmng = resb-bdmng - resb-enmng.
    MOVE resb-matnr TO exceltab-matnr.
    MOVE resb-werks TO exceltab-werks.
    MOVE resb-rsnum TO exceltab-rsnum.
    MOVE resb-rspos TO exceltab-rspos.
    MOVE resb-bdmng TO exceltab-bdmng.       "Requirements qty
*    MOVE resb-wempf TO exceltab-wempf.       "Recipient  "(-) PANUSURI Ticket 57435
    MOVE temp-bdmng TO exceltab-outstand.
    MOVE resb-meins TO exceltab-meins.
    MOVE resb-bdter TO exceltab-bdter.
    exceltab-pkdat = resb-bdter - p_pltm.                     " (+) S01K900422
*    IF exceltab-pkdat LT sy-datum.                            " (+) S01K900422
*      exceltab-pkdat = sy-datum.                              " (+) S01K900422
*    ENDIF.                                                    " (+) S01K900422
    MOVE resb-bwart TO exceltab-bwart.
    MOVE resb-enmng TO exceltab-enmng.       "Quantity Withdrawn

*-----------------------------------------------------------------------
*  Reservation Document Header for Recipient & Accounting Information
*-----------------------------------------------------------------------
*  SELECT SINGLE * FROM rkpf                                  " (-) S01K900422
*    WHERE rsnum = resb-rsnum.                                " (-) S01K900422
*  IF sy-subrc = '0'.                                         " (-) S01K900422

    IF resb-wempf IS NOT INITIAL.                             " (+) S01K900455
      MOVE resb-wempf TO exceltab-wempf.                      " (+) S01K900455

*      IF rkpf-wempf IS NOT INITIAL."(+) PANUSURI Ticket 57435" (-) S01K900455

    ELSEIF rkpf-wempf IS NOT INITIAL.                         " (+) S01K900455
      MOVE rkpf-wempf TO exceltab-wempf.
* BOI by PANUSURI Ticket 57435
    ELSE.
      IF rkpf-umlgo IS NOT INITIAL.
        READ TABLE lt_twlad INTO lwa_twlad WITH KEY werks = rkpf-umwrk
                                                    lgort = rkpf-umlgo.
        IF sy-subrc = 0.
          READ TABLE lt_adrc INTO lwa_adrc WITH KEY addrnumber = lwa_twlad-adrnr.
          IF sy-subrc = 0.
            MOVE lwa_adrc-name4 TO exceltab-wempf.  "Recipient
          ENDIF.
        ENDIF.
*         Receiving/Issuing Storage Location
        MOVE rkpf-umlgo TO exceltab-umlgo.
      ENDIF.
    ENDIF.
* EOI by PANUSURI Ticket 57435
    IF rkpf-aufnr <> space.
*      MOVE rkpf-aufnr+6(6)   TO exceltab-accounting.          " (-) S01K900422
      WRITE rkpf-aufnr TO exceltab-accounting NO-ZERO RIGHT-JUSTIFIED.  " (+) S01K900422
    ELSEIF rkpf-kostl <> space.
*      MOVE rkpf-kostl      TO exceltab-accounting.            " (-) S01K900422
      WRITE rkpf-kostl TO exceltab-accounting NO-ZERO RIGHT-JUSTIFIED.  " (+) S01K900422
    ELSEIF rkpf-ps_psp_pnr <> space.
*      WRITE rkpf-ps_psp_pnr TO exceltab-accounting.           " (-) S01K900422
      WRITE rkpf-ps_psp_pnr TO exceltab-accounting RIGHT-JUSTIFIED.     " (+) S01K900422
    ENDIF.
*  ENDIF.                                                      " (-) S01K900422

    APPEND exceltab.
    CLEAR exceltab.
    CLEAR qtyhand.
*ENDSELECT.   "end of RESB "                                   " (-) S01K900422
  ENDLOOP.                                                     " (+) S01K900422


  IF exceltab[] IS NOT INITIAL.                                 " (+) S01K900422
    SELECT * FROM marc                                          " (+) S01K900422
             INTO TABLE gt_marc                                 " (+) S01K900422
             FOR ALL ENTRIES IN exceltab                        " (+) S01K900422
             WHERE matnr = exceltab-matnr AND                   " (+) S01K900422
                   werks = exceltab-werks AND                   " (+) S01K900422
                   lvorm NE 'X'.                                " (+) S01K900422

    IF sy-subrc EQ 0.                                           " (+) S01K900422
      SORT gt_marc BY matnr werks.                              " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422

    SELECT * FROM mard                                          " (+) S01K900422
             INTO TABLE gt_mard                                 " (+) S01K900422
             FOR ALL ENTRIES IN exceltab                        " (+) S01K900422
             WHERE matnr = exceltab-matnr AND                   " (+) S01K900422
                   werks = exceltab-werks AND                   " (+) S01K900422
                   lgort = exceltab-lgort AND                   " (+) S01K900422
                   lvorm NE 'X'.                                " (+) S01K900422

    IF sy-subrc EQ 0.                                           " (+) S01K900422
      SORT gt_mard BY matnr werks lgort.                        " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422

    SELECT * FROM makt                                          " (+) S01K900422
             INTO TABLE gt_makt                                 " (+) S01K900422
             FOR ALL ENTRIES IN exceltab                        " (+) S01K900422
             WHERE matnr = exceltab-matnr AND                   " (+) S01K900422
                   spras = sy-langu.                            " (+) S01K900422

    IF sy-subrc EQ 0.                                           " (+) S01K900422
      SORT gt_makt BY matnr.                                    " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422

    SELECT * FROM ekpo                                          " (+) S01K900422
             INTO TABLE gt_ekpo                                 " (+) S01K900422
             FOR ALL ENTRIES IN exceltab                        " (+) S01K900422
             WHERE matnr = exceltab-matnr AND                   " (+) S01K900422
                   werks = exceltab-werks AND                   " (+) S01K900422
                   ( lgort = exceltab-lgort OR lgort = space )  " (+) S01K900422
            AND loekz NE 'L' AND                                " (+) S01K900422
                   elikz NE 'X'.                                " (+) S01K900422

    IF sy-subrc EQ 0.                                           " (+) S01K900422
      SORT gt_ekpo BY matnr werks lgort.                        " (+) S01K900422

      SELECT * FROM ekbe                                        " (+) S01K900422
               INTO TABLE gt_ekbe                               " (+) S01K900422
               FOR ALL ENTRIES IN gt_ekpo                       " (+) S01K900422
               WHERE ebeln = gt_ekpo-ebeln AND                  " (+) S01K900422
                     ebelp = gt_ekpo-ebelp AND                  " (+) S01K900422
                     bewtp = 'E'.                               " (+) S01K900422
      IF sy-subrc EQ 0.                                         " (+) S01K900422
        SORT gt_ekbe BY ebeln ebelp.                            " (+) S01K900422
      ENDIF.                                                    " (+) S01K900422
    ENDIF.                                                      " (+) S01K900422
  ENDIF.                                                        " (+) S01K900422

*-----------------------------------------------------------------------
*  extract rest of information from MARC, MARD, MAKT,
  LOOP AT exceltab.
*    SELECT SINGLE * FROM marc WHERE matnr = exceltab-matnr      " (-) S01K900422
*                         AND werks = exceltab-werks             " (-) S01K900422
*                         AND lvorm NE 'X'.                      " (-) S01K900422
    READ TABLE gt_marc INTO marc WITH KEY matnr = exceltab-matnr " (+) S01K900422
                                          werks = exceltab-werks " (+) S01K900422
                                          BINARY SEARCH.         " (+) S01K900422
    IF sy-subrc = 0.
      MOVE marc-dismm TO exceltab-dismm.
*     Calculate estimated delivery date                 "(+) PANUSURI Ticket 57435
      exceltab-deltime = marc-plifz + exceltab-bdter.   "(+) PANUSURI Ticket 57435
    ENDIF.

*    SELECT SINGLE * FROM mard WHERE matnr = exceltab-matnr      " (-) S01K900422
*                                AND werks = exceltab-werks      " (-) S01K900422
*                                AND lgort = exceltab-lgort      " (-) S01K900422
*                                AND lvorm NE 'X'.               " (-) S01K900422
    READ TABLE gt_mard INTO mard WITH KEY matnr = exceltab-matnr " (+) S01K900422
                                          werks = exceltab-werks " (+) S01K900422
                                          lgort = exceltab-lgort " (+) S01K900422
                                          BINARY SEARCH.         " (+) S01K900422
    IF  sy-subrc = '0'.
      qtyhand = mard-labst + mard-umlme +
                mard-insme + mard-speme + mard-einme.
      MOVE qtyhand        TO exceltab-qtyhand.
    ENDIF.


*    SELECT SINGLE * FROM makt WHERE matnr = exceltab-matnr      " (-) S01K900422
*                                AND spras = sy-langu.           " (-) S01K900422
    READ TABLE gt_makt INTO makt WITH KEY matnr = exceltab-matnr " (+) S01K900422
                                 BINARY SEARCH.                  " (+) S01K900422
    IF sy-subrc = '0'.
      MOVE makt-maktx TO exceltab-maktx.            "Description
    ENDIF.

* determine quantity on order
    CLEAR qooamt.
*    SELECT * FROM ekpo                                          " (-) S01K900422
*      WHERE matnr = exceltab-matnr                              " (-) S01K900422
*        AND werks = exceltab-werks                              " (-) S01K900422
*        AND ( lgort = exceltab-lgort OR lgort = space )         " (-) S01K900422
*        AND loekz <> 'L'       "can't be flagged for deletion   " (-) S01K900422
*        AND elikz <> 'X'.      "delivery still required         " (-) S01K900422
*      ADD ekpo-menge TO qooamt.                                 " (-) S01K900422
    CLEAR ekpo.                                                  " (+) S01K900422
    LOOP AT gt_ekpo INTO ekpo WHERE matnr = exceltab-matnr       " (+) S01K900422
                                AND werks = exceltab-werks       " (+) S01K900422
                              AND ( lgort = exceltab-lgort       " (+) S01K900422
                                 OR lgort = space ).             " (+) S01K900422
      ADD ekpo-menge TO qooamt.                                  " (+) S01K900422

*    SELECT * FROM ekbe                                          " (-) S01K900422
*        WHERE ebeln = ekpo-ebeln                                " (-) S01K900422
*          AND ebelp = ekpo-ebelp                                " (-) S01K900422
*          AND bewtp = 'E'.                                      " (-) S01K900422
      LOOP AT gt_ekbe INTO ekbe WHERE ebeln = ekpo-ebeln AND     " (+) S01K900422
                                      ebelp = ekpo-ebelp.        " (+) S01K900422

        IF ekbe-shkzg = 'H'.
          qooamt =  qooamt + ekbe-menge.
        ELSE.
          qooamt = qooamt - ekbe-menge.
        ENDIF.

*    ENDSELECT.                                  "end of EKBE    " (-) S01K900422
      ENDLOOP.                                                   " (+) S01K900422
*  ENDSELECT.                                    "end of EKPO    " (-) S01K900422
    ENDLOOP.                                                     " (+) S01K900422

    MOVE qooamt TO exceltab-qooamt.
* end of quantity on order

    MODIFY exceltab.
  ENDLOOP.
*-----------------------------------------------------------------------
  SORT exceltab BY werks  ASCENDING
                 lgort  ASCENDING
                 bdter  ASCENDING
                 matnr  ASCENDING.

  IF p_excl = 'X'.
    PERFORM create_excel_report.
  ELSE.
    PERFORM create_print_report.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  create_print_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_print_report.
*{   INSERT         D30K924605                                        1
  IF NOT exceltab IS INITIAL.
*}   INSERT
  LOOP AT exceltab.
    WRITE: / exceltab-werks UNDER text-020,        "Plant
             exceltab-lgort UNDER text-028,        "Storage Loc
             exceltab-matnr+12(6) UNDER text-004,  "Material
             exceltab-rsnum UNDER text-007,        "Reserv#
             exceltab-bdter UNDER text-024,        "Req. Date
             exceltab-pkdat UNDER text-104,       "Pick Date    " (+) S01K900422
             exceltab-rspos UNDER text-008,        "Item #
             exceltab-bdmng UNDER text-030,        "Requirement Qty
             exceltab-meins UNDER text-040,        "Unit of M
             exceltab-bwart UNDER text-041,        "Movement Type
             exceltab-outstand UNDER text-034,     "Outstanding
             exceltab-enmng UNDER text-033,        "Qty Withdrn
             exceltab-wempf UNDER text-043,        "Recipient
*             exceltab-accounting UNDER text-042,   "Accounting " (-) S01K900422
             exceltab-accounting UNDER text-042 RIGHT-JUSTIFIED,   "Accounting " (+) S01K900422
             exceltab-maktx UNDER text-029,       "Description
             exceltab-dismm UNDER text-021,       "MRP type
             exceltab-qtyhand UNDER text-036,     "QOH
             exceltab-qooamt UNDER text-039,      "Qoo
             exceltab-umlgo  UNDER text-044,      "Rcvg Sloc    "(+) PANUSURI Ticket 57435
             exceltab-deltime UNDER text-045 .    "Est Deltime  "(+) PANUSURI Ticket 57435

  ENDLOOP.
*{   INSERT         D30K924605                                        2
 ELSE.
   MESSAGE 'No Records found'(003) TYPE 'I'.
 ENDIF.
*}   INSERT
ENDFORM.                    "create_print_report

*---------------------------  CREATE_OUTPUT_REPORT ---------------------
FORM create_excel_report.
  PERFORM prot_header.
  MOVE text-ttl             TO w_repttl.
  MOVE text-dte             TO w_head01.
  WRITE sy-datum            TO w_head01+6(10).
  WRITE text-amp            TO w_head01+17(1).
  MOVE sy-uzeit             TO w_head01+19(10).
  MOVE text-clt             TO w_head02.
  MOVE sy-mandt             TO w_head02+8(4).
  MOVE sy-sysid             TO w_head02+13(4).
  IF p_rprt = 'X'.
    CLEAR w_option.
    IF sy-batch = 'X'.
      w_option = 'LINESELMOD:1'.
    ENDIF.
  ENDIF.

*  call function 'HR_DISPLAY_BASIC_LIST'
*      EXPORTING
*          basic_list_title         = w_repttl
*          file_name                = sy-cprog
*          head_line1               = w_head01
*          head_line2               = w_head02
*          additional_options       = w_option
*      IMPORTING
*          return_code              = retcode
*      TABLES
*          data_tab                 = exceltab
*          fieldname_tab            = prot_header
*          error_tab                = errortab
*      EXCEPTIONS
*          download_problem         = 1
*          no_data_tab_entries      = 2
*          tables_mismatch          = 3
*          print_problems           = 4
*          others                   = 5.

*{   INSERT         D30K924605                                        1
IF NOT exceltab IS INITIAL.
*}   INSERT
  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
    EXPORTING
      file_name                 = p_file                    "TR995
*     FILE_NAME                 = 'C:\SAPTEMP\ZMINR028.xls' "TR995
      create_pivot              = 0
    TABLES
      data_tab                  = exceltab
      fieldnames                = prot_header
    EXCEPTIONS
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_block = 5
      invalid_filename          = 6
      invalid_pivot_fields      = 7
      download_problem          = 8
      OTHERS                    = 9.

  IF sy-subrc <> '0'.
    WRITE: /1 'table download unsuccessful - reason = ', sy-subrc.
  ENDIF.
*{   INSERT         D30K924605                                        2
 ELSE.
   MESSAGE 'No Records found'(003) TYPE 'I'.
 ENDIF.
*}   INSERT
ENDFORM.                    "create_excel_report

*--------------------------- PROT_HEADER -------------------------------
* Each field title in the spreadsheet must be added in the same order as
* the order of the data.
*-----------------------------------------------------------------------
FORM prot_header.
  MOVE text-020 TO prot_header-spaltenname.   "Plant
  APPEND prot_header.

  MOVE text-028 TO prot_header-spaltenname.   "Storage Location
  APPEND prot_header.

  MOVE text-004 TO prot_header-spaltenname.   "Material #
  APPEND prot_header.

  MOVE text-029 TO prot_header-spaltenname.   "Material Description
  APPEND prot_header.

  MOVE text-021 TO prot_header-spaltenname.   "MRP
  APPEND prot_header.

  MOVE text-024 TO prot_header-spaltenname.   "Requirement Date
  APPEND prot_header.

  MOVE text-104 TO prot_header-spaltenname.   "Pick Date        " (+) S01K900422
  APPEND prot_header.

  MOVE text-007 TO prot_header-spaltenname.   "Reservation #
  APPEND prot_header.

  MOVE text-008 TO prot_header-spaltenname.   "Item
  APPEND prot_header.

  MOVE text-030 TO prot_header-spaltenname.   "Requirement Qty
  APPEND prot_header.

  MOVE text-033 TO prot_header-spaltenname.   "Withdrawn
  APPEND prot_header.

  MOVE text-035 TO prot_header-spaltenname.   "Qty Outstanding
  APPEND prot_header.

  MOVE text-037 TO prot_header-spaltenname.   "Qty on Hand
  APPEND prot_header.

  MOVE text-039 TO prot_header-spaltenname.   "Qty on Order
  APPEND prot_header.

  MOVE text-040 TO prot_header-spaltenname.   "UOM
  APPEND prot_header.

  MOVE text-041 TO prot_header-spaltenname.   "Movement
  APPEND prot_header.

  MOVE text-042 TO prot_header-spaltenname.   "Account
  APPEND prot_header.

  MOVE text-043 TO prot_header-spaltenname.   "Recipient
  APPEND prot_header.
* BOI by PANUSURI Ticket 57435
  MOVE text-044 TO prot_header-spaltenname.   "Rcvg Sloc
  APPEND prot_header.

  MOVE text-045 TO prot_header-spaltenname.   "Est Deltime
  APPEND prot_header.
* EOI by PANUSURI Ticket 57435

ENDFORM.                    "PROT_HEADER

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*lv_dir = sep_path.
  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
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
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
