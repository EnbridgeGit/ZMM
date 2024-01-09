report zmmmr062 no standard page heading line-size 256 line-count 65
       message-id zm.

******************************************************************
*       Owner: Union                                             *
*  Programmer: M DeMeester                                       *
*        Date: September 19,2008                                 *
*  Request ID: TR005                                             *
*                                                                *
* This report produces a Characteristics / Catalogue based on    *
* selection criteria.                                            *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
*  2008/09/19 mdemeester TR005 - New report                      *
*  2009/12/07 lritchie         - Performance improvements        *
* 2012/09/04 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
*
******************************************************************

tables: mara,      "Material information
        makt,      "Description
        mbew,      "Pricing
        t023,      "Material Group Table
        klah,      "Class --> Client ID
        kssk,      "Client ID --> Material
        cabn,      "Characteristics
        mard,      "Material master, storage location/batch segment
        ausp,      "Characteristic values
        marc.      "Plant description

data:  delimiter(1) type c value '_'.
data:  begin of mattab   occurs 0,
       mgrp_byr(9) type c,              "Material Group / buyer
       matnr(18) type c,                 "Material number
       maktx    like makt-maktx,        "Description
       clint    like kssk-clint,        "Class
       class    like klah-class,        "Class
       char1(80) type c,
       char2(80) type c,
       char3(80) type c,
       char4(80) type c,
       char5(80) type c,
       char6(80) type c,
       char7(80) type c,
       char8(80) type c,
       char9(80) type c,
       verpr    like mbew-verpr,        "Average unit price
       meins    like mara-meins,        "Unit of measure
       bismt    like mara-bismt,        "Old material number
       end of mattab.

data:  begin of planttab   occurs 0,
       werks    like marc-werks,        "Plant
       mgrp_byr(9) type c,              "Material Group / buyer
       matnr(18) type c,                 "Material number
       maktx    like makt-maktx,        "Description
       class    like klah-class,        "Class
       char1(80) type c,
       char2(80) type c,
       char3(80) type c,
       char4(80) type c,
       char5(80) type c,
       char6(80) type c,
       char7(80) type c,
       char8(80) type c,
       char9(80) type c,
       meins    like mara-meins,        "Unit of measure
       qoh(18) type c,                  "Plant QOH
       qoha001(18) type c,                                  "A001 QOH
       verpr(18) type c,   "like mbew-verpr,  "Average unit price
       lgpbe(18),     "like mard-lgpbe, "Bin Loc
       bismt    like mara-bismt,        "Old material number
       end of planttab.

data:  begin of classtab occurs 100,
       class     like klah-class,
       clint     like klah-clint,
       end of classtab.

data:  begin of tabchar occurs 0,
       len       like cabn-anzst,
       atinn     like cabn-atinn,
       atnam     like cabn-atnam,
       end of tabchar.

data: begin of tbl_kssk occurs 0,
        objek    like kssk-objek,
        clint    like kssk-clint,        "Class
      end of tbl_kssk.

data: begin of tablechar occurs 9,
        objek    like ausp-objek,
        atinn    like ausp-atinn,
        atwrt    like ausp-atwrt,
        atflv    like ausp-atflv,
      end of tablechar.

data: tmp_matnr  like mara-matnr.
data: tmp_objek  like ausp-objek.
data: tmp_tabix  like sy-tabix.
data: charx(20)  type c.
data: keyword_atinn like cabn-atinn.
data: model_atinn   like cabn-atinn.
data: manuf_atinn   like cabn-atinn.
data: model(40) type c.
data: manuf(40) type c.

data:  begin of csvtab occurs 0,
       csv_file(360) type c,
       end of csvtab.

* This first block produces a border like frame around the following: *
selection-screen begin of block outputoptions with frame title text-102.
selection-screen skip.
selection-screen begin of line.           "Message from variant
selection-screen comment 5(79) text-300.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(79) text-301.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(79) text-302.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(79) text-303.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(79) text-304.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(79) text-305.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
selection-screen comment 5(24) text-251.
parameter:  p_back  radiobutton group rbrc default 'X'. "excel file
selection-screen comment 35(10) text-252.
parameter:  p_name(15) lower case default 'file.csv'.
parameter:  p_file like rfpdo-rfbifile
             default '/usr/sap/interfaces/P01/CFMM001/' no-display.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment  5(24) text-240.
parameter:  p_excl  radiobutton group rbrc. "default 'X'. "Screen Excel

selection-screen end of line.
parameter:  p_xlfile LIKE RLGRAP-FILENAME DEFAULT 'H:\SAPTEMP'. "TR995

selection-screen end of block outputoptions.


selection-screen begin of block general with frame title text-001.
select-options:
    s_matnr            for mara-matnr.     "Material


selection-screen begin of line.
selection-screen comment 1(15) text-201.    "Material Group
selection-screen end of line.

selection-screen begin of line.               "Single material group
selection-screen comment 5(30)  text-216.
parameters: p_single radiobutton group mtgr.
select-options: s_matkl for mara-matkl no intervals.
selection-screen end of line.

selection-screen begin of line.               "PIPE
selection-screen comment 5(30)  text-202.
parameters: p_pipe  radiobutton group mtgr.
selection-screen comment 43(20) text-203.
selection-screen end of line.

selection-screen begin of line.               "Construction Material
selection-screen comment 5(30)  text-204.
parameters: p_const  radiobutton group mtgr.
selection-screen comment 43(20) text-205.
selection-screen end of line.

selection-screen begin of line.               "Appliances
selection-screen comment 5(30)  text-206.
parameters: p_appl  radiobutton group mtgr.
selection-screen comment 43(30) text-207.
selection-screen end of line.

selection-screen begin of line.               "Compressor
selection-screen comment 5(30)  text-208.
parameters: p_comp  radiobutton group mtgr.
selection-screen comment 43(20) text-209.
selection-screen end of line.

selection-screen begin of line.               "Meter & Regulators
selection-screen comment 5(30)  text-210.
parameters: p_mtr  radiobutton group mtgr.
selection-screen comment 43(20) text-211.
selection-screen end of line.

selection-screen begin of line.               "General Supplies
selection-screen comment 5(30)  text-212.
parameters: p_gen  radiobutton group mtgr.
selection-screen comment 43(20) text-213.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(79) text-217.
selection-screen end of line.

*-----------------------------------------------------------------
selection-screen begin of line.               "Combined Above
selection-screen comment 5(30)  text-218.
parameters: p_cinv  radiobutton group mtgr.
selection-screen comment 43(40) text-219.
selection-screen end of line.

selection-screen begin of line.               "Combined Non-Inventory
selection-screen comment 5(30)  text-220.
parameters: p_cninv  radiobutton group mtgr.
selection-screen comment 43(40) text-221.
selection-screen end of line.

selection-screen begin of line.               "All material groups
selection-screen comment 5(30)  text-214.
parameters: p_all  radiobutton group mtgr default 'X'.
selection-screen comment 43(40) text-215.
selection-screen end of line.

select-options:
  s_class   for klah-class, "default ' ',        "Class Name
  s_clint   for kssk-clint no intervals no-display,
  s_kschg  for cabn-atnam no intervals. " default ' '. "English keyword

selection-screen end of block general.

* The following second block produces a frame within the first block. *

selection-screen begin of block criteria with frame title text-002.
selection-screen begin of line.               "Characteristics
selection-screen comment  5(16) text-230.
parameter: p_char1 like cabn-atnam.
selection-screen comment 60(16) text-234.
parameter: p_char5 like cabn-atnam.
selection-screen end of line.

selection-screen begin of line.               "Characteristics
selection-screen comment  5(16) text-231.
parameter: p_char2 like cabn-atnam.
selection-screen comment 60(16) text-235.
parameter: p_char6 like cabn-atnam.
selection-screen end of line.

selection-screen begin of line.               "Characteristics
selection-screen comment  5(16) text-232.
parameter: p_char3 like cabn-atnam.
selection-screen comment 60(16) text-236.
parameter: p_char7 like cabn-atnam.
selection-screen end of line.

selection-screen begin of line.               "Characteristics
selection-screen comment  5(16) text-233.
parameter: p_char4 like cabn-atnam.
selection-screen comment 60(16) text-237.
parameter: p_char8 like cabn-atnam.
selection-screen end of line.

select-options: s_charx for cabn-atinn no intervals no-display.

selection-screen end of block criteria.

* The following is the additional search parameters.

selection-screen begin of block searchparms with frame title text-003.
select-options:
  s_werks     for marc-werks modif id abc. "obligatory.  "Plant code

selection-screen end of block searchparms.

selection-screen begin of block reportoptions with frame title text-100.

selection-screen begin of line.
selection-screen comment 1(38) text-241.
selection-screen comment 45(4) text-yes.
parameter:  p_yold   radiobutton group rold.
selection-screen comment 58(4) text-noo.
parameter:  p_nold   radiobutton group rold.
selection-screen end of line.

selection-screen begin of line.              "Also available
selection-screen comment 1(50) text-242.
selection-screen end of line.

selection-screen begin of line.              "Display Plant QOH
selection-screen comment  5(38) text-243.
selection-screen comment 45(4) text-yes.
parameter:   p_yqoh radiobutton group rqoh.
selection-screen comment 58(4) text-noo.
parameter:   p_nqoh radiobutton group rqoh.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment  5(38) text-244.
selection-screen comment 45(4) text-yes.
parameter:   p_yaup radiobutton group raup.
selection-screen comment 58(4) text-noo.
parameter:   p_naup radiobutton group raup.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment  5(38) text-245.
selection-screen comment 45(4) text-yes.
parameter:   p_ybin radiobutton group rbin.
selection-screen comment 58(4) text-noo.
parameter:   p_nbin radiobutton group rbin.
selection-screen end of line.

selection-screen end of block reportoptions.

********************* INITIALIZATION *******************************
initialization.
  refresh s_kschg.
  refresh s_class.

  move 'ICP'          to s_kschg+0(3).  "keyword
  move '*'            to s_kschg+3(1).
  append s_kschg.

  move 'ICP'          to s_class+0(3).
  move '*'            to s_class+3(1).
  append s_class.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_XLFILE.
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
      P_XLFILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_XLFILE.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_XLFILE.
   PERFORM CHECK_FILE_PATH.
*End of TR995 changes
*-----------------------------------------------------------------------
*    End of selection screen
*------------------------------------------------------------------
start-of-selection.

  move '/usr/sap/interfaces/P01/CFMM001/' to p_file.
  concatenate p_file+0(32) p_name into p_file.
  move sy-sysid to p_file+20(3).

* This routine determines the internal number (atinn) for KEYWORD.
  select single atinn into cabn-atinn from cabn
     where atnam = 'KEYWORD'.
  if sy-subrc = '0'.
    move cabn-atinn to keyword_atinn.
  endif.

*-------------------------------------------------------------
*  This routine translates the entered class names into object
*  object ids.  * can be used in the class name ie *PI* PIPES
*-------------------------------------------------------------
  refresh s_clint.

  select class clint
     into table classtab
     from klah
     where klart = '001'
       and class in s_class.

  loop at classtab.
    move 'IEQ' to s_clint+0(3).
    move classtab-clint to s_clint+3(18).
    append s_clint.
  endloop.

*-------------------------------------------------------------------
* This routine adds the 8 optional characteristics (Characteristic n)
* entered in the variant to the "hidden" selection characteristics,
* resulting in all requested characteristics in one spot for ease of
* processing.
*-------------------------------------------------------------------

  refresh s_charx.
  clear s_charx.
  perform add_char using 'KEYWORD'.  "Keyword
  if p_char1 <> ' '.
    perform add_char using p_char1.     "Up to 8 other
  endif.                                 "characteristics
  if p_char2 <> ' '.
    perform add_char using p_char2.
  endif.
  if p_char3 <> ' '.
    perform add_char using p_char3.
  endif.
  if p_char4 <> ' '.
    perform add_char using p_char4.
  endif.
  if p_char5 <> ' '.
    perform add_char using p_char5.
  endif.
  if p_char6 <> ' '.
    perform add_char using p_char6.
  endif.
  if p_char7 <> ' '.
    perform add_char using p_char7.
  endif.
  if p_char8 <> ' '.
    perform add_char using p_char8.
  endif.

*-----------------------------------------------------------------------
*  This routine retains only those CLASSES with the corresponding object
*  number requested by the user.
*-----------------------------------------------------------------------

  refresh s_clint.
  loop at classtab.
    if classtab-class in s_class.
      move 'IEQ' to s_clint+0(3).
      move classtab-clint to s_clint+3(10).
      append s_clint.
    else.
      delete classtab.
    endif.
  endloop.

  refresh mattab.

  sort classtab by clint.
  if not classtab[] is initial.
    select objek clint
           into table tbl_kssk
           from kssk
           for all entries in classtab
           where mafid = 'O'
             and klart = '001'
             and clint = classtab-clint.
    if sy-subrc = 0.
      loop at tbl_kssk.
        if classtab-clint <> tbl_kssk-clint.
          read table classtab with key clint = tbl_kssk-clint
                                   binary search.
        endif.
        mattab-matnr = tbl_kssk-objek(18).
        if mattab-matnr in s_matnr.
          mattab-clint = tbl_kssk-clint.
          mattab-class = classtab-class.
          append mattab.
        endif.
      endloop.
    endif.
  endif.

  if p_back = 'X'.
    move 'X' to sy-batch.
    move 'SJOB' to sy-ucomm.
  endif.

  perform translate_material_group.

* ------------------------------------------------
*  This routine deletes all materials that are not
*  in the selected material group.
* ------------------------------------------------

  loop at mattab.

    select single matnr matkl bismt meins
         into (mara-matnr, mara-matkl, mara-bismt, mara-meins)
         from mara
         where matnr = mattab-matnr
           and matkl in s_matkl.
    clear: model, manuf.

    if sy-subrc = '0'.
      move mara-bismt    to mattab-bismt.
      move mara-meins    to mattab-meins.
      concatenate mara-matkl '_' into mattab-mgrp_byr.

      select single maktx into makt-maktx
         from makt          "Material Description
         where matnr = mattab-matnr
           and spras = 'EN'.
      if sy-subrc = '0'.
        translate makt-maktx using '" '.
        translate makt-maktx using ', '.
        move makt-maktx to mattab-maktx.
        perform get_material_characteristics.
        modify mattab.
      endif.

    else.
      delete mattab.
    endif.
  endloop.

* ------------------------------------------------
* Keep only those records with specified keyword (s_kyword.
*  delete mattab
*    where char1 not in s_keywd.
* ------------------------------------------------

  loop at mattab.
    move mattab-matnr to tmp_objek.
    select single atwrt into ausp-atwrt
      from ausp
      where objek = tmp_objek
        and atinn = keyword_atinn
        and klart = '001'.
    if sy-subrc = '0' and ausp-atwrt in s_kschg.
      move ausp-atwrt to mattab-char1.
      modify mattab.
    else.
      delete mattab.
    endif.
  endloop.

  delete mattab
    where clint not in s_clint.

  sort mattab by matnr.             "ler
  refresh planttab.
  loop at mattab.

    move-corresponding mattab to planttab.

    select matnr werks ekgrp into (marc-matnr, marc-werks, marc-ekgrp)
     from marc
     where werks in s_werks
      and matnr = mattab-matnr.
      perform get_clint_english.

      move marc-werks           to planttab-werks.
      concatenate mattab-mgrp_byr marc-ekgrp
                                   into planttab-mgrp_byr.

      select single verpr into mbew-verpr
        from mbew
        where matnr = marc-matnr
          and bwkey = marc-werks.
      if sy-subrc = '0'.
        move mbew-verpr to planttab-verpr.
      endif.

      clear: planttab-qoha001, planttab-qoh.
      select lgort labst lgpbe into (mard-lgort, mard-labst, mard-lgpbe)
         from mard
         where werks = marc-werks
           and matnr = marc-matnr.

        if mard-lgort = 'A001'.
          add mard-labst to planttab-qoha001.
          add  mard-labst to planttab-qoh.
          if p_ybin = 'X'.                 "optional bin location
            move mard-lgpbe to planttab-lgpbe.
          endif.
        else.
          add  mard-labst to planttab-qoh.
        endif.
      endselect.

      append planttab.
    endselect.            "end of MARC
    if sy-subrc <> 0 and s_werks[] is initial.
      move-corresponding mattab to planttab.
      append planttab.
    endif.
  endloop.

  loop at planttab.

    if planttab-werks = ' '.    "remove blank plants
      delete planttab.
      continue.
    endif.

    if p_yold = ' '.  "Old material number
      clear planttab-bismt.
    endif.

    if p_ybin = ' '.   "Bin location.
      move planttab-bismt to planttab-lgpbe.
      clear planttab-bismt.
    endif.

    if p_yaup = ' '.    "Average unit price
      move planttab-lgpbe to planttab-verpr.
      move planttab-bismt to planttab-lgpbe.
      clear planttab-bismt.
    endif.

    if p_yqoh = ' '.     "Quantity on hand
      move planttab-verpr to planttab-qoh.
      move planttab-lgpbe to planttab-qoha001.
      move planttab-bismt to planttab-verpr.
      clear: planttab-lgpbe, planttab-bismt.
    endif.

    if p_yold = ' ' or p_ybin = ' ' or p_yaup = ' ' or p_yaup = ' '.
      modify planttab.
    endif.

  endloop.

  if p_excl = 'X'.
    perform display_excel.
  elseif p_back = 'X'.
    open dataset p_file for output in text mode.
    perform create_csv_title.
    loop at planttab.
      perform create_csv_file.
      transfer csvtab to p_file.
    endloop.
    close dataset p_file.

    write: /1 'File Name is: ', p_file.
  endif.

*&---------------------------------------------------------------------*
*&      Form  append_to_single
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form append_to_single.

  move 'IEQ' to s_matkl.
  move t023-matkl  to s_matkl+3(4).
  append s_matkl.
endform.                    "append_to_single

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_excel.

  data: begin of lt_fnames occurs 0,
        text(60) type c,
        end of lt_fnames.

  lt_fnames-text = text-011.
  append lt_fnames.
  lt_fnames-text = text-014.
  append lt_fnames.
  lt_fnames-text = text-009.
  append lt_fnames.
  lt_fnames-text = text-007.
  append lt_fnames.
  lt_fnames-text = text-019.
  append lt_fnames.
  lt_fnames-text = 'KEYWORD'.                               "TEXT-041.
  append lt_fnames.
  lt_fnames-text = p_char1.                                 "TEXT-042.
  append lt_fnames.
  lt_fnames-text = p_char2.                                 "TEXT-043.
  append lt_fnames.
  lt_fnames-text = p_char3.
  append lt_fnames.
  lt_fnames-text = p_char4.
  append lt_fnames.
  lt_fnames-text = p_char5.
  append lt_fnames.
  lt_fnames-text = p_char6.
  append lt_fnames.
  lt_fnames-text = p_char7.
  append lt_fnames.
  lt_fnames-text = p_char8.
  append lt_fnames.
  lt_fnames-text = text-015.
  append lt_fnames.

  if p_yqoh = 'X'.    "Quantity of hand
    lt_fnames-text = text-008.
    append lt_fnames.
    lt_fnames-text = text-017.
    append lt_fnames.
  endif.

  if p_yaup = 'X'.    "Average unit price
    lt_fnames-text = text-018.
    append lt_fnames.
  endif.

  if p_ybin = 'X'.      "Bin Location optional
    lt_fnames-text = text-010.
    append lt_fnames.
  endif.

  if p_yold = 'X'.     "Old material# optional
    lt_fnames-text = text-016.
    append lt_fnames.
  endif.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
       exporting
*            file_name                 = 'C:\SAPTEMP'"TR995
            file_name                 = P_XLFILE     "TR995
            create_pivot              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       tables
*           PIVOT_FIELD_TAB           =
            data_tab                  = planttab
            fieldnames                = lt_fnames
       exceptions
            file_not_exist            = 1
            filename_expected         = 2
            communication_error       = 3
            ole_object_method_error   = 4
            ole_object_property_error = 5
            invalid_filename          = 6
            invalid_pivot_fields      = 7
            download_problem          = 8
            others                    = 9.
  if sy-subrc ne 0.
  endif.
endform.                    "DISPLAY_EXCEL

*&---------------------------------------------------------------------*
*&      Form  translate_material_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form translate_material_group.

  if p_single = 'X'.       "single entry already in table
    exit.
  else.

    refresh s_matkl.
    if p_pipe = 'X'.
      select matkl into t023-matkl from t023
        where matkl like '01%'.
        perform append_to_single.
      endselect.
    else.
      if p_const = 'X'.
        select matkl into t023-matkl from t023
          where matkl like '02%'.
          perform append_to_single.
        endselect.
      else.
        if p_appl = 'X'.
          select matkl into t023-matkl from t023
            where matkl like '03%' or matkl like '04%'.
            perform append_to_single.
          endselect.
        else.
          if p_comp = 'X'.
            select matkl into t023-matkl from t023
              where matkl like '05%'.
              perform append_to_single.
            endselect.
          else.
            if p_mtr = 'X'.
              select matkl into t023-matkl from t023
                where matkl like '06%'.
                perform append_to_single.
              endselect.
            else.
              if p_gen = 'X'.
                select matkl into t023-matkl from t023
                  where matkl like '07%'.
                  perform append_to_single.
                endselect.
              else.
                if p_cinv = 'X'.
                  select matkl into t023-matkl from t023
                    where matkl like '01%' or matkl like '02%' or matkl like '05%' or
                          matkl like '06%' or matkl like '07%'.
                    perform append_to_single.
                  endselect.
                else.
                  if p_cninv = 'X'.
                    select matkl into t023-matkl from t023
                      where matkl between '0800' and '9999'.
                      perform append_to_single.
                    endselect.
                  else.
                    if p_all = 'X'.
                      select matkl into t023-matkl from t023
                        where matkl between '0100' and '9999'.
                        perform append_to_single.
                      endselect.

                    endif.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.
endform.                    "translate_material_group

*&---------------------------------------------------------------------*
*&      Form  add_char
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CHARACTERISTIC  text
*----------------------------------------------------------------------*
form add_char using characteristic.

  select single atinn into cabn-atinn from cabn
    where atnam = characteristic.

  if characteristic <> space.
    move 'IEQ'          to s_charx+0(3).
    move cabn-atinn     to s_charx+3(10).
    move '0000000000'   to s_charx+13(10).
    collect s_charx.                        "eliminates duplicates
  endif.

endform.                    "add_char

*&---------------------------------------------------------------------*
*&      Form  get_material_characteristics
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_material_characteristics.

  refresh tablechar.
  move mattab-matnr to tmp_objek.

  select objek atinn atwrt atflv
         into table tablechar
         from ausp
         where objek = tmp_objek
           and atinn in s_charx.
  if sy-subrc = '0'.

    sort tablechar by atinn.

* Match the characteristics values from AUSP to the characteristics entered on the
* selection screen.

    loop at s_charx.
      move sy-tabix to tmp_tabix.
      loop at tablechar.

        if s_charx+3(10) = tablechar-atinn.

          case tmp_tabix.
            when '1'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char1.
            when '2'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char2.
            when '3'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char3.
            when '4'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char4.
            when '5'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char5.
            when '6'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char6.
            when '7'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char7.
            when '8'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char8.
            when '9'.
              perform move_char changing tablechar-atwrt
                                         tablechar-atflv
                                         mattab-char9.
          endcase.

        endif.
      endloop.
    endloop.
  endif.

endform.                    "get_material_characteristics

*&---------------------------------------------------------------------*
*&      Form  get_clint_english
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_clint_english.

  select single class into klah-class from klah
    where clint = mattab-clint.
  if sy-subrc = '0'.
    move klah-class to planttab-class.
  endif.

endform.                    "get_clint_english

*&---------------------------------------------------------------------*
*&      Form  create_csv_title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_csv_title.

  clear csvtab.
  concatenate text-011 text-014 text-009
              text-007 text-019 'KEYWORD'
              p_char1  p_char2  p_char3
              p_char4  p_char5  p_char6
              p_char7  p_char8
              text-015 text-008 text-017
              text-018 text-010 text-016
              into csvtab separated by ','.
  append csvtab.
  transfer csvtab to p_file.

endform.                    "create_csv_title
*&---------------------------------------------------------------------*
*&      Form  create_csv_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_csv_file.

  clear csvtab.
  concatenate planttab-werks
              planttab-mgrp_byr
              planttab-matnr
              planttab-maktx
              planttab-class
              planttab-char1
              planttab-char2
              planttab-char3
              planttab-char4
              planttab-char5
              planttab-char6
              planttab-char7
              planttab-char8
              planttab-char9
              planttab-meins
              planttab-qoh
              planttab-qoha001
              planttab-verpr
              planttab-lgpbe
              planttab-bismt
                    into csvtab separated by ','.
  append csvtab.

endform.                    "create_csv_file

*&---------------------------------------------------------------------*
*&      Form  move_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ATWRT      text
*      -->ATFLV      text
*      -->CHARX      text
*----------------------------------------------------------------------*
form move_char changing atwrt atflv charx.

  data: charp type p.
  data: delim(1) type c value '^'.

  if atwrt <> ' '.
    translate atwrt using '" '.
    translate atwrt using ', '.
    if charx = ' '.
      move atwrt to charx.
    else.
      concatenate charx delim atwrt into charx.
    endif.

  else.
    move tablechar-atflv to charp.
    move charp to charx.
  endif.

endform.                    "move_CHAR

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
         FULL_NAME           = P_XLFILE
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
