*---------------------------------------------------------------UGL
* CHANGES                                                       UGL
* 2007/02/20 TR333 mdemeest                                     UGL
*   Change RM07ICN1 to ZRM07ICN1 in recursive calls (7 calls)   UGL
*---------------------------------------------------------------UGL
* INCLUDE RM07IBDC.

* Sept. 2003 MM                                             "n654402
* check the physical inventory indicators of the provious   "n654402
* year when creating physical inventory documents with      "n654402
* planned counting date in the previous year                "n654402

* error fixed reported by the extended progam check         "n514677

* 28.01.2000 define new working table t_tab_save            "XJD
* 19.01.2000 dynamic perform "AT SELECTION-SCREEN OUTPUT"   "XJD
* 22.12.1999 dynamic perform "AT SELECTION-SCREEN"          "XJD
* 24.03.1999 : new function AUTHORITY-CHECK                 "XJD

*------ allgemeine Tabellendefinitionen
tables: t001,
        t001k,
        t001w,
        v134w,
* &jhm01 begin: korrektur cycle counting
        t134,
* &jhm01 end
* &002 begin: Performanceverbesserung
        t134m,
* &002 end
        t159b,
        t159c,
        t320,
        v_mmim_bew,
        marv.                                             "note 372744

*------ Ranges für Buchhaltungsprüfung
ranges: r_bwkey for v_mmim_bew-bwkey.
* &001 begin: Anpassung wertartikelinventur
ranges: rw_bwkey for mbew-bwkey.
ranges: rw_mtart for t134m-mtart.

*       Ja-Nein Constante
constants: c_ja    value 'X',
           c_nein  value ' '.

constants: c_sammat(2)   value '01'.                        " 94970
* &001 end

* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
* Festwerte der Domäne KEORD
constants: c_leer(2)  type c value '  ',
           c_matkl(2) type c value '01',
           c_lgpbe(2) type c value '02'.
* &jk01 end
*#jko2 begin
* action-codes form error_list_handle
constants: c_set_message_head type c value '1'.
constants: c_set_message_item type c value '2'.
constants: c_init             type c value '3'.
constants: c_show_list        type c value '4'.
data: g_s_eikpf_dummy type eikpf.
data: g_s_eiseg_dummy type eiseg.
data: g_number_of_docs type i.
*#jko2 end

data: tablines type i.

* dispatch inventory diffences
data: dispatch type rm07i-dispatch.

* Steuerung, ob ueber Batch Input oder Funktionsbaustein gebucht werden
* soll; entsprechend aendert sich das Selektionsbild
* Bisher nur MI31.
constants: c_use_function_modul type c value ' '.   "Batch Input
*constants: c_use_function_modul type c value 'X'.    "Funktionsbaustein

*------ allgemeine Strukturdefinitionen
data: begin of tab occurs 0,
        matnr like mard-matnr,
        werks like mard-werks,
        lgort like mard-lgort,
        lifnr like mkop-lifnr,
        kunnr like msku-kunnr,
        vbeln like mska-vbeln,
        posnr like mska-posnr,
        pspnr like mspr-pspnr,
        charg like mchb-charg,
        bwtar like mcha-bwtar,
        bstar type c,
        lgpbe like mard-lgpbe,
        matkl like mara-matkl,
        abcin like marc-abcin,
        gidat like am07m-gidav,
        gpdat like am07m-gidav,
        dlinl like mard-dlinl,
        cycle type c,
* &jhm01 begin: Reparatur Cyclecounting
        cflag type c,
* &jhm01 end
* &001 begin: Anpassungen Wertartikelinventur
        wlinl like mbew-wlinl,
        abciw like mbew-abciw,
* &001 end
* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
        ordng like ikpf-ordng,
* &jk01 end
      end of tab.

* this working table contains all entries of table tab for
* interactive functions in the reports RM07I*31
data : t_tab_save            like tab occurs 0 with header line.

data: begin of pre_tab occurs 0,
        matnr like mara-matnr,
        werks like marc-werks,
        lgort like mard-lgort,
        lifnr like mkop-lifnr,
        kunnr like msku-kunnr,
        vbeln like mska-vbeln,
        posnr like mska-posnr,
        pspnr like mspr-pspnr,
        charg like mchb-charg,
        bwtar like mcha-bwtar,
        lvorm like mara-lvorm,
        mtart like mara-mtart,
        abcin like marc-abcin,
        lgpbe like mard-lgpbe,
        matkl like mara-matkl,
        lfgja like marv-lfgja,
        kzill like mard-kzill,
        kzilq like mard-kzilq,
        kzils like mard-kzils,
        kzvll like mard-kzvll,
        kzvlq like mard-kzvlq,
        kzvls like mard-kzvls,
        dlinl like mard-dlinl,
        ersda like mchb-ersda,
        idatu like mchb-ersda,
        labst like mard-labst,
        insme like mard-insme,
        speme like mard-speme,
* &001 begin: Anpassungen Wertartikelinventur
        vklab like mard-vklab,
* &001 end
* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
        ordng like ikpf-ordng,
* &jk01 end
        mdjin like mard-mdjin,                           "note 372744
      end of pre_tab.

data: begin of tab_error occurs 0,
        matnr like mard-matnr,
        werks like mard-werks,
        lgort like mard-lgort,
        charg like mchb-charg,
        mtart like mara-mtart,
        bwtar like mbew-bwtar,
        ernum type c,
      end of tab_error.

data: begin of tab_key,
        matnr like mard-matnr,
        werks like mard-werks,
      end of tab_key.

data: begin of fehltab occurs 0,
        werks like mard-werks,
        gidat like sy-datum,
        lfgja like marv-lfgja,
      end of fehltab.

data: begin of bdcdata occurs 0.
        include structure bdcdata.
data: end of bdcdata.

data: begin of xwebu occurs 10,
        werks like marc-werks,
        bwkey like mbew-bwkey,
* &001 begin: Anpassungen Wertartikelinventur
        xvkbw like t001k-xvkbw,
* &001 end
        bukrs like t001-bukrs,
        periv like t001-periv,
        fabkl like t001w-fabkl,
        fdyea like bkpf-budat,
        ldyea like scal-facdate,
        actud like scal-facdate,
        frstd like scal-facdate,
        lastd like scal-facdate.
data: end of xwebu.

data: begin of xv_bew occurs 0.
        include structure v_mmim_bew.
data: end of xv_bew.

data: begin of xv_bew_key.
*       INCLUDE STRUCTURE V_MMIM_BEW.
data: mandt like mbew-mandt,
      matnr like mbew-matnr,
      bwkey like mbew-bwkey,
      bwtar like mbew-bwtar.
data: end of xv_bew_key.

data: begin of aktiv,
        xispe type c,
        xiakt type c,
        xiinv type c,
      end of aktiv.

data: begin of xv134w occurs 10.
        include structure v134w.
data: end of xv134w.

* &002 begin: Performanceverbesserungen
data: begin of xt134m occurs 10.
        include structure t134m.
data: end of xt134m.

* &002 end

data: begin of v134w_key,
        mandt like v134w-mandt,
        werks like v134w-werks,
        mtart like v134w-mtart.
data: end of v134w_key.

data: begin of xt159c occurs 10.
        include structure t159c.
data: end of xt159c.

data: begin of t159c_key,
        mandt like t159c-mandt,
        werks like t159c-werks,
        abcin like t159c-abcin.
data: end of t159c_key.

data: begin of xt320 occurs 20.
        include structure t320.
data: end of xt320.

data: begin of t320_key,
        mandt like t320-mandt,
        werks like t320-werks,
        lgort like t320-lgort.
data: end of t320_key.

data: begin of xmbew occurs 0,
        matnr like mbew-matnr,
        werks like marc-werks,
        bwkey like mbew-bwkey,
        bwtar like mbew-bwtar.
data: end of xmbew.

DATA: BEGIN OF TXRUEJ OCCURS 10,                "note193359
        WERKS LIKE MARCV-WERKS,                 "note193359
        BUKRS LIKE T001-BUKRS,                  "note193359
        XRUEJ,                                  "note193359
        XZUKU,                                  "note193359
      END OF TXRUEJ.                            "note193359
                                                "note193359
DATA: BEGIN OF TXKEY,                           "note193359
        WERKS LIKE MARCV-WERKS,                 "note193359
        BUKRS LIKE T001-BUKRS,                  "note193359
      END OF TXKEY.                             "note193359

data: begin of count,
        pendg type i,
        overd type i,
        outof type i,
        inint type i,
        total type i,
        fjahr type i,
        error type i,
      end of count.

*---- allgemeine DATA-Definitionen
data: head_typ            type c.
data: rc                  like sy-subrc.
* &001 begin: Anpassungen Wertartikelinventur
data: rc2                 like sy-subrc.
* &001 end
data: rc3                 like sy-subrc.
data: l_no_data_found     type c.
data: xbuch               type c.
data: sobkz               like mseg-sobkz.
data: c_matnr             type i.
data: c_bwkey             type i.
data: loc_index           like sy-tabix.
data: old_gidat           like am07m-gidav.
data: old_werks           like mard-werks.
data: old_lgort           like mard-lgort.
data: old_lgpbe           like mard-lgpbe.
data: old_matkl           like mara-matkl.
* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
data: old_ordng           like ikpf-ordng.
* &jk01 end
* &PPF begin: Änderung wg. Belegwechsel bei Gruppenwechsel Sortierung
data: old_lifnr           like iseg-lifnr.
data: old_vbeln           like mska-vbeln.
data: old_kunnr           like iseg-kunnr.
data: old_pspnr           like mspr-pspnr.
* &PPF end
data: datum               like sy-datum.
data: monat               like marv-lfmon.
DATA: GJAHR               LIKE MARV-LFGJA.         "note193359
data: xruej               type c.
data: xraus               type c.
data: anzahl              type i.
data: ctext(80)           type c.
data: lsize               type i value 79.
data: xcycl               type c.
data: matnr_conv(18)      type c.
data: pspnr_conv(24)      type c.
data: inventref           like ikpf-xblni.
data: kein_buch           type c.
data: error_grund(26)     type c.

*begin of note 210283
DATA: BEGIN OF WERKS_LFGJA OCCURS 0,
      WERKS LIKE MARD-WERKS,
      BUKRS LIKE MARV-BUKRS,
      LFGJA LIKE MARV-LFGJA,
      END OF WERKS_LFGJA.
*end of note 210283
* counter fields for  AUTHORITY-CHECK    24.03.1999         "XJD
data : z_lines_after         type p,                        "XJD
       z_lines_before        type p.                        "XJD
                                                            "XJD
* field for dynamic perform            22.12.1999           "XJD
DATA : dyn_form              like      sy-repid,            "XJD
       dyn_repid             like      sy-repid.            "XJD

* for dynamic perform "AT SELECTION-SCREEN OUTPUT" 19.01.2000  "XJD
DATA : dyn_form_pbo          like      sy-repid,               "XJD
       dyn_repid_pbo         like      sy-repid.               "XJD

*------- INCLUDES
include: mm07mabc,
         rm07data,
         rm07musr,
         rm07mend,
         rm07mbdc,
         rm07grid.

*---------------------------------------------------------------------*
*       INITIALIZATION                                                *
*---------------------------------------------------------------------*
initialization.
  set titlebar '100'.
  select single * from t159b where repid = syst-repid.
  if sy-subrc is initial.
    if xcycl is initial.
      gidat = t159b-gidat.
      xlabst = t159b-xlabst.
      ximat  = t159b-ximat.
      xicha  = t159b-xicha.
      xinsme = t159b-xinsme.
      xspeme = t159b-xspeme.
    else.
      xlabst = x.
      ximat  = space.
      xicha  = space.
      xinsme = space.
    endif.
    sperr  = t159b-sperr.
*   #jko
    batin  = t159b-batin.
    if sy-tcode = 'MI31'.
      if c_use_function_modul is initial.
        batin  = t159b-batin.
*       clear call_fb.
      else.
*       call_fb = t159b-batin.
        clear batin.
      endif.
    endif.
    mappe  = t159b-mappe.
    xprot  = t159b-xprot.
    maxpo  = t159b-maxpo.
  else.
    xlabst = x.
    xprot  = x.
  endif.
* begin of note 494237
  if sy-repid = 'RM07SVOR' or sy-repid = 'RM07ICN1'.
  if maxpo is initial.
    maxpo = 20.
  elseif maxpo > 333.
    maxpo = 333.
    message i717(m7).
  endif.
  else.
    if maxpo is initial.
       maxpo = 20.
    elseif maxpo > 300.
       maxpo = 300.
       message i717(m7).
    endif.
  endif.
* end of note 494237
  if mappe is initial.
    mappe = sy-repid.
  endif.
  if gidat is initial.
* &001 begin: Anpassung auf Zeitzonen
*   GIDAT = SY-DATUM.
    gidat = sy-datlo.
* &001 end
  endif.

*---------------------------------------------------------------------*
*       SELECTION-SCREEN                                              *
*---------------------------------------------------------------------*
at selection-screen output.

  loop at screen.
    check screen-group1 = '001'.
    screen-input = 0.
    modify screen.
  endloop.
  loop at screen.
    if screen-group1 = 'BI' and not c_use_function_modul is initial.
      screen-invisible = '1'.
      screen-input = '0'.
      modify screen.
    endif.
    if screen-group1 = 'FB' and c_use_function_modul is initial.
      screen-invisible = '1'.
      screen-input = '0'.
      modify screen.
    endif.
  endloop.

* &004: end

* more functions possible with dynamic perform 19.01.2000   "XJD
  if  dyn_form_pbo   is  initial  or                        "XJD
      dyn_repid_pbo  is  initial.                           "XJD
  else.                                                     "XJD
*   names of form and report must be written in upper case  "XJD
*   error fixed reported by the extended progam check       "n514677
    translate : dyn_form_pbo  to upper case, "#EC TRANSLANG "n514677
                dyn_repid_pbo to upper case. "#EC TRANSLANG "n514677
                                                            "XJD
    PERFORM (DYN_FORM_PBO) IN PROGRAM (DYN_REPID_PBO)       "XJD
                                       IF FOUND.            "XJD
  endif.                                                    "XJD

*---------------------------------------------------------------------*
*       SELECTION-SCREEN                                              *
*---------------------------------------------------------------------*
at selection-screen.

* &001 begin: Anpassungen Wertartikelinventur
* IF XLABST IS INITIAL AND XINSME IS INITIAL AND XSPEME IS INITIAL.
  if xlabst is initial and xinsme is initial and xspeme is initial
    and xwart is initial.
* &001 end
    message e831.
  endif.
  if mappe is initial.
    message e830.
  endif.
  if not xlgpbe is initial and not xmatkl is initial.
    message e833.
  endif.
  if not swbst is initial and not xnulb is initial.
    message e783.
  endif.
* &003 begin: neue Selektionskriterien
  if not swbst is initial and not xonul is initial.
    message e783.
  endif.
  if not swbstw is initial and not xonul is initial.
    message e783.
  endif.
  if not xnulb is initial and not xonul is initial.
    message e828.
  endif.
  if not swbst is initial and not xnegb is initial.
    message e802.
  endif.
  if not swbstw is initial and not xnegb is initial.
    message e802.
  endif.
  if swbst is initial and not xumke is initial.
    message e788.
  endif.
  if swbstw is initial and not xumkew is initial.
    message e788.
  endif.
* &003 end
* &001 begin: Anpassung auf Zeitzonen
* IF GIDAT < SY-DATUM AND NOT GIDAT IS INITIAL.
  if gidat < sy-datlo and not gidat is initial.
* &001 end
    if xcycl is initial.
*     perform nachrichtencode_ermitteln(sapfm07m) using 'M7' '834'.
      call function 'MB_CHECK_T160M'
           exporting
                i_msgnr = '834'
           importing
                rc      = rc.
*     case sy-subrc.
      case rc.
        when 2.
          message e834.
        when 1.
          message w834.
      endcase.
    else.
      message e834.
* &001 begin: Anpassung auf Zeitzonen
*     GIDAT = SY-DATUM.
      gidat = sy-datlo.
* &001 end
    endif.
  endif.
* begin of note 494237
  if sy-repid = 'RM07SVOR' or sy-repid = 'RM07ICN1'.
  if maxpo is initial.
    maxpo  = 20.
  elseif maxpo > 333.
    maxpo = 333.
    message i717(m7).
  endif.
  else.
    if maxpo is initial.
       maxpo = 20.
    elseif maxpo > 300.
       maxpo = 300.
       message i717(m7).
    endif.
  endif.
* end of note 494237

* more checks possible with dynamic perform 22.12.1999      "XJD
  if  dyn_form   is  initial  or                            "XJD
      dyn_repid  is  initial.                               "XJD
  else.                                                     "XJD
*   names of form and report must be written in upper case  "XJD
    translate : dyn_form     to  upper case, "#EC TRANSLANG "XJD
                dyn_repid    to  upper case. "#EC TRANSLANG "XJD
                                                            "XJD
    PERFORM (DYN_FORM) IN PROGRAM (DYN_REPID) IF FOUND.     "XJD
  endif.                                                    "XJD

*---------------------------------------------------------------------*
*       TOP-OF-PAGE                                                   *
*---------------------------------------------------------------------*
top-of-page.
  case head_typ.
    when a.
      perform set_format using 0 space space.
      ctext = text-a01.
      write count-inint to ctext+8(10).
      condense ctext.
      write:/ ctext.
      perform open_grid using lsize 1 x.
      write:2 text-l01.
      perform sep_grid using 2 space.
    when b.
      perform set_format using 0 space space.
      write:/ text-b01, / text-b02, / text-b03, / text-b04.
      perform open_grid using lsize 1 x.
      write:2 text-015 no-gap.
      perform sep_grid using 2 space.
    when c.
      perform set_format using 0 space space.
      ctext = text-c01.
      write count-error to ctext+8(10).
      condense ctext.
      write:/ ctext.
      perform open_grid using lsize 1 x.
      write:2 text-f01.
      perform sep_grid using 2 space.
    when d.
      perform set_format using 0 space space.
      ctext = text-d01.
      write count-pendg to ctext+8(10).
      condense ctext.
      write:/ ctext.
      perform open_grid using lsize 1 x.
      write:2 text-l02.
      perform sep_grid using 2 space.
  endcase.

*---------------------------------------------------------------------*
*       FORM PRUEFUNG_VORBEREITEN                                     *
*---------------------------------------------------------------------*
form pruefung_vorbereiten.

* new function AUTHORITY-CHECK :                            "XJD
* save number of plants before AUTHORITY-CHECK              "XJD
  describe  table r_werks    lines  z_lines_before.         "XJD
                                                            "XJD
  loop at r_werks.                                          "XJD
*   check authority for each plant                          "XJD
    AUTHORITY-CHECK OBJECT 'M_ISEG_WIB'                     "XJD
                 ID 'ACTVT' FIELD '01'                      "XJD
                 ID 'WERKS' FIELD R_WERKS-low.              "XJD
                                                            "XJD
    IF NOT SY-SUBRC IS INITIAL.                             "XJD
*     no authority ? yes --> delete plant in table R_WERKS  "XJD
      delete                 r_werks.                       "XJD
    endif.                                                  "XJD
  endloop.                                                  "XJD
                                                            "XJD
* save number of remaining plants after AUTHORITY-CHECK     "XJD
  describe  table r_werks    lines  z_lines_after.          "XJD
                                                            "XJD
  case    z_lines_after.     "results ?                     "XJD
    when  z_lines_before.                                   "XJD
*     user has authority for all plants in R_WERKS          "XJD
    when  0.                                                "XJD
*     user has no authority for the plants in R_WERKS       "XJD
      message s124.   "Wegen fehlender Berechtigung ist ... "XJD
*     leave report to selection screen                      "XJD
      perform                 Anforderungsbild.             "XJD
    when  others.                                           "XJD
*     user has authority for only a part of the plants      "XJD
      message s124.   "Wegen fehlender Berechtigung ist ... "XJD
  endcase.                                                  "XJD
                                                            "XJD
* the following SELECT's improved, because table R_WERKS    "XJD
* could contain more than 256 entries                       "XJD
                                                            "XJD
* Steuertabelle T320 für LVS-Lagerortprüfung einlesen       "XJD
* select * from t320 appending table xt320                  "XJD
*                              where werks in r_werks       "XJD
*                                and obest = space.         "XJD
  select * from t320 appending table xt320                  "XJD
                            FOR ALL ENTRIES IN  R_WERKS     "XJD
                            where  werks = r_werks-low      "XJD
                              and  obest = space.           "XJD
  sort xt320 by mandt werks lgort.

* Steuertabelle T159C für Cycle-Counting einlesen           "XJD
* select * from t159c appending table xt159c                "XJD
*                               where werks in r_werks.     "XJD
* table R_WERKS could contain more than 256 entries         "XJD
  select * from t159c appending table xt159c                "XJD
                             FOR ALL ENTRIES IN  R_WERKS    "XJD
                             where  werks = r_werks-low.    "XJD
  sort xt159c by mandt werks abcin.

* Range und XWEBU für den Bewertugskreis, Buchungskreis füllen
  r_bwkey-sign   = 'I'.
  r_bwkey-option = 'EQ'.
  loop at r_werks.
    on change of r_werks-low.
      select single * from t001w where werks = r_werks-low.
      if sy-subrc is initial.
        r_bwkey-low = t001w-bwkey.
        xwebu-werks = r_werks-low.
        xwebu-bwkey = t001w-bwkey.
        xwebu-fabkl = t001w-fabkl.
        collect r_bwkey.
        collect xwebu.
      else.
* pfaffp: begin: Kein Abbruch bei gelöschtem Werk
        continue.
*       MESSAGE E001 WITH 'T001W' R_WERKS-LOW.
* end
      endif.
    endon.
  endloop.
  describe table r_bwkey lines c_bwkey.

* Steuertabelle T134M einlesen
  select * from t134m appending table xt134m
                                where bwkey in r_bwkey.
  loop at xwebu.
    loop at xt134m where bwkey = xwebu-bwkey.
      move xwebu-werks to xv134w-werks.
      move-corresponding xt134m to xv134w.
      collect xv134w.
    endloop.
  endloop.
  sort xv134w by mandt werks mtart.

  loop at xwebu.

*-- Bewertungskreis lesen und prüfen
    select single * from t001k where bwkey = xwebu-bwkey.
    if sy-subrc is initial.
      xwebu-bukrs = t001k-bukrs.
      xwebu-xvkbw = t001k-xvkbw.
      modify xwebu.
    else.
      message e001 with 'T001K' xwebu-bwkey.      "#EC *    "n391015
    endif.

*-- Buchungskreis prüfen - Gschäftsjahresvariante setzten
    select single * from t001 where bukrs = xwebu-bukrs.
    if sy-subrc is initial.
      xwebu-periv = t001-periv.
      modify xwebu.
    else.
      message e001 with 'T001' xwebu-bukrs.       "#EC *    "n391015
    endif.
  endloop.
  sort xwebu by werks.

* Range zurücksetzten
  clear   r_matnr.
  refresh r_matnr.
  r_matnr-sign   = 'I'.
  r_matnr-option = 'EQ'.
endform.

*---------------------------------------------------------------------*
*       FORM BSE_PRUEFEN                                              *
*---------------------------------------------------------------------*
form bse_pruefen.

  statics: l_werks_old like t001w-werks.
  data: l_kzrfb like mtcom-kzrfb.
  data: l_maxtz like mtcom-maxtz value '2000'.

  data: bf_matnr like mara-matnr,
        xwaart   type c.

  clear: fehltab,
         txruej,                                          "note193359
         xruej,
         xraus.

* does the user has authority for this plant ?              "XJD
  check : pre_tab-werks in r_werks.                         "XJD
                                                            "XJD
* Prüfen ob Löschvormerkung sitzt, wenn gefordert
  if xdele is initial.
    if not pre_tab-lvorm is initial.
      move-corresponding pre_tab to tab_error.
      tab_error-ernum = '4'.
      append tab_error.
      xraus = x.
    endif.
  endif.
  check xraus is initial.

* Prüfen, ob der Lagerort nicht LVS-relevant ist
  move sy-mandt      to t320_key-mandt.
  move pre_tab-werks to t320_key-werks.
  move pre_tab-lgort to t320_key-lgort.
  read table xt320 with key t320_key binary search.
  if sy-subrc is initial.
    move-corresponding pre_tab to tab_error.
    tab_error-ernum = '2'.
    append tab_error.
    xraus = x.
  endif.
  check xraus is initial.

* Prüfen der Buchhaltung: Ist Mengen und Wertfortschreigung gepflegt ?
  move sy-mandt      to v134w_key-mandt.
  move pre_tab-werks to v134w_key-werks.
  move pre_tab-mtart to v134w_key-mtart.
  read table xv134w with key v134w_key binary search.
  if sy-subrc is initial.

* take "special stocks sales order" always;                note 212834
* independent of the indicator for quantity update in      note 212834
* table T134M                                              note 212834
  if  sobkz         =  E         and                      "note 212834
      xv134w-mengu  is initial.                           "note 212834
    move  X                  to  xv134w-mengu.            "note 212834
  endif.                                                  "note 212834

*-- Keine Mengenfortschreibung, d.h. keine Inventur möglich
    if xv134w-mengu is initial and
       pre_tab-vbeln is initial
* &001 begin: Anpassung Wertartikelinventur
       and xv134w-wertu is initial.
* &001 end
      move-corresponding pre_tab to tab_error.
      tab_error-ernum = '5'.
      append tab_error.
      xraus = x.
* &001 begin: Anpassung Wertartikelinventur
    elseif xv134w-mengu is initial and
       not xv134w-wertu is initial.
* Wertartikel werden nur aus Selektionsmenge rausgenommen, aber nicht
* in die Fehlertabelle geschrieben.
      if xwart is initial.
        xraus = x.
      endif.
    elseif not xv134w-mengu is initial and
           not xv134w-wertu is initial.
* Artikel werden nur aus Selektionsmenge rausgenommen, aber nicht
* in die Fehlertabelle geschrieben.
      if xlabst is initial and
         xinsme is initial and
         xspeme is initial.
        xraus = x.
      endif.
      if kein_buch is initial.
        xbuch = x.
      endif.
* &001 end
*-- Wenn Wertfortschreibung gesetzt ist, muß Buchhaltung geprüft werden
    elseif not xv134w-wertu is initial.
      if kein_buch is initial.
        xbuch = x.
      endif.
    endif.
  else.

*-- Fehlender Eintrag in Tabelle V134W
    move-corresponding pre_tab to tab_error.
    tab_error-ernum = '3'.
    append tab_error.
    xraus = x.
  endif.

* finale Prüfung, ob Material drin bleibt
  check xraus is initial.

* neuen Buchungskreis lesen
  on change of pre_tab-werks.
    read table xwebu with key pre_tab-werks binary search.
  endon.

* &001 begin: Anpassungen Wertartikelinventur
* Bei Werken, in denen die Verkaufspreisbewertung aktiv ist
* wird geprüft, ob das Material auf sich selbst bestandsgeführt
* wird. Wenn nicht, fliegt es raus.
  if not xwebu-xvkbw is initial.
*   bei neuem Werk den internen Puffer in VALUE_ARTICLE_FIND loeschen
*   l_maxtz steht auf 2000
    if l_werks_old ne pre_tab-werks.
      l_kzrfb = x.
      l_werks_old = pre_tab-werks.
    else.
      clear l_kzrfb.
    endif.
    call function 'VALUE_ARTICLE_FIND'
         exporting
              i_matnr                = pre_tab-matnr
              i_werks                = pre_tab-werks
              i_kzrfb                = l_kzrfb
              i_maxtz                = l_maxtz
         importing
              e_matnr                = bf_matnr
              e_waart                = xwaart
         exceptions
              entry_not_found        = 01
              wertm_bf_nicht_erlaubt = 02
              buchung_nicht_erlaubt  = 03
              material_not_found     = 04.

    if sy-subrc is initial.
      if pre_tab-matnr ne bf_matnr.
*        MOVE-CORRESPONDING PRE_TAB TO TAB_ERROR.
*        TAB_ERROR-ERNUM = '6'.
*        APPEND TAB_ERROR.
        xraus = x.
      endif.
    else.
      move-corresponding pre_tab to tab_error.
      tab_error-ernum = '7'.
      append tab_error.
      xraus = x.
    endif.
  endif.

  check xraus is initial.
* &001 end

* Sätze für spätere Buchhaltungsüberprüfung wegschreiben
  if not xbuch is initial.
    r_matnr-low = pre_tab-matnr.
    collect r_matnr.
    describe table r_matnr lines c_matnr.
    c_matnr = c_matnr + c_bwkey.
    if c_matnr > 253.
      perform buchhaltung_lesen.
    endif.
    xmbew-matnr = pre_tab-matnr.
    xmbew-werks = xwebu-werks.
    xmbew-bwkey = xwebu-bwkey.
    xmbew-bwtar = pre_tab-bwtar.
    collect xmbew.
    clear xbuch.
  endif.

* Begin of note 193359
    TXKEY-WERKS = XWEBU-WERKS.
    TXKEY-BUKRS = XWEBU-BUKRS.
    READ TABLE TXRUEJ WITH KEY TXKEY BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.

*-- Geschäftsjahr ermitteln
      CALL FUNCTION 'FI_PERIOD_DETERMINE'
           EXPORTING
               I_BUDAT = GIDAT
                I_BUKRS = XWEBU-BUKRS
           IMPORTING
                E_GJAHR = GJAHR
                E_MONAT = MONAT
           EXCEPTIONS                                       "n391015
            OTHERS         = 1.                             "n391015
                                                            "n391015
      IF SY-SUBRC <> 0.                                     "n391015
        MESSAGE ID     SY-MSGID                             "n391015
                TYPE   SY-MSGTY                             "n391015
                NUMBER SY-MSGNO                             "n391015
                WITH   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "n391015
      ENDIF.                                                "n391015

      IF GJAHR < PRE_TAB-LFGJA.
        TXRUEJ-XRUEJ = X.
      ENDIF.
      IF GJAHR > PRE_TAB-LFGJA.
        TXRUEJ-XZUKU = X.
     ENDIF.
      TXRUEJ-WERKS = XWEBU-WERKS.
      TXRUEJ-BUKRS = XWEBU-BUKRS.
      APPEND TXRUEJ.
      SORT TXRUEJ BY WERKS BUKRS.
      IF TXRUEJ-XZUKU = X.
        FEHLTAB-WERKS = PRE_TAB-WERKS.
        FEHLTAB-GIDAT = GIDAT.
        FEHLTAB-LFGJA = PRE_TAB-LFGJA.
        APPEND FEHLTAB.
      ENDIF.
    ENDIF.
   IF NOT TXRUEJ-XZUKU IS INITIAL.
      XRAUS = X.
    ENDIF.
    XRUEJ = TXRUEJ-XRUEJ.
* End of note193359
* changing the PI-indicators according to MDJIN
      if not pre_tab-mdjin is initial.
        if pre_tab-mdjin eq marv-lfgja.
        elseif pre_tab-mdjin eq marv-vjgja.
           pre_tab-kzvll = pre_tab-kzill.
           pre_tab-kzvlq = pre_tab-kzilq.
           pre_tab-kzvls = pre_tab-kzils.
           clear: pre_tab-kzill,
                  pre_tab-kzilq,
                  pre_tab-kzils.
        else.
           clear: pre_tab-kzill,
                  pre_tab-kzilq,
                  pre_tab-kzils,
                  pre_tab-kzvll,
                  pre_tab-kzvlq,
                  pre_tab-kzvls.
        endif.
      else.
        if marv-gja_46c eq marv-lfgja.
        else.
          if marv-gja_46c eq marv-vjgja.
           pre_tab-kzvll = pre_tab-kzill.
           pre_tab-kzvlq = pre_tab-kzilq.
           pre_tab-kzvls = pre_tab-kzils.
           clear: pre_tab-kzill,
                  pre_tab-kzilq,
                  pre_tab-kzils.
          else.
           clear: pre_tab-kzill,
                  pre_tab-kzilq,
                  pre_tab-kzils,
                  pre_tab-kzvll,
                  pre_tab-kzvlq,
                  pre_tab-kzvls.
          endif.
        endif.
      endif.
* end of note 353075
  check xraus is initial.

* Frei verwendbar
  if not xlabst is initial.
* &001 begin: Anpassung Wertartikelinventur
    if not xv134w-mengu is initial or
       xv134w-wertu is initial.
* &001 end
      perform bestand_pruefen using pre_tab-labst.
      if xraus is initial.
* Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
        if xcycl is initial.
          perform inventurzustand_pruefen using pre_tab-kzill
                                                pre_tab-kzvll
                                                eins.
        else.
*---------------------------------------------------------begin of UGL
*          perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzill
*                                                         pre_tab-kzvll
*                                                         pre_tab-dlinl
*                                                         eins.
         perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzill
                                                         pre_tab-kzvll
                                                         pre_tab-dlinl
                                                         eins.
*-----------------------------------------------------------end of UGL

        endif.
      else.
        clear xraus.
      endif.
* &001 begin: Anpassung Wertartikelinventur
    endif.
* &001 end
  endif.

* In Qualitätsprüfung
  if not xinsme is initial.
* &001 begin: Anpassung Wertartikelinventur
    if not xv134w-mengu is initial or
       xv134w-wertu is initial.
* &001 end
      perform bestand_pruefen using pre_tab-insme.
      if xraus is initial.

* &004 begin: Anpassung Bestandsarten Q und S
        if xcycl is initial.
* &004 end

*     Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
          perform inventurzustand_pruefen using pre_tab-kzilq
                                                pre_tab-kzvlq
                                                zwei.
* &004 begin: Anpassung Bestandsarten Q und S
        else.
*---------------------------------------------------------begin of UGL
*         perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzilq
*                                              pre_tab-kzvlq
*                                               pre_tab-dlinl
*                                               zwei.
          perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzilq
                                               pre_tab-kzvlq
                                               pre_tab-dlinl
                                               zwei.
*------------------------------------------------------------end of UGL
        endif.
* &004 end

      else.
        clear xraus.
      endif.
* &001 begin: Anpassung Wertartikelinventur
    endif.
* &001 end
  endif.

* Gesperrt
  if not xspeme is initial.
* &001 begin: Anpassung Wertartikelinventur
    if not xv134w-mengu is initial or
       xv134w-wertu is initial.
* &001 end
      perform bestand_pruefen using pre_tab-speme.
      if xraus is initial.
* &004 begin: Anpassungen Bestandsarten Q und S
        if xcycl is initial.
* &004 end

*     Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
          perform inventurzustand_pruefen using pre_tab-kzils
                                                pre_tab-kzvls
                                                vier.
* &004 begin: Anpassungen Bestadnsarten Q und S
        else.
*-----------------------------------------------------------begin of UGL
          perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzils
                                               pre_tab-kzvls
                                               pre_tab-dlinl
                                               vier.
          perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzils
                                               pre_tab-kzvls
                                               pre_tab-dlinl
                                               vier.
*-------------------------------------------------------------end of UGL

        endif.
* &004 end
      else.
        clear xraus.
      endif.
* &001 begin: Anpassung Wertartikelinventur
    endif.
* &001 end
  endif.

* &001 begin: Anpassung Wertartikelinventur
* Wertartikel
  if not xwart  is initial.
    if xv134w-mengu is initial and
       not xv134w-wertu is initial.

      perform bestand_pruefen_wart using pre_tab-vklab.
      if xraus is initial.
* Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
        if xcycl is initial.
          perform inventurzustand_pruefen using pre_tab-kzill
                                                space
                                                space.
        else.
*--------------------------------------------------------begin of UGL
*        perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzill
*                                                       space
*                                                       pre_tab-dlinl
*                                                       space.
         perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzill
                                                         space
                                                         pre_tab-dlinl
                                                         space.
*----------------------------------------------------------end of UGL
        endif.

      else.
        clear xraus.
      endif.
    endif.
  endif.
endform.


*---------------------------------------------------------------------*
*     FORM INVENTURZUSTAND_PRUEFEN                                    *
*---------------------------------------------------------------------*
*       Die Inventurflags werden entsprechend der getroffenen Selek-  *
*       tion geprüft. Auf keinen Fall dürfen Sonderbestände mit akt.  *
*       Inventurbelegen in die Mappe aufgenommen werden.              *
*---------------------------------------------------------------------*
form inventurzustand_pruefen using m_kzill
                                   m_kzvll
                                   m_bstar.
* Rückbuchung prüfen
  if xruej is initial.
    aktiv = m_kzill.
  else.
    aktiv = m_kzvll.
  endif.
* &jhm01 Reparatur cycle counting
* Abfrage Kalkulationssicht-Buchhaltungssicht
  perform kalku_pruef using pre_tab-mtart.


* Aktive Inventur prüfen
  check aktiv-xiakt is initial.

* Bereits inventarisierte Materialien/Charge prüfen
  if pre_tab-charg is initial.
    if ximat is initial.
      check aktiv-xiinv is initial.
    endif.
  else.
    if xicha is initial.
      check aktiv-xiinv is initial.
    endif.
  endif.

* Weitere Bestandseinheit für Mappe
  move-corresponding pre_tab to tab.
  tab-bstar = m_bstar.
  tab-gidat = gidat.
  tab-cycle = x.
  count-inint = count-inint + 1.
  append tab.
  clear tab.                                     "note 409950

endform.

*---------------------------------------------------------------------*
*       FORM BUCHHALTUNG_LESEN                                        *
*---------------------------------------------------------------------*
form buchhaltung_lesen.

  select * from v_mmim_bew appending table xv_bew
                               where matnr in r_matnr
                               and   bwkey in r_bwkey.
* begin of note 501666
  if gl_sobkz = 'E'.
     select * from ebew appending corresponding fields of
                                  table xv_bew
                            where matnr in r_matnr
                              and bwkey in r_bwkey.
  elseif gl_sobkz = 'Q'.
     select * from qbew appending corresponding fields of
                                  table xv_bew
                            where matnr in r_matnr
                              and bwkey in r_bwkey.
  endif.
  sort xv_bew by matnr bwkey bwtar.
  delete adjacent duplicates from xv_bew.
* end of note 501666
  clear   c_matnr.
  clear   r_matnr.
  refresh r_matnr.
  r_matnr-sign   = 'I'.
  r_matnr-option = 'EQ'.

endform.

*---------------------------------------------------------------------*
*       FORM PRUEFUNG_DURCHFUEHREN                                    *
*---------------------------------------------------------------------*
form pruefung_durchfuehren.
  if c_matnr > 0.
    perform buchhaltung_lesen.
  endif.
  sort xv_bew by mandt matnr bwkey bwtar.
  sort tab by matnr werks.
  loop at xmbew.
    move sy-mandt    to xv_bew_key-mandt.
    move xmbew-matnr to xv_bew_key-matnr.
    move xmbew-bwkey to xv_bew_key-bwkey.
    move xmbew-bwtar to xv_bew_key-bwtar.
    CLEAR xv_bew-pstat.                                      "note523215
    read table xv_bew with key xv_bew_key binary search.
    if not sy-subrc is initial or
       xv_bew-pstat eq 'G'.
      clear tab_key.
      move-corresponding xmbew to tab_key.
      read table tab with key tab_key binary search.
      if not ( tab-cflag eq x and xv_bew-pstat eq g ).
        loc_index = sy-tabix.
        while tab-matnr eq tab_key-matnr and
             tab-werks eq tab_key-werks and
             sy-subrc is initial.
          if xmbew-bwtar eq tab-bwtar.
            move-corresponding tab to tab_error.
            tab_error-ernum = '1'.
            collect tab_error.
            move x to tab-bstar.
            modify tab index loc_index.
          endif.
          loc_index = loc_index + 1.
          read table tab index loc_index.
        endwhile.
      endif.
    endif.
    clear tab-cflag.
  endloop.
  loop at tab where bstar eq x.
    delete tab.
    count-inint = count-inint - 1.
  endloop.

* Ergebnis der Selektionen
  describe table fehltab lines count-fjahr.
  if not count-fjahr is initial.
    perform fehlerprotokoll_ausgeben.
  endif.
  describe table tab_error lines count-error.
  if not count-error is initial.
    perform fehlende_buchhaltung_ausgeben.
  endif.
  count-inint = count-inint + count-overd.
  count-total = count-inint + count-pendg.
  if count-total is initial.
*   this pf-status is only used in report RM07ICN1          "n391015
    set pf-status '200'.               "#EC *               "n391015
  else.
    perform ausgabe.
  endif.
* bei MI31: keine Meldung ausgeben, sondern flag setzen
  if not flg_mappe is initial.
    if count-total is initial and count-error is
                      initial and count-fjahr is initial.
* P.Pfaff: begin: Message ausgetauscht
*   MESSAGE S083.
      message s198.
*   PERFORM ANFORDERUNGSBILD.
      if sy-batch is initial.                               "note 351338
      leave to transaction sy-tcode.                        "note 351338
      else.                                                 "note 351338
         leave.                                             "note 351338
      endif.                                                "note 351338
* P.Pfaff: end
    endif.
  else.
    if   not count-total is initial
      or not count-error is initial
      or not count-fjahr is initial.
      clear l_no_data_found.
    endif.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM FEHLERPROTOKOLL_AUSGEBEN                                 *
*---------------------------------------------------------------------*
*       Das Fehlerprotokoll wird ausgegeben.                          *
*       Fehler tritt dann auf, wenn das Jahr des geplanten Aufnahme-  *
*       datums größer ist, als das aktuelle Jahr im Materialver-      *
*       waltungssatz.                                              -  *
*---------------------------------------------------------------------*
form fehlerprotokoll_ausgeben.
  head_typ = b.
  set titlebar '100'.
  new-page no-heading no-title line-size lsize.
  loop at fehltab.
    perform next_row using 2 space.
    write:2 fehltab-werks no-gap, sy-vline no-gap,
            fehltab-lfgja no-gap, 20 sy-vline no-gap,
            fehltab-gidat dd/mm/yyyy no-gap.
  endloop.
  perform close_grid.
endform.

*---------------------------------------------------------------------*
*       FORM FEHLENDE_BUCHHALTUNG_AUSGEBEN                            *
*---------------------------------------------------------------------*
form fehlende_buchhaltung_ausgeben.
  head_typ = c.
  set titlebar '100'.
  new-page no-heading no-title line-size lsize.
  loop at tab_error.
    perform next_row using 2 space.
    write:2 tab_error-werks no-gap, sy-vline no-gap,
            tab_error-lgort no-gap, sy-vline no-gap,
            tab_error-matnr no-gap, sy-vline no-gap,
            tab_error-charg no-gap, sy-vline no-gap,
            tab_error-bwtar no-gap, sy-vline no-gap.
    case tab_error-ernum.
      when '1'.
        error_grund = text-ft1.
      when '2'.
        error_grund = text-ft2.
      when '3'.
        error_grund(21) = text-ft3.
        write tab_error-mtart to error_grund+22.
      when '4'.
        error_grund = text-ft4.
      when '5'.
        error_grund = text-ft5.
* &001 begin: Anpassung Wertartikelinventur
      when '6'.
        error_grund = text-ft6.
      when '7'.
        error_grund = text-ft7.

* &001 end
    endcase.
    write: error_grund no-gap color col_negative.
    clear error_grund.
  endloop.
  perform close_grid.
endform.

*---------------------------------------------------------------------*
*   FORM BESTAND_PRUEFEN                                              *
*---------------------------------------------------------------------*
*   Prüfen, ob Bestand kleiner oder gleich dem Schwellenwert oder     *
*   gleich Null ist, wenn gefordert                                   *
*---------------------------------------------------------------------*
form bestand_pruefen using labst.
  if not swbst is initial.
* &003 begin: neues Selektionskriterium
    if xumke is initial.
* &003 end
      if labst gt swbst.
        xraus = x.
      endif.
* &003 begin: neues Selektionskriterium
    else.
      if labst lt swbst.
        xraus = x.
      endif.
    endif.
* &003 end
  else.
* &003 begin: neues Selektionskriterium
    if not xnulb is initial and not xnegb is initial.
      if not labst is initial and not labst lt 0.
        xraus = x.
      endif.
    elseif not xonul is initial and not xnegb is initial.
      if labst is initial and not labst lt 0.
        xraus = x.
      endif.
    elseif not xnegb is initial.
      if not labst lt 0.
        xraus = x.
      endif.
    elseif not xonul is initial.
      if labst is initial.
        xraus = x.
      endif.
    elseif not xnulb is initial.
*  IF NOT XNULB IS INITIAL.
* &003 end
      if not labst is initial.
        xraus = x.
      endif.
    endif.
  endif.
endform.

* &003 begin: neues Selektionskriterium
*---------------------------------------------------------------------*
*   FORM BESTAND_PRUEFEN_WART                                         *
*---------------------------------------------------------------------*
*   Prüfen, ob Bestand kleiner oder gleich dem Schwellenwert oder     *
*   gleich Null ist, wenn gefordert. Formroutine für Verkaufswert      *
*   bei Wertartikel
*---------------------------------------------------------------------*
form bestand_pruefen_wart using vkwrt.
  if not swbstw is initial.
    if xumkew is initial.
      if vkwrt gt swbstw.
        xraus = x.
      endif.
    else.
      if vkwrt lt swbstw.
        xraus = x.
      endif.
    endif.
  else.
    if not xnulb is initial and not xnegb is initial.
      if not vkwrt is initial and not vkwrt lt 0.
        xraus = x.
      endif.
    elseif not xonul is initial and not xnegb is initial.
      if vkwrt is initial and not vkwrt lt 0.
        xraus = x.
      endif.
    elseif not xnegb is initial.
      if not vkwrt lt 0.
        xraus = x.
      endif.
    elseif not xonul is initial.
      if vkwrt is initial.
        xraus = x.
      endif.
    elseif not xnulb is initial.
      if not vkwrt is initial.
        xraus = x.
      endif.
    endif.
  endif.
endform.
* &003 end

*       Dynprofolgen in der BTCI-Mappe

*---------------------------------------------------------------------*
*       FORM MAPPE_AUFBAUEN                                           *
*---------------------------------------------------------------------*
*       Aufbauen der Batch-Input-Mappe aus der internen Tabelle       *
*---------------------------------------------------------------------*
form mappe_aufbauen.
  data : mappe_erzeugen(1) type c.     "=X: Es wurde eine Mappe erzeugt
  clear: flg_mappe_offen,
         flg_beleg_offen,
         flg_beleg_voll,
         mappe_erzeugen.

  loop at tab where cycle eq o or cycle eq x.
    mappe_erzeugen = x.

*-- Ist bereits eine Mappe eröffnet?
*   bei MI31: Mappe ist bereits geoeffnet
    if     flg_mappe_offen is initial and
       not flg_mappe is initial.
      perform mappe_oeffnen using mappe.
    endif.

*-- Ist bereits ein Beleg eröffnet?
    if flg_beleg_offen is initial.
      perform beleg_oeffnen.
    else.

*---- Ist der aktuelle Beleg voll?
      if not flg_beleg_voll is initial.
        perform beleg_schliessen.
        perform beleg_oeffnen.

*---- Liegt ein Datumswechsel vor?
*      elseif old_gidat ne tab-gidat.  "UGL
*        perform beleg_schliessen.     "UGL
*        perform beleg_oeffnen.        "UGL

*---- Liegt ein Werkswechsel vor?
      elseif old_werks ne tab-werks.
        perform beleg_schliessen.
        perform beleg_oeffnen.

*---- Liegt ein Lagerortwechsel vor?
      elseif old_lgort ne tab-lgort.
        perform beleg_schliessen.
        perform beleg_oeffnen.

* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
      elseif old_ordng ne tab-ordng.
        perform beleg_schliessen.
        perform beleg_oeffnen.
* &jk01 end

*---- Belegwechsel nach Gruppenwechsel der Sortierung?
      elseif not xgrupp is initial.

*------ Liegt ein Lagerplatzwechsel vor?
        if not xlgpbe is initial and old_lgpbe ne tab-lgpbe.
          perform beleg_schliessen.
          perform beleg_oeffnen.

*------ Liegt ein Warengruppenwechsel vor?
        elseif not xmatkl is initial and old_matkl ne tab-matkl.
          perform beleg_schliessen.
          perform beleg_oeffnen.
* &PPF begin: Änderung wg. Belegwechsel bei Gruppenwechsel Sortierung
        elseif  xmatkl is initial and xlgpbe is initial and sobkz eq k
                and old_lifnr ne tab-lifnr.
          perform beleg_schliessen.
          perform beleg_oeffnen.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq e
               and old_vbeln ne tab-vbeln.
          perform beleg_schliessen.
          perform beleg_oeffnen.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq o
                and old_lifnr ne tab-lifnr.
          perform beleg_schliessen.
          perform beleg_oeffnen.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq v
               and old_kunnr ne tab-kunnr.
          perform beleg_schliessen.
          perform beleg_oeffnen.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq w
               and old_kunnr ne tab-kunnr.
          perform beleg_schliessen.
          perform beleg_oeffnen.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq m
               and old_lifnr ne tab-lifnr.
          perform beleg_schliessen.
          perform beleg_oeffnen.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq q
                and old_pspnr ne tab-pspnr.
          perform beleg_schliessen.
          perform beleg_oeffnen.
* &PPF end
        endif.
      endif.
    endif.
*   Nächste Position bearbeiten
    perform ibpos_erzeugen.
    old_gidat = tab-gidat.
    old_werks = tab-werks.
    old_lgort = tab-lgort.
    old_lgpbe = tab-lgpbe.
    old_matkl = tab-matkl.
* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
    old_ordng = tab-ordng.
* &jk01 end.
* &PPF begin: Änderung wg. Belegwechsel bei Gruppenwechsel Sortierung
    old_lifnr = tab-lifnr.
    old_vbeln = tab-vbeln.
    old_kunnr = tab-kunnr.
    old_pspnr = tab-pspnr.
* &PPF end
  endloop.

*--- Fehlermeldung, wenn keine Mappe erzeugt wurde
  if mappe_erzeugen is initial.
    message e836.
  endif.

  if not flg_beleg_offen is initial.
    perform beleg_schliessen.
  endif.
* Bei MI31: Mappe wird in RM07II31 geschlossen
  if not flg_mappe_offen is initial and
     not flg_mappe is initial.
    perform mappe_schliessen.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM BELEG_OEFFNEN                                            *
*---------------------------------------------------------------------*
*       Eröffnen eines Inventurbeleges. Es ist sicher, dass keiner    *
*       offen ist und mindestens noch eine Position kommt.            *
*---------------------------------------------------------------------*
form beleg_oeffnen.

  refresh bdcdata.
  clear bdcdata.

  perform bdcdynpro using 'SAPMM07I'
                          '0700'.
  write tab-gidat to datum.
  perform bdcdaten using 'RM07I-GIDAT'
                          datum.
  perform bdcdaten using 'RM07I-BLDAT'
                          datum.
  perform bdcdaten using 'IKPF-WERKS'
                          tab-werks.
  perform bdcdaten using 'IKPF-LGORT'
                          tab-lgort.
  perform bdcdaten using 'IKPF-SOBKZ'
                          sobkz.
  perform bdcdaten using 'IKPF-SPERR'
                          sperr.
  perform bdcdaten using 'IKPF-INVNU'
                          invnu.
  perform bdcdaten using 'IKPF-XBLNI'
                          inventref.
  perform bdcdaten using 'IKPF-XBUFI'
                          xbufi.
* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
  perform bdcdaten using 'IKPF-KEORD'
                          keord.
* &jk02 end
  perform bdcdaten using 'BDC_OKCODE'
                          nl.
  flg_beleg_offen = x.
  clear flg_beleg_voll.

endform.

*---------------------------------------------------------------------*
*       FORM IBPOS_ERZEUGEN                                           *
*---------------------------------------------------------------------*
*       Erzeugen einer Inventurbelegposition.                         *
*---------------------------------------------------------------------*
form ibpos_erzeugen.

  perform bdcdynpro using 'SAPMM07I'
                          '0721'.
  perform bdcdaten using 'ISEGK-KDAUF'
                          tab-vbeln.
  perform bdcdaten using 'ISEGK-KDPOS'
                          tab-posnr.
  perform bdcdaten using 'ISEGK-LIFNR'
                          tab-lifnr.
  perform bdcdaten using 'ISEGK-KUNNR'
                          tab-kunnr.
  write tab-pspnr to pspnr_conv.
  perform bdcdaten using 'ISEGK-PS_PSP_PNR'
                          pspnr_conv.
  write tab-matnr to matnr_conv.
  perform bdcdaten using 'ISEG-MATNR(01)'
                          matnr_conv.
  perform bdcdaten using 'ISEG-CHARG(01)'
                          tab-charg.
  perform bdcdaten using 'ISEG-BSTAR(01)'
                          tab-bstar.
  perform bdcdaten using 'BDC_OKCODE'
                          nl.
  anzahl = anzahl + 1.
  if anzahl = maxpo.
    flg_beleg_voll = x.
    clear anzahl.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM BELEG_SCHLIESSEN                                         *
*---------------------------------------------------------------------*
*       Schließen eines Inventurbeleges. Es ist sicher, dass einer    *
*       offen ist.                                                    *
*---------------------------------------------------------------------*
form beleg_schliessen.

  clear: flg_beleg_offen,
         anzahl.

  perform bdcdynpro using 'SAPMM07I'
                          '0721'.

  perform bdcdaten using 'BDC_OKCODE'
                          bu.
  call function 'BDC_INSERT'
       exporting
            tcode     = 'MI01'
       tables
            dynprotab = bdcdata.
endform.

*---------------------------------------------------------------------*
*       AT USER-COMMAND                                               *
*---------------------------------------------------------------------*
at user-command.
  case sy-ucomm.
    when ucom-mapp.
* #jko3 begin
      flg_mappe = x.
* #jko3 end
      perform mappe_aufbauen.
      message s832 with mappe.
*     this pf-status is only used in report RM07ICN1        "n391015
      set pf-status '200' immediately.      "#EC *          "n391051
* #jko2 begin
    when ucom-buch.
      perform create_docs_via_mb_physinv changing g_number_of_docs.
      message s457 with g_number_of_docs.
      perform error_list_handle using c_show_list
                                      g_s_eiseg_dummy
                                      g_s_eikpf_dummy.
* #jko2 end
    when ucom-back.
      perform anforderungsbild.
    when ucom-abbr.
      perform anforderungsbild.
    when ucom-ende.
      perform beenden.
    when 'SM35'.
      set parameter id 'MPN' field mappe.
      call transaction 'SM35' and skip first screen.
  endcase.
*&---------------------------------------------------------------------*
*&      Form  KALKU_PRUEF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PRE_TAB text
*----------------------------------------------------------------------*
form kalku_pruef using p_mtart.

  select * from t134 where mtart = p_mtart.
    if not t134-pstat ca b.
      tab-cflag = x.
    endif.
  endselect.
endform.                               " KALKU_PRUEF
*&---------------------------------------------------------------------*
*&      Form  CREATE_DOCS_VIA_MB_PHYSINV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_docs_via_mb_physinv changing p_number_of_docs type i.

  data: l_t_iiseg like standard table of iiseg with header line.
  data: l_s_iikpf like iikpf.

  clear: flg_beleg_offen,
         flg_beleg_voll.
  p_number_of_docs = 0.

  perform error_list_handle using c_init
                                  g_s_eiseg_dummy
                                  g_s_eikpf_dummy.

  loop at tab where cycle eq o or cycle eq x.
*-- Ist bereits ein Beleg eröffnet?
    if flg_beleg_offen is initial.
      perform beleg_oeffnen_fb tables l_t_iiseg
                             changing l_s_iikpf.
    else.

*---- Ist der aktuelle Beleg voll?
      if not flg_beleg_voll is initial.
        perform beleg_anlegen_fb tables l_t_iiseg
                               changing l_s_iikpf
                                        p_number_of_docs.
        perform beleg_oeffnen_fb tables l_t_iiseg
                               changing l_s_iikpf.

*---- Liegt ein Datumswechsel vor?
      elseif old_gidat ne tab-gidat.
        perform beleg_anlegen_fb tables l_t_iiseg
                               changing l_s_iikpf
                                        p_number_of_docs.
        perform beleg_oeffnen_fb tables l_t_iiseg
                               changing l_s_iikpf.

*---- Liegt ein Werkswechsel vor?
      elseif old_werks ne tab-werks.
        perform beleg_anlegen_fb tables l_t_iiseg
                               changing l_s_iikpf
                                        p_number_of_docs.
        perform beleg_oeffnen_fb tables l_t_iiseg
                               changing l_s_iikpf.

*---- Liegt ein Lagerortwechsel vor?
      elseif old_lgort ne tab-lgort.
        perform beleg_anlegen_fb tables l_t_iiseg
                               changing l_s_iikpf
                                        p_number_of_docs.
        perform beleg_oeffnen_fb tables l_t_iiseg
                               changing l_s_iikpf.

      elseif old_ordng ne tab-ordng.
        perform beleg_anlegen_fb tables l_t_iiseg
                               changing l_s_iikpf
                                        p_number_of_docs.
        perform beleg_oeffnen_fb tables l_t_iiseg
                               changing l_s_iikpf.

*---- Belegwechsel nach Gruppenwechsel der Sortierung?
      elseif not xgrupp is initial.

*------ Liegt ein Lagerplatzwechsel vor?
        if not xlgpbe is initial and old_lgpbe ne tab-lgpbe.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.

*------ Liegt ein Warengruppenwechsel vor?
        elseif not xmatkl is initial and old_matkl ne tab-matkl.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        elseif  xmatkl is initial and xlgpbe is initial and sobkz eq k
                and old_lifnr ne tab-lifnr.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq e
               and old_vbeln ne tab-vbeln.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq o
                and old_lifnr ne tab-lifnr.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq v
               and old_kunnr ne tab-kunnr.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.

        elseif xmatkl is initial and xlgpbe is initial and sobkz eq w
               and old_kunnr ne tab-kunnr.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq m
               and old_lifnr ne tab-lifnr.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        elseif xmatkl is initial and xlgpbe is initial and sobkz eq q
                and old_pspnr ne tab-pspnr.
          perform beleg_anlegen_fb tables l_t_iiseg
                                 changing l_s_iikpf
                                          p_number_of_docs.
          perform beleg_oeffnen_fb tables l_t_iiseg
                                 changing l_s_iikpf.
        endif.
      endif.
    endif.
*   Nächste Position bearbeiten
    perform ibpos_erzeugen_fb tables l_t_iiseg.
    old_gidat = tab-gidat.
    old_werks = tab-werks.
    old_lgort = tab-lgort.
    old_lgpbe = tab-lgpbe.
    old_matkl = tab-matkl.
    old_ordng = tab-ordng.
    old_lifnr = tab-lifnr.
    old_vbeln = tab-vbeln.
    old_kunnr = tab-kunnr.
    old_pspnr = tab-pspnr.
  endloop.

  if not flg_beleg_offen is initial.
    perform beleg_anlegen_fb tables l_t_iiseg
                           changing l_s_iikpf
                                    p_number_of_docs.
  endif.
endform.                               " CREATE_DOCS_VIA_MB_PHYSINV
*&---------------------------------------------------------------------*
*&      Form  BELEG_OEFFNEN_FB
*&---------------------------------------------------------------------*
*       #jko
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form beleg_oeffnen_fb tables p_t_iiseg structure iiseg
                    changing p_s_iikpf like iikpf.

  clear p_s_iikpf.
  clear p_t_iiseg.
  refresh p_t_iiseg.

  p_s_iikpf-gidat = tab-gidat.
  p_s_iikpf-bldat = tab-gidat.
  p_s_iikpf-werks = tab-werks.
  p_s_iikpf-lgort = tab-lgort.
  p_s_iikpf-sobkz = sobkz.
  p_s_iikpf-sperr = sperr.
  p_s_iikpf-invnu = invnu.
  p_s_iikpf-xblni = inventref.
  p_s_iikpf-xbufi = xbufi.
  p_s_iikpf-keord = keord.

  flg_beleg_offen = x.
  clear flg_beleg_voll.

endform.                               " BELEG_OEFFNEN_FB
*&---------------------------------------------------------------------*
*&      Form  beleg_anlegen_fb
*&---------------------------------------------------------------------*
*       #jko
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form beleg_anlegen_fb tables p_t_iiseg structure iiseg
                    changing p_s_iikpf like iikpf
                             p_number_of_docs type i.

  data: begin of l_t_eiseg occurs 20.
          include structure eiseg.
  data: end of l_t_eiseg.
  data: l_s_eikpf like eikpf.
  data: l_tcode   like sy-tcode value 'MI01'.
  data: l_action  type n.
  data: l_still_items type c.
  data: l_post type c.
  data: l_anz type i.
  data: l_anz1 type i.

* erster Start: l_action = 1.
  l_action = 1.

  l_still_items = x.
  l_post = x.

  while l_still_items = x.
    clear l_still_items.
    describe table p_t_iiseg lines l_anz1.
    call function 'MB_PHYSICAL_INVENTORY'
         exporting
              s_iikpf       = p_s_iikpf
              ctcod         = l_tcode
              inv_action    = l_action
         importing
              s_eikpf       = l_s_eikpf
         tables
              t_iiseg       = p_t_iiseg
              t_eiseg       = l_t_eiseg
         exceptions
              error_message = 1
              others        = 100.

    if not l_s_eikpf-subrc is initial or not sy-subrc is initial.
      clear l_post.
      perform error_list_handle using c_set_message_head
                                      g_s_eiseg_dummy
                                      l_s_eikpf.
    else.
*     error at item-level?
      loop at l_t_eiseg where not subrc is initial.
        l_t_eiseg-werks = p_s_iikpf-werks.
        l_t_eiseg-lgort = p_s_iikpf-lgort.
        perform error_list_handle using c_set_message_item
                                        l_t_eiseg
                                        g_s_eikpf_dummy.

        delete p_t_iiseg where matnr = l_t_eiseg-matnr and
                               charg = l_t_eiseg-charg and
                               bstar = l_t_eiseg-bstar.

        describe table p_t_iiseg lines l_anz.
*       emergency exit if l_anz1 = l_anz.
        if l_anz > 0 and l_anz1 > l_anz.
          l_still_items = x.
        else.
          clear l_still_items.
          clear l_post.
        endif.
      endloop.
    endif.
  endwhile.
  if l_post = x.
*   2. Start mir l_action = 2 zum Buchen.
    l_action = 2.

    call function 'MB_PHYSICAL_INVENTORY'
         exporting
              s_iikpf       = p_s_iikpf
              ctcod         = l_tcode
              inv_action    = l_action
         importing
              s_eikpf       = l_s_eikpf
         tables
              t_iiseg       = p_t_iiseg
              t_eiseg       = l_t_eiseg
         exceptions
              error_message = 1
              others        = 100.

    if not l_s_eikpf-subrc is initial or not sy-subrc is initial.
      l_s_eikpf-iblnr = p_s_iikpf-iblnr.
      l_s_eikpf-gjahr = p_s_iikpf-gjahr.
      perform error_list_handle using c_set_message_head
                                      g_s_eiseg_dummy
                                      l_s_eikpf.
    else.
      p_number_of_docs = p_number_of_docs + 1.
      commit work and wait.
    endif.
  endif.

endform.                               " beleg_anlegen_fb

*&---------------------------------------------------------------------*
*&      Form  IBPOS_ERZEUGEN_FB
*&---------------------------------------------------------------------*
*       #jko
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ibpos_erzeugen_fb tables p_t_iiseg structure iiseg.

  clear p_t_iiseg.

  p_t_iiseg-kdauf = tab-vbeln.
  p_t_iiseg-kdpos = tab-posnr.
  p_t_iiseg-lifnr = tab-lifnr.
  p_t_iiseg-kunnr = tab-kunnr.
  p_t_iiseg-ps_psp_pnr = tab-pspnr.
  p_t_iiseg-matnr = tab-matnr.
  p_t_iiseg-charg = tab-charg.
  p_t_iiseg-bstar = tab-bstar.
  append p_t_iiseg.

  anzahl = anzahl + 1.
  if anzahl = maxpo.
    flg_beleg_voll = x.
    clear anzahl.
  endif.

endform.                               " IBPOS_ERZEUGEN_FB
*#jko2 begin
*&---------------------------------------------------------------------*
*&      Form  error_list_handle
*&---------------------------------------------------------------------*
*       #jko2
*----------------------------------------------------------------------*
*      -->P_ACTION  text
*----------------------------------------------------------------------*
form error_list_handle using p_action type c
                             p_eiseg type eiseg
                             p_eikpf type eikpf.

  data: l_anzahl type i.
  data: l_s_balmi like balmi.
  data: l_msgtx  like message-msgtx.
  data: begin of l_msgtx_3,
         zeile1(35) type c,
         zeile2(35) type c,
         zeile3(35) type c,
        end of l_msgtx_3.

  statics: begin of l_t_list occurs 10,
             werks like iseg-werks,
             lgort like iseg-lgort,
             matnr like iseg-matnr,
             charg like iseg-charg,
             bstar like iseg-bstar.
          include structure balmi.
  statics: end of l_t_list.

  case p_action.
    when c_init.
      clear l_t_list. refresh l_t_list.

    when c_set_message_head.
      move-corresponding p_eikpf to l_t_list.
      append l_t_list.

    when c_set_message_item.
      move-corresponding p_eiseg to l_t_list.
      append l_t_list.

    when c_show_list.
      describe table l_t_list lines l_anzahl.
      if l_anzahl = 0.
        exit.
      endif.

      loop at l_t_list.
        if sy-tabix = 1.
          new-page no-heading no-title line-size 79.
          perform set_format using 0 space space.
          write:/ text-042.
          perform open_grid using 79 1 x.
          write:2 text-043 no-gap.
          perform sep_grid using 2 space.
        endif.

        perform next_row using 2 space.

        format color col_normal intensified off.

        write: sy-vline no-gap.

*       Werk
        write: (4) l_t_list-werks color col_key intensified no-gap.
        write: sy-vline no-gap.

*       Lagerort
        write: (4) l_t_list-lgort color col_key intensified no-gap.
        write: sy-vline no-gap.

*       Matnr
        write: (18) l_t_list-matnr color col_key intensified no-gap.
        write: sy-vline no-gap.

*       Charge
        write: (10) l_t_list-charg color col_key intensified no-gap.
        write: sy-vline no-gap.

*       Bestandsart
        write: (1) l_t_list-bstar color col_key intensified no-gap.
        write: sy-vline no-gap.

        move-corresponding l_t_list to l_s_balmi.
        perform expand_message using l_s_balmi
                            changing l_msgtx.

        move l_msgtx to l_msgtx_3.
        perform write_message using l_msgtx_3-zeile1 l_t_list-msgty.
        if not l_msgtx_3-zeile2 is initial.
          perform next_row using 2 space.
          format color col_normal intensified off.
          write: sy-vline no-gap.
          write: (4) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (4) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (18) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (10) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (1) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          perform write_message using l_msgtx_3-zeile2 l_t_list-msgty.
        endif.
        if not l_msgtx_3-zeile3 is initial.
          perform next_row using 2 space.
          format color col_normal intensified off.
          write: sy-vline no-gap.
          write: (4) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (4) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (18) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (10) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          write: (1) space color col_key intensified no-gap.
          write: sy-vline no-gap.
          perform write_message using l_msgtx_3-zeile3 l_t_list-msgty.
        endif.
*       Meldungsfarbe in Abhängigkeit vom Meldungstyp einstellen
      endloop.
      if l_anzahl > 0.
        perform close_grid.
      endif.
  endcase.
endform.                               " error_list_handle
*&---------------------------------------------------------------------*
*&      Form  EXPAND_MESSAGE
*&---------------------------------------------------------------------*
*       #jko2
*----------------------------------------------------------------------*
*      -->BALMI            text                                        *
*      <--P_MSGTX  text                                              *
*----------------------------------------------------------------------*
form expand_message using    p_s_balmi structure balmi
                    changing p_msgtx   like message-msgtx.

* error variable
  data: l_error         type c.

* structure for message
  data: begin of l_s_message.
          include structure message.
  data: end of l_s_message.

* message length
  data: l_msgln         like sy-fdpos.

* Meldung aus T100 lesen und Parameter einsetzen
* Ergebnis steht in l_s_message_msgtx.
  call function 'WRITE_MESSAGE_NEW'
       exporting
            msgid  = p_s_balmi-msgid
            msgno  = p_s_balmi-msgno
            msgty  = p_s_balmi-msgty
            msgv1  = p_s_balmi-msgv1
            msgv2  = p_s_balmi-msgv2
            msgv3  = p_s_balmi-msgv3
            msgv4  = p_s_balmi-msgv4
            msgv5  = ' '
       importing
            error  = l_error
            messg  = l_s_message
            msgln  = l_msgln
       exceptions
            others = 1.

* Ergebnis zurueckliefern
  if sy-subrc is initial.
    p_msgtx = l_s_message-msgtx.
  endif.
endform.                               " EXPAND_MESSAGE
*#jko2 end
*&---------------------------------------------------------------------*
*&      Form  write_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSGTX_3_ZEILE1  text
*----------------------------------------------------------------------*
form write_message using p_zeile p_msgty.

*       Meldungsfarbe in Abhängigkeit vom Meldungstyp einstellen
  if p_msgty = 'E'.
    write: (35) p_zeile color col_negative intensified no-gap.
  endif.
  if p_msgty = 'A'.
    write: (35) p_zeile color col_negative intensified no-gap.
  endif.
  if p_msgty = 'W'.
    write: (35) p_zeile color col_total intensified no-gap.
  endif.
  if p_msgty = 'I'.
    write: (35) p_zeile color col_normal intensified no-gap.
  endif.
endform.                               " write_message

*---------------------------------------------------------------------*
*       FORM BSE_PRUEFEN_NEU                                           *
*---------------------------------------------------------------------*
form bse_pruefen_neu.

  statics: l_werks_old like t001w-werks.
  data: l_kzrfb like mtcom-kzrfb.
  data: l_maxtz like mtcom-maxtz value '2000'.

  data: bf_matnr like mara-matnr,
        xwaart   type c.

  clear: fehltab,
         xruej,
         xraus.

* does the user has authority for this plant ?              "XJD
  check : pre_tab-werks in r_werks.                         "XJD
                                                            "XJD
* Prüfen ob Löschvormerkung sitzt, wenn gefordert
  if xdele is initial.
    if not pre_tab-lvorm is initial.
      move-corresponding pre_tab to tab_error.
      tab_error-ernum = '4'.
      append tab_error.
      xraus = x.
    endif.
  endif.
  check xraus is initial.

* Prüfen, ob der Lagerort nicht LVS-relevant ist
  move sy-mandt      to t320_key-mandt.
  move pre_tab-werks to t320_key-werks.
  move pre_tab-lgort to t320_key-lgort.
  read table xt320 with key t320_key binary search.
  if sy-subrc is initial.
    move-corresponding pre_tab to tab_error.
    tab_error-ernum = '2'.
    append tab_error.
    xraus = x.
  endif.
  check xraus is initial.

* Prüfen der Buchhaltung: Ist Mengen und Wertfortschreigung gepflegt ?
  move sy-mandt      to v134w_key-mandt.
  move pre_tab-werks to v134w_key-werks.
  move pre_tab-mtart to v134w_key-mtart.
  read table xv134w with key v134w_key binary search.
  if sy-subrc is initial.

* take "special stocks sales order" always;                note 212834
* independent of the indicator for quantity update in      note 212834
* table T134M                                              note 212834
  if  sobkz         =  E         and                      "note 212834
      xv134w-mengu  is initial.                           "note 212834
    move  X                  to  xv134w-mengu.            "note 212834
  endif.                                                  "note 212834

*-- Keine Mengenfortschreibung, d.h. keine Inventur möglich
    if xv134w-mengu is initial and
       pre_tab-vbeln is initial
* &001 begin: Anpassung Wertartikelinventur
       and xv134w-wertu is initial.
* &001 end
      move-corresponding pre_tab to tab_error.
      tab_error-ernum = '5'.
      append tab_error.
      xraus = x.
* &001 begin: Anpassung Wertartikelinventur
    elseif xv134w-mengu is initial and
       not xv134w-wertu is initial.
* Wertartikel werden nur aus Selektionsmenge rausgenommen, aber nicht
* in die Fehlertabelle geschrieben.
      if xwart is initial.
        xraus = x.
      endif.
    elseif not xv134w-mengu is initial and
           not xv134w-wertu is initial.
* Artikel werden nur aus Selektionsmenge rausgenommen, aber nicht
* in die Fehlertabelle geschrieben.
      if xlabst is initial and
         xinsme is initial and
         xspeme is initial.
        xraus = x.
      endif.
      if kein_buch is initial.
        xbuch = x.
      endif.
* &001 end
*-- Wenn Wertfortschreibung gesetzt ist, muß Buchhaltung geprüft werden
    elseif not xv134w-wertu is initial.
      if kein_buch is initial.
        xbuch = x.
      endif.
    endif.
  else.

*-- Fehlender Eintrag in Tabelle V134W
    move-corresponding pre_tab to tab_error.
    tab_error-ernum = '3'.
    append tab_error.
    xraus = x.
  endif.

* finale Prüfung, ob Material drin bleibt
  check xraus is initial.

* neuen Buchungskreis lesen
  on change of pre_tab-werks.
    read table xwebu with key pre_tab-werks binary search.
  endon.

* &001 begin: Anpassungen Wertartikelinventur
* Bei Werken, in denen die Verkaufspreisbewertung aktiv ist
* wird geprüft, ob das Material auf sich selbst bestandsgeführt
* wird. Wenn nicht, fliegt es raus.
  if not xwebu-xvkbw is initial.
*   bei neuem Werk den internen Puffer in VALUE_ARTICLE_FIND loeschen
*   l_maxtz steht auf 2000
    if l_werks_old ne pre_tab-werks.
      l_kzrfb = x.
      l_werks_old = pre_tab-werks.
    else.
      clear l_kzrfb.
    endif.
    call function 'VALUE_ARTICLE_FIND'
         exporting
              i_matnr                = pre_tab-matnr
              i_werks                = pre_tab-werks
              i_kzrfb                = l_kzrfb
              i_maxtz                = l_maxtz
         importing
              e_matnr                = bf_matnr
              e_waart                = xwaart
         exceptions
              entry_not_found        = 01
              wertm_bf_nicht_erlaubt = 02
              buchung_nicht_erlaubt  = 03
              material_not_found     = 04.

    if sy-subrc is initial.
      if pre_tab-matnr ne bf_matnr.
*        MOVE-CORRESPONDING PRE_TAB TO TAB_ERROR.
*        TAB_ERROR-ERNUM = '6'.
*        APPEND TAB_ERROR.
        xraus = x.
      endif.
    else.
      move-corresponding pre_tab to tab_error.
      tab_error-ernum = '7'.
      append tab_error.
      xraus = x.
    endif.
  endif.

  check xraus is initial.
* &001 end

* Sätze für spätere Buchhaltungsüberprüfung wegschreiben
  if not xbuch is initial.
    r_matnr-low = pre_tab-matnr.
    collect r_matnr.
    describe table r_matnr lines c_matnr.
    c_matnr = c_matnr + c_bwkey.
    if c_matnr > 253.
      perform buchhaltung_lesen.
    endif.
    xmbew-matnr = pre_tab-matnr.
    xmbew-werks = xwebu-werks.
    xmbew-bwkey = xwebu-bwkey.
    xmbew-bwtar = pre_tab-bwtar.
    collect xmbew.
    clear xbuch.
  endif.

  check xraus is initial.

* find out whether the planned counting date is in the      "n654402
* current or previos year                                   "n654402
  CALL FUNCTION 'MB_SELECT_MAT_PHYSINV_XRUEJ'               "n654402
    EXPORTING                                               "n654402
      GIDAT                  =  gidat                       "n654402
      WERKS                  =  pre_tab-werks               "n654402
   IMPORTING                                                "n654402
     XRUEJ                   =  xruej                       "n654402
   EXCEPTIONS                                               "n654402
     IGNORE_RECORD           = 1                            "n654402
     OTHERS                  = 2.                           "n654402
                                                            "n654402
  case  sy-subrc.           "evaluate the result            "n654402
    when  0.                "no error -> go on              "n654402
    when  1.                                                "n654402
*     the planned counting date is in the future -> exit    "n654402
      move  x               to  xraus.                      "n654402
    when others.                                            "n654402
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO     "n654402
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.           "n654402
  endcase.                                                  "n654402
                                                            "n654402
  check xraus is initial.                                   "n654402

* Frei verwendbar
  if not xlabst is initial.
* &001 begin: Anpassung Wertartikelinventur
    if not xv134w-mengu is initial or
       xv134w-wertu is initial.
* &001 end
      perform bestand_pruefen using pre_tab-labst.
      if xraus is initial.
* Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
        if xcycl is initial.
          perform inventurzustand_pruefen using pre_tab-kzill
                                                pre_tab-kzvll
                                                eins.
        else.
*---------------------------------------------------------begin of UGL
*          perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzill
*                                                         pre_tab-kzvll
*                                                         pre_tab-dlinl
*                                                         eins.
          perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzill
                                                         pre_tab-kzvll
                                                         pre_tab-dlinl
                                                         eins.
*-----------------------------------------------------------end of UGL
        endif.
      else.
        clear xraus.
      endif.
* &001 begin: Anpassung Wertartikelinventur
    endif.
* &001 end
  endif.

* In Qualitätsprüfung
  if not xinsme is initial.
* &001 begin: Anpassung Wertartikelinventur
    if not xv134w-mengu is initial or
       xv134w-wertu is initial.
* &001 end
      perform bestand_pruefen using pre_tab-insme.
      if xraus is initial.

* &004 begin: Anpassung Bestandsarten Q und S
        if xcycl is initial.
* &004 end

*     Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
          perform inventurzustand_pruefen using pre_tab-kzilq
                                                pre_tab-kzvlq
                                                zwei.
* &004 begin: Anpassung Bestandsarten Q und S
        else.
*---------------------------------------------------------begin of UGL
*          perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzilq
*                                               pre_tab-kzvlq
*                                               pre_tab-dlinl
*                                               zwei.
          perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzilq
                                               pre_tab-kzvlq
                                               pre_tab-dlinl
                                               zwei.

*-----------------------------------------------------------end of UGL
        endif.
* &004 end

      else.
        clear xraus.
      endif.
* &001 begin: Anpassung Wertartikelinventur
    endif.
* &001 end
  endif.

* Gesperrt
  if not xspeme is initial.
* &001 begin: Anpassung Wertartikelinventur
    if not xv134w-mengu is initial or
       xv134w-wertu is initial.
* &001 end
      perform bestand_pruefen using pre_tab-speme.
      if xraus is initial.
* &004 begin: Anpassungen Bestandsarten Q und S
        if xcycl is initial.
* &004 end

*     Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
          perform inventurzustand_pruefen using pre_tab-kzils
                                                pre_tab-kzvls
                                                vier.
* &004 begin: Anpassungen Bestadnsarten Q und S
        else.
*---------------------------------------------------------- begin of UGL
*          perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzils
*                                               pre_tab-kzvls
*                                               pre_tab-dlinl
*                                               vier.
         perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzils
                                               pre_tab-kzvls
                                               pre_tab-dlinl
                                               vier.

*------------------------------------------------------------ end of UGL
        endif.
* &004 end
      else.
        clear xraus.
      endif.
* &001 begin: Anpassung Wertartikelinventur
    endif.
* &001 end
  endif.

* &001 begin: Anpassung Wertartikelinventur
* Wertartikel
  if not xwart  is initial.
    if xv134w-mengu is initial and
       not xv134w-wertu is initial.

      perform bestand_pruefen_wart using pre_tab-vklab.
      if xraus is initial.
* Inventurzustand prüfen und (wenn in Ordnung) Satz an Tab anhängen
        if xcycl is initial.
          perform inventurzustand_pruefen using pre_tab-kzill
                                                space
                                                space.
        else.
*--------------------------------------------------------begin of UGL
*          perform inventurzyklus_pruefen(rm07icn1) using pre_tab-kzill
*                                                         space
*                                                         pre_tab-dlinl
*                                                         space.
          perform inventurzyklus_pruefen(zrm07icn1) using pre_tab-kzill
                                                         space
                                                         pre_tab-dlinl
                                                         space.
*----------------------------------------------------------end of UGL
        endif.
      else.
        clear xraus.
      endif.
    endif.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  MARV_SINGLE_READ
*&---------------------------------------------------------------------*
*       Bestimmung des LFGJA der MARV
*       Zum Vergleich MDJIN
*----------------------------------------------------------------------*
form marv_single_read.
    CLEAR MARV.
    CALL FUNCTION 'MARV_SINGLE_READ'
           EXPORTING
               KZRFB      = X
               bukrs      = xwebu-bukrs
           IMPORTING
               wmarv      = marv
           EXCEPTIONS
               not_found  = 1
               wrong_call = 2
               OTHERS     = 3.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e176 WITH xwebu-bukrs.
    ENDIF.
endform.
*&---------------------------------------------------------------------*
*&      Form  MARV_BESTIMMEN
*&---------------------------------------------------------------------*
*       Bestimmung des LFGJA der MARV
*       Füllen der Tabelle WERK_LFGJA
*----------------------------------------------------------------------*
*        >P_PRE_TAB text
*----------------------------------------------------------------------*
FORM MARV_BESTIMMEN.

SELECT WERKS T001K~BUKRS LFGJA
       INTO CORRESPONDING FIELDS OF TABLE WERKS_LFGJA
       FROM ( T001W INNER JOIN T001K ON T001W~BWKEY = T001K~BWKEY )
            INNER JOIN MARV ON MARV~BUKRS = T001K~BUKRS
         WHERE T001W~WERKS IN R_WERKS.

SORT WERKS_LFGJA BY WERKS.

ENDFORM.
