*####################################################################*
* Datenteil                                                          *
*####################################################################*

*-------------------------------------------------------------------*
* Datenbanktabellen                                                 *
*-------------------------------------------------------------------*
TABLES: DIEQUI,
        DIEQUZ,
        ZQUI,
        IHPA,
        V_EQUI,
        MAKT.
*-------------------------------------------------------------------*
* ATAB-Tabellen                                                     *
*-------------------------------------------------------------------*
TABLES:
    T370A.

*-------------------------------------------------------------------*
* Interne Tabellen                                                  *
*-------------------------------------------------------------------*
DATA: BEGIN OF OBJECT_TAB OCCURS 0.
        INCLUDE STRUCTURE RIHEQUI.
DATA:   CLDA1 LIKE DICLDAT-AUSP1.
DATA:   CLDA2 LIKE DICLDAT-AUSP1.
DATA:   CLDA3 LIKE DICLDAT-AUSP1.
DATA:   CLDA4 LIKE DICLDAT-AUSP1.
DATA:   CLDA5 LIKE DICLDAT-AUSP1.
DATA:   CLDA6 LIKE DICLDAT-AUSP1.
DATA:   CLDA7 LIKE DICLDAT-AUSP1.
DATA:   CLDA8 LIKE DICLDAT-AUSP1.
DATA:   CLDA9 LIKE DICLDAT-AUSP1.
DATA:   CLDA10 LIKE DICLDAT-AUSP1.
DATA:   CLDA11 LIKE DICLDAT-AUSP1.
DATA:   CLDA12 LIKE DICLDAT-AUSP1.
DATA:   CLDA13 LIKE DICLDAT-AUSP1.
DATA:   CLDA14 LIKE DICLDAT-AUSP1.
DATA:   CLDA15 LIKE DICLDAT-AUSP1.
DATA:   CLDA16 LIKE DICLDAT-AUSP1.
DATA:   CLDA17 LIKE DICLDAT-AUSP1.
DATA:   CLDA18 LIKE DICLDAT-AUSP1.
DATA:   CLDA19 LIKE DICLDAT-AUSP1.
DATA:   CLDA20 LIKE DICLDAT-AUSP1.
DATA:   PPSID LIKE V_EQUI-PPSID.
DATA:   IGEWRK LIKE V_EQUI-GEWRK.
DATA:   EQASP  LIKE V_EQUI-EQASP.
DATA:   EQLFN  LIKE V_EQUI-EQLFN.
DATA:   SELECTED,
      END OF OBJECT_TAB.

DATA: BEGIN OF EXCEL_TAB OCCURS 0.
        INCLUDE STRUCTURE RIHEQUI.
DATA: END OF EXCEL_TAB.

DATA: BEGIN OF SEL_TAB OCCURS 50.
        INCLUDE STRUCTURE RIHEQUI.
DATA: END OF SEL_TAB.

DATA: BEGIN OF L_CLOBJ OCCURS 50.
        INCLUDE STRUCTURE CLOBJ.
DATA: END OF L_CLOBJ.

DATA: BEGIN OF L_JSTO_PRE_TAB OCCURS 0.
        INCLUDE STRUCTURE JSTO_PRE.
DATA: END OF L_JSTO_PRE_TAB.

DATA: BEGIN OF L_TARBID OCCURS 0.
        INCLUDE STRUCTURE CRID.
DATA: END OF L_TARBID.
DATA: BEGIN OF G_OBJNR_TAB OCCURS 0,   "Für Partnerselektion
        OBJNR LIKE ZQUI-OBJNR,
      END OF G_OBJNR_TAB.

RANGES: OBJECT FOR OBJECT_TAB-EQUNR.
RANGES: R_SUBMT FOR OBJECT_TAB-SUBMT.

*-------------------------------------------------------------------*
* Feldleisten                                                       *
*-------------------------------------------------------------------*
TABLES: RIHEQUI.
TABLES: RSSUBINFO.

*-------------------------------------------------------------------*
* Flags Sonderverarbeitungen                                        *
*-------------------------------------------------------------------*
DATA: G_ADRES_FLAG.
DATA: G_STTXT_FLAG.
DATA: G_ARBPL_FLAG.
DATA: G_GEWRK_FLAG.
DATA: G_STASL_FLAG.
DATA: G_ANSWER.
DATA  H LIKE SY-TABIX.

*eject
*-------------------------------------------------------------------*
* INCLUDES                                                          *
*-------------------------------------------------------------------*
INCLUDE MIOLXTOP.

*eject
*####################################################################*
* Selektionsbild                                                     *
*####################################################################*
INCLUDE MIOLESEL.
PARAMETERS:
  DY_SELM DEFAULT '0' NO-DISPLAY,
  DY_TCODE LIKE SY-TCODE NO-DISPLAY,
  DY_MODE  DEFAULT ' ' NO-DISPLAY.

*eject
*---------------------------------------------------------------------
* Initialization
*---------------------------------------------------------------------
INITIALIZATION.

*--- Aktivitätstyp bestimmen -----------------------------------------
  SELECT SINGLE * FROM T370A WHERE TCODE = SY-TCODE.
  IF SY-SUBRC <> 0.
    IF SY-REPID = 'RIzqui20'.
      SELECT SINGLE * FROM T370A WHERE TCODE = 'IH08'.
      IF SY-SUBRC <> 0.
        RAISE IH08_NOT_IN_T370A.
      ENDIF.
*   elseif sy-repid = 'RIzqui21'.      "Serialnummernversion   "mjw
    ELSEIF SY-REPID = 'ZIZQUI21'.      "Serialnummernversion   "mjw
*     select single * from t370a where tcode = 'IQ09'.         "mjw
      SELECT SINGLE * FROM T370A WHERE TCODE = 'ZQ09'.         "mjw
      IF SY-SUBRC <> 0.
        RAISE IH08_NOT_IN_T370A.
      ENDIF.
    ENDIF.
  ENDIF.

  G_KEY = 'EQUNR'.
  G_TEXT = 'EQTXT'.

  PERFORM VARIANT_START_F10.

*eject
*---------------------------------------------------------------------
* SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM INIT_SELECTION_SCREEN_F10.
  PERFORM MODIF_SELECTION_SCREEN_L.
  PERFORM HEADER_INTENSIFIED_L.

*eject
*---------------------------------------------------------------------
* START-OF-SELECTION
*---------------------------------------------------------------------
START-OF-SELECTION.
*--- Adressen selektieren ----------------------------------------
  IF NOT ADRMCO IS INITIAL.
    PERFORM FILL_ADRNR(SAPDBEQI) USING ADRMCO.
  ENDIF.
*--- Selektion nach Adressen -> keine oder zuviele Adresssen gefunden -*
*--- wenn mehr als 250 Einträge in Range -> Datenbankabbruch ----------*
  DATA: H_LINES LIKE SY-TABIX.
  DATA: G_ERRORCODE LIKE SY-SUBRC.
  IF NOT ADRMCO IS INITIAL.
    DESCRIBE TABLE ADRNR LINES H_LINES.
    IF H_LINES = 0.
      MESSAGE S047.
      EXIT.
    ELSEIF H_LINES > 250.
      MESSAGE S103.
      G_ERRORCODE = '0010'.
      EXPORT G_ERRORCODE TO MEMORY ID SY-REPID.
      EXIT.                            "bei Aufruf durch Funktbstein
    ENDIF.
  ENDIF.

*--- Aktivitätstyp bestimmen -----------------------------------------
  IF DY_TCODE IS INITIAL.
    DY_TCODE = SY-TCODE.
  ENDIF.
  SELECT SINGLE * FROM T370A WHERE TCODE = DY_TCODE.
  IF SY-SUBRC <> 0.
    IF SY-REPID = 'RIzqui20'.
      SELECT SINGLE * FROM T370A WHERE TCODE = 'IH08'.
      IF SY-SUBRC <> 0.
        RAISE IH08_NOT_IN_T370A.
      ENDIF.
    ELSEIF SY-REPID = 'RIzqui21'.      "Serialnummernversion
      SELECT SINGLE * FROM T370A WHERE TCODE = 'IQ09'.
      IF SY-SUBRC <> 0.
        RAISE IH08_NOT_IN_T370A.
      ENDIF.
    ENDIF.
  ENDIF.
  PERFORM INIT_HEADER_TAB_F10 USING T370A-TABARG T370A-TABNAME.
  REFRESH SEL_TAB.
  G_SELMOD = DY_SELM.
  IF G_SELMOD <> SELMOD_0.
    EXPORT SEL_TAB TO MEMORY ID SY-REPID.
  ENDIF.
  PERFORM SEL_SERIAL_VIA_STATUS.
  PERFORM SELECTION_L.
*eject
*---------------------------------------------------------------------
* END-OF-SELECTION
*---------------------------------------------------------------------
END-OF-SELECTION.

*--- Sortieren -------------------------------------------------------*
  PERFORM SORT_F10 USING 'X'.

  G_AKTYP = T370A-AKTYP.
  PERFORM PF_STATUS_F10.

*--- Liste ausgeben --------------------------------------------------*
  PERFORM DISPLAY_LIST_F10 USING 'SD  '
                                 RIHEQUI.


*eject
*---------------------------------------------------------------------
* TOP-OF-PAGE
*---------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM LISTHEADER_F10.

*eject
*---------------------------------------------------------------------
* TOP-OF-PAGE DURING LINE-SELECTION
*---------------------------------------------------------------------
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM LISTHEADER_F10.

*eject
*---------------------------------------------------------------------
* AT USER-COMMAND
*---------------------------------------------------------------------
AT USER-COMMAND.

  PERFORM SAVE_SELECTION_F10.
  PERFORM CHECK_PF_WITH_OBJECT USING 'rihequi-EQUNR'.

  CASE SY-UCOMM.
    WHEN 'CLVI'.
      PERFORM CLASSIFICATION_F50.
    WHEN 'IFSL'.
      IF G_CLASS_ON = YES.
        PERFORM CLASSIFICATION_OFF_F50.
        G_CLASS_ON = YES.
      ENDIF.
      PERFORM SELECT_DISPLAY_FIELDS_F10 USING T370A-TABARG
                                              T370A-TABNAME.
      PERFORM FILL_OBJECT_TAB_L.
      IF G_CLASS_ON = YES.
        PERFORM CLASSIFICATION_ON_F50.
      ENDIF.
    WHEN 'IOBJ'.
      IF G_LINE_ID = LINE_ID_HEAD.
        MESSAGE I011.
      ELSE.
        READ TABLE OBJECT_TAB INDEX G_INDEX.
        MOVE-CORRESPONDING OBJECT_TAB TO RIHEQUI.
        PERFORM MASTER_DATA_F10.
      ENDIF.
    WHEN 'NET1'.
      PERFORM DISPLAY_NET1_L.
    WHEN 'NET2'.
      PERFORM DISPLAY_NET2_L.
    WHEN 'MELD'.
      PERFORM DISPLAY_QMEL_L.
    WHEN 'AUFK'.
      PERFORM DISPLAY_AUFK_L.
    WHEN 'MHIS'.
      PERFORM DISPLAY_MHIS_L.
    WHEN 'MPOS'.
      PERFORM DISPLAY_MPOS_L.
    WHEN 'PLKO'.
      PERFORM DISPLAY_PLKO_L.
    WHEN 'PLAN'.
      PERFORM DISPLAY_PLAN_L.
    WHEN 'MPTS'.
      PERFORM DISPLAY_MPTS_L.
    WHEN 'VERT'.
      PERFORM DISPLAY_VERT_L.
    WHEN 'MUOB'.
      PERFORM MULTI_OBJECT_L.
    WHEN 'MUQM'.
      PERFORM MULTI_QMEL_L.
    WHEN 'MUAU'.
      PERFORM MULTI_AUFK_L.
    WHEN 'SD  '.
      PERFORM FCODES_WITH_MARK_F10.
    WHEN 'SN  '.
      PERFORM FCODES_WITH_MARK_F10.
    WHEN 'ST  '.
      PERFORM FCODES_WITH_MARK_F10.
    WHEN 'IH  '.
      PERFORM FCODES_WITH_MARK_F10.
    WHEN 'HILI'.
      PERFORM FCODES_WITH_MARK_F10.
    WHEN 'HIGR'.
      PERFORM FCODES_WITH_MARK_F10.
    WHEN 'IW24'.
      PERFORM EXECUTE_TCODE_L.
    WHEN 'IW25'.
      PERFORM EXECUTE_TCODE_L.
    WHEN 'IW26'.
      PERFORM EXECUTE_TCODE_L.
    WHEN 'IW31'.
      PERFORM EXECUTE_TCODE_L.
    WHEN 'AKTU'.
      IF G_CLASS_ON = YES.
        PERFORM CLASSIFICATION_OFF_F50.
        G_CLASS_ON = YES.
      ENDIF.
      PERFORM SEL_SERIAL_VIA_STATUS.
      PERFORM SELECTION_L.
      PERFORM INIT_HEADER_TAB_F10 USING T370A-TABARG T370A-TABNAME.
      IF G_CLASS_ON = YES.
        PERFORM CLASSIFICATION_ON_F50.
      ENDIF.
      IF G_FELDLIST-S_FIELD IS INITIAL
         AND G_FELDLIST-G_SORT_FLAG IS INITIAL.
        PERFORM SORT_F10 USING 'X'.
      ELSE.
        PERFORM SORT_F10 USING G_FELDLIST-G_SORT_FLAG.
      ENDIF.
      G_STARO = 1.
    WHEN OTHERS.
      PERFORM USER_COMMAND_F10.
  ENDCASE.

*--- Liste ausgeben --------------------------------------------------*
  IF SY-UCOMM <> 'IOBJ' AND                                 "P30K036367
     SY-UCOMM <> 'IMRK' AND                                 "P30K036367
     SY-UCOMM <> 'ILGN'.                                    "P30K036367
    PERFORM DISPLAY_LIST_F10 USING 'SD  '
                                   RIHEQUI.
    SCROLL LIST INDEX SY-LSIND TO PAGE SY-PAGNO LINE G_STARO.
    SY-LSIND = SY-LSIND - 1.
  ENDIF.
  PERFORM HIDE_RESET_F10.


*eject
*---------------------------------------------------------------------*
*       FORM FCODES_WITH_MARK_L                                       *
*---------------------------------------------------------------------*
*       FCodes, die auch im Loop verarbeitet werden können            *
*---------------------------------------------------------------------*
FORM FCODES_WITH_MARK_L.

  TABLES: RM63E.
  DATA: BEGIN OF H_ZQUI_TAB OCCURS 0.
          INCLUDE STRUCTURE RIHEQUI.
  DATA: END OF H_ZQUI_TAB.

  DATA: BEGIN OF H_IFLO_TAB OCCURS 0.
          INCLUDE STRUCTURE RIHIFLO.
  DATA: END OF H_IFLO_TAB.

  DATA: BEGIN OF H_STPO_TAB OCCURS 0.
          INCLUDE STRUCTURE RIHSTPX.
  DATA: END OF H_STPO_TAB.

  DATA: H_RETC  LIKE SY-SUBRC.
  DATA: H_TCODE LIKE SY-TCODE.

  MOVE-CORRESPONDING OBJECT_TAB TO RIHEQUI.
  CASE SY-UCOMM.
    WHEN 'HILI'.
      CALL FUNCTION 'PM_HIERARCHY_CALL'
           EXPORTING
                GRAFICS        = ' '
                EQUNR          = RIHEQUI-EQUNR
                WITH_ZQUI      = 'X'
                WITH_ZQUI_HIER = 'X'
                WITH_IFLO_HIER = 'X'
                WITH_MARA      = 'X'
           TABLES
                ZQUI_TAB       = H_ZQUI_TAB
                IFLO_TAB       = H_IFLO_TAB
                STPO_TAB       = H_STPO_TAB
           EXCEPTIONS
                NO_HIERARCHY   = 01
                NO_SELECTION   = 02.
      IF SY-SUBRC = 01.
        MESSAGE S019.
      ENDIF.
    WHEN 'HIGR'.
      CALL FUNCTION 'PM_HIERARCHY_CALL'
           EXPORTING
                GRAFICS        = 'X'
                EQUNR          = RIHEQUI-EQUNR
                WITH_ZQUI      = 'X'
                WITH_ZQUI_HIER = 'X'
                WITH_IFLO_HIER = 'X'
                WITH_MARA      = 'X'
           TABLES
                ZQUI_TAB       = H_ZQUI_TAB
                IFLO_TAB       = H_IFLO_TAB
                STPO_TAB       = H_STPO_TAB
           EXCEPTIONS
                NO_HIERARCHY   = 01
                NO_SELECTION   = 02.
      IF SY-SUBRC = 01.
        MESSAGE S019.
      ENDIF.
    WHEN OTHERS.
*---  Versorgen lfnd. zquinummer damit Popup bei Einstieg in   ------*
*---  IE02 nicht gezeigt wird (Auswahl Zeitsegment bereits in Liste -*
      EXPORT RM63E-EQLFN FROM OBJECT_TAB-EQLFN TO MEMORY ID 'EQL'.
      SET PARAMETER ID 'EQN' FIELD OBJECT_TAB-EQUNR.
      SET PARAMETER ID 'DSE' FIELD OBJECT_TAB-DATBI.
      RM63E-FCODE = SY-UCOMM.
      EXPORT RM63E-FCODE TO MEMORY ID 'FCzqui'.
      MOVE-CORRESPONDING OBJECT_TAB TO RIHEQUI.
      EXPORT RIHEQUI  TO MEMORY ID 'zqui'.
      CLEAR RETURN_CODE.
      EXPORT RETURN_CODE TO MEMORY ID 'Rzqui'.

      IF T370A-AKTYP = 'V'.
        H_TCODE = 'IE02'.
      ELSE.
        H_TCODE = 'IE03'.
      ENDIF.
      PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                                   CHANGING H_RETC.
      IF H_RETC IS INITIAL.
        CALL TRANSACTION H_TCODE AND SKIP FIRST SCREEN.
        IMPORT V_EQUI FROM MEMORY ID 'v_equi'.
*       IF SY-SUBRC = 0.                                  "P30K041640
        IF SY-SUBRC     = 0 AND                           "P30K041640
           V_EQUI-EQUNR = OBJECT_TAB-EQUNR.               "P30K041640
          CLEAR OBJECT_TAB.
          PERFORM MOVE_V_EQUI_TO_OBJECT_TAB_L.
          PERFORM FILL_OBJECT_TAB_LATE_L.
        ENDIF.
        IMPORT RETURN_CODE FROM MEMORY ID 'Rzqui'.
        IF G_LINES = 1.
          LEAVE.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM SELECTION_L                                              *
*---------------------------------------------------------------------*
*       zquipments selektieren                                        *
*---------------------------------------------------------------------*
FORM SELECTION_L.

  DATA  H_SEL_ZQUI.                    "Muss zqui nachgelesen werden
  DATA  H_TEXT(40).
  DATA  H LIKE SY-TABIX.

  CLEAR: G_STTXT_FLAG,
         G_ARBPL_FLAG,
         G_GEWRK_FLAG,
         G_ADRES_FLAG,
         G_STASL_FLAG.

*--- ist Gültigkeitszeitraum vorbelegt (datuv und datub) -----------
  IF DATUV IS INITIAL.
    DATUV = '00000000'.
  ENDIF.
  IF DATUB IS INITIAL.
    DATUB = '99993112'.
  ENDIF.
*--- Groß und Kleinschreibung bei Kurztext ignorieren ----------------
  LOOP AT EQKTX.
    TRANSLATE EQKTX-LOW TO UPPER CASE.
    TRANSLATE EQKTX-HIGH TO UPPER CASE.
    MODIFY EQKTX.
  ENDLOOP.

  CLEAR OBJECT_TAB.
  REFRESH OBJECT_TAB.

  CASE SY-REPID.
    WHEN 'RIzqui20'.
      H_TEXT = TEXT-201.               "zquipmentselektion
    WHEN 'RIzqui21'.
      H_TEXT = TEXT-B04.               "Serialnummernselektion
  ENDCASE.

*--- zquipment/Serialnummer lesen ---------------------------------
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT = H_TEXT.

*--- Feld zqui-KRFKZ neu zu 3.0 -> bei zquis aus PUT von 2.2 Systemen
*--- Feld undefiniert -> bei Selektion berücksichtigen
  IF KRFKZ IS INITIAL.
    KRFKZ = '%'.
  ENDIF.

*--- Wenn Selektion nach zquipments -> lesen über v_equi ------------*
*--- Wenn Selektion nach Serialnummern -> lesen über zqui evtl.    --*
*--- Nachlesen über v_equi                                         --*

  CASE SY-REPID.
*---                                       zquipmentselektion
    WHEN 'RIzqui20'.
      IF DY_PARNR IS INITIAL.
        PERFORM SELECT_V_EQUI_L.
      ELSE.
*---                                       Selektion über Partner
        CLEAR G_OBJNR_TAB.
        REFRESH G_OBJNR_TAB.
        SELECT OBJNR FROM IHPA INTO TABLE G_OBJNR_TAB
                           WHERE PARNR = DY_PARNR
                           AND   PARVW = DY_PARVW
                           AND   OBTYP = 'IEQ'
                           AND   KZLOESCH = ' '.
        IF SY-SUBRC IS INITIAL.
          PERFORM SELECT_V_EQUI_OBJ_L.
        ENDIF.
      ENDIF.
    WHEN 'RIzqui21'.
*---                                          Serialnummernselektion
      IF DY_PARNR IS INITIAL.
        PERFORM SELECT_ZQUI_ONLY_L.
      ELSE.
*---                                          Selektion über Partner
        CLEAR G_OBJNR_TAB.
        REFRESH G_OBJNR_TAB.
        SELECT OBJNR FROM IHPA INTO TABLE G_OBJNR_TAB
                           WHERE PARNR = DY_PARNR
                           AND   PARVW = DY_PARVW
                           AND   OBTYP = 'ISE'
                           AND   KZLOESCH = ' '.
        IF SY-SUBRC IS INITIAL.
          PERFORM SELECT_ZQUI_ONLY_OBJ_L.
        ENDIF.
      ENDIF.
      PERFORM POST_READ_V_EQUI_L.
  ENDCASE.

  DESCRIBE TABLE OBJECT_TAB LINES H.
  CHECK NOT H IS INITIAL.

  PERFORM DELETE_WRONG_LANG_L.
  PERFORM STATUS_CHECK_F10 USING SELSCHEM.
  PERFORM AUTHORITY-CHECK_L.
  PERFORM FILL_OBJECT_TAB_L.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM WRITE_L                                                  *
*---------------------------------------------------------------------*
*       Felder aus rihequi werden ausgegeben                          *
*---------------------------------------------------------------------*
*  -->  F_FELDNAME                                                    *
*---------------------------------------------------------------------*
FORM WRITE_L USING
             F_FELDNAME.

  DATA: BEGIN OF H_FIELDNAME,
        FILLER(11) VALUE 'OBJECT_TAB-',
        FIELDNAME LIKE DFIES-TABNAME,
      END OF H_FIELDNAME.

  FIELD-SYMBOLS:
    <H_FIELD>.

  CASE F_FELDNAME.
    WHEN 'EQUNR'.
      WRITE RIHEQUI-EQUNR.
    WHEN 'SPRAS'.
      WRITE RIHEQUI-SPRAS.
    WHEN 'EQKTX'.
      WRITE RIHEQUI-EQKTX.
    WHEN 'EQTYP'.
      WRITE RIHEQUI-EQTYP.
    WHEN 'IWERK'.
      WRITE RIHEQUI-IWERK.
    WHEN 'SWERK'.
      WRITE RIHEQUI-SWERK.
    WHEN 'STORT'.
      WRITE RIHEQUI-STORT.
    WHEN 'MSGRP'.
      WRITE RIHEQUI-MSGRP.
    WHEN 'BEBER'.
      WRITE RIHEQUI-BEBER.
    WHEN 'ABCKZ'.
      WRITE RIHEQUI-ABCKZ.
    WHEN 'EQFNR'.
      WRITE RIHEQUI-EQFNR.
    WHEN 'BUKRS'.
      WRITE RIHEQUI-BUKRS.
    WHEN 'ANLNR'.
      WRITE RIHEQUI-ANLNR.
    WHEN 'ANLUN'.
      WRITE RIHEQUI-ANLUN.
    WHEN 'GSBER'.
      WRITE RIHEQUI-GSBER.
    WHEN 'KOSTL'.
      WRITE RIHEQUI-KOSTL.
    WHEN 'DAUFN'.
      WRITE RIHEQUI-DAUFN.
    WHEN 'AUFNR'.
      WRITE RIHEQUI-AUFNR.
    WHEN 'TPLNR'.
      WRITE RIHEQUI-TPLNR.
    WHEN 'TIDNR'.
      WRITE RIHEQUI-TIDNR.
    WHEN 'Hzqui'.
      WRITE RIHEQUI-HEQUI.
    WHEN 'HEQNR'.
      WRITE RIHEQUI-HEQNR.
    WHEN 'SUBMT'.
      WRITE RIHEQUI-SUBMT.
    WHEN 'SERNR'.
      WRITE RIHEQUI-SERNR.
    WHEN 'MAPAR'.
      WRITE RIHEQUI-MAPAR.
    WHEN 'INGRP'.
      WRITE RIHEQUI-INGRP.
    WHEN 'ELIEF'.
      WRITE RIHEQUI-ELIEF.
    WHEN 'ANSWT'.
      WRITE RIHEQUI-ANSWT CURRENCY RIHEQUI-WAERS.
    WHEN 'KRFKZ'.
      WRITE RIHEQUI-KRFKZ.
    WHEN 'WAERS'.
      WRITE RIHEQUI-WAERS.
    WHEN 'INVNR'.
      WRITE RIHEQUI-INVNR.
    WHEN 'GROES'.
      WRITE RIHEQUI-GROES.
    WHEN 'BRGEW'.
      WRITE RIHEQUI-BRGEW UNIT RIHEQUI-GEWEI.
    WHEN 'BAUJJ'.
      WRITE RIHEQUI-BAUJJ.
    WHEN 'BAUMM'.
      WRITE RIHEQUI-BAUMM.
    WHEN 'HERST'.
      WRITE RIHEQUI-HERST.
    WHEN 'HERLD'.
      WRITE RIHEQUI-HERLD.
    WHEN 'HZEIN'.
      WRITE RIHEQUI-HZEIN.
    WHEN 'SERGE'.
      WRITE RIHEQUI-SERGE.
    WHEN 'KUND1'.
      WRITE RIHEQUI-KUND1.
    WHEN 'KUND2'.
      WRITE RIHEQUI-KUND2.
    WHEN 'KUND3'.
      WRITE RIHEQUI-KUND3.
    WHEN 'LIZNR'.
      WRITE RIHEQUI-LIZNR.
    WHEN 'ARBPL'.
      WRITE RIHEQUI-ARBPL.
    WHEN 'GEWRK'.
      WRITE RIHEQUI-GEWRK.
    WHEN 'ERDAT'.
      WRITE RIHEQUI-ERDAT.
    WHEN 'ERNAM'.
      WRITE RIHEQUI-ERNAM.
    WHEN 'AEDAZ'.
      WRITE RIHEQUI-AEDAZ.
    WHEN 'AENAZ'.
      WRITE RIHEQUI-AENAZ.
    WHEN 'DATAB'.
      WRITE RIHEQUI-DATAB.
    WHEN 'BEGRU'.
      WRITE RIHEQUI-BEGRU.
    WHEN 'KMATN'.
      WRITE RIHEQUI-KMATN.
    WHEN 'MATNR'.
      WRITE RIHEQUI-MATNR.
    WHEN 'WERK'.
      WRITE RIHEQUI-WERK.
    WHEN 'TYPBZ'.
      WRITE RIHEQUI-TYPBZ.
    WHEN 'LAGER'.
      WRITE RIHEQUI-LAGER.
    WHEN 'CHARGE'.
      WRITE RIHEQUI-CHARGE.
    WHEN 'KUNDE'.
      WRITE RIHEQUI-KUNDE.
    WHEN 'EQBER'.
      WRITE RIHEQUI-EQBER.
    WHEN 'DATBI'.
      WRITE RIHEQUI-DATBI.
    WHEN 'STTXT'.
      WRITE RIHEQUI-STTXT.
    WHEN 'ILOAN'.
      WRITE RIHEQUI-ILOAN.
    WHEN 'USTXT'.
      WRITE RIHEQUI-USTXT.
    WHEN 'OBJNR'.
      WRITE RIHEQUI-OBJNR.
    WHEN 'RBNR'.
      WRITE RIHEQUI-RBNR.
    WHEN 'PROID'.
      WRITE RIHEQUI-PROID.
    WHEN 'EQART'.
      WRITE RIHEQUI-EQART.
    WHEN 'ANSDT'.
      WRITE RIHEQUI-ANSDT.
    WHEN 'VKORG'.
      WRITE RIHEQUI-VKORG.
    WHEN 'VTWEG'.
      WRITE RIHEQUI-VTWEG.
    WHEN 'SPART'.
      WRITE RIHEQUI-SPART.
    WHEN 'ADRNR'.
      WRITE RIHEQUI-ADRNR.
    WHEN 'MANDT'.
      WRITE RIHEQUI-MANDT.
    WHEN 'NAME_LIST'.
      WRITE RIHEQUI-NAME_LIST.
    WHEN 'TEL_NUMBER'.
      WRITE RIHEQUI-TEL_NUMBER.
    WHEN 'POST_CODE1'.
      WRITE RIHEQUI-POST_CODE1.
    WHEN 'CITY1'.
      WRITE RIHEQUI-CITY1.
    WHEN 'CITY2'.
      WRITE RIHEQUI-CITY2.
    WHEN 'COUNTRY'.
      WRITE RIHEQUI-COUNTRY.
    WHEN 'REGION'.
      WRITE RIHEQUI-REGION.
    WHEN 'STREET'.
      WRITE RIHEQUI-STREET.
    WHEN 'RKEOBJNR'.
      WRITE RIHEQUI-RKEOBJNR.
    WHEN 'CUOBJ'.
      WRITE RIHEQUI-CUOBJ.
    WHEN 'GEWEI'.
      WRITE RIHEQUI-GEWEI.
    WHEN 'KOKRS'.
      WRITE RIHEQUI-KOKRS.
    WHEN OTHERS.
      H_FIELDNAME-FIELDNAME = F_FELDNAME.
      ASSIGN (H_FIELDNAME) TO <H_FIELD>.
      WRITE <H_FIELD>.
  ENDCASE.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM DELETE_WRONG_LANG_L                                      *
*---------------------------------------------------------------------*
*       Einträge in überflüssigen Sprachen entfernen                  *
*---------------------------------------------------------------------*
FORM DELETE_WRONG_LANG_L.

*### Datenvereinbarungen ############################################*
  DATA: H_TABIX LIKE SY-TABIX,
        H_EQUNR LIKE RIHEQUI-EQUNR.

*### Verarbeitung ###################################################*
  SORT OBJECT_TAB BY EQUNR.
  LOOP AT OBJECT_TAB WHERE SPRAS <> SY-LANGU.
    H_EQUNR = OBJECT_TAB-EQUNR.
    H_TABIX = SY-TABIX + 1.
    READ TABLE OBJECT_TAB INDEX H_TABIX.
    IF H_EQUNR = OBJECT_TAB-EQUNR AND SY-SUBRC = 0.
      DELETE OBJECT_TAB.
    ELSE.
      H_TABIX = H_TABIX - 2.
      IF H_TABIX > 0.
        READ TABLE OBJECT_TAB INDEX H_TABIX.
        IF H_EQUNR = OBJECT_TAB-EQUNR.
          DELETE OBJECT_TAB.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM MOVE_v_equi_TO_OBJECT_TAB_L                              *
*---------------------------------------------------------------------*
*       Move von v_equi nach OBJECT_TAB                               *
*       mit versorgen aller besonders zu ermittelnden Felder          *
*---------------------------------------------------------------------*
FORM MOVE_V_EQUI_TO_OBJECT_TAB_L.

  MOVE-CORRESPONDING V_EQUI TO OBJECT_TAB.
  OBJECT_TAB-OBJNR = V_EQUI-OBJNR.
  L_JSTO_PRE_TAB = V_EQUI-OBJNR.
  APPEND L_JSTO_PRE_TAB.
  OBJECT_TAB-PPSID = V_EQUI-PPSID.
  IF NOT V_EQUI-PPSID IS INITIAL.
    L_TARBID-MANDT = V_EQUI-MANDT.
    L_TARBID-OBJTY = 'A '.
    L_TARBID-OBJID = V_EQUI-PPSID.
    COLLECT L_TARBID.
  ENDIF.
  OBJECT_TAB-IGEWRK = V_EQUI-GEWRK.
  IF NOT V_EQUI-GEWRK IS INITIAL.
    L_TARBID-MANDT = V_EQUI-MANDT.
    L_TARBID-OBJTY = 'A '.
    L_TARBID-OBJID = V_EQUI-GEWRK.
    COLLECT L_TARBID.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FILL_OBJECT_TAB_LATE_L                                   *
*---------------------------------------------------------------------*
*       Sonderbehandlung Felder nach Ausflug                          *
*---------------------------------------------------------------------*
FORM FILL_OBJECT_TAB_LATE_L.

  DATA: BEGIN OF H_SADR.
          INCLUDE STRUCTURE SADR.
  DATA: END OF H_SADR.

  DATA: BEGIN OF H_VTCOM.
          INCLUDE STRUCTURE VTCOM.
  DATA: END OF H_VTCOM.

  DATA: BEGIN OF H_KUPAV.
          INCLUDE STRUCTURE KUPAV.
  DATA: END OF H_KUPAV.
*--- Puffer zurücksetzten damit neuester Stand gelesen wird ---------*
  CALL FUNCTION 'STATUS_BUFFER_REFRESH'.
*--- Statusleiste füllen ---------------------------------------------*
  IF NOT OBJECT_TAB-OBJNR IS INITIAL.
    IF G_STTXT_FLAG = YES OR G_STTXT_FLAG = OK.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
           EXPORTING
                OBJNR            = OBJECT_TAB-OBJNR
                FLG_USER_STAT    = 'X'
                SPRAS            = SY-LANGU
           IMPORTING
                LINE             = OBJECT_TAB-STTXT
                USER_LINE        = OBJECT_TAB-USTXT
           EXCEPTIONS
                OBJECT_NOT_FOUND = 01.
      IF SY-SUBRC IS INITIAL.
        MODIFY OBJECT_TAB.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Externe Arbeitsplatznummer bestimmen ----------------------------*
  IF G_ARBPL_FLAG = YES OR G_ARBPL_FLAG = OK.
    IF NOT OBJECT_TAB-PPSID IS INITIAL.
      CALL FUNCTION 'CR_WORKSTATION_READ'
           EXPORTING
                ID        = OBJECT_TAB-PPSID
                MSGTY     = 'S'
           IMPORTING
                ARBPL     = OBJECT_TAB-ARBPL
           EXCEPTIONS
                NOT_FOUND = 01.
      IF SY-SUBRC IS INITIAL.
        MODIFY OBJECT_TAB.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Externes Leitgewerk bestimmen -----------------------------------*
  IF G_GEWRK_FLAG = YES OR G_GEWRK_FLAG = OK.
    IF NOT OBJECT_TAB-IGEWRK IS INITIAL.
      CALL FUNCTION 'CR_WORKSTATION_READ'
           EXPORTING
                ID        = OBJECT_TAB-IGEWRK
                MSGTY     = 'S'
           IMPORTING
                ARBPL     = OBJECT_TAB-GEWRK
           EXCEPTIONS
                NOT_FOUND = 01.
      IF SY-SUBRC IS INITIAL.
        MODIFY OBJECT_TAB.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Adresse bestimmen -----------------------------------------------*
  IF G_ADRES_FLAG = YES OR G_ADRES_FLAG = OK.
    IF NOT OBJECT_TAB-ADRNR IS INITIAL.
      CALL FUNCTION 'ADDRESS_GET_DATA'
           EXPORTING
                ENTRY_ADRNR    = OBJECT_TAB-ADRNR
           IMPORTING
                COMPANY_DATA   = H_SADR
           EXCEPTIONS
                ADDR_NOT_EXIST = 01
                WRONG_DATA     = 02.
      IF SY-SUBRC IS INITIAL.
        OBJECT_TAB-TEL_NUMBER = H_SADR-TELF1.
        OBJECT_TAB-NAME_LIST  = H_SADR-NAME1.
        OBJECT_TAB-POST_CODE1 = H_SADR-PSTLZ.
        OBJECT_TAB-CITY1      = H_SADR-ORT01.
        OBJECT_TAB-CITY2      = H_SADR-ORT02.
        OBJECT_TAB-COUNTRY    = H_SADR-LAND1.
        OBJECT_TAB-REGION     = H_SADR-REGIO.
        OBJECT_TAB-STREET     = H_SADR-STRAS.
      ENDIF.
    ELSE.
      IF NOT OBJECT_TAB-KUND3 IS INITIAL.
        H_VTCOM-KUNNR = OBJECT_TAB-KUND3.
        H_VTCOM-MSGKZ = ' '.
        H_VTCOM-PARVW = 'AG'.
        CALL FUNCTION 'VIEW_KUPAV'
             EXPORTING
                  COMWA    = H_VTCOM
             IMPORTING
                  PAWA     = H_KUPAV
             EXCEPTIONS
                  NO_KNA1  = 01
                  NO_KNVK  = 02
                  NO_LFA1  = 03
                  NO_PERNR = 04.
        IF SY-SUBRC IS INITIAL.
          OBJECT_TAB-TEL_NUMBER = H_KUPAV-TELF1.
          OBJECT_TAB-NAME_LIST  = H_KUPAV-NAME1.
          OBJECT_TAB-POST_CODE1 = H_KUPAV-PSTLZ.
          OBJECT_TAB-CITY1      = H_KUPAV-ORT01.
          OBJECT_TAB-CITY2      = H_KUPAV-ORT02.
          OBJECT_TAB-COUNTRY    = H_KUPAV-LAND1.
          OBJECT_TAB-REGION     = H_KUPAV-REGIO.
          OBJECT_TAB-STREET     = H_KUPAV-STRAS.
        ENDIF.
      ELSE.
        CLEAR OBJECT_TAB-TEL_NUMBER .
        CLEAR OBJECT_TAB-POST_CODE1 .
        CLEAR OBJECT_TAB-CITY1      .
        CLEAR OBJECT_TAB-CITY2      .
        CLEAR OBJECT_TAB-COUNTRY    .
        CLEAR OBJECT_TAB-REGION     .
        CLEAR OBJECT_TAB-STREET     .
      ENDIF.
    ENDIF.
    MODIFY OBJECT_TAB.
  ENDIF.
ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM FILL_OBJECT_TAB_L                                        *
*---------------------------------------------------------------------*
*       Statustexte und Arbeitsplätze nachlesen                       *
*---------------------------------------------------------------------*
FORM FILL_OBJECT_TAB_L.

  DATA: BEGIN OF H_SADR.
          INCLUDE STRUCTURE SADR.
  DATA: END OF H_SADR.

  DATA: BEGIN OF H_VTCOM.
          INCLUDE STRUCTURE VTCOM.
  DATA: END OF H_VTCOM.

  DATA: BEGIN OF H_KUPAV.
          INCLUDE STRUCTURE KUPAV.
  DATA: END OF H_KUPAV.

  DATA: BEGIN OF H_MAKT OCCURS 0.
          INCLUDE STRUCTURE MAKT.
  DATA: END OF H_MAKT.

  DATA: H_MAT_ZQUI_TXT.
  DATA: H LIKE SY-TABIX.

  DATA: H_LINES1 LIKE SY-TABIX.
  DATA: H_LINES2 LIKE SY-TABIX.

*--- keine Objekte in Tabelle --> keine weitere Verarbeit. nötig --*
  DESCRIBE TABLE OBJECT_TAB LINES H.
  CHECK NOT H IS INITIAL.

*--- Wenn kein zqui-text vorhanden soll Materialtext gelesen werden ---*
  READ TABLE HEADER_TAB WITH KEY 'EQKTX'.
  IF HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO.
    H_MAT_ZQUI_TXT = NO.
  ELSE.
    H_MAT_ZQUI_TXT = YES.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'STTXT'.
  IF HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO .
    READ TABLE HEADER_TAB WITH KEY 'USTXT'.
    IF HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO .
      G_STTXT_FLAG = NO.
    ELSE.
      IF G_STTXT_FLAG <> OK.
        G_STTXT_FLAG = YES.
      ENDIF.
    ENDIF.
  ELSE.
    IF G_STTXT_FLAG <> OK.
      G_STTXT_FLAG = YES.
    ENDIF.
  ENDIF.

  READ TABLE HEADER_TAB WITH KEY 'ARBPL'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO )
    AND ARBPL IS INITIAL.
    G_ARBPL_FLAG = NO.
  ELSE.
    IF G_ARBPL_FLAG <> OK.
      G_ARBPL_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'GEWRK'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO )
    AND GEWRK IS INITIAL AND G_ARBPL_FLAG = NO.
    G_GEWRK_FLAG = NO.
  ELSE.
    IF G_GEWRK_FLAG <> OK.
      G_GEWRK_FLAG = YES.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE STAI1 LINES G_STAI1_LINES.
  DESCRIBE TABLE STAE1 LINES G_STAE1_LINES.
  IF G_STAI1_LINES IS INITIAL AND
     G_STAE1_LINES IS INITIAL AND
    G_STASL_FLAG = NO.
  ELSE.
    IF G_STASL_FLAG <> OK.
      G_STASL_FLAG = YES.
    ENDIF.
  ENDIF.

*--- Adresse erforderlich ? ----------------------------------------
  G_ADRES_FLAG = NO.
  READ TABLE HEADER_TAB WITH KEY 'TEL_NUMBER'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'NAME_LIST'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'POST_CODE1'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'CITY1'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'COUNTRY'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'REGION'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.
  READ TABLE HEADER_TAB WITH KEY 'STREET'.
  IF ( HEADER_TAB-DISPLAY = NEVER OR HEADER_TAB-DISPLAY = NO ).
  ELSE.
    IF G_ADRES_FLAG <> OK.
      G_ADRES_FLAG = YES.
    ENDIF.
  ENDIF.

*--- Prefetch -------------------------------------------------------
  IF G_ARBPL_FLAG = YES.
    CALL FUNCTION 'CR_WORKCENTER_PRE_READ'
         TABLES
              TARBID = L_TARBID.
    FREE L_TARBID.
  ENDIF.

*--- Status vorlesen -------------------------------------------------*
  IF G_STASL_FLAG = YES OR G_STTXT_FLAG = YES.
    CALL FUNCTION 'STATUS_BUFFER_REFRESH'.
    CALL FUNCTION 'STATUS_PRE_READ'
         TABLES
              JSTO_PRE_TAB = L_JSTO_PRE_TAB.
    FREE L_JSTO_PRE_TAB.
  ENDIF.
*--- Materialbezeichnungen werden gelesen ----------------------------*
  IF H_MAT_ZQUI_TXT = YES.
    SELECT * FROM MAKT FOR ALL ENTRIES IN OBJECT_TAB
                   WHERE MATNR = OBJECT_TAB-MATNR
                   AND  SPRAS = SY-LANGU.
      READ TABLE H_MAKT WITH KEY MATNR = MAKT-MATNR
                                 SPRAS = MAKT-SPRAS
                                 BINARY SEARCH.
      IF NOT ( SY-SUBRC IS INITIAL ).
        H_MAKT = MAKT.
        INSERT H_MAKT INDEX SY-TABIX.
      ENDIF.
    ENDSELECT.
  ENDIF.

  LOOP AT OBJECT_TAB.

*--- Status prüfen ---------------------------------------------------*
    IF G_STASL_FLAG = YES.
      PERFORM STATUS_PROOF_L USING G_ANSWER.
      IF G_ANSWER = NO.
        DELETE OBJECT_TAB.
      ENDIF.
      CHECK G_ANSWER = YES.
    ENDIF.
*
*--- Statusleiste füllen ---------------------------------------------*
    IF G_STTXT_FLAG = YES AND NOT OBJECT_TAB-OBJNR IS INITIAL.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
           EXPORTING
                OBJNR            = OBJECT_TAB-OBJNR
                SPRAS            = SY-LANGU
                FLG_USER_STAT    = 'X'
           IMPORTING
                LINE             = OBJECT_TAB-STTXT
                USER_LINE        = OBJECT_TAB-USTXT
           EXCEPTIONS
                OBJECT_NOT_FOUND = 01.
      IF SY-SUBRC IS INITIAL.
        MODIFY OBJECT_TAB.
      ENDIF.
    ENDIF.

*--- Externe Arbeitsplatznummer bestimmen ----------------------------*
    IF G_ARBPL_FLAG = YES.
      IF NOT OBJECT_TAB-PPSID IS INITIAL.

        CALL FUNCTION 'CR_WORKSTATION_READ'
             EXPORTING
                  ID        = OBJECT_TAB-PPSID
                  MSGTY     = 'S'
             IMPORTING
                  ARBPL     = OBJECT_TAB-ARBPL
             EXCEPTIONS
                  NOT_FOUND = 01.
        IF SY-SUBRC IS INITIAL.
          MODIFY OBJECT_TAB.
        ENDIF.
      ENDIF.
      IF NOT ARBPL IS INITIAL.
        IF NOT OBJECT_TAB-ARBPL IN ARBPL.
          DELETE OBJECT_TAB.
          CHECK 1 = 2.                 "Zum nächsten Objekt
        ENDIF.
      ENDIF.
    ENDIF.

*--- Externes Leitgewerk bestimmen -----------------------------------*
    IF G_GEWRK_FLAG = YES.
      IF NOT OBJECT_TAB-IGEWRK IS INITIAL.
        CALL FUNCTION 'CR_WORKSTATION_READ'
             EXPORTING
                  ID        = OBJECT_TAB-IGEWRK
                  MSGTY     = 'S'
             IMPORTING
                  ARBPL     = OBJECT_TAB-GEWRK
             EXCEPTIONS
                  NOT_FOUND = 01.
        IF SY-SUBRC IS INITIAL.
          MODIFY OBJECT_TAB.
        ENDIF.
      ENDIF.
      IF NOT GEWRK IS INITIAL.
        IF NOT OBJECT_TAB-GEWRK IN GEWRK.
          DELETE OBJECT_TAB.
          CHECK 1 = 2.
        ENDIF.
      ENDIF.
    ENDIF.

*--- Adresse bestimmen -----------------------------------------------*
    IF G_ADRES_FLAG = YES OR G_ADRES_FLAG = OK.
      IF NOT OBJECT_TAB-ADRNR IS INITIAL.
        CALL FUNCTION 'ADDRESS_GET_DATA'
             EXPORTING
                  ENTRY_ADRNR    = OBJECT_TAB-ADRNR
             IMPORTING
                  COMPANY_DATA   = H_SADR
             EXCEPTIONS
                  ADDR_NOT_EXIST = 01
                  WRONG_DATA     = 02.
        IF SY-SUBRC IS INITIAL.
          OBJECT_TAB-TEL_NUMBER = H_SADR-TELF1.
          OBJECT_TAB-NAME_LIST  = H_SADR-NAME1.
          OBJECT_TAB-POST_CODE1 = H_SADR-PSTLZ.
          OBJECT_TAB-CITY1      = H_SADR-ORT01.
          OBJECT_TAB-CITY2      = H_SADR-ORT02.
          OBJECT_TAB-COUNTRY    = H_SADR-LAND1.
          OBJECT_TAB-REGION     = H_SADR-REGIO.
          OBJECT_TAB-STREET     = H_SADR-STRAS.
        ENDIF.
      ELSE.
        IF NOT OBJECT_TAB-KUND3 IS INITIAL.
          H_VTCOM-KUNNR = OBJECT_TAB-KUND3.
          H_VTCOM-MSGKZ = ' '.
          H_VTCOM-PARVW = 'AG'.
          CALL FUNCTION 'VIEW_KUPAV'
               EXPORTING
                    COMWA    = H_VTCOM
               IMPORTING
                    PAWA     = H_KUPAV
               EXCEPTIONS
                    NO_KNA1  = 01
                    NO_KNVK  = 02
                    NO_LFA1  = 03
                    NO_PERNR = 04.
          IF SY-SUBRC IS INITIAL.
            OBJECT_TAB-TEL_NUMBER = H_KUPAV-TELF1.
            OBJECT_TAB-NAME_LIST  = H_KUPAV-NAME1.
            OBJECT_TAB-POST_CODE1 = H_KUPAV-PSTLZ.
            OBJECT_TAB-CITY1      = H_KUPAV-ORT01.
            OBJECT_TAB-CITY2      = H_KUPAV-ORT02.
            OBJECT_TAB-COUNTRY    = H_KUPAV-LAND1.
            OBJECT_TAB-REGION     = H_KUPAV-REGIO.
            OBJECT_TAB-STREET     = H_KUPAV-STRAS.
          ENDIF.
        ELSE.
          CLEAR OBJECT_TAB-TEL_NUMBER .
          CLEAR OBJECT_TAB-POST_CODE1 .
          CLEAR OBJECT_TAB-CITY1      .
          CLEAR OBJECT_TAB-CITY2      .
          CLEAR OBJECT_TAB-COUNTRY    .
          CLEAR OBJECT_TAB-REGION     .
          CLEAR OBJECT_TAB-STREET     .
        ENDIF.
      ENDIF.
      MODIFY OBJECT_TAB.
    ENDIF.
*--- zqui-Text nicht vorhanden --> Text von Material in zqui-Text ----*
*--- stellen (nur sinnvoll für Serialnummern)                   ----*
    IF OBJECT_TAB-EQKTX IS INITIAL AND NOT OBJECT_TAB-MATNR IS INITIAL
                                   AND H_MAT_ZQUI_TXT = YES.
      READ TABLE H_MAKT WITH KEY MATNR = OBJECT_TAB-MATNR
                                 SPRAS = SY-LANGU
                                 BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        OBJECT_TAB-EQKTX = H_MAKT-MAKTX.
        OBJECT_TAB-SPRAS = H_MAKT-SPRAS.
        MODIFY OBJECT_TAB.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF G_STASL_FLAG = YES.
    G_STASL_FLAG = OK.
  ENDIF.

  IF G_STTXT_FLAG = YES.
    G_STTXT_FLAG = OK.
  ENDIF.

  IF G_ARBPL_FLAG = YES.
    G_ARBPL_FLAG = OK.
  ENDIF.

  IF G_GEWRK_FLAG = YES.
    G_GEWRK_FLAG = OK.
  ENDIF.

  IF G_ADRES_FLAG = YES.
    G_ADRES_FLAG = OK.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM STATUS_PROOF_L                                           *
*---------------------------------------------------------------------*
*       Statusbedingungen überprüfen                                  *
*---------------------------------------------------------------------*
*  -->  F_ANSWER                                                      *
*---------------------------------------------------------------------*
FORM STATUS_PROOF_L USING F_ANSWER.

*### Datenvereinbarungen #############################################*
  DATA: BEGIN OF H_STATUS_TAB OCCURS 20.
          INCLUDE STRUCTURE JSTAT.
  DATA: END OF H_STATUS_TAB.

  DATA: BEGIN OF H_STATUS_TEXT_TAB OCCURS 20,
          TXT04 LIKE TJ02T-TXT04.
  DATA: END OF H_STATUS_TEXT_TAB.
  DATA: H_STAT_FLAG.

*### Verarbeitung ###################################################*
  IF OBJECT_TAB-OBJNR IS INITIAL.
    F_ANSWER = NO.
    EXIT.
  ENDIF.

  REFRESH: H_STATUS_TAB,
           H_STATUS_TEXT_TAB.
  CALL FUNCTION 'STATUS_READ'
       EXPORTING
            OBJNR            = OBJECT_TAB-OBJNR
            ONLY_ACTIVE      = 'X'
       TABLES
            STATUS           = H_STATUS_TAB
       EXCEPTIONS
            OBJECT_NOT_FOUND = 01.

*--- Texte zur Tabelle besorgen -------------------------------------
  LOOP AT H_STATUS_TAB.

    CALL FUNCTION 'STATUS_NUMBER_CONVERSION'
         EXPORTING
              LANGUAGE           = SY-LANGU
              OBJNR              = OBJECT_TAB-OBJNR
              STATUS_NUMBER      = H_STATUS_TAB-STAT
         IMPORTING
              TXT04              = H_STATUS_TEXT_TAB-TXT04
         EXCEPTIONS
*             INSUFFICIENT_INPUT                          "P30K037563
*             OBJECT_NOT_FOUND                            "P30K037563
*             STATUS_NOT_FOUND                            "P30K037563
*             STSMA_NOT_FOUND.                            "P30K037563
              OTHERS             = 01.                      "P30K037563
    IF SY-SUBRC = 0.
      APPEND H_STATUS_TEXT_TAB.
    ENDIF.
  ENDLOOP.

  F_ANSWER = NO.

*--- 1. Status inclusiv ---------------------------------------------
  IF NOT G_STAI1_LINES IS INITIAL.
    H_STAT_FLAG = ' '.
    LOOP AT H_STATUS_TEXT_TAB.
      CHECK H_STATUS_TEXT_TAB-TXT04 IN STAI1.
      H_STAT_FLAG = 'X'.
      EXIT.
    ENDLOOP.
    IF H_STAT_FLAG = ' '.
      EXIT.
    ENDIF.
  ENDIF.

*--- 1. Status exclusiv ---------------------------------------------
  IF NOT G_STAE1_LINES IS INITIAL.
    H_STAT_FLAG = ' '.
    LOOP AT H_STATUS_TEXT_TAB.
      CHECK H_STATUS_TEXT_TAB-TXT04 IN STAE1.
      H_STAT_FLAG = 'X'.
      EXIT.
    ENDLOOP.
    IF H_STAT_FLAG = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  F_ANSWER = YES.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM AUTHORITY-CHECK_L                                        *
*---------------------------------------------------------------------*
*       Berechtigungen prüfen                                         *
*---------------------------------------------------------------------*
FORM AUTHORITY-CHECK_L.

*### Datenvereinbarungen #############################################*
  DATA: H_NO_AUTH.

*### Verarbeitung ####################################################*
  H_NO_AUTH = NO.
  LOOP AT OBJECT_TAB.

    CALL FUNCTION 'INST_AUTHORITY_CHECK_ALL'
         EXPORTING
              BEGRP                    = OBJECT_TAB-BEGRU
              IWERK                    = OBJECT_TAB-IWERK
              SWERK                    = OBJECT_TAB-SWERK
              TCODE                    = 'IE03'
         EXCEPTIONS
              KEINE_BERECHTIGUNG_BEGRP = 01
              KEINE_BERECHTIGUNG_IWERK = 02
              KEINE_BERECHTIGUNG_SWERK = 03.

    IF SY-SUBRC <> 0.
      H_NO_AUTH = YES.
      DELETE OBJECT_TAB.
    ENDIF.
  ENDLOOP.

  IF H_NO_AUTH = YES.
    MESSAGE I046.
  ENDIF.

ENDFORM.


*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_NET1_L                                           *
*---------------------------------------------------------------------*
*       Verbindg. von/nach                                            *
*---------------------------------------------------------------------*
FORM DISPLAY_NET1_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IN19'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RINET0E0 WITH S_EQVON IN OBJECT
                      WITH DY_TCODE =  H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_NET2_L                                           *
*---------------------------------------------------------------------*
*       VerbindObjekt                                                 *
*---------------------------------------------------------------------*
FORM DISPLAY_NET2_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IN19'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RINET0E0 WITH S_EQKANT IN OBJECT
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_QMEL_L                                           *
*---------------------------------------------------------------------*
*       Meldungen                                                     *
*---------------------------------------------------------------------*
FORM DISPLAY_QMEL_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
*### Wenn in Serviceliste (zqui) dann in Serviceliste (Meld) #########*

  IF DY_TCODE = 'IE06' OR DY_TCODE = 'IH10'.
    H_TCODE = 'IW59'.
  ELSE.
    H_TCODE = 'IW29'.
  ENDIF.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIQMEL20 VIA SELECTION-SCREEN
                      WITH EQUNR IN OBJECT
                      WITH DY_OFN   = 'X'
                      WITH DY_RST   = 'X'
                      WITH DY_IAR   = 'X'
                      WITH DY_MAB   = 'X'
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_AUFK_L                                           *
*---------------------------------------------------------------------*
*       Aufträge                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_AUFK_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
*### Wenn in Serviceliste (zqui) dann in Serviceliste (Auft) #########*

  IF DY_TCODE = 'IE06' OR DY_TCODE = 'IH10'.
    H_TCODE = 'IW73'.
  ELSE.
    H_TCODE = 'IW39'.
  ENDIF.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIAUFK20 VIA SELECTION-SCREEN
                      WITH EQUNR IN OBJECT
                      WITH DY_OFN   = 'X'
                      WITH DY_IAR   = 'X'
                      WITH DY_MAB   = 'X'
                      WITH DY_HIS   = 'X'
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_MHIS_L                                           *
*---------------------------------------------------------------------*
*       Wartungsplanvorschau                                          *
*---------------------------------------------------------------------*
FORM DISPLAY_MHIS_L.

  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING 'IP19'
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIMHIS00 WITH EQUNR IN OBJECT
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_MPOS_L                                           *
*---------------------------------------------------------------------*
*       Wartungspositionen                                            *
*---------------------------------------------------------------------*
FORM DISPLAY_MPOS_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IP18'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIMPOS00 WITH EQUNR IN OBJECT
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_PLKO_L                                           *
*---------------------------------------------------------------------*
*       Arbeitspläne                                                  *
*---------------------------------------------------------------------*
FORM DISPLAY_PLKO_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IA09'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIPLKO10 WITH PN_EQUNR IN OBJECT
                      WITH PN_ZQUI = 'X'
                      WITH PN_IHAN = ' '
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_PLAN_L                                           *
*---------------------------------------------------------------------*
*       Arbeitsanleitungen                                            *
*---------------------------------------------------------------------*
FORM DISPLAY_PLAN_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IA09'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_SUBMT_L.
    IF NOT R_SUBMT IS INITIAL.
      SUBMIT RIPLKO10 WITH ISTRU IN R_SUBMT
                      WITH PN_ZQUI = ' '
                      WITH PN_IHAN = 'X'
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_MPTS_L                                           *
*---------------------------------------------------------------------*
*       Meßpunkte                                                     *
*---------------------------------------------------------------------*
FORM DISPLAY_MPTS_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IK07'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIIMPT20 WITH EQUNR IN OBJECT
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM DISPLAY_VERT_L                                           *
*---------------------------------------------------------------------*
*       Verträge                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_VERT_L.

*### Datenvereinbarungen #############################################*
  DATA: H_TCODE LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

*### Verarbeitung ####################################################*
  H_TCODE = 'IW75'.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIVEDA20 WITH EQUNR IN OBJECT
                      WITH DY_TCODE = H_TCODE
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM CREATE_RANGE_L                                           *
*---------------------------------------------------------------------*
*       Range mit selektierten Objekten erstellen                     *
*---------------------------------------------------------------------*
FORM CREATE_RANGE_L.

  CLEAR OBJECT.
  REFRESH OBJECT.
  LOOP AT OBJECT_TAB WHERE SELECTED = YES.
    CLEAR OBJECT.
    OBJECT-OPTION = 'EQ'.
    OBJECT-SIGN   = 'I'.
    OBJECT-LOW    = OBJECT_TAB-EQUNR.
    APPEND OBJECT.
    OBJECT_TAB-SELECTED = ASTERISK.
    MODIFY OBJECT_TAB.
  ENDLOOP.
  IF OBJECT IS INITIAL.
    MESSAGE I011.
  ENDIF.


ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM CREATE_RANGE_SUBMT_L                                     *
*---------------------------------------------------------------------*
*       Range mit selektierten Objekten erstellen                     *
*---------------------------------------------------------------------*
FORM CREATE_RANGE_SUBMT_L.

  CLEAR R_SUBMT.
  REFRESH R_SUBMT.
  LOOP AT OBJECT_TAB WHERE SELECTED = YES.
    IF NOT OBJECT_TAB-SUBMT IS INITIAL.
      CLEAR R_SUBMT.
      R_SUBMT-OPTION =  'EQ'.
      R_SUBMT-SIGN   = 'I'.
      R_SUBMT-LOW    = OBJECT_TAB-SUBMT.
      COLLECT R_SUBMT.
    ENDIF.
    OBJECT_TAB-SELECTED = ASTERISK.
    MODIFY OBJECT_TAB.
  ENDLOOP.
  IF R_SUBMT IS INITIAL.
    MESSAGE S047.
  ENDIF.


ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM MULTI_OBJECT_l                                           *
*---------------------------------------------------------------------*
*       zquipments mehrstufig                                         *
*---------------------------------------------------------------------*
FORM MULTI_OBJECT_L.

  DATA: H_RETC LIKE SY-SUBRC.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING 'IE07'
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIZQUI30 WITH EQUNR IN OBJECT
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM MULTI_QMEL_l                                             *
*---------------------------------------------------------------------*
*       Meldungen mehrstufig                                          *
*---------------------------------------------------------------------*
FORM MULTI_QMEL_L.

  DATA: H_RETC LIKE SY-SUBRC.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING 'IW30'
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIQMEL10 WITH EQUNR IN OBJECT
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM MULTI_AUFK_l                                             *
*---------------------------------------------------------------------*
*       Aufträge mehrstufig                                           *
*---------------------------------------------------------------------*
FORM MULTI_AUFK_L.

  DATA: H_RETC LIKE SY-SUBRC.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING 'IW40'
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    PERFORM CREATE_RANGE_L.
    IF NOT OBJECT IS INITIAL.
      SUBMIT RIAUFK10 WITH EQUNR IN OBJECT
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SELECT_zqui_ONLY
*&---------------------------------------------------------------------*
*       select ueber die tabelle zqui ONLY, um auch                    *
*       serialnummerstammsaetze zu bekommen, die nicht als volle
*       zquipmentstammsaetze vorliegen
*       Kennzeichen für Serialnummer: Matnr und Sernr nicht initial
*----------------------------------------------------------------------*
FORM SELECT_ZQUI_ONLY_L.

  RANGES: I_MATNR FOR ZQUI-MATNR.
  RANGES: I_SERNR FOR ZQUI-SERNR.

  REFRESH I_MATNR.
  REFRESH I_SERNR.

  I_MATNR-LOW = SPACE.
  I_MATNR-SIGN = 'I'.
  I_MATNR-OPTION = 'NE'.
  APPEND I_MATNR.

  I_SERNR-LOW = SPACE.
  I_SERNR-SIGN = 'I'.
  I_SERNR-OPTION = 'NE'.
  APPEND I_SERNR.

  SELECT * FROM ZQUI
       WHERE              EQUNR      IN EQUNR
                    AND   ERDAT      IN ERDAT
                    AND   ERNAM      IN ERNAM
*                   AND   EQASP      EQ SPACE
                    AND   AEDAT      IN AEDAT
                    AND   AENAM      IN AENAM
                    AND   BEGRU      IN BEGRU
                    AND   EQTYP      IN EQTYP
                    AND   EQART      IN EQART
                    AND   ANSDT      IN ANSDT
                    AND   ANSWT      IN ANSWT
                    AND   ELIEF      IN ELIEF
                    AND   HERST      IN HERST
                    AND   SERGE      IN SERGE
                    AND   TYPBZ      IN TYPBZ
                    AND   KRFKZ      LIKE KRFKZ
*                   AND   KRFKZ      EQ KRFKZ
                    AND   KMATN      IN KMATN
                    AND   MATNR      IN MATNR
                    AND   SERNR      IN SERNR
                    AND   WERK       IN WERK
                    AND   LAGER      IN LAGER
                    AND   CHARGE     IN CHARGE
                    AND   KUNDE      IN KUNDE
                    AND   MATNR      IN I_MATNR
                    AND   SERNR      IN I_SERNR
                    AND   HERLD      IN HERLD
                    AND   BAUJJ      IN BAUJJ
                    AND   INVNR      IN INVNR
                    AND   GROES      IN GROES
                    AND   BRGEW      IN BRGEW
                   AND    GEWEI      IN GEWEI.
    CLEAR V_EQUI.
*--- AEDAT und AENAM werden genommen, da kein Zeitsegment vorhanden --*
    MOVE ZQUI-AEDAT         TO V_EQUI-AEDAZ.
    MOVE ZQUI-AENAM         TO V_EQUI-AENAZ.
    MOVE-CORRESPONDING ZQUI TO V_EQUI.
    PERFORM MOVE_V_EQUI_TO_OBJECT_TAB_L.
    APPEND  OBJECT_TAB.
  ENDSELECT.

ENDFORM.                               " SELECT_zqui_ONLY

*eject
*&---------------------------------------------------------------------*
*&      Form  SELECT_zqui_ONLY_OBJ_L
*&---------------------------------------------------------------------*
*       select ueber die tabelle zqui ONLY, um auch                    *
*       serialnummerstammsaetze zu bekommen, die nicht als volle
*       zquipmentstammsaetze vorliegen
*
*----------------------------------------------------------------------*
FORM SELECT_ZQUI_ONLY_OBJ_L.

  RANGES: I_MATNR FOR ZQUI-MATNR.
  RANGES: I_SERNR FOR ZQUI-SERNR.

  REFRESH I_MATNR.
  REFRESH I_SERNR.

  I_MATNR-LOW = SPACE.
  I_MATNR-SIGN = 'I'.
  I_MATNR-OPTION = 'NE'.
  APPEND I_MATNR.

  I_SERNR-LOW = SPACE.
  I_SERNR-SIGN = 'I'.
  I_SERNR-OPTION = 'NE'.
  APPEND I_SERNR.

  SELECT * FROM ZQUI
               FOR ALL ENTRIES IN G_OBJNR_TAB
               WHERE OBJNR = G_OBJNR_TAB-OBJNR
                    AND   EQUNR      IN EQUNR
                    AND   ERDAT      IN ERDAT
                    AND   ERNAM      IN ERNAM
*                   AND   EQASP      EQ SPACE
                    AND   AEDAT      IN AEDAT
                    AND   AENAM      IN AENAM
                    AND   BEGRU      IN BEGRU
                    AND   EQTYP      IN EQTYP
                    AND   EQART      IN EQART
                    AND   ANSDT      IN ANSDT
                    AND   ANSWT      IN ANSWT
                    AND   ELIEF      IN ELIEF
                    AND   HERST      IN HERST
                    AND   SERGE      IN SERGE
                    AND   TYPBZ      IN TYPBZ
                    AND   KRFKZ      LIKE KRFKZ
*                   AND   KRFKZ      EQ KRFKZ
                    AND   KMATN      IN KMATN
                    AND   MATNR      IN MATNR
                    AND   SERNR      IN SERNR
                    AND   WERK       IN WERK
                    AND   LAGER      IN LAGER
                    AND   CHARGE     IN CHARGE
                    AND   KUNDE      IN KUNDE
                    AND   MATNR      IN I_MATNR
                    AND   SERNR      IN I_SERNR
                    AND   HERLD      IN HERLD
                    AND   BAUJJ      IN BAUJJ
                    AND   INVNR      IN INVNR
                    AND   GROES      IN GROES
                    AND   BRGEW      IN BRGEW
                    AND   GEWEI      IN GEWEI.
    CLEAR V_EQUI.
    MOVE-CORRESPONDING ZQUI TO V_EQUI.
*--- AEDAT und AENAM werden genommen, da kein Zeitsegment vorhanden --*
    MOVE ZQUI-AEDAT         TO V_EQUI-AEDAZ.
    MOVE ZQUI-AENAM         TO V_EQUI-AENAZ.
    PERFORM MOVE_V_EQUI_TO_OBJECT_TAB_L.
    APPEND  OBJECT_TAB.
  ENDSELECT.

ENDFORM.                               " SELECT_zqui_ONLY

*eject
*---------------------------------------------------------------------*
*       FORM SELECT_v_equi_L                                          *
*---------------------------------------------------------------------*
*       Standardselektion zquipments                                  *
*---------------------------------------------------------------------*
FORM SELECT_V_EQUI_L.

  DATA: H_EQKTX LIKE EQKT-EQKTX.

*                  lesen ueber v_equi view fuer alle  zquipments
  SELECT * FROM V_EQUI WHERE EQUNR IN EQUNR
                       AND   EQART IN EQART
                       AND   INGRP IN INGRP
                       AND   RBNR  IN RBNR
                       AND   PROID IN PROID
                       AND   SUBMT IN SUBMT
                       AND   ERDAT IN ERDAT
                       AND   ERNAM IN ERNAM
                       AND   AEDAZ IN AEDAT
                       AND   AENAZ IN AENAM
                       AND   BEGRU IN BEGRU
                       AND   KRFKZ  LIKE KRFKZ
*                      AND   KRFKZ  EQ KRFKZ
                       AND   KMATN  IN KMATN
                       AND   MATNR  IN MATNR
                       AND   SERNR  IN SERNR
                       AND   WERK   IN WERK
                       AND   LAGER  IN LAGER
                       AND   CHARGE IN CHARGE
                       AND   KUNDE  IN KUNDE
                       AND   ANLNR IN ANLNR
*                      AND   GSBER IN GSBER
*                      AND   KOKRS IN KOKRS
                       AND   HEQUI IN HEQUI
                       AND   SWERK IN SWERK
                       AND   EQFNR IN EQFNR
                       AND   STORT IN STORT
                       AND   EQTYP IN EQTYP
                       AND   LIZNR IN LIZNR
                       AND   KOSTL IN KOSTL
                       AND   SERGE IN SERGE
*                      AND   BUKRS IN BUKRS
                       AND   TIDNR IN TIDNR
*                      AND   DAUFN IN DAUFN
                       AND   KUND2 IN KUND2
                       AND   ANLUN IN ANLUN
                       AND   IWERK IN IWERK
                       AND   TYPBZ IN TYPBZ
                       AND   BEBER IN BEBER
                       AND   MAPAR IN MAPAR
                       AND   KUND3 IN KUND3
                       AND   ELIEF IN ELIEF
                       AND   ABCKZ IN ABCKZ
                       AND   ANSWT IN ANSWT
                       AND   KUND1 IN KUND1
                       AND   MSGRP IN MSGRP
                       AND   HERST IN HERST
                       AND   ANSDT IN ANSDT
                       AND   HEQNR IN HEQNR
                       AND   TPLNR IN TPLNR
                       AND   VKORG IN VKORG
                       AND   VTWEG IN VTWEG
                       AND   SPART IN SPART
                       AND   ADRNR IN ADRNR
                       AND   DATAB <= DATUB
                       AND   DATBI >= DATUV
                       AND   HERLD  IN HERLD
                       AND   BAUJJ  IN BAUJJ
                       AND   INVNR  IN INVNR
                       AND   GROES  IN GROES
                       AND   BRGEW  IN BRGEW
                       AND   GEWEI  IN GEWEI.
*--- Wenn Selektion genau eines Tages, nur das zuletzt gültige EQUZ -*
    IF DATUV = DATUB.
      CHECK V_EQUI-DATBI > DATUB.
    ENDIF.
    H_EQKTX = V_EQUI-EQKTX.
    TRANSLATE H_EQKTX TO UPPER CASE.
    CHECK H_EQKTX IN EQKTX.

    CHECK V_EQUI-SPRAS EQ SY-LANGU OR V_EQUI-SPRAS EQ V_EQUI-EQASP.
    PERFORM MOVE_V_EQUI_TO_OBJECT_TAB_L.
    APPEND OBJECT_TAB.
  ENDSELECT.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM SELECT_v_equi_OBJ_L                                      *
*---------------------------------------------------------------------*
*       v_equi mit Objektnummer lesen                                 *
*---------------------------------------------------------------------*
FORM SELECT_V_EQUI_OBJ_L.

  DATA: H_EQKTX LIKE EQKT-EQKTX.

  SELECT * FROM V_EQUI
           FOR ALL ENTRIES IN G_OBJNR_TAB
           WHERE OBJNR = G_OBJNR_TAB-OBJNR
           AND   EQUNR IN EQUNR
           AND   EQART IN EQART
           AND   INGRP IN INGRP
           AND   RBNR  IN RBNR
           AND   PROID IN PROID
           AND   SUBMT IN SUBMT
           AND   ERDAT IN ERDAT
           AND   ERNAM IN ERNAM
           AND   AEDAZ IN AEDAT
           AND   AENAZ IN AENAM
           AND   BEGRU IN BEGRU
           AND   KRFKZ LIKE KRFKZ
*          AND   KRFKZ EQ KRFKZ
           AND   KMATN IN KMATN
           AND   MATNR IN MATNR
           AND   SERNR IN SERNR
           AND   WERK  IN WERK
           AND   LAGER IN LAGER
           AND   CHARGE IN CHARGE
           AND   KUNDE IN KUNDE
    AND   ANLNR IN ANLNR
*   AND   GSBER IN GSBER
*   AND   KOKRS IN KOKRS
    AND   HEQUI IN HEQUI
    AND   SWERK IN SWERK
    AND   EQFNR IN EQFNR
    AND   STORT IN STORT
    AND   EQTYP IN EQTYP
    AND   LIZNR IN LIZNR
    AND   KOSTL IN KOSTL
*   AND   AUFNR IN AUFNR
    AND   SERGE IN SERGE
    AND   TIDNR IN TIDNR
*   AND   DAUFN IN DAUFN
    AND   KUND2 IN KUND2
    AND   ANLUN IN ANLUN
*   AND   BUKRS IN BUKRS
    AND   IWERK IN IWERK
    AND   TYPBZ IN TYPBZ
    AND   BEBER IN BEBER
    AND   MAPAR IN MAPAR
    AND   KUND3 IN KUND3
    AND   ELIEF IN ELIEF
    AND   ABCKZ IN ABCKZ
    AND   ANSWT IN ANSWT
    AND   KUND1 IN KUND1
    AND   MSGRP IN MSGRP
    AND   HERST IN HERST
    AND   ANSDT IN ANSDT
    AND   HEQNR IN HEQNR
    AND   TPLNR IN TPLNR
    AND   VKORG IN VKORG
    AND   VTWEG IN VTWEG
    AND   SPART IN SPART
    AND   ADRNR IN ADRNR
    AND   DATAB <= DATUB
    AND   DATBI >= DATUV
    AND   HERLD  IN HERLD
    AND   BAUJJ  IN BAUJJ
    AND   INVNR  IN INVNR
    AND   GROES  IN GROES
    AND   BRGEW  IN BRGEW
    AND   GEWEI  IN GEWEI.
*--- Wenn Selektion genau eines Tages, nur das zuletzt gültige EQUZ -*
    IF DATUV = DATUB.
      CHECK V_EQUI-DATBI > DATUB.
    ENDIF.
    H_EQKTX = V_EQUI-EQKTX.
    TRANSLATE H_EQKTX TO UPPER CASE.
    CHECK H_EQKTX IN EQKTX.

    CHECK V_EQUI-SPRAS EQ SY-LANGU OR V_EQUI-SPRAS EQ V_EQUI-EQASP.
    PERFORM MOVE_V_EQUI_TO_OBJECT_TAB_L.
    APPEND OBJECT_TAB.
  ENDSELECT.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM SEL_SERIAL_VIA_STATUS                                    *
*---------------------------------------------------------------------*
*       Sonderfall Selektion von Serialnummern/zquis über             *
*       erlaubte/nicht erlaubte Status                                *
*---------------------------------------------------------------------*
FORM SEL_SERIAL_VIA_STATUS.

  DATA: BEGIN OF STATUS_PERMITTING OCCURS 1.
          INCLUDE STRUCTURE TJ07.
  DATA: END OF STATUS_PERMITTING.

  DATA: BEGIN OF STATUS_NOT_PERMITTING OCCURS 1.
          INCLUDE STRUCTURE TJ07.
  DATA: END OF STATUS_NOT_PERMITTING.
  DATA: H_LANGU LIKE SY-LANGU,
        H_LANGU_FLAG,
        LIN TYPE P.

  IMPORT STATUS_PERMITTING STATUS_NOT_PERMITTING FROM MEMORY ID 'STA'.
  CHECK SY-SUBRC = 0.

  CLEAR: STAI1, STAE1.
  REFRESH: STAI1, STAE1.
  DESCRIBE TABLE STATUS_PERMITTING LINES LIN.
  IF LIN > 0.
    LOOP AT STATUS_PERMITTING.
      STAI1-OPTION = 'EQ'.
      STAI1-SIGN   = 'I'.
      CALL FUNCTION 'STATUS_NUMBER_CONVERSION'
           EXPORTING
                CLIENT             = SY-MANDT
                LANGUAGE           = SY-LANGU
                OBJNR              = ' '
                STATUS_NUMBER      = STATUS_PERMITTING-ISTAT
                STSMA              = ' '
           IMPORTING
                LANGUAGE_FOUND     = H_LANGU
                TXT04              = STAI1-LOW
           EXCEPTIONS
                INSUFFICIENT_INPUT = 01
                OBJECT_NOT_FOUND   = 02
                STATUS_NOT_FOUND   = 03
                STSMA_NOT_FOUND    = 04.
      IF SY-SUBRC <> 0.
        CLEAR STAI1.
      ELSE.
        APPEND STAI1.
      ENDIF.
      IF H_LANGU <> SY-LANGU.
        H_LANGU_FLAG = 'X'.
      ENDIF.
    ENDLOOP.
*   IF NOT H_LANGU_FLAG IS INITIAL.
*     MESSAGE W005.                      "Status nicht in Anmeldesprache
*   ENDIF.
  ENDIF.
  DESCRIBE TABLE STATUS_NOT_PERMITTING LINES LIN.
  IF LIN > 0.
    LOOP AT STATUS_NOT_PERMITTING.
      STAE1-OPTION = 'EQ'.
      STAE1-SIGN   = 'I'.
      CALL FUNCTION 'STATUS_NUMBER_CONVERSION'
           EXPORTING
                CLIENT             = SY-MANDT
                LANGUAGE           = SY-LANGU
                OBJNR              = ' '
                STATUS_NUMBER      = STATUS_NOT_PERMITTING-ISTAT
                STSMA              = ' '
           IMPORTING
                LANGUAGE_FOUND     = H_LANGU
                TXT04              = STAE1-LOW
           EXCEPTIONS
                INSUFFICIENT_INPUT = 01
                OBJECT_NOT_FOUND   = 02
                STATUS_NOT_FOUND   = 03
                STSMA_NOT_FOUND    = 04.
      IF SY-SUBRC <> 0.
        CLEAR STAE1.
      ELSE.
        APPEND STAE1.
      ENDIF.
      IF H_LANGU <> SY-LANGU.
        H_LANGU_FLAG = 'X'.
      ENDIF.
    ENDLOOP.
*   IF NOT H_LANGU_FLAG IS INITIAL.
*     MESSAGE W005.                      "Status nicht in Anmeldesprache
*   ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_INTENSIFIED_L
*&---------------------------------------------------------------------*
*       Bei Submit via Selection-Screen sollen bestimmte Felder        *
*       nicht mehr eingabebereit sein                                  *
*----------------------------------------------------------------------*
FORM HEADER_INTENSIFIED_L.

  IF NOT DY_MODE IS INITIAL.
    LOOP AT SCREEN.
      CHECK SCREEN-NAME(5) = 'EQUNR'
      OR SCREEN-NAME(7) = '%_EQUNR'
      OR SCREEN-NAME(5) = 'MATNR'
      OR SCREEN-NAME(7) = '%_MATNR'
      OR SCREEN-NAME(5) = 'STAE1'
      OR SCREEN-NAME(7) = '%_STAE1'
      OR SCREEN-NAME(5) = 'STAI1'
      OR SCREEN-NAME(7) = '%_STAI1'.
      SCREEN-INPUT  = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " HEADER_INTENSIFIED_L

*&---------------------------------------------------------------------*
*&      Form  POST_READ_v_equi_L
*&---------------------------------------------------------------------*
*       Für Serialnummern die kompletten zqui-Stammsatz haben wird
*       dieser nachgelesen.
*----------------------------------------------------------------------*
FORM POST_READ_V_EQUI_L.

  DATA: BEGIN OF H_VZQUI OCCURS 0.
          INCLUDE STRUCTURE V_EQUI.
  DATA: END OF H_VZQUI.
  DATA: H_OBJ(4).

  DESCRIBE TABLE OBJECT_TAB LINES SY-TABIX.
  CHECK NOT ( SY-TABIX IS INITIAL ).
  SORT OBJECT_TAB BY EQUNR.

*--- es wird der Satz mit dem aktuellen Zeitsegment selektiert     --*
  SELECT * FROM V_EQUI INTO TABLE H_VZQUI
           FOR ALL ENTRIES IN OBJECT_TAB
           WHERE EQUNR = OBJECT_TAB-EQUNR
             AND DATBI = '99991231'.

  LOOP AT H_VZQUI.
*--- zquibezeichnung in Anmeldesprache oder Anlegesprache nehmen     --*
    CHECK H_VZQUI-SPRAS EQ SY-LANGU OR H_VZQUI-SPRAS EQ H_VZQUI-EQASP.
    READ TABLE OBJECT_TAB WITH KEY EQUNR = H_VZQUI-EQUNR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING H_VZQUI TO OBJECT_TAB.
      MODIFY OBJECT_TAB INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.
*--- Sonderfall Matchcode 'zquis über Serialnummernliste' -> es     --*
*--- sollen nur vollständige zquis angezeigt werden obwohl die      --*
*--- die selektion über die Serialnummernliste erfolgt              --*
  IMPORT H_OBJ FROM MEMORY ID 'MC_OBJ'.
  IF H_OBJ = 'zqui'.
    DELETE OBJECT_TAB WHERE DATAB IS INITIAL AND DATBI IS INITIAL.
  ENDIF.
ENDFORM.                               " POST_READ_v_equi_L

*&---------------------------------------------------------------------*
*&      Form  MODIF_SELECTION_SCREEN_L
*&---------------------------------------------------------------------*
*       Bei Serialnummern wird Selektion nach Zeitraum und             *
*       Adresse ausgeblendet da die Felder nur bei vollständigen
*       zquipments vorhanden sind
*----------------------------------------------------------------------*
FORM MODIF_SELECTION_SCREEN_L.

  IF SY-REPID = 'RIzqui21'.
    LOOP AT SCREEN.
      CHECK SCREEN-NAME(8) = '%P005018'"bei RIzqui30
         OR SCREEN-NAME(8) = 'DY_ADRFL'"bei allen vier
         OR SCREEN-GROUP1  = 'EQI'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.



    ENDLOOP.
  ENDIF.
ENDFORM.                               " MODIF_SELECTION_SCREEN_L

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_TCODE_L
*&---------------------------------------------------------------------*
*&      Meldung / Auftrag aus zquiliste anlegen                        *
*&---------------------------------------------------------------------*
FORM EXECUTE_TCODE_L.

  DATA H_TCODE LIKE SY-TCODE.

  H_TCODE = SY-UCOMM.

*--- für mehrere objecte können Meldungen angelegt werden ------------*
  LOOP AT OBJECT_TAB WHERE SELECTED = YES.
*--- für reine Serialnummer kann keine Meldung/Auftrag angelegt werden*
    IF OBJECT_TAB-DATAB IS INITIAL AND
       OBJECT_TAB-DATBI IS INITIAL.
      MESSAGE I010.
      CHECK 1 = 2.
    ENDIF.
*--- ins Anlegen Aufträge oder Meldungen springen --------------------*
    IF H_TCODE = 'IW31'.
      PERFORM CREATE_ORDE_L.
    ELSE.
      PERFORM CREATE_QMEL_L USING H_TCODE.
    ENDIF.
*--- Zeile als ausgewählt kennzeichnen (Farbe)  ----------------------*
    OBJECT_TAB-SELECTED = ASTERISK.
    MODIFY OBJECT_TAB.
  ENDLOOP.

*--- Es wurde kein object markiert/ausgewählt  -----------------------*
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE S011.
  ENDIF.

ENDFORM.                               " EXECUTE_TCODE_QMEL_L

*&---------------------------------------------------------------------*
*&      Form  CREATE_QMEL_L using h_tcode
*&---------------------------------------------------------------------*
*       Eine Meldung wird angelegt                                     *
*----------------------------------------------------------------------*
*  -->  H_tcode   Transaction die aufgerufen wird
*----------------------------------------------------------------------*
FORM CREATE_QMEL_L USING H_TCODE LIKE SY-TCODE.

  TABLES T370T.

  DATA: H_TCODE_L LIKE SY-TCODE.
  DATA: H_RETC LIKE SY-SUBRC.

  SET PARAMETER ID 'IFL' FIELD OBJECT_TAB-TPLNR.
  SET PARAMETER ID 'EQN' FIELD OBJECT_TAB-EQUNR.
* SET PARAMETER ID 'MAT' FIELD OBJECT_TAB-SUBMT.

  H_TCODE_L =  H_TCODE.

*--- FCODE verbiegen bei Service wird aus IW24 -> IW54            --*
*---                             und aus  IW25 -> IW55            --*
*--- Kriterium ist der zquipmenttyp                               --*

  SELECT SINGLE * FROM T370T WHERE EQTYP = OBJECT_TAB-EQTYP.
  IF T370T-REFTP = 'S'.
    CASE H_TCODE_L.
      WHEN 'IW24'.
        H_TCODE_L = 'IW54'.
      WHEN 'IW25'.
        H_TCODE_L = 'IW55'.
      WHEN 'IW26'.
        H_TCODE_L = 'IW56'.
    ENDCASE.
  ENDIF.
*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING H_TCODE_L
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    CALL TRANSACTION H_TCODE_L AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                               " CREATE_QMEL_L

*---------------------------------------------------------------------*
*       FORM CREATE_ORDE_L                                            *
*---------------------------------------------------------------------*
*       IH-Aufträge anlegen                                           *
*---------------------------------------------------------------------*
FORM CREATE_ORDE_L.

  DATA: H_RETC LIKE SY-SUBRC.

*--- Berechtigungsprüfung auf T-code ------------------------------*
  PERFORM AUTH_CHECK_TCODE_F10 USING 'IW31'
                               CHANGING H_RETC.
  IF H_RETC IS INITIAL.
    SET PARAMETER ID 'IFL' FIELD OBJECT_TAB-TPLNR.
    SET PARAMETER ID 'EQN' FIELD OBJECT_TAB-EQUNR.
* SET PARAMETER ID 'MAT' FIELD OBJECT_TAB-SUBMT.
    CALL TRANSACTION 'IW31'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_TABLE_L                                              *
*---------------------------------------------------------------------*
*       Tabelle für Klassifizierung                                  *
*---------------------------------------------------------------------*
*  -->  F_TABLE                                                       *
*---------------------------------------------------------------------*
FORM GET_TABLE_L USING F_TABLE LIKE DFIES-TABNAME.

  F_TABLE = 'zqui'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_OBJECT_L                                             *
*---------------------------------------------------------------------*
*       Objekt für Klassifizierung setzen                             *
*---------------------------------------------------------------------*
*  -->  F_OBJECT                                                      *
*---------------------------------------------------------------------*
FORM GET_OBJECT_L USING F_OBJECT LIKE AUSP-OBJEK.

  DATA: BEGIN OF H_OBJECT1,
          EQUNR LIKE ZQUI-EQUNR,
        END OF H_OBJECT1.

  H_OBJECT1-EQUNR = OBJECT_TAB-EQUNR.
  F_OBJECT = H_OBJECT1.

ENDFORM.

*eject
*-------------------------------------------------------------------*
* INCLUDES                                                          *
*-------------------------------------------------------------------*
INCLUDE MIOLXF10.
INCLUDE MIOLXF50.
