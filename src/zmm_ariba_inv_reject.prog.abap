*&---------------------------------------------------------------------*
*& Program Name       :  ZMM_ARIBA_INV_REJECT                          *
*& Author             :  Praveena Anusuri                              *
*& Creation Date      :  07/03/2013                                    *
*& Object ID          :  E_PTP_MM (Ticket 30830)                       *
*& Transport Request  :  D30K921628                                    *
*& Application Area   :  PTP-MM                                        *
*& Description        :  An outbound invoice IDoc is sent to Ariba with*
*                        status 'DELETE'and will update the failed     *
*                        inbound invoice IDoc to '68'.                 *
*                        Copied from standard program RSEIDOC02.       *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*

REPORT ZMM_ARIBA_INV_REJECT LINE-SIZE 132 MESSAGE-ID E0.
INCLUDE AUTH2TOP.                      " alle Konstanten für Berechtigg
INCLUDE <ICON>.
include cnt4defs.
*----------------------------------------------------------------------*
include zrseidoc_dat1.
include rseidoc_alv.
include rseidoc_tree.

include zrseidoc_alv_cl.
include rseidoc_tree_cl.

include rseidoc_f01.
include zrseidoc_i01.
include rseidoc_o01.
*----------------------------------------------------------------------*

INITIALIZATION.
*BOC PANUSURI Ticket 30830
* MOVE text-018 TO sos_tabl.
* MOVE TEXT-019 TO SOS_TAB2.
* MOVE TEXT-020 TO SOS_TAB3.
*EOC PANUSURI Ticket 30830

  DATA: BEGIN OF ex_fcodes OCCURS 0.
          INCLUDE STRUCTURE rsexfcode.
  DATA: END OF ex_fcodes.


  ex_fcodes-fcode = 'PRIN'. "ausführen und drucken
  APPEND ex_fcodes.

  ex_fcodes-fcode = 'SJOB'. "ausführen im Hintergrund
  APPEND ex_fcodes.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
*     P_PROGRAM = ' '
    TABLES
      p_exclude = ex_fcodes.

  CLEAR gv_count."(+)PANUSURI Ticket 30830

START-OF-SELECTION.
  READ TABLE credat INDEX 1.
  READ TABLE cretim INDEX 1.

  PERFORM authority_check_rseidoc2_disp.
  PERFORM select_edidc.
  READ TABLE int_edidc INDEX 1.
  IF sy-subrc EQ 0.
    DESCRIBE TABLE int_edidc LINES number.
*    IF number GT 1.                " eine Liste soll ausgegeben werden
    "(-)PANUSURI Ticket 30830 D30K921664
    i_edidc[] = int_edidc[].
    PERFORM fill_alv_list.
    gs_variant-report = sy-repid.
    x_save = 'A'.
    CREATE OBJECT vh_event.
    CALL SCREEN 100.
*  BOC PANUSURI Ticket 30830 D30K921664
*  ELSE.                          " nur ein IDoc direkt anzeigen
*    sel_index = 0.
*    PERFORM authority_check_rseidoc2_idoc USING authority_ok.
*
*    IF authority_ok = false.
*      EXIT.
*    ENDIF.
*    SUBMIT idoc_tree_control WITH docnum = int_edidc-docnum
*                             AND RETURN.
*  ENDIF.
*  EOC PANUSURI Ticket 30830 D30K921664
  ELSE.
    MESSAGE i113.
  ENDIF.
*----------------------------------------------------------------------*
*    SELECT_EDIDC                                                      *
*----------------------------------------------------------------------*
FORM select_edidc.
  DATA: anzahl_in_docnum TYPE i.
  DATA: int_anzahl TYPE i.
  RANGES: int_range_docnum FOR edidc-docnum.
* nachsehen, ob man all entries benutzen kann - falls man aus der
* Statisktik gerufen wurde, konnte es zu Problemen kommen beim IN-State-
* ment, weil nur eine begrenzte Größe möglich ist
*BOC PANUSURI Ticket 30830
*  loop at direct.
*    if direct-low = '1'.
*      direction = '1'.
*    elseif direct-low = '2'.
*      direction = '2'.
*    else.
*      direction = '3'.
*    endif.
*  endloop.
*  if sy-subrc ne 0. " Richtung wurde nicht eingegeben
*    direction = '0'.
*  else.
*    if direction eq '3'. "falsche Eingabe auf Selektionsschirm
*      exit.
*    endif.
*  endif.
*  direction = '1'.
*  select_all_use = 'Y'.
*  LOOP AT DOCNUM.
*    IF DOCNUM-SIGN NE 'I' OR DOCNUM-OPTION NE 'EQ'.
*      SELECT_ALL_USE = 'N'.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*  IF SY-SUBRC NE 0.
*    SELECT_ALL_USE = 'N'.
*  else.
** dann kommt man eventuell über we07 und kann sehr viele Einträge in der
** tabelle haben
*    describe table docnum lines anzahl_in_docnum.
*  ENDIF.
*  IF direction = '1'.
*    IF select_all_use = 'N'.
*  SELECT * FROM edidc INTO TABLE int_edidc
*        WHERE       UPDDAT >= CREDAT-LOW
*        AND         DOCNUM  IN DOCNUM
*        AND         STATUS  IN STATUS
*        AND         DIRECT  IN DIRECT
*        AND         IDOCTP  IN IDOCTP
*        AND         CIMTYP  IN CIMTYP
*        AND         MESTYP  IN MESTYP
*        and         mescod  in mescod
*        and         mesfct  in mesfct
*        AND         TEST    IN TEST
*        AND         SNDPOR  IN ownPOR
*        AND         SNDPRT  IN ownPRT
*        AND         SNDPFC  IN ownPFC
*        AND         SNDPRN  IN ownPRN
*        AND         RCVPOR  IN ppPOR
*        AND         RCVPRT  IN ppPRT
*        AND         RCVPFC  IN ppPFC
*        AND         RCVPRN  IN ppPRN
*        AND         REFINT  IN REFINT
*        AND         REFGRP  IN REFGRP
*        AND         REFMES  IN REFMES
*        AND         ARCKEY  IN ARCKEY
*        AND         credat  IN credat
*        AND         cretim  IN cretim
*        AND         UPDDAT  IN UPDDAT
*        AND         UPDTIM  IN UPDTIM
*        AND         STD     IN STD
*        AND         STDVRS  IN STDVRS
*        AND         STDMES  IN STDMES
*         ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*    ELSE.                            " select_all_use = 'Y'
*      SELECT * FROM edidc INTO TABLE int_edidc
*                  FOR ALL ENTRIES IN docnum
**        WHERE       upddat >= credat-low
**        AND         DOCNUM  =  DOCNUM-LOW
**        AND         STATUS  IN STATUS
**        AND         DIRECT  IN DIRECT
*         WHERE       idoctp  IN idoctp
**        AND         CIMTYP  IN CIMTYP
**        AND         MESTYP  IN MESTYP
**        and         mescod  in mescod
**        and         mesfct  in mesfct
**        AND         TEST    IN TEST
**        AND         SNDPOR  IN ownPOR
**        AND         SNDPRT  IN ownPRT
**        AND         SNDPFC  IN ownPFC
**        AND         SNDPRN  IN ownPRN
**        AND         RCVPOR  IN ppPOR
**        AND         RCVPRT  IN ppPRT
**        AND         RCVPFC  IN ppPFC
**        AND         RCVPRN  IN ppPRN
**        AND         REFINT  IN REFINT
**        AND         REFGRP  IN REFGRP
**        AND         REFMES  IN REFMES
**        AND         ARCKEY  IN ARCKEY
*        AND         CREDAT  IN CREDAT
*        AND         CRETIM  IN CRETIM
**        AND         UPDDAT  IN UPDDAT
**        AND         UPDTIM  IN UPDTIM
**        AND         STD     IN STD
**        AND         STDVRS  IN STDVRS
**        AND         STDMES  IN STDMES
*      ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*    ENDIF. "abfrage select_all_use für drection 1
*  ELSEIF direction EQ '2'.
*    IF select_all_use = 'N'.
*      SELECT * FROM edidc INTO TABLE int_edidc
*         WHERE       upddat >= credat-low
**        AND         DOCNUM  IN DOCNUM
**        AND         STATUS  IN STATUS
**        AND         DIRECT  IN DIRECT
*         AND         idoctp  IN idoctp
**        AND         CIMTYP  IN CIMTYP
**        AND         MESTYP  IN MESTYP
**        and         mescod  in mescod
**        and         mesfct  in mesfct
**        AND         TEST    IN TEST
**        AND         SNDPOR  IN ppPOR
**        AND         SNDPRT  IN ppPRT
**        AND         SNDPFC  IN ppPFC
**        AND         SNDPRN  IN ppPRN
**        AND         RCVPOR  IN ownPOR
**        AND         RCVPRT  IN ownPRT
**        AND         RCVPFC  IN ownPFC
**        AND         RCVPRN  IN ownPRN
**        AND         REFINT  IN REFINT
**        AND         REFGRP  IN REFGRP
**        AND         REFMES  IN REFMES
**        AND         ARCKEY  IN ARCKEY
**        AND         CREDAT  IN CREDAT
**        AND         CRETIM  IN CRETIM
**        AND         UPDDAT  IN UPDDAT
**        AND         UPDTIM  IN UPDTIM
**        AND         STD     IN STD
**        AND         STDVRS  IN STDVRS
**        AND         STDMES  IN STDMES
*       ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*    ELSE.                            " select_all_use = 'Y'
*      SELECT * FROM edidc INTO TABLE int_edidc
*                    FOR ALL ENTRIES IN docnum
*        WHERE       upddat >= credat-low
**        AND         DOCNUM  =  DOCNUM-LOW
**        AND         STATUS  IN STATUS
**        AND         DIRECT  IN DIRECT
*        AND         idoctp  IN idoctp
**        AND         CIMTYP  IN CIMTYP
**        AND         MESTYP  IN MESTYP
**        and         mescod  in mescod
**        and         mesfct  in mesfct
**        AND         TEST    IN TEST
**        AND         SNDPOR  IN ppPOR
**        AND         SNDPRT  IN ppPRT
**        AND         SNDPFC  IN ppPFC
**        AND         SNDPRN  IN ppPRN
**        AND         RCVPOR  IN ownPOR
**        AND         RCVPRT  IN ownPRT
**        AND         RCVPFC  IN ownPFC
**        AND         RCVPRN  IN ownPRN
**        AND         REFINT  IN REFINT
**        AND         REFGRP  IN REFGRP
**        AND         REFMES  IN REFMES
**        AND         ARCKEY  IN ARCKEY
**        AND         CREDAT  IN CREDAT
**        AND         CRETIM  IN CRETIM
**        AND         UPDDAT  IN UPDDAT
**        AND         UPDTIM  IN UPDTIM
**        AND         STD     IN STD
**        AND         STDVRS  IN STDVRS
**        AND         STDMES  IN STDMES
*      ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*    ENDIF. "abfrage select_all_use für direction 2
*  ELSE. " Richtung wurde nicht explizit eingegeben
** wenn keine Selektion für partner und port gemacht wurde, dann ohne
** sie nachlesen.
*    port_partner = 'Y'.
*    READ TABLE pppor INDEX 1.
*    IF sy-subrc NE 0. " keine Selektion
*      READ TABLE ppprt INDEX 1.
*      IF sy-subrc NE 0.
*        READ TABLE ppprn INDEX 1.
*        IF sy-subrc NE 0.
*          READ TABLE pppfc INDEX 1.
*          IF sy-subrc NE 0.
**        read table ownpor index 1.
**        if sy-subrc ne 0.
**         read table ownprt index 1.
**         if sy-subrc ne 0.
**          read table ownprn index 1.
**          if sy-subrc ne 0.
**           read table ownpfc index 1.
**           if sy-subrc ne 0.
**            port_partner = 'N'.
**           endif. "ownpfc
**          endif.  "ownprn
**         endif.   "ownprt
**        endif.    "ownpor
*          ENDIF.     "pppfc
*        ENDIF.      "ppprn
*      ENDIF.       "ppprt
*    ENDIF.        "pppor
*    IF select_all_use = 'N'.
*      SELECT * FROM edidc INTO TABLE int_edidc
*         WHERE       upddat >= credat-low
**         AND         DOCNUM  IN DOCNUM
**         AND         STATUS  IN STATUS
**         AND         DIRECT  IN DIRECT
*         AND         idoctp  IN idoctp
**         AND         CIMTYP  IN CIMTYP
**         AND         MESTYP  IN MESTYP
**         and         mescod  in mescod
**         and         mesfct  in mesfct
**         AND         TEST    IN TEST
**         AND         REFINT  IN REFINT
**         AND         REFGRP  IN REFGRP
**         AND         REFMES  IN REFMES
**         AND         ARCKEY  IN ARCKEY
**         AND         CREDAT  IN CREDAT
**         AND         CRETIM  IN CRETIM
**         AND         UPDDAT  IN UPDDAT
**         AND         UPDTIM  IN UPDTIM
**         AND         STD     IN STD
**         AND         STDVRS  IN STDVRS
**         AND         STDMES  IN STDMES
*     ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*    ELSE.                            " select_all_use = 'Y'
*      IF anzahl_in_docnum LT 1000.
*        SELECT * FROM edidc INTO TABLE int_edidc
*                     FOR ALL ENTRIES IN docnum
**           WHERE       UPDDAT >= CREDAT-LOW
**           AND         DOCNUM  =  DOCNUM-LOW
**           AND         STATUS  IN STATUS
**           AND         DIRECT  IN DIRECT
*             WHERE       idoctp  IN idoctp
**           AND         CIMTYP  IN CIMTYP
**           AND         MESTYP  IN MESTYP
**           and         mescod  in mescod
**           and         mesfct  in mesfct
**           AND         TEST    IN TEST
**           AND         REFINT  IN REFINT
**           AND         REFGRP  IN REFGRP
**           AND         REFMES  IN REFMES
**           AND         ARCKEY  IN ARCKEY
*            AND         credat  IN credat
*            AND         cretim  IN cretim
**           AND         UPDDAT  IN UPDDAT
**           AND         UPDTIM  IN UPDTIM
**           AND         STD     IN STD
**           AND         STDVRS  IN STDVRS
**           AND         STDMES  IN STDMES
*        ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*      ELSE. "mehr als 1000 in IN-Tabelle -> portionieren
*        REFRESH int_range_docnum.
*        CLEAR int_range_docnum.
*        CLEAR int_anzahl.
*        REFRESH int_edidc.
*        int_range_docnum-sign = 'I'.
*        int_range_docnum-option = 'EQ'.
*        LOOP AT docnum.
*          MOVE docnum-low TO int_range_docnum-low.
*          APPEND int_range_docnum.
*          ADD 1 TO int_anzahl.
*          IF int_anzahl = 1000.
*            SELECT * FROM edidc APPENDING TABLE int_edidc
*                          FOR ALL ENTRIES IN int_range_docnum
*              WHERE       upddat >= credat-low
**              AND         DOCNUM  =  int_range_DOCNUM-LOW
**              AND         STATUS  IN STATUS
**              AND         DIRECT  IN DIRECT
*              AND         idoctp  IN idoctp.
**              AND         CIMTYP  IN CIMTYP
**              AND         MESTYP  IN MESTYP
**              and         mescod  in mescod
**              and         mesfct  in mesfct.
**              AND         TEST    IN TEST
**              AND         REFINT  IN REFINT
**              AND         REFGRP  IN REFGRP
**              AND         REFMES  IN REFMES
**              AND         ARCKEY  IN ARCKEY
**              AND         CREDAT  IN CREDAT
*            and         cretim  in cretim
*            and         upddat  in upddat
**              AND         UPDTIM  IN UPDTIM
**              AND         STD     IN STD
**              AND         STDVRS  IN STDVRS
**              AND         STDMES  IN STDMES.
**      ORDER BY PRIMARY KEY.          " Sortierung erst am ende
*        refresh int_range_docnum.
*            int_anzahl = 0.
*            COMMIT WORK. "vorbeugend gegen runtime-error
*          ENDIF.
*        ENDLOOP.
** jetzt muß noch der Rest nachgelesen werden
*        IF int_anzahl NE 0.
*          SELECT * FROM edidc APPENDING TABLE int_edidc
*                        FOR ALL ENTRIES IN int_range_docnum
*            WHERE       upddat >= credat-low
*            AND         docnum  =  int_range_docnum-low
*            AND         status  IN status
*            AND         direct  IN direct
*            AND         idoctp  IN idoctp
*            AND         cimtyp  IN cimtyp
*            AND         mestyp  IN mestyp
*            AND         mescod  IN mescod
*            AND         mesfct  IN mesfct.
**          AND         TEST    IN TEST
**          AND         REFINT  IN REFINT
**          AND         REFGRP  IN REFGRP
**          AND         REFMES  IN REFMES
**          AND         ARCKEY  IN ARCKEY
**          AND         CREDAT  IN CREDAT
**          AND         CRETIM  IN CRETIM
**          AND         UPDDAT  IN UPDDAT
**          AND         UPDTIM  IN UPDTIM
**          AND         STD     IN STD
**          AND         STDVRS  IN STDVRS
**          AND         STDMES  IN STDMES.
**      ORDER BY PRIMARY KEY.          " Sortierung erst am ende
*          REFRESH int_range_docnum.
*          int_anzahl = 0.
*        ENDIF.
*      ENDIF. "int_anzahl lt 5000-abfrage
*    ENDIF.   "select_all_use_abfrage für direction = 3
*    IF port_partner = 'N'. "
*    ELSE."port oder partner wurden selektiert
** dann muß man aus der internen Tabelle die entfernen, die nicht der
** Port/partnerselektion entsprechen
*      LOOP AT int_edidc.
*        IF int_edidc-direct EQ '1'.
*          IF int_edidc-rcvpor IN pppor.
*            IF int_edidc-rcvprt IN ppprt.
*              IF int_edidc-rcvprn IN ppprn.
*                IF int_edidc-rcvpfc IN pppfc.
**                 if int_edidc-sndpor IN ownpor.
**                   if int_edidc-sndprt IN ownprt.
**                     if int_edidc-sndprn IN ownprn.
**                       if int_edidc-sndpfc IN ownpfc.
**                       else.
**                         delete int_edidc.
**                       endif.
**                     else.
**                       delete int_edidc.
**                     endif.
**                   else.
**                     delete int_edidc.
**                   endif.
**                 else.
**                   delete int_edidc.
**                 endif.
*                ELSE.
*                  DELETE int_edidc.
*                ENDIF.
*              ELSE.
*                DELETE int_edidc.
*              ENDIF.
*            ELSE.
*              DELETE int_edidc.
*            ENDIF.
*          ELSE.
*            DELETE int_edidc.
*          ENDIF.
**        else. " direct eq '2'
**         if int_edidc-rcvpor IN ownpor.
**           if int_edidc-rcvprt IN ownprt.
**             if int_edidc-rcvprn IN ownprn.
**               if int_edidc-rcvpfc IN ownpfc.
**                 if int_edidc-sndpor IN pppor.
**                   if int_edidc-sndprt IN ppprt.
**                     if int_edidc-sndprn IN ppprn.
**                       if int_edidc-sndpfc IN pppfc.
**                       else.
**                         delete int_edidc.
**                       endif.
**                     else.
**                       delete int_edidc.
**                     endif.
**                   else.
**                     delete int_edidc.
**                   endif.
**                 else.
**                   delete int_edidc.
**                 endif.
**               else.
**                 delete int_edidc.
**               endif.
**             else.
**               delete int_edidc.
**             endif.
**           else.
**             delete int_edidc.
**           endif.
**         else.
**           delete int_edidc.
**         endif.
*        ENDIF.
*      ENDLOOP.
*    ENDIF. " partnerabhängig-abfrage
*  ENDIF.   " direction-abfrage
*EOC PANUSURI Ticket 30830
*BOI PANUSURI Ticket 30830
* Fetch failed Ariba inbound invoice idocs
  SELECT * FROM edidc INTO TABLE int_edidc
           WHERE       status  = c_51
           AND         idoctp  = c_zarbinv01
           AND         mestyp  = c_zarbinv
           AND         credat  IN credat
           AND         cretim  IN cretim.
*EOI PANUSURI Ticket 30830
ENDFORM.                    "SELECT_EDIDC
*----------------------------------------------------------------------*
*       Form  AUTHORITY_CHECK_RSEIDOC2_DISP                            *
*----------------------------------------------------------------------*
*       checks, whether the actual user has the necessary authority    *
*       if the user is not authorized the program stoppes and          *
*       a message is displayed to the user                             *
*----------------------------------------------------------------------*
* Output:                                                              *
*       AUTHORITY_OK - if the current user is authorized for the       *
*                      action, so the parameter is set to TRUE         *
*                      otherwise to FALSE                              *
*----------------------------------------------------------------------*
FORM authority_check_rseidoc2_disp.

  AUTHORITY-CHECK OBJECT   authority_obj_edi_monitor
        ID 'EDI_TCD' FIELD authority_tcode_rseidoc2
        ID 'ACTVT' FIELD authority_activity_display
        ID 'EDI_DIR' DUMMY
        ID 'EDI_MES' DUMMY
        ID 'EDI_PRN' DUMMY
        ID 'EDI_PRT' DUMMY.

  IF sy-subrc NE 0.
* may be the user has the older authority for WE05
    AUTHORITY-CHECK OBJECT   authority_obj_edi_monitor
        ID 'EDI_TCD' FIELD authority_tcode_edi_idoc_lists
        ID 'ACTVT'   FIELD authority_activity_display
        ID 'EDI_DIR' DUMMY
        ID 'EDI_MES' DUMMY
        ID 'EDI_PRN' DUMMY
        ID 'EDI_PRT' DUMMY.
    IF sy-subrc NE 0.
* authoritycheck negative; message for the user and stop program
      MESSAGE e168.
    ENDIF.
  ENDIF.

ENDFORM.                               " AUTHORITY_CHECK_RSEIDOC2_DISP

*----------------------------------------------------------------------*
*       Form  AUTHORITY_CHECK_RSEIDOC2_IDOC                            *
*----------------------------------------------------------------------*
FORM authority_check_rseidoc2_idoc USING l_authority_ok.

  IF int_edidc-direct EQ '1'.        " dann Empfänger abzutesten
    AUTHORITY-CHECK OBJECT   authority_obj_edi_monitor
        ID 'EDI_TCD' FIELD authority_tcode_rseidoc2
        ID 'ACTVT' FIELD authority_activity_display
        ID 'EDI_DIR' FIELD int_edidc-direct
        ID 'EDI_MES' FIELD int_edidc-mestyp
        ID 'EDI_PRN' FIELD int_edidc-rcvprn
        ID 'EDI_PRT' FIELD int_edidc-rcvprt.

    IF sy-subrc = 0.
* authoritycheck positive
      MOVE true  TO l_authority_ok.
    ELSE.
* authoritycheck negative; message for the user and stop program
      MESSAGE i169 WITH int_edidc-mestyp int_edidc-direct
                        int_edidc-rcvprt int_edidc-rcvprn.
      MOVE false TO l_authority_ok.
    ENDIF.
  ELSE.                                 " Eingang -> Sender abzutesten
    AUTHORITY-CHECK OBJECT   authority_obj_edi_monitor
        ID 'EDI_TCD' FIELD authority_tcode_rseidoc2
        ID 'ACTVT' FIELD authority_activity_display
        ID 'EDI_DIR' FIELD int_edidc-direct
        ID 'EDI_MES' FIELD int_edidc-mestyp
        ID 'EDI_PRN' FIELD int_edidc-sndprn
        ID 'EDI_PRT' FIELD int_edidc-sndprt.

    IF sy-subrc = 0.
* authoritycheck positive
      MOVE true  TO l_authority_ok.
    ELSE.
* authoritycheck negative; message for the user and stop program
      MESSAGE i169 WITH int_edidc-mestyp int_edidc-direct
                        int_edidc-sndprt int_edidc-sndprn.
      MOVE false TO l_authority_ok.
    ENDIF.
  ENDIF.
ENDFORM.                               " AUTHORITY_CHECK_RSEIDOC2_IDOC
