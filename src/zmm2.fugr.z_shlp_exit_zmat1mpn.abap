FUNCTION z_shlp_exit_zmat1mpn.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

*break sahmad.

  DATA: t_selopt  TYPE ddshselops,
        s_selopt  TYPE ddshselopt,
        s_selopt1 TYPE ddshselopt.

  DATA: it_shlpselop TYPE ddshselops,
      wa_shlpselop LIKE LINE OF it_shlpselop,
      lv_name1 TYPE lfa1-name1,
      lv_hname1 TYPE lfa1-name1,
      gd_tabix TYPE i,
      it_shlp TYPE shlp_descr-fielddescr,
      wa_shlp LIKE LINE OF it_shlp,
      ls_fieldrop type line of DDSHFPROPS,
      lt_fieldrop type DDSHFPROPS.

* EXIT immediately, if you do not want to handle this step
*  IF CALLCONTROL-STEP <> 'SELONE' AND
*     CALLCONTROL-STEP <> 'SELECT' AND
*     " AND SO ON
*     CALLCONTROL-STEP <> 'DISP'.
*     EXIT.
*  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
    break sahmad.
    t_selopt = shlp-selopt.
    READ TABLE t_selopt INTO s_selopt WITH KEY shlpfield = 'MATNR_HTN'.
    s_selopt1-shlpfield = 'MATNR_HTN'.
    IF sy-subrc = 0.
      MODIFY shlp-selopt FROM s_selopt1 TRANSPORTING sign option low high
       WHERE shlpfield = 'MATNR_HTN'.
      s_selopt-shlpfield = 'MATNR_B'.
      APPEND s_selopt TO shlp-selopt.
    ENDIF.
    lt_fieldrop = shlp-FIELDPROP.
    read table lt_fieldrop into ls_fieldrop with key FIELDNAME = 'MATNR_HTN'.
    clear ls_fieldrop-SHLPSELDIS. "enable field
    modify shlp-FIELDPROP from ls_fieldrop transporting SHLPSELDIS
    where FIELDNAME = 'MATNR_HTN'.
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT'.
    break sahmad.
*   PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB
*                       CHANGING SHLP CALLCONTROL RC.
*   IF RC = 0.
*     CALLCONTROL-STEP = 'DISP'.
*   ELSE.
*     CALLCONTROL-STEP = 'EXIT'.
*   ENDIF.
    LOOP AT shlp-selopt INTO wa_shlpselop.
      IF wa_shlpselop-shlpfield EQ 'NAME1'.
        lv_name1 = wa_shlpselop-low.
        EXPORT  lv_name1 TO MEMORY ID 'ZLFANAME1'.
        DELETE shlp-selopt INDEX sy-tabix.
      ENDIF.
    ENDLOOP.


    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
break sahmad.
    IMPORT lv_name1 TO lv_name1 FROM MEMORY ID 'ZLFANAME1'.
    IF NOT lv_name1 IS INITIAL.
      DELETE FROM MEMORY ID 'ZLFANAME1'.
      TRANSLATE lv_name1 TO UPPER CASE.
      LOOP AT record_tab.
        gd_tabix = sy-tabix.
        READ TABLE shlp-fielddescr INTO wa_shlp
                                         WITH KEY "tabname   = 'H_ZMATSEARCH'
                                                  fieldname = 'NAME1'.
*        wa_shlp-offset = wa_shlp-offset / 2.
*
        lv_hname1 = record_tab-string+wa_shlp-offset(wa_shlp-leng).
        TRANSLATE lv_hname1 TO UPPER CASE.
        IF NOT lv_hname1 CP lv_name1.
          DELETE record_tab INDEX gd_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.


    EXIT.
  ENDIF.
  IF callcontrol-step = 'RETURN'.
*BREAK SAHMAD.
    EXIT.
  ENDIF.

ENDFUNCTION.
