*-------------------------------------------------------------------
***INCLUDE LMGD1OXX .
***zentrale PBO-Module für alle Bildbausteine
*-------------------------------------------------------------------

  INCLUDE LMGD1O1G .                   " BEZEICHNUNGEN_LESEN


  INCLUDE LMGD1O1F .                   " BILDSTATUS

  INCLUDE LMGD1O1E .                   " FAUSW_BEZEICHNUNGEN


  INCLUDE LMGD1O1D .                   " FELDAUSWAHL


  INCLUDE LMGD1O1C .                   " GET_DATEN_SUB


  INCLUDE LMGD1O1B .                   " INIT_SUB

  INCLUDE LMGD1O1A .                   " REFDATEN_VORSCHLAGEN

  INCLUDE LMGD1O19 .                   " SET_DATEN_SUB

  INCLUDE LMGD1O18 .                   " SONFAUSW_IN_FGRUPPEN


  INCLUDE LMGD1O17 .                   " ZUSREF_VORSCHLAGEN_B


  INCLUDE LMGD1O16 .                   " ZUSREF_VORSCHLAGEN_A

  INCLUDE LMGD1O15 .                   " FELDHISTORIE

  INCLUDE LMGD1O14 .                   " SONDERFAUS
  INCLUDE LMGD1O13 .                   " ORG_BEZEICHNUNGEN_LESEN

*&---------------------------------------------------------------------*
*&      Module  CHECK_SUB_ML  OUTPUT
*&---------------------------------------------------------------------*
* Falls laut T001K das Material Ledger aktiv ist und die sonstigen
* Customizing-Einstellungen nicht dagegen sprechen, wird anstelle der
* drei Standard-Bewertungs-Subscreens der ML-Bewertungs-Subscreen
* prozessiert.
*----------------------------------------------------------------------*
MODULE CHECK_SUB_ML OUTPUT.

  check mbew-bwkey ne space.           "Ist in OMT3-Simulation = SPACE
  clear: t001k, t134m.

* note 657418: reset subscreen settings for orglevel change in MM03
  PROG1_VAL = 'SAPLMGD1'.
  SUB1_VAL = '2801'.
  SUB2_VAL = '2802'.
  SUB3_VAL = '2804'.

  CALL FUNCTION 'T001K_SINGLE_READ'
       EXPORTING
            BWKEY      = mbew-bwkey
       IMPORTING
            WT001K     = t001k.
  CALL FUNCTION 'T134M_SINGLE_READ'
       EXPORTING
            T134M_BWKEY = mbew-bwkey
            T134M_MTART = mara-mtart
       IMPORTING
            WT134M      = t134m
       exceptions
            others      = 1.
*  Ein nicht vorhandener T134M-Eintrag wird wie ein initialer Eintrag
*  behandelt (d.h. Default = keine Mengen/Wertefortschreibung)
  if t001k-mlbwa = 'X'.
    call function 'CKML_F_CKML1_2_NECESSARY'
         exporting
              l_matnr        = mbew-matnr
              l_bwtar        = mbew-bwtar
              l_bwkey        = mbew-bwkey
              l_bwtty        = mbew-bwtty
              s_wertu_valid  = 'X'
              s_wertu        = t134m-wertu
         exceptions
              no_ml_records  = 1
              only_ckmlpr    = 2
              internal_error = 3.
    if sy-subrc eq 0 or sy-subrc = 2. "note 375600
      prog1_val = 'SAPLCKMMAT'.
      sub1_val  = '0010'.
      prog2_val = 'SAPLMGD1'.
      sub2_val  = '0001'.
      prog3_val = 'SAPLMGD1'.
      sub3_val  = '0001'.
    endif.
  endif.

ENDMODULE.                 " CHECK_SUB_ML  OUTPUT
