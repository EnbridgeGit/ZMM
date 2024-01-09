*----------------------------------------------------------------------*
***INCLUDE LMGD1O1J .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PREPARE_SUB_DOCU  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE prepare_sub_docu OUTPUT.
  sub_prog_docu = 'SAPLCV140'.
  sub_dynp_docu = '0204'.
  ls_drad-dokob = 'MARA'.
  ls_drad-objky = rmmg1-matnr.
* Sonderbehandlung, falls Vorlagematerial zu berücksichtigen ist
  data dok_t130f like t130f. clear dok_t130f.
  If Rmmzu-Flgdokmref Ne Space.      "prüfen RefKz in T130F
    CALL FUNCTION 'T130F_SINGLE_READ'
          EXPORTING
               kzrfb       = kzrfb
               t130f_fname = 'DOKUMENTE'
          IMPORTING
               wt130f      = dok_t130f
          EXCEPTIONS
               not_found   = 01.
    If dok_t130f-Kzref = Space.
      drad_ref-dokob = Space.
      drad_ref-objky = Space.
    Else.
      drad_ref-dokob = 'MARA'.
      drad_ref-objky = rmmg1_ref-matnr.
    Endif.
    CLEAR rmmzu-flgdokmref.
  ENDIF.
*  IF drad_ref-objky IS INITIAL.
*    drad_ref-objky = ls_drad-objky.
*  ENDIF.
  IF rmmg1-dline IS INITIAL.
    hdatum = sy-datlo.
  ELSE.
    hdatum = rmmg1-dline.
  ENDIF.
*change/display?
  IF t130m-aktyp = aktypa.
    docu_opcode = '3'.
  ELSE.
    docu_opcode = '2'.
  ENDIF.
* note 522456: Dokumentdaten nur anzeigen wenn dies über Feldauswahl
* gecustomized wurde
  IF NOT dokumente_input = 1.
    docu_opcode = '3'.
  ENDIF.
  IF docu_not_first IS INITIAL.
    docu_not_first = x.
    CALL FUNCTION 'CV140_LINKS_INIT'.
    CALL FUNCTION 'CV140_LINKS_CREATE_WORKAREA'
         EXPORTING
              ps_drad   = ls_drad
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    IF sy-subrc <> 0.
    ENDIF.
** Copy refrences.
    IF NOT drad_ref-objky IS INITIAL AND docu_opcode NE 3.


ENHANCEMENT-POINT LMGD1O1J_01 SPOTS ES_LMGD1O1J INCLUDE BOUND.
      CALL FUNCTION 'CV140_LINKS_CREATE_WORKAREA'
           EXPORTING
                ps_drad   = drad_ref
           EXCEPTIONS
                not_found = 1
                OTHERS    = 2.
      IF sy-subrc <> 0.
      ENDIF.
*wk/Hg
      CALL FUNCTION 'CV140_LINKS_COPY'
           EXPORTING
                PF_DOKOB        = 'MARA'
                PF_OBJKY_NEW    = ls_drad-objky
*               PF_DOKOB_OLD    = ' '
                PF_OBJKY_OLD    = drad_ref-objky
                PF_CHECK        = 'X'                       "note 835080
                PF_CHECK_EVERY  = 'X'                       "note 835080
           EXCEPTIONS
                NO_DOCUMENT     = 1
                ERROR_EXIST     = 2
                ERROR_ENQUE     = 3
                ERROR_1_N       = 4
                ERROR_AUTHORITY = 5
                OTHERS          = 6.
      IF SY-SUBRC <> 0.
      ENDIF.
*      CALL FUNCTION 'CV140_LINKS_CHANGE'
*           EXPORTING
**         PF_DOKAR     =
**         PF_DOKNR     =
**         PF_DOKOB     =
**         PF_DOKVR     =
*                pf_objky_new = ls_drad-objky
*                pf_objky_old = drad_ref-objky
**         PF_TLDOK     =
*           EXCEPTIONS
*                not_found    = 1
*                OTHERS       = 2.
*      IF sy-subrc <> 0.
*      ENDIF.
*wk/Hg end
    ENDIF.
  ENDIF.
  CALL FUNCTION 'CV140_LINKS_SCREEN'
       EXPORTING
*         PF_INCLUDE_DYNPRO  = ' '
            pf_opcode          = docu_opcode
            pf_sub_screen      = 'X'
            pf_check_exist     = 'X'
*         PF_VORSCHLAG_DOKAR =
            pf_dokob           = 'MARA'
            pf_objky           =  ls_drad-objky
            pf_actuell_show    = 'X'
            pf_datum           = hdatum.
*    IMPORTING
*         RETURN_CODE        =
*         VB_FLAG            =
*         CAD_LEAVE          =
*    TABLES
*         PT_DRAD            =
*         PT_DRAD_CHANGE     =

ENDMODULE.                             " PREPARE_SUB_DOCU  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_DOKUMENTE_FELDAUSWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_dokumente_feldauswahl OUTPUT.

  dokumente_feldauswahl =  dokumente_bild.


ENDMODULE.                 " SET_DOKUMENTE_FELDAUSWAHL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_DOKUMENTE_FELDAUSWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_dokumente_feldauswahl OUTPUT.

  CLEAR dokumente_feldauswahl.


ENDMODULE.                 " CLEAR_DOKUMENTE_FELDAUSWAHL  OUTPUT
