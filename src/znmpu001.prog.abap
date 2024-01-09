*  All Union Gas Changes identified for "UGL"
* 2004/08/18 - mdemeest - 4.6C - Copied FM06PE02 to ZNMPU001
*                       - Change ME_PRINT_PO to ZME_PRINT_PO_UGL
*----------------------------------------------------------------------*
*   INCLUDE FM06PE02                                                   *
*----------------------------------------------------------------------*
form entry_neu using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '1'.
  else.
    l_druvo = '2'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
*--------------------------------------------------------------------UGL
* 2004/08/18 - 4.6C Upgrade - Reapply Changes                        UGL
*--------------------------------------------------------------------UGL
*  call function 'ME_PRINT_PO'                                       UGL
*       EXPORTING                                                    UGL
*            ix_nast        = l_nast                                 UGL
*            ix_druvo       = l_druvo                                UGL
*            doc            = l_doc                                  UGL
*            ix_screen      = ent_screen                             UGL
*            ix_from_memory = l_from_memory                          UGL
*            ix_toa_dara    = toa_dara                               UGL
*            ix_arc_params  = arc_params                             UGL
*            ix_fonam       = tnapr-fonam           "HW 214570       UGL
*       IMPORTING                                                    UGL
*            ex_retco       = ent_retco.                             UGL
*--------------------------------------------------------------------UGL
  call function 'ZME_PRINT_PO_UGL'                                  "UGL
       EXPORTING                                                    "UGL
            ix_nast        = l_nast                                 "UGL
            ix_druvo       = l_druvo                                "UGL
            doc            = l_doc                                  "UGL
            ix_screen      = ent_screen                             "UGL
            ix_from_memory = l_from_memory                          "UGL
            ix_toa_dara    = toa_dara                               "UGL
            ix_arc_params  = arc_params                             "UGL
            ix_fonam       = tnapr-fonam           "HW 214570       "UGL
       IMPORTING                                                    "UGL
            ex_retco       = ent_retco.                             "UGL
*-------------------------------------------------------------------"UGL
endform.

*eject
*----------------------------------------------------------------------*
* Umlagerungsbestellung,  Hinweis 670912                               *
*----------------------------------------------------------------------*
form entry_neu_sto using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print,
        F_STO.                                              "670912


  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '1'.
  else.
    l_druvo = '2'.
  endif.

  F_STO = 'X'.                                              "670912

  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
            ix_sto         = F_STO                          "670912
       IMPORTING
            ex_retco       = ent_retco.
endform.


*eject
*----------------------------------------------------------------------*
* Mahnung
*----------------------------------------------------------------------*
form entry_mahn using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '3'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.

*eject
*----------------------------------------------------------------------*
* Auftragsbestätigungsmahnung
*----------------------------------------------------------------------*
form entry_aufb using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '7'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferabrufdruck für Formular MEDRUCK mit Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lphe using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_xfz,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '9'.
  l_xfz = 'X'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_xfz         = l_xfz
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferabrufdruck für Formular MEDRUCK ohne Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lphe_cd using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '9'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Feinabrufdruck für Formular MEDRUCK mit Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lpje using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_xfz,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = 'A'.
  l_xfz = 'X'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_xfz         = l_xfz
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Feinabrufdruck für Formular MEDRUCK ohne Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lpje_cd using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = 'A'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
*   INCLUDE FM06PE02                                                   *
*----------------------------------------------------------------------*
form entry_neu_matrix using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '1'.
  else.
    l_druvo = '2'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_mflag       = 'X'
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Angebotsabsage
*----------------------------------------------------------------------*
form entry_absa using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  l_druvo = '4'.
  clear ent_retco.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferplaneinteilung
*----------------------------------------------------------------------*
form entry_lpet using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '5'.
  else.
    l_druvo = '8'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferplaneinteilung
*----------------------------------------------------------------------*
form entry_lpfz using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '5'.
  else.
    l_druvo = '8'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_xfz         = 'X'
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Mahnung
*----------------------------------------------------------------------*
form entry_lpma using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '6'.
  call function 'ME_READ_PO_FOR_PRINTING'
       EXPORTING
            ix_nast        = nast
            ix_screen      = ent_screen
       IMPORTING
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       CHANGING
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       EXPORTING
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
            ix_fonam       = tnapr-fonam                    "HW 214570
       IMPORTING
            ex_retco       = ent_retco.
endform.
