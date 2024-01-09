  INCLUDE LMGD1I50 .                   " MPOP-ALPHA

  INCLUDE LMGD1I4Z .                   " MPOP-ANZPR
  INCLUDE LMGD1I4Y .                   " MPOP-BETA1

  INCLUDE LMGD1I4X .                   " MPOP-DELTA

  INCLUDE LMGD1I4W .                   " MPOP-FIMON


  INCLUDE LMGD1I4V .                   " MPOP-GAMMA

  INCLUDE LMGD1I4U .                   " MPOP-GEWGR


  INCLUDE LMGD1I4T .                   " MPOP-GLATT


  INCLUDE LMGD1I4S .                   " MPOP-GWERT

  INCLUDE LMGD1I4R .                   " MPOP-KZINI

  INCLUDE LMGD1I4Q .                   " MPOP-MODAV


  INCLUDE LMGD1I4P .                   " MPOP-MODAW


  INCLUDE LMGD1I4O .                   " MPOP-OPGRA
  INCLUDE LMGD1I4N .                   " MPOP-PERIN


  INCLUDE LMGD1I4M .                   " MPOP-PERIO

  INCLUDE LMGD1I4L .                   " MPOP-PERIODS

  INCLUDE LMGD1I4K .                   " MPOP-PRMAD

  INCLUDE LMGD1I4J .                   " MPOP-PRMOD

  INCLUDE LMGD1I4I .                   " MPOP-SIGGR


  INCLUDE LMGD1I4H .                   " MPOP-TWERT
*&---------------------------------------------------------------------*
*&      Module  GET_TC_LONGTEXT_SELECTED_LINE  INPUT
*&---------------------------------------------------------------------*
*       OKcode TableControl LAngtexte auswerten
*----------------------------------------------------------------------*
  MODULE GET_TC_LONGTEXT_SELECTED_LINE INPUT.
    if rmmzu-okcode(4) = 'TLAN'.
      lineindex = tc_longtext-top_line + rmmzu-okcode+4(2) - 1.
      read table lang_tc_tab_tc index lineindex.
      <desc_langu> = lang_tc_tab_tc-sprsl.
      lang_tc_tab_tc-mark = 'X'.
      modify lang_tc_tab_tc index lineindex.
      rmmzu-okcode+4(2) = '  '.
    endif.
  ENDMODULE.                 " GET_TC_LONGTEXT_SELECTED_LINE  INPUT
