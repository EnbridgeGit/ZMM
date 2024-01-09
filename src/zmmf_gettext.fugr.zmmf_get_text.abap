FUNCTION zmmf_get_text.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_LONGTEXT) TYPE  CHAR1000SF
*"     VALUE(E_SHRTTEXT) TYPE  CHAR1000SF
*"  TABLES
*"      T_LTEXT STRUCTURE  TLINE
*"----------------------------------------------------------------------
  DATA:lt_lines       TYPE TABLE OF tline,
        lwa_lines     TYPE tline.

  DATA  : lv_shorttext TYPE string,
          lv_text  TYPE  z_comments,
          lv_name  TYPE thead-tdname.

  lv_name = i_matnr.
  SELECT SINGLE maktx
         FROM makt
         INTO lv_shorttext
         WHERE matnr = i_matnr
         AND   spras = 'EN'.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      object    = 'MATERIAL'
      id        = 'GRUN'
      name      = lv_name
      language  = 'E'
    TABLES
      lines     = lt_lines
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
T_LTEXT[] = lt_lines[].

E_SHRTTEXT = lv_shorttext.
ENDFUNCTION.
