FUNCTION Z_GET_OLA_DETAILS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_LIFNR) TYPE  EKKO-LIFNR OPTIONAL
*"     VALUE(IMP_EKORG) TYPE  EKKO-EKORG OPTIONAL
*"     VALUE(IMP_ZTERM) TYPE  EKKO-ZTERM OPTIONAL
*"     VALUE(IMP_MATKL) TYPE  EKPO-MATKL OPTIONAL
*"     VALUE(IMP_COUNT) TYPE  SY-TABIX OPTIONAL
*"  EXPORTING
*"     VALUE(EXP_EKKO_EKPO) TYPE  ZTTY_EKKO_EKPO
*"----------------------------------------------------------------------


  TYPES : BEGIN OF ty_ddshselopt.
          INCLUDE STRUCTURE ddshselopt.
  TYPES : END OF ty_ddshselopt.
  DATA : lit_ddshselopt TYPE STANDARD TABLE OF ty_ddshselopt,
         st_ddshselopt TYPE ty_ddshselopt,
         lv_string TYPE string.

  CLEAR st_ddshselopt.
  IF NOT imp_lifnr IS INITIAL.
    st_ddshselopt-shlpfield = 'A~LIFNR'.
    st_ddshselopt-sign      = 'I'.
    st_ddshselopt-option    = 'EQ'.
    st_ddshselopt-low       = imp_lifnr.
    APPEND st_ddshselopt TO lit_ddshselopt  .
  ENDIF.

  CLEAR st_ddshselopt.
  IF NOT imp_ekorg IS INITIAL.
    st_ddshselopt-shlpfield = 'A~EKORG'.
    st_ddshselopt-sign      = 'I'.
    st_ddshselopt-option    = 'EQ'.
    st_ddshselopt-low       = imp_ekorg.
    APPEND st_ddshselopt TO lit_ddshselopt  .
  ENDIF.

  CLEAR st_ddshselopt.
  st_ddshselopt-shlpfield = 'A~BSTYP'.
  st_ddshselopt-sign      = 'I'.
  st_ddshselopt-option    = 'EQ'.
  st_ddshselopt-low       = 'K'.
  APPEND st_ddshselopt TO lit_ddshselopt  .

  CLEAR st_ddshselopt.
  IF NOT imp_zterm IS INITIAL.
    st_ddshselopt-shlpfield = 'A~ZTERM'.
    st_ddshselopt-sign      = 'I'.
    st_ddshselopt-option    = 'EQ'.
    st_ddshselopt-low       = imp_zterm.
    APPEND st_ddshselopt TO lit_ddshselopt  .
  ENDIF.

  CLEAR st_ddshselopt.
  IF NOT imp_matkl IS INITIAL.
    st_ddshselopt-shlpfield = 'B~MATKL'.
    st_ddshselopt-sign      = 'I'.
    st_ddshselopt-option    = 'EQ'.
    st_ddshselopt-low       = imp_matkl.
    APPEND st_ddshselopt TO lit_ddshselopt  .
  ENDIF.

******start of changes by Harish Manohar for deletion indicator****05/09

    st_ddshselopt-shlpfield = 'B~LOEKZ'.
    st_ddshselopt-sign      = 'I'.
    st_ddshselopt-option    = 'EQ'.
    st_ddshselopt-low       = ' '.
    APPEND st_ddshselopt TO lit_ddshselopt  .

******end of changes by Harish Manohar for deletion indicator****05/09

* FM to create dynamic where clause
  CALL FUNCTION 'F4_CONV_SELOPT_TO_WHERECLAUSE'
   IMPORTING
     where_clause          = lv_string
    TABLES
      selopt_tab            = lit_ddshselopt
            .

  IF NOT lv_string IS INITIAL.
* Select Statement to get the details from tables EKKO, EKPO and LFA1

    SELECT a~ebeln
           a~lifnr
           a~zterm
           a~ekorg
           b~ebelp
           b~matnr
           b~matkl
           b~txz01
           c~name1
           up to IMP_COUNT rows
            FROM ( ( ekko AS a INNER JOIN ekpo AS b
                     ON a~ebeln = b~ebeln )
                      INNER JOIN lfa1 AS c ON a~lifnr = c~lifnr )
            INTO TABLE exp_ekko_ekpo
            WHERE (lv_string).
  ENDIF.

ENDFUNCTION.
