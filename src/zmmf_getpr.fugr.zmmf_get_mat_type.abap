FUNCTION zmmf_get_mat_type.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_MTART) TYPE  MTART
*"     VALUE(E_FLAG) TYPE  CHAR01
*"----------------------------------------------------------------------

  IF i_matnr IS NOT INITIAL.
    CLEAR: e_flag,
           e_mtart.

    SELECT SINGLE
            mtart
           FROM mara
           INTO e_mtart
           WHERE matnr = i_matnr.
    IF sy-subrc ne 0.
      e_flag = 'X'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
