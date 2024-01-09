*----------------------------------------------------------------------*
***INCLUDE LZMM_MASTAGREEF01 .
*----------------------------------------------------------------------*
FORM SAVE.

ZMMT_MASTAGREE-ERDAT = SY-DATUM.
ZMMT_MASTAGREE-ERZET = SY-UZEIT.
ZMMT_MASTAGREE-ERNAM = SY-UNAME.

ENDFORM.


FORM UPDATE.
FIELD-SYMBOLS: <fs_field> TYPE any .
LOOP AT total.
CHECK <action> EQ aendern.

** -- Updated On
ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <vim_total_struc> TO <fs_field>.
IF sy-subrc EQ 0.
<fs_field> = sy-datum.
ENDIF.

** -- Updated Time
ASSIGN COMPONENT 'CPUTM' OF STRUCTURE <vim_total_struc> TO <fs_field>.
IF sy-subrc EQ 0.
<fs_field> = sy-uzeit.
ENDIF.

** -- Updated By
ASSIGN COMPONENT 'USNAM' OF STRUCTURE <vim_total_struc> TO <fs_field>.
IF sy-subrc EQ 0.
<fs_field> = sy-uname.
ENDIF.

READ TABLE extract WITH KEY <vim_xtotal_key>.
IF sy-subrc EQ 0.
extract = total.
MODIFY extract INDEX sy-tabix.
ENDIF.
IF total IS NOT INITIAL.
MODIFY total.
ENDIF.
ENDLOOP.

ENDFORM.
