FUNCTION zhr_get_acct_assign.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_UNAME) TYPE  SYUNAME
*"     VALUE(IM_BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     VALUE(EV_KOSTL) TYPE  KOSTL
*"     VALUE(EV_AUFNR) TYPE  AUFNR
*"     VALUE(EV_POSID) TYPE  PS_POSID
*"----------------------------------------------------------------------

  DATA: lt_data   TYPE STANDARD TABLE OF p0027,
        lv_pernr  TYPE pernr_d,
        lv_subrc  TYPE sysubrc,                             "#EC NEEDED
        lv_kostl  TYPE kostl,
        lv_bukrs  TYPE bukrs,
        lv_aufnr  TYPE aufnr,
        lv_pspnr  TYPE prps-pspnr,
        lv_posid  TYPE prps-posid,
        lv_usrid  TYPE p0105-usrid.

  FIELD-SYMBOLS: <lfs_data>       LIKE LINE OF lt_data.

  CLEAR: ev_kostl,
         ev_aufnr,
         ev_posid.

  lv_usrid = im_uname.
  CALL FUNCTION 'RP_GET_PERNR_FROM_USERID'
    EXPORTING
      begda     = sy-datum
      endda     = sy-datum
      usrid     = lv_usrid
      usrty     = '0001'
    IMPORTING
      usr_pernr = lv_pernr
    EXCEPTIONS
      retcd     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
*       TCLAS           = 'A'
        pernr           = lv_pernr
        infty           = '0027'
        begda           = sy-datum
        endda           = sy-datum
        bypass_buffer   = 'X'
*       LEGACY_MODE     = ' '
      IMPORTING
        subrc           = lv_subrc
      TABLES
        infty_tab       = lt_data
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      READ TABLE lt_data ASSIGNING <lfs_data> WITH KEY subty = '02'.
      IF sy-subrc EQ 0.
        DO 25 TIMES VARYING lv_bukrs FROM <lfs_data>-kbu01 NEXT <lfs_data>-kbu02
                    VARYING lv_kostl FROM <lfs_data>-kst01 NEXT <lfs_data>-kst02
                    VARYING lv_aufnr FROM <lfs_data>-auf01 NEXT <lfs_data>-auf02
                    VARYING lv_pspnr FROM <lfs_data>-psp01 NEXT <lfs_data>-psp02.
          IF lv_bukrs EQ space.
            EXIT.
          ENDIF.
          IF lv_bukrs EQ im_bukrs.
            IF lv_kostl IS NOT INITIAL.
              ev_kostl = lv_kostl.
              EXIT.
            ENDIF.

            IF lv_aufnr IS NOT INITIAL.
              ev_aufnr = lv_aufnr.
              EXIT.
            ENDIF.
            IF lv_pspnr IS NOT INITIAL.
              SELECT SINGLE posid FROM prps INTO lv_posid
                WHERE pspnr = lv_pspnr.
              ev_posid = lv_posid.
              EXIT.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
