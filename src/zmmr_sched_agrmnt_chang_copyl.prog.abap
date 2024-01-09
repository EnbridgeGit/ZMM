*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHANG_LGC
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Program Name       :  ZMMR_SCHED_AGRMNT_CHNG_HISTORY                 *
*& Include            :  ZMMR_SCHED_AGRMNT_CHNAG_LGC                    *
*& Author             :  Sudheer Kumar Chinnam                          *
*& Creation Date      :  20/05/2021                                     *
*& Application Area   :  SCM                                            *
*& Description        :  By based on EKKO,CDHDR and CDPOS tables        *
*&                       CDHDR values are passed to the function module *
*&                       CHANGEDOCUMENT_READ_POSITIONS and to get the   *
*&                       Scheduling agreemnt history records            *
*&--------------------------------------------------------------------- *
*&----------------------------------------------------------------------*
*&                      Modification Log                                *
*&                                                                      *
*& Changed On   Changed By  CTS        Description                      *
*& ---------------------------------------------------------------------*
*& 05-Mar-2018  KUMARCHS   D30K930966  CHG0212611 : Scheduling Agreement*
*&                                     Change History Report            *
*&----------------------------------------------------------------------*

********************************Program Logic***************************
START-OF-SELECTION.

  DATA : lv_index TYPE sy-tabix.
  CLEAR: lt_ekko[],lt_cdhdr[],lt_cdpos[],lt_cdhdr1[],lt_cdpos2[], lt_cdpos3[], lt_cdhdr2[].
  SELECT ebeln FROM ekko INTO TABLE lt_ekko WHERE ebeln IN s_ebeln
                                            AND  bstyp = 'L'
                                            AND ekorg = p_ekorg.
  IF sy-subrc IS INITIAL AND lt_ekko IS NOT INITIAL.
    SELECT objectclas
           objectid
           changenr
           udate
           utime
           change_ind FROM cdhdr INTO TABLE lt_cdhdr
                 FOR ALL ENTRIES IN lt_ekko WHERE objectclas = lv_objectclas
                                            AND   objectid = lt_ekko-ebeln.
    IF sy-subrc IS INITIAL.
      SORT lt_cdhdr BY objectid  changenr.
    ENDIF.
  endif.

  DATA:LV_FLAG TYPE FLAG,
       lv_date type cdhdr-udate,
       lv_time type cdhdr-utime.
  CLEAR:LV_FLAG,lv_date,lv_time.
  LOOP AT lt_cdhdr INTO ls_cdhdr .

    IF (  ls_cdhdr-change_ind <> 'I' ).

      CALL FUNCTION 'CHANGEDOCUMENT_READ_POSITIONS'
        EXPORTING
          changenumber            = ls_cdhdr-changenr
          i_prep_unit             = 'X'
        IMPORTING
          header                  = lt_hdr
        TABLES
          editpos                 = lt_editpos
          editpos_with_header     = lt_editpos_with_header
        EXCEPTIONS
          no_position_found       = 1
          wrong_access_to_archive = 2
          OTHERS                  = 3.


      IF sy-subrc = 0.
        IF LV_FLAG IS INITIAL.
          READ TABLE lt_editpos_with_header INTO ls_editpos1 with key objectid = ls_cdhdr-objectid
                                                                     tabname = 'EKKO'
                                                                     tabkey+3(10) = ls_cdhdr-objectid
                                                                     FNAME = 'FRGKE'
                                                                     F_NEW = 'Y'.
          IF SY-SUBRC IS INITIAL.
            " EXECUTE BELOW LOGIC
            LV_FLAG = 'X'.
            LV_DATE = ls_cdhdr-udate.
            LV_TIME = ls_cdhdr-utime.
          ELSE.
            clear: ls_editpos1.
            CONTINUE.
          ENDIF.
        ENDIF.
        SORT lt_editpos_with_header BY objectid changenr.
        READ TABLE lt_editpos_with_header INTO ls_editpos WITH KEY objectid = ls_cdhdr-objectid
                                                                         changenr = ls_cdhdr-changenr.
        IF sy-subrc = 0.
          CLEAR : lv_index.
          lv_index = sy-tabix.

          LOOP AT lt_editpos_with_header INTO ls_editpos FROM lv_index.

            IF ( ls_editpos-objectid NE ls_cdhdr-objectid ) AND ( ls_editpos-changenr NE ls_cdhdr-changenr ).
              EXIT.
            ENDIF.

            ls_final-objid  = ls_editpos-objectid.
            ls_final-username = ls_editpos-username.
*            READ TABLE lt_cdhdr2 INTO ls_cdhdr2 WITH KEY changenr = ls_cdhdr1-changenr." BINARY SEARCH.
            " If no record found then update previous record sucess record date and time
            READ TABLE lt_editpos_with_header INTO ls_editpos1 with key objectid = ls_cdhdr-objectid
                                                           tabname = 'EKKO'
                                                           tabkey+3(10) = ls_cdhdr-objectid
                                                           FNAME = 'FRGKE'
                                                           F_NEW = 'Y'.
            IF sy-subrc is INITIAL.
              LV_DATE = ls_cdhdr-udate.
              LV_TIME = ls_cdhdr-utime.
              clear:ls_editpos1.
            ENDIF.
            ls_final-usdate = lv_date.
            ls_final-ustime = lv_time.

            ls_final-udate = ls_editpos-udate.
            ls_final-utime = ls_editpos-utime.
            ls_final-tcode = ls_editpos-tcode.
            ls_final-tabkey = ls_editpos-tabkey.
            ls_final-changenr = ls_editpos-changenr. "ls_cdpos2-changenr.
            ls_final-chngind = ls_editpos-chngind.
            ls_final-tabname = ls_editpos-tabname.
            ls_final-fname = ls_editpos-fname.
            ls_final-ftext = ls_editpos-ftext.
            ls_final-f_old = ls_editpos-f_old.
            ls_final-f_new = ls_editpos-f_new.
            ls_final-text_case = ls_editpos-text_case.
            ls_final-textart = ls_editpos-textart.

            CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
              EXPORTING
                user_name              = ls_editpos-username
                read_db_directly       = ' '
                cache_results          = 'X'
              IMPORTING
                user_usr03             = ls_user
              EXCEPTIONS
                user_address_not_found = 1
                OTHERS                 = 2.
            IF sy-subrc IS INITIAL.
              ls_final-name1 = ls_user-name1.
              ls_final-name2 = ls_user-name2.
            ENDIF.
            IF ls_final-udate GE p_udate.
              APPEND ls_final TO lt_final.
            ENDIF.
            CLEAR:ls_final,ls_final1,ls_editpos,ls_user.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR:ls_cdhdr1.
  ENDLOOP.

*******************************Program Logic ****************************************
