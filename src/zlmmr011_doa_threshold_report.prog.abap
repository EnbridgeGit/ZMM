
******************************************************************
*                                                                *
*   PROGRAM: ZMMR_DOA_THRESHOLD_REPORT                               *
*   AUTHOR:
*   Developer: Mathan Thyagarajan                                *
*   CREATED Date: 11/07/2011                                     *
*                                                                *
*   DESCRIPTION: This report will list the change documents on   *
*                material masters.                               *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
*                                                                *
******************************************************************

REPORT  zlmmr011_doa_threshold_report LINE-SIZE 255 NO STANDARD PAGE HEADING LINE-COUNT 65.

TABLES:  ekko.

TYPES:  BEGIN OF ty_thresh_alv.
INCLUDE TYPE zmmt_thresh_rep.
TYPES:    name1               LIKE lfa1-name1,
          afnam_s             TYPE string,
          doa_approver_s      TYPE string,
          doa_approver_email  LIKE adr6-smtp_addr,
        END OF ty_thresh_alv.

DATA: lt_thresh     TYPE STANDARD TABLE OF zmmt_thresh_rep,
      lt_thresh_upd LIKE lt_thresh,
      ls_thresh     LIKE LINE OF lt_thresh,

      lt_thresh_alv TYPE STANDARD TABLE OF ty_thresh_alv,
      ls_thresh_alv TYPE ty_thresh_alv.



*ALV data declarations
DATA: lt_fieldcat   TYPE lvc_t_fcat,     "slis_t_fieldcat_alv WITH HEADER LINE,
      ls_fieldcat   TYPE lvc_s_fcat,
      ls_layout     TYPE lvc_s_layo,
      lc_container  TYPE REF TO cl_gui_custom_container,
      cl_alv        TYPE REF TO cl_gui_alv_grid.


DATA: lc_gui_container    TYPE REF TO cl_gui_custom_container,
      lc_gui_text_editor  TYPE REF TO cl_gui_textedit,

      lv_datum            TYPE sy-datum,
      lv_enddatum         TYPE sy-datum,

      lv_lifnr            TYPE lifnr,
      lv_fname            LIKE adrp-name_first,
      lv_lname            TYPE adrp-name_last,
      lv_uname            LIKE usr21-bname,

      lv_textst           TYPE string,
      lv_text             TYPE char256,
      lv_response(1)      TYPE c.

"FROM SCREEN
DATA: chk_review(1).

CONSTANTS:  co_line_len    TYPE i      VALUE 254.




*----------------------------------------------------------------------------------------*
*  Selection Screen
*----------------------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
SELECT-OPTIONS:   s_po_dt  FOR ekko-aedat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE text-002.
PARAMETERS:
          rusr_a RADIOBUTTON GROUP rad1 USER-COMMAND upd,
          rusr_m RADIOBUTTON GROUP rad1 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE text-003.
PARAMETERS: ritm_a RADIOBUTTON GROUP rad2  DEFAULT 'X',
            ritm_o RADIOBUTTON GROUP rad2.


SELECTION-SCREEN END OF BLOCK c.

*----------------------------------------------------------------------------------------*
*  Selection Screen Initialization
*----------------------------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF rusr_a = 'X'.
    LOOP AT SCREEN.
      IF screen-name CS 'ritm_a' OR screen-name CS 'ritm_o' .
        CLEAR ritm_o.
        ritm_a = 'X'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      currdate   = sy-datum
      backmonths = '001'
    IMPORTING
      newdate    = lv_datum.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = lv_datum
    IMPORTING
      ev_month_begin_date = lv_datum
      ev_month_end_date   = lv_enddatum.

  IF s_po_dt[] IS INITIAL.
    s_po_dt-option = 'BT'.
    s_po_dt-sign = 'I'.
    s_po_dt-low  = lv_datum.
    s_po_dt-high = lv_enddatum.
    APPEND s_po_dt TO s_po_dt.
  ENDIF.

*----------------------------------------------------------------------------------------*
*  Start-of-selection
*----------------------------------------------------------------------------------------*
START-OF-SELECTION.


  IF rusr_a = 'X'.
* Authorization Check For Auditor
    AUTHORITY-CHECK OBJECT 'Z_DOA_RPT'
                    ID     'ZDOA_USR'
                    FIELD  'AUDIT'.

    IF sy-subrc <> 0.
      MESSAGE text-004 TYPE 'E'.
    ENDIF.

*  Auditor All Items
    PERFORM data_retrieval.

  ELSEIF rusr_m = 'X'.
* Authorization Check For Approver
    AUTHORITY-CHECK OBJECT 'Z_DOA_RPT'
                    ID     'ZDOA_USR'
                    FIELD  'APPROVER'.


    IF sy-subrc <> 0.
      MESSAGE text-004 TYPE 'E'.
    ENDIF.
    IF ritm_o = 'X'.

      PERFORM data_retrieval_usr.
    ELSEIF ritm_a = 'X'.
      PERFORM data_retrieval_all.
    ENDIF.
  ENDIF.
  IF lt_thresh[] IS INITIAL.
    IF rusr_a = 'X'.
      MESSAGE i208(00) WITH text-022 .
    ELSEIF rusr_m = 'X'.
      CONCATENATE text-022
                  text-025
                  sy-uname
                  INTO lv_text
                  SEPARATED BY space.
      MESSAGE i208(00) WITH lv_text.
    ENDIF.
  ELSE.
    PERFORM get_additional_data.
    CALL SCREEN 100.
  ENDIF.




*----------------------------------------------------------------------------------------*
*  SUBROUTINES
*----------------------------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  data_retrieval
*&---------------------------------------------------------------------*
*       Retrieve data form EKPO and EKKO table and populate itab lt_thresh
*----------------------------------------------------------------------*
FORM data_retrieval.
  SELECT *
    FROM zmmt_thresh_rep
    INTO TABLE lt_thresh
    WHERE aedat IN s_po_dt.
ENDFORM.                    "data_retrieval

*&---------------------------------------------------------------------*
*&      Form  data_retrieval_all
*&---------------------------------------------------------------------*
*       Get all records for a user
*----------------------------------------------------------------------*

FORM data_retrieval_all .
  SELECT *
    FROM zmmt_thresh_rep
    INTO TABLE lt_thresh
    WHERE aedat IN s_po_dt
      AND doa_approver = sy-uname.
ENDFORM.                    "data_retrieval_all

*&---------------------------------------------------------------------*
*&      Form  data_retrieval_usr
*&---------------------------------------------------------------------*
*       Get all records for a user that have not been confirmed
*----------------------------------------------------------------------*
FORM data_retrieval_usr.
  SELECT *
    FROM zmmt_thresh_rep
    INTO TABLE lt_thresh
    WHERE aedat IN s_po_dt
      AND apr_confirm NE 'X'
      AND doa_approver = sy-uname.
ENDFORM.                    "data_retrieval_usr


*&---------------------------------------------------------------------*
*&      Form  get_additional_data
*&---------------------------------------------------------------------*
*       Get additional data from SAP and update lt_thresh_alv
*----------------------------------------------------------------------*
FORM get_additional_data.

  DELETE lt_thresh WHERE netpr = 0.

  LOOP AT lt_thresh INTO ls_thresh.
    CLEAR: ls_thresh_alv.
    MOVE-CORRESPONDING ls_thresh TO ls_thresh_alv.
    "Get the vendor number
    SELECT SINGLE lifnr
      FROM ekko
      INTO lv_lifnr
      WHERE ebeln = ls_thresh_alv-ebeln
    .

    "Get the vendor name
    SELECT SINGLE name1
      FROM lfa1
      INTO ls_thresh_alv-name1
      WHERE lifnr = lv_lifnr
    .

    IF ls_thresh_alv-afnam IS NOT INITIAL.
      "Get goods receipient name
      CLEAR: lv_lname, lv_fname, lv_uname.
      lv_uname = ls_thresh_alv-afnam.
      SELECT SINGLE name_last name_first
        INTO (lv_lname, lv_fname)
        FROM adrp
          INNER JOIN usr21
            ON adrp~persnumber = usr21~persnumber
        WHERE usr21~bname = lv_uname
      .

      CONDENSE: lv_lname, lv_fname.
      CONCATENATE lv_lname ', ' lv_fname INTO ls_thresh_alv-afnam_s.
    ENDIF.

    IF ls_thresh_alv-doa_approver IS NOT INITIAL.
      "Get DOA Approver name
      CLEAR: lv_lname, lv_fname, lv_uname.
      lv_uname = ls_thresh_alv-doa_approver.
      SELECT SINGLE name_last name_first
        INTO (lv_lname, lv_fname)
        FROM adrp
          INNER JOIN usr21
            ON adrp~persnumber = usr21~persnumber
        WHERE usr21~bname = lv_uname
      .

      CONDENSE: lv_lname, lv_fname.
      CONCATENATE lv_lname ', ' lv_fname INTO ls_thresh_alv-doa_approver_s.


      "Get DOA Approver Email
      SELECT SINGLE smtp_addr
        INTO ls_thresh_alv-doa_approver_email
        FROM adr6
          INNER JOIN usr21
            ON  adr6~addrnumber = usr21~addrnumber
            AND adr6~persnumber = usr21~persnumber
        WHERE usr21~bname = lv_uname
      .
    ENDIF.

    APPEND ls_thresh_alv TO lt_thresh_alv.
  ENDLOOP.
ENDFORM.                    "get_additional_data


*----------------------------------------------------------------------*
* Module  STATUS_0100  OUTPUT
*----------------------------------------------------------------------*
*       PBO for screen 100
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.

  CLEAR  ls_fieldcat.
  ls_fieldcat-fieldname   = 'EBELN'.
  ls_fieldcat-scrtext_m   = text-006.
  ls_fieldcat-col_pos     = 0.
  ls_fieldcat-outputlen   = 10.
  ls_fieldcat-emphasize   = 'X'.
  ls_fieldcat-key         = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'AEDAT'.
  ls_fieldcat-scrtext_m   = text-007.
  ls_fieldcat-col_pos     = 1.
  ls_fieldcat-outputlen   = 10.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

*  ls_fieldcat-fieldname   = 'NET_PRICE'.
*  ls_fieldcat-scrtext_m   = text-023.
*  ls_fieldcat-col_pos     = 4.
*  APPEND ls_fieldcat TO lt_fieldcat.
*  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'SHOPNG_CRT_NO'.
  ls_fieldcat-scrtext_m   = text-008.
  ls_fieldcat-col_pos     = 2.
  ls_fieldcat-outputlen   = 21.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'NAME1'.
  ls_fieldcat-scrtext_m   = text-019.
  ls_fieldcat-col_pos     = 3.
  ls_fieldcat-outputlen   = 11.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'SHOPNG_DECRIP'.
  ls_fieldcat-scrtext_l   = text-009.
  ls_fieldcat-col_pos     = 4.
  ls_fieldcat-outputlen   = 24.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'NETPR'.
  ls_fieldcat-scrtext_m   = text-010.
  ls_fieldcat-col_pos     = 5.
  ls_fieldcat-outputlen   = 20.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'AFNAM_S'.
  ls_fieldcat-scrtext_m   = text-011.
  ls_fieldcat-col_pos     = 6.
  ls_fieldcat-outputlen   = 20.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'EKGRP'.
  ls_fieldcat-scrtext_m   = text-012.
  ls_fieldcat-col_pos     = 7.
  ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.


  ls_fieldcat-fieldname   = 'DOA_APPROVER_S'.
  ls_fieldcat-scrtext_m   = text-013.
  ls_fieldcat-col_pos     = 8.
  ls_fieldcat-outputlen   = 20.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'DOA_APPROVER_EMAIL'.
  ls_fieldcat-scrtext_m   = text-016.
  ls_fieldcat-col_pos     = 9.
  ls_fieldcat-outputlen   = 25.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'CONFRIM_DATE'.
  ls_fieldcat-scrtext_m   = text-014.
  ls_fieldcat-col_pos     = 10.
  ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'CONFRIM_TIME'.
  ls_fieldcat-scrtext_m   = text-015.
  ls_fieldcat-col_pos     = 11.
  ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'ERNAM'.
  ls_fieldcat-scrtext_m   = text-024.
  ls_fieldcat-col_pos     = 12.
  ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'COMMENTS'.
  ls_fieldcat-scrtext_l   = text-018.
  ls_fieldcat-col_pos     = 13.
  ls_fieldcat-outputlen   = 60.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_layout-sel_mode = 'D'.
  ls_layout-grid_title = text-028.

  IF lc_container IS INITIAL.
    CREATE OBJECT lc_container
      EXPORTING
        container_name              = 'CON_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT cl_alv
      EXPORTING
        i_parent          = lc_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = lt_thresh_alv
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
  IF rusr_m NE 'X' .

    LOOP AT SCREEN.
      IF screen-name = 'REVIEW_CHECK'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF ritm_o NE 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'REVIEW_CHECK'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ritm_o = 'X' AND rusr_m = 'X'.

    IF lc_gui_text_editor IS INITIAL.

      CREATE OBJECT lc_gui_container
        EXPORTING
          container_name              = 'TEXTEDITOR'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.


      CREATE OBJECT lc_gui_text_editor
        EXPORTING
          parent                     = lc_gui_container
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = co_line_len
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true
          max_number_chars           = 1000.


      CALL METHOD lc_gui_text_editor->set_toolbar_mode
        EXPORTING
          toolbar_mode = cl_gui_textedit=>false.

      CALL METHOD lc_gui_text_editor->set_statusbar_mode
        EXPORTING
          statusbar_mode = cl_gui_textedit=>false.

    ENDIF.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'CHK_REVIEW'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                    "status_0100 OUTPUT


*----------------------------------------------------------------------*
* Module  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
*       PAI for screen 100
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      "Only save when you are in as a manager open items
      IF ritm_o = 'X' AND rusr_m = 'X'.

        IF chk_review = 'X'.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = text-029
              text_question         = text-030
              text_button_1         = text-031
              text_button_2         = text-032
              display_cancel_button = ' '
            IMPORTING
              answer                = lv_response
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF lv_response = '2'. "NO
            LEAVE TO SCREEN 100.
          ELSE .

            CALL METHOD lc_gui_text_editor->get_textstream
              IMPORTING
                text                   = lv_textst
              EXCEPTIONS
                error_cntl_call_method = 1
                not_supported_by_gui   = 2
                OTHERS                 = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            CALL METHOD cl_gui_cfw=>flush
              EXCEPTIONS
                cntl_system_error = 1
                cntl_error        = 2
                OTHERS            = 3.

            LOOP AT lt_thresh INTO ls_thresh.
              ls_thresh-confrim_date = sy-datum.
              ls_thresh-confrim_time = sy-uzeit.
              ls_thresh-ernam = sy-uname.
              ls_thresh-apr_confirm = 'X'.

              MOVE lv_textst TO ls_thresh-comments.
              APPEND ls_thresh TO lt_thresh_upd .
            ENDLOOP.


            MODIFY zmmt_thresh_rep  FROM TABLE lt_thresh_upd.
            MESSAGE s208(00) WITH text-021.
          ENDIF.

        ELSE.
          MESSAGE s208(00) WITH text-026.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.                    "user_command_0100 INPUT
