*&---------------------------------------------------------------------*
*& Program Name       :  ZMM_ARIBA_INV_REJECT                          *
*& Include Name       :  ZRSEIDOC_ALV_CL                               *
*& Author             :  Praveena Anusuri                              *
*& Creation Date      :  07/03/2013                                    *
*& Object ID          :  E_PTP_MM (Ticket 30830)                       *
*& Transport Request  :  D30K921628                                    *
*& Application Area   :  PTP-MM                                        *
*& Description        :  An outbound invoice IDoc is sent to Ariba with*
*                        status 'DELETE'and will update the failed     *
*                        inbound invoice IDoc to '68'.                 *
*                        Copied from standard program RSEIDOC2.       *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*
* LOCAL CLASSES: Definition für ALV Ereignisse
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
    handle_delayed_changed_sel_cb
        FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid,

    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*
* class lcl_event_receiver (Implementation)
*
*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
* § 2.In event handler method for event TOOLBAR: Append own functions
*   by using event parameter E_OBJECT.
    DATA: ls_toolbar  TYPE stb_button.
*....................................................................
* E_OBJECT of event TOOLBAR is of type REF TO CL_ALV_EVENT_TOOLBAR_SET.
* This class has got one attribute, namly MT_TOOLBAR, which
* is a table of type TTB_BUTTON. One line of this table is
* defined by the Structure STB_BUTTON (see data deklaration above).
*
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'IDOC' TO ls_toolbar-function.
    MOVE icon_idoc TO ls_toolbar-icon.
    MOVE text-032 TO ls_toolbar-quickinfo. "IDoc-Anzeige
    MOVE text-031 TO ls_toolbar-text.      "IDoc
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*   BOI PANUSURI Ticket 30830
*   Add custom button on the ALV toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'INVS' TO ls_toolbar-function.
    MOVE icon_create TO ls_toolbar-icon.
    MOVE text-040 TO ls_toolbar-quickinfo. "Create Invoice Status
    MOVE text-041 TO ls_toolbar-text.      "Create INV. Stat
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*   EOI PANUSURI Ticket 30830

  ENDMETHOD.                    "handle_toolbar
*-------------------------------------------------------------------
  METHOD handle_double_click.
*       FOR EVENT double_click OF cl_gui_alv_grid
*       IMPORTING e_row.
* in e_row steht die Zeilennummer
    DATA: lt_rows TYPE lvc_t_row.
    DATA: l_row   TYPE lvc_s_row.

    CALL METHOD grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.
    CALL METHOD cl_gui_cfw=>flush.

    PERFORM show_idoc TABLES lt_rows.

    PERFORM fill_special_values TABLES lt_rows.

    need_refresh = 'N'.

    SET SCREEN 100.
    LEAVE SCREEN.

  ENDMETHOD.                    "handle_double_click
*-------------------------------------------------------------------
*-------------------------------------------------------------------
  METHOD handle_delayed_changed_sel_cb.

    DATA: lt_rows TYPE lvc_t_row.
    DATA: l_row   TYPE lvc_s_row.

    CALL METHOD grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.
    CALL METHOD cl_gui_cfw=>flush.
    READ TABLE lt_rows INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    PERFORM fill_special_values TABLES lt_rows.

    need_refresh = 'N'.

    SET SCREEN 100.
    LEAVE SCREEN.

  ENDMETHOD.                    "handle_delayed_changed_sel_cb
*-------------------------------------------------------------------
*-------------------------------------------------------------------
  METHOD handle_user_command.
* § 3.In event handler method for event USER_COMMAND: Query your
*   function codes defined in step 2 and react accordingly.

    DATA: lt_rows TYPE lvc_t_row.
    DATA: l_row   TYPE lvc_s_row.
*   BOI PANUSURI Ticket 30830
    DATA: lv_selected_line TYPE lvc_s_row,
          lv_row_index     TYPE lvc_index.
    DATA: lv_lifnr         TYPE elifn,
          lv_emnfr         TYPE emnfr.
    DATA: lv_rc            TYPE c.
    DATA: lv_doc_typ       TYPE blart,
          lv_inv_doc_no    TYPE re_belnr,
          lv_vnd_no        TYPE lifnr,
          lv_arib_com_sup  TYPE emnfr,
          lv_sup_inv_no    TYPE xblnr,
          lv_inv_status    TYPE bktxt,
          lv_inv_amt       TYPE wrbtr,
          lv_year          TYPE gjahr.
    DATA: lv_segnam        TYPE ddobjname.
    DATA: lv_docnum        TYPE edi_docnum.
    DATA: lv_answer        TYPE c.
    DATA: lv_flag          TYPE c.
    DATA: lt_fields        TYPE STANDARD TABLE OF sval ,
          lwa_fields       TYPE sval.
    DATA: lt_status        TYPE STANDARD TABLE OF edi_ds.
    DATA: lwa_status       TYPE edi_ds.
    DATA: lt_edidc         TYPE STANDARD TABLE OF edidc.
    DATA: lwa_edidc        TYPE edidc.

    CONSTANTS : lc_segnam(25)          TYPE c VALUE 'Z1BP_INCINV_CREATE_HEADER',
                lc_tabname(25)         TYPE c VALUE 'Z1BP_INCINV_HEADER_STATUS',
                lc_po_ref_no(9)        TYPE c VALUE 'PO_REF_NO',
                lc_doc_type(8)         TYPE c VALUE 'DOC_TYPE',
                lc_zdoc_typ(8)         TYPE c VALUE 'ZDOC_TYP',
                lc_zinv_doc_no(11)     TYPE c VALUE 'ZINV_DOC_NO',
                lc_zvnd_no(7)          TYPE c VALUE 'ZVND_NO',
                lc_zarib_com_sup(13)   TYPE c VALUE 'ZARIB_COM_SUP',
                lc_ref_doc_no_long(15) TYPE c VALUE 'REF_DOC_NO_LONG',
                lc_zsup_inv_no(11)     TYPE c VALUE 'ZSUP_INV_NO',
                lc_zinv_status(11)     TYPE c VALUE 'ZINV_STATUS',
                lc_zinv_amt(8)         TYPE c VALUE 'ZINV_AMT',
                lc_zyear(5)            TYPE c VALUE 'ZYEAR',
*                lc_port                TYPE edi_rcvpor VALUE 'SONIC',
                lc_sonic               TYPE edi_rcvprn VALUE 'SONIC',
                lc_ls                  TYPE edi_rcvprt VALUE 'LS'.
*   EOI PANUSURI Ticket 30830

    CASE e_ucomm.
      WHEN 'IDOC'.
        CALL METHOD grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.
        CALL METHOD cl_gui_cfw=>flush.
* in lt_rows (tabelle ohne Kopf) stehen alle selektierten Einträge
* im Feld INDEX steht der Index in der internen Tabelle des ALV
        PERFORM show_idoc TABLES lt_rows.
*   BOI PANUSURI Ticket 30830
      WHEN 'INVS'.
*       Get selected row, when 'Create INV.Stat' button is selected
        CALL METHOD grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.
        CALL METHOD cl_gui_cfw=>flush.

        LOOP AT lt_rows INTO lv_selected_line.
          lv_row_index = lv_selected_line-index.
          READ TABLE i_listedidc INTO wa_listedidc INDEX lv_row_index .
          IF sy-subrc = 0.
            lv_docnum = wa_listedidc-docnum.
            IF gv_count > 0.
              REFRESH lt_edidc.
*             Fetch again failed Ariba inbound invoice idocs to check the status '51'
              PERFORM select_edidc.
              lt_edidc[] = int_edidc[].
              READ TABLE lt_edidc INTO lwa_edidc WITH KEY docnum = lv_docnum.
              IF sy-subrc <> 0.
                MESSAGE ID 'EA' TYPE 'I' NUMBER '869' WITH '68' .
                lv_flag = 'X'.
                EXIT.
              ENDIF.
            ENDIF.
            REFRESH int_edidd.
            CALL FUNCTION 'IDOC_READ_COMPLETELY'
              EXPORTING
                document_number         = wa_listedidc-docnum
              TABLES
                int_edidd               = int_edidd
              EXCEPTIONS
                document_not_exist      = 1
                document_number_invalid = 2
                OTHERS                  = 3.
            IF sy-subrc <> 0.
*              Implement suitable error handling here
            ENDIF.
            READ TABLE int_edidd INTO int_edidd WITH KEY segnam = lc_segnam. " 'Z1BP_INCINV_CREATE_HEADER'.
            IF sy-subrc = 0.
              MOVE int_edidd-segnam TO lv_segnam.
              REFRESH int_dfies.
              CALL FUNCTION 'DDIF_NAMETAB_GET'
                EXPORTING
                  tabname   = lv_segnam
                TABLES
                  dfies_tab = int_dfies
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
              offset = 0.
              REFRESH int_seg.
              LOOP AT int_dfies INTO wa_dfies.
                ASSIGN int_edidd-sdata+offset(wa_dfies-leng) TO <feld>.
                string = <feld>.
                offset = offset + wa_dfies-leng.
*               CHECK NOT string IS INITIAL.
                MOVE wa_dfies-fieldname TO wa_seg-fieldname.
                MOVE string TO wa_seg-string.
                APPEND wa_seg TO int_seg.
                CLEAR wa_seg.
              ENDLOOP.
              READ TABLE int_seg INTO wa_seg WITH KEY fieldname = lc_po_ref_no.
              IF sy-subrc = 0.
*               Get SAP vendor number
                SELECT SINGLE lifnr FROM ekko INTO lv_lifnr WHERE ebeln = wa_seg-string.
                IF sy-subrc = 0.
*                 Get Ariba CS ID
                  SELECT SINGLE emnfr FROM lfa1 INTO lv_emnfr WHERE lifnr = lv_lifnr.
                  IF sy-subrc = 0.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR wa_seg.
              REFRESH lt_fields.
              READ TABLE int_seg INTO wa_seg WITH KEY fieldname = lc_doc_type.
              IF sy-subrc = 0.
                lwa_fields-tabname = lc_tabname. " 'Z1BP_INCINV_HEADER_STATUS'.
                lwa_fields-fieldname = lc_zdoc_typ.
                lwa_fields-value = wa_seg-string.
                lwa_fields-novaluehlp = 'X'.
                APPEND lwa_fields TO lt_fields.
                CLEAR lwa_fields.
              ENDIF.
              lwa_fields-tabname = lc_tabname. " 'Z1BP_INCINV_HEADER_STATUS'.
              lwa_fields-fieldname = lc_zinv_doc_no.
              lwa_fields-value = '0000000000'.
              lwa_fields-novaluehlp = 'X'.
              APPEND lwa_fields TO lt_fields.
              CLEAR lwa_fields.

              lwa_fields-tabname = lc_tabname. " 'Z1BP_INCINV_HEADER_STATUS'.
              lwa_fields-fieldname = lc_zvnd_no.
              lwa_fields-value = lv_lifnr.
              lwa_fields-novaluehlp = 'X'.
              APPEND lwa_fields TO lt_fields.
              CLEAR lwa_fields.

              lwa_fields-tabname = lc_tabname. "'Z1BP_INCINV_HEADER_STATUS'.
              lwa_fields-fieldname = lc_zarib_com_sup.
              lwa_fields-value = lv_emnfr.
              lwa_fields-novaluehlp = 'X'.
              APPEND lwa_fields TO lt_fields.
              CLEAR lwa_fields.

              READ TABLE int_seg INTO wa_seg WITH KEY fieldname = lc_ref_doc_no_long.
              IF sy-subrc = 0.
                lwa_fields-tabname = lc_tabname. "'Z1BP_INCINV_HEADER_STATUS'.
                lwa_fields-fieldname = lc_zsup_inv_no.
                lwa_fields-value = wa_seg-string.
                lwa_fields-novaluehlp = 'X'.
                APPEND lwa_fields TO lt_fields.
                CLEAR lwa_fields.
              ENDIF.
              lwa_fields-tabname = lc_tabname. "'Z1BP_INCINV_HEADER_STATUS'.
              lwa_fields-fieldname = lc_zinv_status.
              lwa_fields-value = 'DELETE'.
              lwa_fields-novaluehlp = 'X'.
              APPEND lwa_fields TO lt_fields.
              CLEAR lwa_fields.
              lwa_fields-tabname = lc_tabname. " 'Z1BP_INCINV_HEADER_STATUS'.
              lwa_fields-fieldname = lc_zinv_amt.
              lwa_fields-value = '0.00'.
              lwa_fields-novaluehlp = 'X'.
              APPEND lwa_fields TO lt_fields.
              CLEAR lwa_fields.
              lwa_fields-tabname = lc_tabname. " 'Z1BP_INCINV_HEADER_STATUS'.
              lwa_fields-fieldname = lc_zyear.
              lwa_fields-value = sy-datum+0(4).
              lwa_fields-novaluehlp = 'X'.
              APPEND lwa_fields TO lt_fields.
              CLEAR lwa_fields.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_flag = ''.
          CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
            EXPORTING
              f4_programname            = sy-cprog
              popup_title               = 'Change data record'(048)
              programname               = sy-cprog
              start_column              = '35'
              start_row                 = '10'
              no_check_for_fixed_values = 'X'
            IMPORTING
              returncode                = lv_rc
            TABLES
              fields                    = lt_fields
            EXCEPTIONS
              error_in_fields           = 1
              OTHERS                    = 2.
          IF sy-subrc <> 0.
*         Implement suitable error handling here
          ELSE.
            IF lv_rc <> 'A'."Cancel
              CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                  titlebar              = 'REJECT TO ARIBA'(046)
                  text_question         = 'Do you want to send outbound invoice IDoc to Ariba?'(047)
                  text_button_1         = 'Yes'(042)
                  text_button_2         = 'No'(043)
                  display_cancel_button = ' '
                  start_column          = 35
                  start_row             = 10
                IMPORTING
                  answer                = lv_answer
                EXCEPTIONS
                  text_not_found        = 1
                  OTHERS                = 2.
              IF sy-subrc <> 0.
*               Implement suitable error handling here
              ENDIF.
              IF lv_answer = '1'.
                LOOP AT lt_fields INTO lwa_fields.
                  IF lwa_fields-fieldname = lc_zdoc_typ.
                    lv_doc_typ = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zinv_doc_no.
                    lv_inv_doc_no = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zvnd_no.
                    lv_vnd_no = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zarib_com_sup.
                    lv_arib_com_sup = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zsup_inv_no.
                    lv_sup_inv_no = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zinv_status.
                    lv_inv_status = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zinv_amt.
                    lv_inv_amt = lwa_fields-value.
                  ENDIF.
                  IF lwa_fields-fieldname = lc_zyear.
                    lv_year = lwa_fields-value.
                  ENDIF.
                ENDLOOP.
                CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
                  EXPORTING
                    im_doc_typ                   = lv_doc_typ
                    im_inv_doc_no                = lv_inv_doc_no
                    im_vnd_no                    = lv_vnd_no
                    im_arib_com_sup              = lv_arib_com_sup
                    im_sup_inv_no                = lv_sup_inv_no
                    im_inv_status                = lv_inv_status
*                   IM_ZTEXT                     =
                    im_inv_amt                   = lv_inv_amt
                    im_year                      = lv_year
*                    im_port                      = lc_port  "SONIC
                    im_partno                    = lc_sonic
                    im_parttype                  = lc_ls
                  EXCEPTIONS
                    error_idoc_control           = 1
                    error_idoc_status            = 2
                    error_idoc_data              = 3
                    error_logical_system_unknown = 4
                    error_other                  = 5
                    OTHERS                       = 6.
                IF sy-subrc <> 0.
*                  Implement suitable error handling here
                ELSE.
                  MESSAGE 'Invoice outbound IDoc sent to Ariba'(044) TYPE 'I'.
                  SUBMIT rc1_idoc_set_status USING SELECTION-SCREEN '1000' WITH p_idoc =  wa_listedidc-docnum
                        WITH p_mestyp = c_zarbinv "'ZARBINV'
                        WITH p_status = c_51                "'51'
                        WITH p_staneu = c_68                "'68'
                        WITH p_test = '' EXPORTING LIST TO MEMORY AND RETURN.
                  IF sy-subrc = 0.
                    MESSAGE 'Invoice inbound IDoc status changed to 68'(045) TYPE 'I'.
                    gv_count = gv_count + 1.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        lv_flag = ''.
*   EOI PANUSURI Ticket 30830
    ENDCASE.
  ENDMETHOD.                           "handle_user_command
*-----------------------------------------------------------------
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
