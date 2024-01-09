*&---------------------------------------------------------------------*
*& Program Name       :  ZMM_ARIBA_INV_REJECT                          *
*& Include Name       :  ZRSEIDOC_I01                                  *
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
MODULE pai INPUT.

* methode dispatch muß gerufen werden, weil diese Methode den Event-
* handler für die events ruft

  CALL METHOD cl_gui_cfw=>dispatch.

  IF ok_code = 'EXIT' OR ok_code = 'BACK'.                  "#EC NOTEXT
* aufräumen
*    if docking = 'O'.
    CLEAR custom_container.
    CLEAR vh_tree.
    CALL METHOD docking_control->free
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CLEAR docking_control.
*   endif.
    CLEAR akt_statustext.
    CLEAR akt_message.
    REFRESH change_mestyp.
*    IF ( sy-tcode = 'WE02' OR sy-tcode = 'WE05' ) AND ok_code = 'BACK'. "BOC PANUSURI Ticket 30830
    IF ( sy-tcode = 'WE02' OR sy-tcode = 'WE05' OR sy-tcode = 'ZARB_INV_RJCT' ) AND ok_code = 'BACK'. "BOI PANUSURI Ticket 30830
      SET SCREEN 0.
      LEAVE SCREEN.
    ELSE.
      LEAVE PROGRAM.
    ENDIF.
  ELSEIF ok_code = 'STAL'.
    IF g_node_key IS INITIAL. " dann nichts selektiert
      MESSAGE i109.
    ELSE.
      PERFORM show_status_list.
    ENDIF.
  ELSEIF ok_code = 'DATL'.
    IF g_node_key IS INITIAL. " dann nichts selektiert
      MESSAGE i109.
    ELSE.
      PERFORM data_record_list.
    ENDIF.
  ELSEIF ok_code = 'TEXP'.                   "#EC NOTEXT   "Expandieren
    IF g_node_key IS INITIAL." dann nichts selektiert
      MESSAGE i109.
    ELSE.
      CALL METHOD vh_tree->expand_node
        EXPORTING
          node_key       = g_node_key
          expand_subtree = 'X'
        EXCEPTIONS
          OTHERS         = 04.
    ENDIF.
  ELSEIF ok_code = 'TCOM'.                  "#EC NOTEXT   "Komprimieren
    IF g_node_key IS INITIAL." dann nichts selektiert
      MESSAGE i109.
    ELSE.
      CALL METHOD vh_tree->collapse_subtree
        EXPORTING
          node_key = g_node_key
        EXCEPTIONS
          OTHERS   = 04.
    ENDIF.
  ELSEIF ok_code = 'AUFF'.                   "#EC NOTEXT   "Auffrischen
    CLEAR vh_tree.
* neu selektieren int_edidc, i_edidc, i_listedidc muß alles neu gefüllt
* werden
    PERFORM select_edidc.
    i_edidc[] = int_edidc[].
    PERFORM fill_alv_list.
    gs_layout-grid_title = text-021.
    CALL METHOD docking_control->free
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CLEAR docking_control.
    CLEAR akt_statustext.
    CLEAR akt_message.
    REFRESH change_mestyp.
  ENDIF.
  CASE ok_code.
    WHEN 'PARS'.
      PERFORM save_layout_settings.
    WHEN 'PARD'.
      PERFORM delete_layout_settings.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                    "pai INPUT
*----------------------------------------------------------------------*
