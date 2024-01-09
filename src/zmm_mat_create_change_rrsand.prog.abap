*&---------------------------------------------------------------------*
*& Report  ZMM_MAT_CREATE_CHANGE_RRSAND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmm_mat_create_change_rrsand.


INCLUDE zmm_mat_cc_rrsand_top.


SELECT-OPTIONS: s_matnr FOR mara-matnr,
*                s_mfrnr FOR mara-mfrnr.
                s_extid FOR mara-matnr.

PARAMETERS: p_log AS CHECKBOX.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_email FOR gv_email DEFAULT 'test@enbridge.com'.
PARAMETERS: p_dli TYPE soobjinfi1-obj_name.
PARAMETERS: p_subj TYPE so_obj_des OBLIGATORY DEFAULT 'ERROR'.
SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_pc  RADIOBUTTON GROUP r1,
            p_app RADIOBUTTON GROUP r1.

PARAMETERS: p_fname TYPE rlgrap-filename,
            p_arch AS CHECKBOX,
            p_aname TYPE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK b2 .

AT SELECTION-SCREEN.

  PERFORM validate_screen.

  INCLUDE zmm_mat_cc_rrsand_forms.

INITIALIZATION.
  p_fname = p_aname.
  REPLACE ALL OCCURRENCES OF '.xml' IN p_aname WITH 'arch.xml'.

*&---------------------------------------------------------------------*
*&      start-of-selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

* Read XML file
  PERFORM read_xml.

* Parse XML data
  PERFORM parse_xml.

* Create or Update material/clasi based
  PERFORM create_change_material.

END-OF-SELECTION.

*archive the file
*  PERFORM archive_file.

  PERFORM archive_file_v2.

*  Perform file_archive.

* Send error email
  IF s_email[] IS NOT INITIAL OR p_dli IS NOT INITIAL.
    PERFORM generate_error_email.
  ENDIF.

*Display application log(SLG1)
  IF p_log EQ 'X'.
    PERFORM log_display.
  ENDIF.

END-OF-SELECTION.
  IF gv_dset_msg IS INITIAL.
    WRITE: /'Please check t-code SLG1 with object type ZRIVERSAND for detailed log '.
  ELSE.
    WRITE:/ 'Application server File read error:'.
    WRITE: / p_fname.
    WRITE:/ gv_dset_msg.
  ENDIF.
