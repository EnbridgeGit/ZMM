FUNCTION zmm_schdagr_smartform.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"     REFERENCE(IV_WID) TYPE  SWR_STRUCT-WORKITEMID
*"  EXPORTING
*"     REFERENCE(EV_DOCID) TYPE  SWR_ATT_ID
*"  TABLES
*"      LT_MLINES STRUCTURE  TLINE
*"      LT_MSTRUCT STRUCTURE  TLINE
*"----------------------------------------------------------------------
  INCLUDE <cntn01>.

  gv_ebeln = iv_ebeln.
  CLEAR: gv_okcode,
         gs_ekko,
         gs_ekpo,
         gs_eket,
         gs_lfa1,
         gs_lfm1,
         gs_t001,
         gs_t880,
         gs_t024,
         gs_rm06e,
         fs_stxh,
         w_potexttab,
         w_sahdrtexttab,
         w_remainder .
  REFRESH: fs_stxh,
           w_potexttab,
           w_sahdrtexttab.

  DATA: lt_job_output_info TYPE ssfcrescl,
        lt_otf TYPE itcoo OCCURS 0 WITH HEADER LINE,
        lt_pdf_tab LIKE tline OCCURS 0 WITH HEADER LINE,
        lv_bin_filesize TYPE i.

  DATA: ls_print_data_to_read TYPE ledlv_print_data_to_read,
        ls_dlv_delnote        TYPE ledlv_delnote,
        lf_fm_name            TYPE rs38l_fnam,
        ls_control_param      TYPE ssfctrlop,
        ls_composer_param     TYPE ssfcompop,
        ls_recipient          TYPE swotobjid,
        ls_sender             TYPE swotobjid,
        lf_formname           TYPE tdsfname,
        ls_addr_key           LIKE addr_key.
  DATA: lt_msglines TYPE TABLE OF swr_messag,
        ls_msglines TYPE swr_messag,
        lt_msgstruct TYPE TABLE OF swr_mstruc,
        ls_msgstruct TYPE swr_mstruc,
        ls_mlines   TYPE tline.

  DATA: lv_size TYPE i,
        lt_solix_tab TYPE TABLE OF tsfixml,
*      lv_bin_file type XSTRING,
        ls_att_head TYPE  swr_att_header,
        lv_att_txt  TYPE  string,
        lv_subrc TYPE sy-subrc,
        lv_archive_index  LIKE  toa_dara,
        lv_bin_file TYPE xstring.
* select print data
  PERFORM get_data CHANGING iv_ebeln.

  lf_formname = 'ZSCHEDAGREE'.

*  PERFORM set_print_param USING    ls_addr_key
*                          CHANGING ls_control_param
*                                   ls_composer_param
*                                   ls_recipient
*                                   ls_sender
*                                   cf_retcode.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
*     variant            = ' '
*     direct_call        = ' '
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language             = sy-langu
*     I_APPLICATION          = 'SAPDEFAULT'
    IMPORTING
      e_devtype              = ls_composer_param-tdprinter
    EXCEPTIONS
      no_language            = 1
      language_not_installed = 2
      no_devtype_found       = 3
      system_error           = 4
      OTHERS                 = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  ls_composer_param-tdnoprev = 'X'.
  ls_control_param-no_dialog = 'X'.
*ls_control_param-preview = space. " Suppressing the dialog box
  ls_control_param-getotf = 'X'.
*ls_control_param-device = 'PRINTER'.


  CALL FUNCTION lf_fm_name
    EXPORTING
*     archive_index        = toa_dara
*     archive_parameters   = arc_params
      control_parameters   = ls_control_param
*     mail_appl_obj        =
*     mail_recipient       = ls_recipient
*     mail_sender          = ls_sender
      output_options       = ls_composer_param
      user_settings        = 'X'
      comm_method          = w_comm_method
      fs_ekko              = gs_ekko
      fs_ekpo              = gs_ekpo
      fs_lfa1              = gs_lfa1
      fs_lfm1              = gs_lfm1
      fs_t001              = gs_t001
      fs_t880              = gs_t880
      fs_t024              = gs_t024
      fs_t001w             = fs_t001w  "COG
    IMPORTING
*     document_output_info =
      job_output_info      = lt_job_output_info
*     job_output_options   =
    TABLES
      vdr_potext           = w_potexttab
      sa_hdrtext           = w_sahdrtexttab
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.
  CHECK sy-subrc = 0.

  lt_otf[] = lt_job_output_info-otfdata[].

*COPYNUMBER LIKE  ITCPO-TDCOPIES
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
      archive_index         = lv_archive_index
      copynumber            = 0
      ascii_bidi_vis2log    = ' '
      pdf_delete_otftab     = ' '
    IMPORTING
      bin_filesize          = lv_bin_filesize
      bin_file              = lv_bin_file
    TABLES
      otf                   = lt_otf
      lines                 = lt_pdf_tab
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  IF sy-subrc <> 0.

  ENDIF.
*  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*    EXPORTING
*      buffer          = lv_bin_file
**     APPEND_TO_TABLE = ' '
*    IMPORTING
*      output_length   = lv_size
*    TABLES
*      binary_tab      = lt_solix_tab.

  ls_att_head-file_type = 'B'.
  ls_att_head-file_name = 'Purchase Schedule Agreement (COGS)'.
  ls_att_head-file_extension = 'PDF'.
  ls_att_head-language = 'EN'.

  CALL FUNCTION 'SAP_WAPI_ATTACHMENT_ADD'
    EXPORTING
      workitem_id    = iv_wid
      att_header     = ls_att_head
      att_txt        = 'Purchase Schedule Agreement (COGS)'
      att_bin        = lv_bin_file
      document_owner = sy-uname
      language       = sy-langu
      do_commit      = 'X'
    IMPORTING
      return_code    = lv_subrc
      att_id         = ev_docid
    TABLES
      message_lines  = lt_msglines
      message_struct = lt_msgstruct.

  LOOP AT lt_msglines INTO ls_msglines.
    CONCATENATE ls_msglines-msg_type '-'
                ls_msglines-line INTO ls_mlines-tdline.
    ls_mlines-tdformat = '*'.
    APPEND ls_mlines TO lt_mlines.
  ENDLOOP.
  LOOP AT lt_msgstruct INTO ls_msgstruct.
    CONCATENATE ls_msgstruct-msgty '-' ls_msgstruct-msgid '-'
                ls_msgstruct-msgno '-'
                ls_msgstruct-msgv1
                ls_msgstruct-msgv2
                INTO ls_mlines-tdline.
    ls_mlines-tdformat = '*'.
    APPEND ls_mlines TO lt_mstruct.
*     ls_msgstruct-MSGV3
*     ls_msgstruct-MSGV4
  ENDLOOP.
ENDFUNCTION.
