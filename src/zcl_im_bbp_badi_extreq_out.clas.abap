class ZCL_IM_BBP_BADI_EXTREQ_OUT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BBP_BADI_EXTREQ_OUT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BBP_BADI_EXTREQ_OUT .
*"* protected components of class ZCL_IM_BBP_BADI_EXTREQ_OUT
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_BBP_BADI_EXTREQ_OUT
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_BBP_BADI_EXTREQ_OUT IMPLEMENTATION.


METHOD if_ex_bbp_badi_extreq_out~bbp_grouping_mapping.
  DATA : ls_item_cust_imp TYPE bbps_er_sc_item_cust_c_pi ,
         lt_item_cust_imp TYPE bbpt_er_sc_item_cust_c_pi,
         lt_text_imp      TYPE bbpt_er_text_i,
         ls_text_imp      TYPE bbps_er_text_i,
         ls_item_imp      TYPE bbps_er_item_c,
         lt_item_imp      TYPE bbpt_er_item_c,
         ls_eban          TYPE eban,
         lv_name          TYPE thead-tdname,
         lt_lines         TYPE TABLE OF tline,
         ls_lines          TYPE tline,
         lt_limit         TYPE bbpt_er_limit_c,
         ls_limit         TYPE bbps_er_limit_c.
  DATA : lv_first         TYPE c VALUE 'X'. "(+)PANUSURI Ticket 90464

  lt_item_imp[] = item_imp[].
*  lt_text_imp[] = text_imp[].
  ls_eban = transtab_imp-eban.
  lt_limit[] = limit_imp[].

  CLEAR ls_item_cust_imp.


  LOOP AT lt_item_imp INTO ls_item_imp WHERE item_guid = transtab_imp-eban-bnfpo .
    CONCATENATE ls_eban-banfn ls_item_imp-item_number+5(5) INTO lv_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*     CLIENT                        = SY-MANDT
        id                            = 'B03'
        language                      = 'E'
        name                          = lv_name
        object                        = 'EBAN'
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
      TABLES
        lines                         = lt_lines
     EXCEPTIONS
        id                            = 1
        language                      = 2
        name                          = 3
        not_found                     = 4
        object                        = 5
        reference_check               = 6
        wrong_access_to_archive       = 7
        OTHERS                        = 8 .


    IF sy-subrc = 0.

      LOOP AT lt_lines INTO ls_lines.
        ls_text_imp-parent_guid = ls_item_imp-item_number+5(5).
        ls_text_imp-text_id = 'CIT1'.
*       BOI by PANUSURI Ticket 90464
        IF ls_lines-tdformat = space.
          CONCATENATE ' ' ls_lines-tdline INTO ls_lines-tdline RESPECTING BLANKS.
        ENDIF.
        IF lv_first <> 'X'.
          IF ls_lines-tdformat = '*'.
            CONCATENATE cl_abap_char_utilities=>newline ls_lines-tdline INTO ls_lines-tdline.
          ENDIF.
        ENDIF.
*       EOI by PANUSURI Ticket 90464
        ls_text_imp-text_line = ls_lines-tdline.
        ls_text_imp-langu_iso = 'EN'.

        APPEND ls_text_imp TO lt_text_imp.

        APPEND LINES OF lt_text_imp TO text_imp.
        lv_first = ' '.   "(+)PANUSURI Ticket 90464
        CLEAR lt_text_imp.
        REFRESH lt_text_imp.

      ENDLOOP.

    ENDIF.
    CLEAR : ls_item_imp.
  ENDLOOP.

  LOOP AT lt_item_imp INTO ls_item_imp WHERE parent = transtab_imp-eban-bnfpo .
    ls_item_cust_imp-zzariba_approver = transtab_imp-eban-zzariba_approver .
    ls_item_cust_imp-zzorigreq = transtab_imp-eban-zzorigreq .
    ls_item_cust_imp-parent_guid = ls_item_imp-item_guid.
    APPEND ls_item_cust_imp TO lt_item_cust_imp.
    CLEAR ls_item_cust_imp.
  ENDLOOP.

  APPEND LINES OF lt_item_cust_imp TO item_cust_imp.

*Limit PR contract transfer
  IF ls_eban-pstyp = '9'.
    READ TABLE lt_limit INTO ls_limit WITH KEY parent_guid = ls_eban-packno lim_type = 'G'. "Added by PANUSURI on 01/03/2012 - Service contract issue
    IF sy-subrc = 0 AND ls_eban-konnr IS NOT INITIAL AND ls_eban-ktpnr IS NOT INITIAL.
      ls_limit-lim_type = 'K'.
      CLEAR ls_limit-exp_value.
      ls_limit-lim_ref_h_id = ls_eban-konnr.
      ls_limit-lim_ref_i_id = ls_eban-ktpnr.
      APPEND ls_limit TO limit_imp.
      CLEAR ls_limit.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
