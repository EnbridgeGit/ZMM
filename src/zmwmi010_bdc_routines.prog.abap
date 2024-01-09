***INCLUDE ZCABDC_ROUTINES
************************************************************************
* Author       : Len Harris (with credit to Santosh Cordeiro)          *
* Date Created : August 14, 2001                                       *
* Function     : This include is for using BDC to load data to SAP     *
*                The main forms are:                                   *
*                Form CLOSE_BDC_GROUP                                  *
*                Form LOAD_EDITED_DATA                                 *
*                Form DYNPRO                                           *
************************************************************************
* Program Maintenance History                                          *
*----------------------------------------------------------------------*
* Date Changed | Developer       | Description Of Change               *
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
************************************************************************

* Global internal table 'bdc_data' MUST be used to use the call
* transaction/bdc functions.
data: begin of common part znybdc,
        bdc_data       like bdcdata occurs 50 with header line,
      end of common part.
data: subrc       like sy-subrc,
      bdc_created type c.
*&---------------------------------------------------------------------*
*&      Form  OPEN_BDC
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p_group, BDC session name
*  -->  p_user,  SAP User Name
*  -->  p_keep,  Keep Session
*  <--  p_created, Flag indicating successfully opened.
*----------------------------------------------------------------------*
form open_bdc using value(p_group) like rptaxxxx-map_name
                    value(p_user)  like apqi-userid
                    value(p_keep)  like apqi-qerase
          changing  p_created   type c.

  if p_created = ' '.

    call function 'BDC_OPEN_GROUP'
         exporting
              client              = sy-mandt
              group               = p_group
              user                = p_user
              keep                = p_keep
         exceptions
              client_invalid      = 1
              destination_invalid = 2
              group_invalid       = 3
              group_is_locked     = 4
              holddate_invalid    = 5
              internal_error      = 6
              queue_error         = 7
              running             = 8
              system_lock_error   = 9
              user_invalid        = 10
              others              = 11.

    if sy-subrc = 0.
      p_created = 'X'.
    endif.

  endif.

endform.                               " OPEN_BDC
*&---------------------------------------------------------------------*
*&      Form  INSERT_BDC
*&---------------------------------------------------------------------*
*  --> p_tcode, Transaction Code
*----------------------------------------------------------------------*
form insert_bdc using p_tcode like tstc-tcode.

  call function 'BDC_INSERT'
       exporting
            tcode            = p_tcode
       tables
            dynprotab        = bdc_data
       exceptions
            internal_error   = 1
            not_open         = 2
            queue_error      = 3
            tcode_invalid    = 4
            printing_invalid = 5
            posting_invalid  = 6
            others           = 7.

endform.                               " INSERT_BDC
*&---------------------------------------------------------------------*
*&      Form  CLOSE_BDC_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form close_bdc_group.

  call function 'BDC_CLOSE_GROUP'
       exceptions
            not_open    = 1
            queue_error = 2
            others      = 3.

endform.                               " CLOSE_BDC_GROUP
*&---------------------------------------------------------------------*
*&      Form  LOAD_EDITED_DATA
*&---------------------------------------------------------------------*
* --> p_tcode,    Transaction code
* --> p_online,   Mode of call transaction, A -> Online
*                                           E -> Display errors only
*                                           N -> No display, write
*                                                errors to spool, save
*                                                data to bdc session
*                                       blank -> Batch input mode
* --> p_group,    Name of the BDC session to be created if one hasn't
*                 been created yet.
* --> p_keep,     Flag, Keep BDC session after it has been successfully
*                 run.
* --> p_update_mode, 'S' run Synchronous,
*                    'A' run Asynchronous.
* <-- p_created,  Flag, if a BDC session has already been created or
*                 one has just been created this flag is set to 'X'
* <-- p_rcode,    Sub return code of the Call Transaction or the BDC
*                 insert call.
*----------------------------------------------------------------------*
form load_edited_data using    value(p_tcode)       type c
                               value(p_online)      type c
                               value(p_group)       type c
                               value(p_keep)        type c
                               value(p_update_mode) type c
                      changing p_created            type c
                               p_rcode              like sy-subrc
                               p_inserted           type c.

clear p_inserted .

  data: l_group like rptaxxxx-map_name,
        l_user  like apqi-userid,
        l_keep  like apqi-qerase.

  data: l_messtab like bdcmsgcoll occurs 10 with header line.

  l_group = p_group.
  l_user  = sy-uname.
  l_keep  = p_keep.

  if p_online ca 'AEN'.
    call transaction p_tcode using  bdc_data
                             mode   p_online
                             update p_update_mode
                             messages into l_messtab.
* if an error occurs in the call transaction send the information to
* a batch session.
    p_rcode = sy-subrc.
    if p_rcode ne 0.
      perform write_error_message tables l_messtab.
      if p_created = ' '.
        perform open_bdc using    l_group
                                  l_user
                                  l_keep
                         changing p_created.
      endif.
      perform insert_bdc using p_tcode.
     p_inserted = 'X'.

    endif.
  elseif p_online = ' '.
    if p_created = ' '.
      perform open_bdc using    l_group
                                l_user
                                l_keep
                       changing p_created.
    endif.
    perform insert_bdc using p_tcode.

    p_rcode = sy-subrc.
    p_inserted = 'X'.
  endif.

endform.                               " LOAD_EDITED_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR_MESSAGE
*&---------------------------------------------------------------------*
* --> p_messtab, Errors from a call transaction into messtab
*----------------------------------------------------------------------*
form write_error_message tables   p_messtab structure bdcmsgcoll.

  data: l_message like t100-text.

* The following statement only allows unique error messages to be
* displayed any duplicates entries are deleted.

  delete adjacent duplicates from p_messtab comparing all fields.

  loop at p_messtab.

    call function 'MESSAGE_TEXT_BUILD'
         exporting
              msgid               = p_messtab-msgid
              msgnr               = p_messtab-msgnr
              msgv1               = p_messtab-msgv1
              msgv2               = p_messtab-msgv2
              msgv3               = p_messtab-msgv3
              msgv4               = p_messtab-msgv4
         importing
              message_text_output = l_message
         exceptions
              others              = 1.

    concatenate p_messtab-msgtyp ':' l_message into l_message.

    write: / l_message color col_negative.

  endloop.

endform.                               " WRITE_ERROR_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
* --> p_begin, Flag indicating 'X' for screen information or ' ' for
*              field
* --> p_fnam,  Program name or field name depending on p_begin
* --> p_fval,  Screen number or field value depending on p_begin
*----------------------------------------------------------------------*
form dynpro using p_begin type c
                  p_fnam  type c
                  p_fval  type any.

  data: l_field_type(1) type c,
        l_field_length type i.

  clear bdc_data.

  if p_begin = 'X'.
    bdc_data-dynbegin = 'X'.
    bdc_data-program  = p_fnam.
    bdc_data-dynpro   = p_fval.
  else.
* this check will properly format the date field to the users defaults.
* Note: for this 'describe' to work properly, p_fval should be defined
* with the correct type and length as the screen field that your
* filling
    describe field p_fval type          l_field_type
                          output-length l_field_length.
    if l_field_type = 'D'.
      if not ( p_fval = 0 or p_fval = space )." Do not write a zero date
        write p_fval to bdc_data-fval dd/mm/yyyy.
      endif.
    else.
      move p_fval to bdc_data-fval(l_field_length).
    endif.
    move p_fnam to bdc_data-fnam.
  endif.

  append bdc_data.

endform.                               " DYNPRO
