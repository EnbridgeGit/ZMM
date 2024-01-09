report  zlmmi007_ekkoupd line-size 132 message-id zs
        line-count 65 no standard page heading.
*&---------------------------------------------------------------------*
*& Author: Glenn Ymana
*& Date  : May, 2011.
*& Description:
*& This program will update the Sent to CARE indicator field on table
*& EKKO based on the values in the selection screen.
*&---------------------------------------------------------------------*
tables: ekko.

data: begin of itab_pdoc occurs 0,
          ebeln      like ekko-ebeln,
          oldsentind like ekko-zzsent_ind,
          newsentind like ekko-zzsent_ind,
          upd_msg(50) type c,
      end of itab_pdoc.

data: begin of itab_err occurs 0,
          msg(50) type c,
      end of itab_err.

data: w_returncode  type i,
      w_msg(50)     type c,
      ln_cntr       type i value 99,
      i_count       type i value 0,
      w_err_flag    type c value 'N',
      w_nodata_flag type c value 'N'.

*----------------------------------------------------------------------*
* selection screen
*----------------------------------------------------------------------*
selection-screen begin of block box1 with frame title text-001.
select-options:  p_prdoc  for ekko-ebeln obligatory,
                 p_doccat for ekko-bstyp obligatory default 'L'.
parameters:      p_ekorg  like ekko-ekorg obligatory default 'GASA'.
selection-screen end of block box1.
selection-screen begin of block box2 with frame title text-st1.
parameters:      p_sntind like ekko-zzsent_ind obligatory default 'Y',
                 p_testrn(1)   type c default 'X'.
selection-screen end of block box2.

*----------------------------------------------------------------------*
*                  at selection-screen
*----------------------------------------------------------------------*
at selection-screen output.

*  move 'E' to p_s_type-sign.
*  move 'EQ' to p_s_type-option.
*  move 'ZLOC' to p_s_type-low.
*  append p_s_type.
*  move 'E' to p_s_type-sign.
*  move 'EQ' to p_s_type-option.
*  move 'ZDP' to p_s_type-low.
*  append p_s_type.

  loop at screen.
    if screen-name = 'P_EKORG'.
      screen-input = '0'.
      modify screen.
    endif.
  endloop.

*----------------------------------------------------------------------*
*                  start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

  perform generate_report_header.

  select * from ekko
    where ebeln in p_prdoc
      and bstyp in p_doccat
      and ekorg = p_ekorg
      and frgke = 'Y'
    order by ebeln.

    if sy-subrc = 0.
      perform update_sent_ind.
    else.
      w_nodata_flag = 'Y'.
    endif.

  endselect.

  if w_nodata_flag <> 'Y'.
    sort itab_pdoc by ebeln.
    loop at itab_pdoc.
      perform write_report_detail.
    endloop.
  else.
    skip.
    write: /45    '*** No data to process ***'.
    add +2 to ln_cntr.
  endif.

*&---------------------------------------------------------------------*
*&      Form  update_sent_ind
*&---------------------------------------------------------------------*
form update_sent_ind.

  move ekko-ebeln      to itab_pdoc-ebeln.
  move ekko-zzsent_ind to itab_pdoc-oldsentind.

  if p_sntind = 'Y'.
    if ekko-zzsent_ind = 'Y'.
      move 'ERROR: Indicator already set to Y.'
            to itab_pdoc-upd_msg.
    else.
      move 'Y' to: ekko-zzsent_ind,
                   itab_pdoc-newsentind.
      perform update_ekko.
    endif.
  elseif p_sntind = 'X'.
    if ekko-zzsent_ind = ''.
      move 'ERROR: Indicator already blank (Not set).'
           to itab_pdoc-upd_msg.
    else.
      move ' ' to: ekko-zzsent_ind,
                   itab_pdoc-newsentind.
      perform update_ekko.
    endif.
  endif.

  append itab_pdoc.

endform.                    "update_sent_ind

*&---------------------------------------------------------------------*
*&      Form  update_ekko
*&---------------------------------------------------------------------*
form update_ekko.

  if p_testrn = 'X'.
    move 'Not Updated - Test Run Only' to itab_pdoc-upd_msg.
  else.
    move 'Updated Successfully' to itab_pdoc-upd_msg.
    update ekko.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  generate_report_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form generate_report_header.
  new-page.
  clear ln_cntr.
  format intensified on.
  write: /1 text-002, 35 text-003.
  write: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  write: /50 sy-datum.
  write: 121 text-pge, sy-pagno.
  skip.
  write:  /3 text-004,        "Scheduling
          23 text-006,        "Old
          38 text-008.        "New
  write:  /3 text-005,        "Agreement
          20 text-007,        "Sent Ind
          35 text-009,        "Sent Ind
          50 text-010.        "Message

  skip 1.
  move '6' to ln_cntr.
  format intensified off.

endform.                    "generate_report_header
*&---------------------------------------------------------------------*
*&      Form  write_report_detail
*&---------------------------------------------------------------------*
form write_report_detail.

  if ln_cntr >= 55.
    perform generate_report_header.
  endif.

  write: /3 itab_pdoc-ebeln,
         24 itab_pdoc-oldsentind,
         39 itab_pdoc-newsentind,
         50 itab_pdoc-upd_msg.
  skip.
  add +2 to ln_cntr.

endform.                    "write_report_detail
