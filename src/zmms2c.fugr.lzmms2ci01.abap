*----------------------------------------------------------------------*
***INCLUDE LZMMS2CI01 .
*----------------------------------------------------------------------*


*  MODULE TC_ZISNQUAL_MODIFY INPUT
MODULE tc_zisnqual_modify INPUT.

  MOVE-CORRESPONDING zisnqual TO ls_zisnqual.
  MOVE selected TO ls_zisnqual-selected.
  MODIFY  lt_zisnqual
    FROM  ls_zisnqual
    INDEX tc_zisnqual-current_line.
  IF sy-subrc = 0.
    "at least 1 value changed
    IF lv_table_change = abap_true.
      lv_tblchanged = abap_true.
    ENDIF.
  ENDIF.
ENDMODULE.                    "TC_ZISNQUAL_MODIFY INPUT



*----------------------------------------------------------------------*
*  MODULE btn_control INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE btn_control INPUT.

  ASSIGN (c_okcode) TO <fs_okcode>.
  lv_okcode = <fs_okcode>.

  IF lv_okcode IS INITIAL.
    "Enter was pressed.
    EXIT.
  ENDIF.

  IF sy-ucomm <> 'UPDA' AND lv_okcode = 'UPDA'.
    "A different function was called but the main
    "screen changed it to update.
    "Update and exit the PAI
    IF lv_tblchanged = abap_true.
      PERFORM save_table.
      PERFORM update_table.
      lv_tblchanged = abap_false.
    ENDIF.
    EXIT.
  ENDIF.

  CASE sy-ucomm.

    WHEN 'UPDA'.
      IF lv_tblchanged = abap_true.
        PERFORM save_table.
        PERFORM update_table.
        lv_tblchanged = abap_false.
      ENDIF.
      EXIT.

    WHEN 'PF03' OR 'STRD'.
      IF lv_tblchanged = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            titel         = 'Save Changes?'
            textline1     = 'Should the changes to ISNetworld Vendor Qualifications'
            textline2     = 'be saved?'
            defaultoption = 'J'
          IMPORTING
            answer        = lv_answer.

        CASE lv_answer.

          WHEN 'J'.
            "Yes
            PERFORM save_table.
            "PERFORM update_table.
          WHEN 'N'.
            "No
            "Allow to continue
          WHEN 'A'.
            "Cancel
            "Change lv_okcode
            lv_okcode = ''.
            <fs_okcode> = lv_okcode.
            EXIT.
        ENDCASE.
      ENDIF.
      lv_table_change = abap_false.
      PERFORM update_table.

    WHEN 'CHGDSP'.

      IF lv_tblchanged = abap_true.

        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            titel         = 'Save Changes?'
            textline1     = 'Should the changes to ISNetworld Vendor Qualifications'
            textline2     = 'be saved?'
            defaultoption = 'J'
          IMPORTING
            answer        = lv_answer.

        CASE lv_answer.

          WHEN 'J'.
            "Yes
            PERFORM save_table.
            PERFORM update_table.
          WHEN 'N'.
            "No
            PERFORM update_table.
          WHEN 'A'.
            "Cancel
            EXIT.
        ENDCASE.

      ENDIF.

      IF lv_table_change = abap_true.
        lv_table_change = abap_false.
      ELSEIF lv_table_change = abap_false.
        lv_table_change = abap_true.
      ENDIF.
      PERFORM update_display.

    WHEN 'TBLADD'.
      INSERT INITIAL LINE INTO lt_zisnqual INDEX 1.
      lv_tblchanged = abap_true.

    WHEN 'TBLDEL'.
      LOOP AT lt_zisnqual INTO ls_zisnqual WHERE selected = abap_true.
        DELETE TABLE lt_zisnqual FROM ls_zisnqual.
      ENDLOOP.
      lv_tblchanged = abap_true.

    WHEN 'TBLSAVE'.
      PERFORM save_table.
      lv_table_change = abap_false.
      PERFORM update_table.

  ENDCASE.



ENDMODULE.                    "btn_control INPUT



*  MODULE export_isnvdr INPUT
MODULE export_isnvdr INPUT.
  "CLEAR: gv_isnvdr.
  ASSIGN (c_field_zzisnvdr) TO <isnvdr>.
  IF gv_isnvdr <> <isnvdr>.
  gv_isnvdr = <isnvdr>.

    EXPORT act FROM gv_isnvdr TO MEMORY ID 'VENDORADDDATACS_S2C-ZZISNVDR'.
    PERFORM update_table.
  ENDIF.
ENDMODULE.                    "export_isnvdr INPUT



















************************************
**** Display Control
************************************
FORM update_display.
  "Update Table
  IF lv_aktyp = 'A'.
    LOOP AT tc_zisnqual-cols INTO ls_col.
      ls_col-screen-input = 0.
      MODIFY tc_zisnqual-cols FROM ls_col.
    ENDLOOP.
  ELSE.
    IF lv_table_change = abap_true.

      CALL FUNCTION 'ENQUEUE_EZ_ZISNQUAL'
        EXPORTING
          isnvdr = gv_isnvdr.

      LOOP AT tc_zisnqual-cols INTO ls_col.
        ls_col-screen-input = 1.
        MODIFY tc_zisnqual-cols FROM ls_col.
      ENDLOOP.
    ELSE.
      CALL FUNCTION 'DEQUEUE_EZ_ZISNQUAL'
        EXPORTING
          isnvdr = gv_isnvdr.

      LOOP AT tc_zisnqual-cols INTO ls_col.
        ls_col-screen-input = 0.
        MODIFY tc_zisnqual-cols FROM ls_col.
      ENDLOOP.
    ENDIF.
  ENDIF.



  "Update Screen Elements.
  LOOP AT SCREEN.
    IF lv_aktyp = 'A'.
      "Display Only Everything
      screen-input = 0.
    ELSE.
      IF screen-group1  = 'TBL'.
        "Table Objects
        IF lv_table_change = abap_true.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
      ELSEIF screen-group1 = 'VND'.
        "Vendor ID
        IF lv_table_change = abap_true.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      ELSE.
        "screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.



  tc_zisnqual-lines = 0.
  LOOP AT lt_zisnqual INTO ls_zisnqual.
    tc_zisnqual-lines = tc_zisnqual-lines + 1.
  ENDLOOP.

  IF tc_zisnqual-lines = 0.
    CLEAR ls_zisnqual.
    APPEND ls_zisnqual TO lt_zisnqual.
    tc_zisnqual-lines = 1.
  ENDIF.

ENDFORM.                    "update_display

**** save_table
FORM save_table.
  LOOP AT lt_zisnqual INTO ls_zisnqual.
    SELECT SINGLE * FROM zisnqual
                    INTO ls_zisnqualdb
                    WHERE isnvdr = gv_isnvdr
                      AND land1 = ls_zisnqual-land1
                      AND regio = ls_zisnqual-regio
    .

    IF sy-subrc <> 0.
      "If line is not in DB, add it.
      CLEAR ls_zisnqualdb.
      IF ls_zisnqual-land1 IS NOT INITIAL AND ls_zisnqual-regio IS NOT INITIAL.
        ls_zisnqualdb-isnvdr  = gv_isnvdr.
        ls_zisnqualdb-land1   = ls_zisnqual-land1.
        ls_zisnqualdb-regio   = ls_zisnqual-regio.
        ls_zisnqualdb-isnqual = ls_zisnqual-isnqual.
        ls_zisnqualdb-erdat   = sy-datum.
        ls_zisnqualdb-ernam   = sy-uname.
        INSERT zisnqual FROM ls_zisnqualdb.

        ls_zisnqualdb-mandt = sy-mandt.
        lv_objid = gv_isnvdr.
        ls_zisnqual2 = ls_zisnqualdb.
        CLEAR ls_zisnqual2-isnvdr.
        CALL FUNCTION 'ZISNQUAL_WRITE_DOCUMENT'
          EXPORTING
            objectid                = lv_objid
            tcode                   = sy-tcode
            utime                   = sy-uzeit
            udate                   = sy-datum
            username                = sy-uname
            object_change_indicator = 'U'
            n_zisnqual              = ls_zisnqualdb
            o_zisnqual              = ls_zisnqualdb
            upd_zisnqual            = 'I'.

      ENDIF.
    ELSE.
      "Line does exist, update it with new isnqual value
      IF ls_zisnqualdb-isnqual <> ls_zisnqual-isnqual.
        ls_zisnqualdb-laeda = sy-datum.
        ls_zisnqualdb-aenam = sy-uname.
        ls_zisnqual2 = ls_zisnqualdb.
        ls_zisnqualdb-isnqual = ls_zisnqual-isnqual.
        MODIFY zisnqual FROM ls_zisnqualdb.

        lv_objid = gv_isnvdr.
        CALL FUNCTION 'ZISNQUAL_WRITE_DOCUMENT'
          EXPORTING
            objectid                = lv_objid
            tcode                   = sy-tcode
            utime                   = sy-uzeit
            udate                   = sy-datum
            username                = sy-uname
            object_change_indicator = 'U'
            n_zisnqual              = ls_zisnqualdb
            o_zisnqual              = ls_zisnqual2
            upd_zisnqual            = 'U'.

      ENDIF.



    ENDIF.
  ENDLOOP.

  "Look for stuff that was deleted from local table.
  SELECT * FROM zisnqual
           INTO ls_zisnqualdb
           WHERE isnvdr = gv_isnvdr.

    CLEAR ls_zisnqual.
    ls_zisnqual-land1 = ls_zisnqualdb-land1.
    ls_zisnqual-regio = ls_zisnqualdb-regio.
    ls_zisnqual-isnqual = ls_zisnqualdb-isnqual.

    READ TABLE lt_zisnqual FROM ls_zisnqual INTO ls_zisnqual COMPARING ALL FIELDS TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      "If it doesn't exist in the local table delete it
      DELETE zisnqual FROM ls_zisnqualdb.

      lv_objid = gv_isnvdr.
      ls_zisnqual2 = ls_zisnqualdb.
      ls_zisnqual2-isnvdr = ' '.
      CALL FUNCTION 'ZISNQUAL_WRITE_DOCUMENT'
        EXPORTING
          objectid                = lv_objid
          tcode                   = sy-tcode
          utime                   = sy-uzeit
          udate                   = sy-datum
          username                = sy-uname
          object_change_indicator = 'U'
          n_zisnqual              = ls_zisnqual2
          o_zisnqual              = ls_zisnqualdb
          upd_zisnqual            = 'D'.

    ENDIF.
  ENDSELECT.
ENDFORM.                    "save_table

**** update_table
FORM update_table.
  CLEAR lt_zisnqual.
  SELECT * FROM zisnqual
           INTO CORRESPONDING FIELDS
           OF TABLE lt_zisnqual
           WHERE isnvdr = gv_isnvdr.
  lv_tblchanged = abap_false.
  CALL FUNCTION 'DEQUEUE_EZ_ZISNQUAL'
    EXPORTING
      isnvdr = gv_isnvdr.

ENDFORM.                    "update_table
