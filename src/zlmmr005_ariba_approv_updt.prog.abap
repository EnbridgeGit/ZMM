*&---------------------------------------------------------------------*
*& Report  ZLMMR005_ARIBA_APPROV_UPDT
*&---------------------------------------------------------------------*
************************************************************************
*                                                                      *
*  Client:    Spectra Energy                                           *
*  Author:    John Hartung                                             *
*  Date:      March 30, 2011                                           *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*     - Maintain the Ariba Approver transparent table                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 03/30/11 0872 JRHARTU C11K921919 - Initial program development       *
* 04/16/11 0872 JRHARTU C11K921993 - Initial program development       *
*                                    Pre-go-live enhancements          *
* 01/08/12 0926 BTBOUNDY - Add Service Confirmer to Ariba Table        *
*----------------------------------------------------------------------*
************************************************************************
REPORT  zlmmr005_ariba_approv_updt  MESSAGE-ID  za
                                    NO STANDARD PAGE HEADING
                                    LINE-SIZE   132.

************************************************************************
*                                Tables                                *
************************************************************************
TABLES: zaribaaprv.                              "Ariba Approver

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_gs_aprvr,                     "Ariba Approver
        zariba_approver  TYPE z_ariba_approver,  "Ariba Approver
       END   OF ty_gs_aprvr.

TYPES:  ty_gt_aprvr      TYPE STANDARD TABLE OF ty_gs_aprvr.

TYPES: BEGIN OF ty_gs_aribaaprv.                 "Ariba Approver-Table
INCLUDE TYPE zaribaaprv.
TYPES: END   OF ty_gs_aribaaprv.

TYPES:  ty_gt_aribaaprv  TYPE STANDARD TABLE OF ty_gs_aribaaprv.

TYPES: BEGIN OF ty_gs_aribaaprv_upload,          "Ariba Approver Upload
        zariba_approver  TYPE z_ariba_approver,  "Ariba Approver
        zariba_email     TYPE z_ariba_email,     "Ariba Appr Email Addr
        zsvr_conf_sap_id TYPE xubname, "BTBOUNDY
        flag_delete      TYPE flag,              "Flag-Delete
       END   OF ty_gs_aribaaprv_upload.

TYPES:  ty_gt_aribaaprv_upload
                         TYPE STANDARD TABLE OF ty_gs_aribaaprv_upload.

TYPES: BEGIN OF ty_gs_aribaaprv_alv,             "Ariba Approver ALV Dsp
        zariba_approver  TYPE z_ariba_approver,  "Ariba Approver
        zariba_email     TYPE z_ariba_email,     "Ariba Appr Email Addr
        zsvr_conf_sap_id TYPE xubname, "BTBOUNDY
        flag_update      TYPE flag,
        linecolor        TYPE char4,
        cellio_tab       TYPE lvc_t_styl,
       END   OF ty_gs_aribaaprv_alv.

TYPES:  ty_gt_aribaaprv_alv
                         TYPE STANDARD TABLE OF ty_gs_aribaaprv_alv.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE char1              "X, True, Yes
                         VALUE 'X',
        gc_asc           TYPE char10             "ASCII
                         VALUE 'ASC',
        gc_alv_container TYPE scrfname           "ALV Container
                         VALUE 'SCREEN_9000_CONTAINER',
        gc_alv_structure TYPE tabname            "ALV Structure
                         VALUE 'ZMMS_ARIBAAPRV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_subrc         TYPE sysubrc,           "Return Value
        gv_tabix         TYPE sytabix,           "Internal Table Index
        gv_repid         TYPE sycprog,           "Report ID
        gv_ucomm         TYPE syucomm,           "Screen Function Code
        gv_ok_code       TYPE syucomm.           "Screen Function Code

DATA:   gv_zariba_approver                       "Ariba Approver
                         TYPE z_ariba_approver,
        gv_filename      TYPE localfile.         "Filename

DATA:   gv_count_rows_deleted                    "Count-Rows Deleted
                         TYPE syindex,
        gv_flag_alv_dsp  TYPE flag,              "Flag-Display
        gv_flag_alv_upd  TYPE flag,              "Flag-Update
        gv_flag_capture_delete                   "Flag-Capture Delete
                         TYPE flag,
        gv_flag_marked_delete                    "Flag-Marked For Delete
                         TYPE flag,
        gv_flag_error_data                       "Flag-Data Error
                         TYPE flag,
        gv_flag_exit     TYPE flag,              "Flag-Exit
        gv_flag_save     TYPE flag,              "Flag-Save
        gv_flag_sel_opt  TYPE flag,              "Flag-Select By Option
        gv_flag_sel_tab  TYPE flag.              "Flag-Select By Table

************************************************************************
*                                Ranges                                *
************************************************************************

*eject
************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_aprvr         TYPE ty_gs_aprvr,       "Ariba Approver
        gs_aribaaprv     TYPE ty_gs_aribaaprv,   "Ariba Approver-Table
        gs_aribaaprv_upload                      "Ariba Approver Upload
                         TYPE ty_gs_aribaaprv_upload,
        gs_aribaaprv_alv                         "Ariba Approver ALV Dsp
                         TYPE ty_gs_aribaaprv_alv.

DATA:   gs_variant       TYPE disvariant,        "ALV Display Variant
        gs_variant_p     TYPE disvariant,        "ALV Display Variant
        gs_layout        TYPE lvc_s_layo,        "LVC Layout Structure
        gs_print         TYPE lvc_s_prnt,        "LVC Print Settings
        gs_sort          TYPE lvc_s_sort.        "LVC Sort

DATA:   gs_del_row       TYPE lvc_s_moce.        "ALV Contrl Modfd Cells

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_aprvr         TYPE ty_gt_aprvr.       "Ariba Approver

DATA:   gt_aribaaprv     TYPE ty_gt_aribaaprv.   "Ariba Approver-Table

DATA:   gt_aribaaprv_upload                      "Ariba Approver Upload
                         TYPE ty_gt_aribaaprv_upload.

DATA:   gt_aribaaprv_errors                      "Ariba Approver Errors
                         TYPE ty_gt_aribaaprv_upload.

DATA:   gt_aribaaprv_alv                         "Ariba Approver ALV Dsp
                         TYPE ty_gt_aribaaprv_alv.

DATA:   gt_aribaaprv_alv_del                     "Ariba Approver ALV Del
                         TYPE ty_gt_aribaaprv_alv.

DATA:   gt_field_cat     TYPE lvc_t_fcat.        "LVC Field Catalog

DATA:   gt_sort          TYPE lvc_t_sort.        "LVC Sort

DATA:   gt_toolbar_excl_cat                      "ALV Toolbar Exclude
                         TYPE ui_functions.      "Codes

*eject
************************************************************************
* Predefine a local class for event handling to allow the              *
* declaration of a reference variable.                                 *
************************************************************************
CLASS   lcl_event_receiver   DEFINITION DEFERRED.

************************************************************************
*                               Objects                                *
************************************************************************
DATA:   go_custom_container  TYPE REF TO cl_gui_custom_container,
        go_grid1             TYPE REF TO cl_gui_alv_grid,
        go_event_receiver    TYPE REF TO lcl_event_receiver.

****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
*===============================================================
* class c_event_receiver: local class to handle changed-data
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
        FOR EVENT data_changed      OF cl_gui_alv_grid
            IMPORTING er_data_changed.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*
* c_event_receiver (Definition)
*===============================================================

*eject
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class c_event_receiver (Implementation)
*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_data_changed.

    PERFORM  f_data_changed USING er_data_changed.

  ENDMETHOD.                           "handle_data_changed

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*
* c_event_receiver (Implementation)
*===================================================================

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select options
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-110.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   so_aprvr                        "Ariba Approver
                           FOR  zaribaaprv-zariba_approver.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run options
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-120.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       sp_rbg1d RADIOBUTTON GROUP rbg1 "Radio Grp 1-Display M
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       sp_rbg1u RADIOBUTTON GROUP rbg1."Radio Grp 1-Update Md
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 04.
PARAMETERS:       sp_rbg2a RADIOBUTTON GROUP rbg2 "Radio Grp 2-ALV Maint
                           DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  06(20) text-121.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 04.
PARAMETERS:       sp_rbg2f RADIOBUTTON GROUP rbg2."Radio Grp 2-File Main
SELECTION-SCREEN: COMMENT  06(24) text-122.
SELECTION-SCREEN  END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  10(14) text-123.
SELECTION-SCREEN: POSITION 33.
PARAMETERS:       sp_upldf TYPE localfile.        "Upload File
SELECTION-SCREEN  END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 07.
PARAMETERS:       sp_del1r AS CHECKBOX.           "Delete First Row
SELECTION-SCREEN: COMMENT  10(24) text-124.
SELECTION-SCREEN  END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       sp_alvdv TYPE slis_vari         "ALV Display Variant
                           NO-DISPLAY.
SELECTION-SCREEN: END   OF BLOCK ssb2.

*eject
************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

  CLEAR                      gv_repid.
  MOVE     sy-repid       TO gv_repid.

  CLEAR    gv_subrc.

  PERFORM  f_check_authority  USING    'D'
                              CHANGING gv_subrc.

  IF ( gv_subrc NE 0 ).
    LEAVE    PROGRAM.
    MESSAGE  e001 WITH text-371.
  ENDIF.

* PERFORM  f_alv_variant_default_get.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF  ( screen-group1 EQ 'DSP' ).
      screen-input     = 0.
      screen-output    = 1.
      screen-invisible = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sp_upldf.

  PERFORM  f_f4_filename CHANGING sp_upldf.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR sp_alvdv.

* PERFORM  f_alv_variant_f4.

AT SELECTION-SCREEN.

* PERFORM  f_alv_variant_exists.

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Initial data elements
  PERFORM  f_initial_data_elements.

* Perform file processing
  IF     ( sp_rbg1u   IS NOT INITIAL ). "update selected

    CLEAR    gv_subrc.

    PERFORM  f_check_authority  USING    'U'
                                CHANGING gv_subrc.

    IF ( gv_subrc NE 0 ).

      MESSAGE  i001 WITH text-372.
      LEAVE    LIST-PROCESSING.

    ENDIF.

    IF   ( sp_rbg2f   IS NOT INITIAL ). "file selected

      IF ( so_aprvr[] IS NOT INITIAL ). "approvers entered

        MESSAGE  i001 WITH text-311 text-312.
        LEAVE    LIST-PROCESSING.

      ENDIF.

      PERFORM  f_process_file.

    ENDIF.

  ENDIF.

* Select the approver data
  PERFORM  f_select_data.

*eject
************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Format the ALV style data
  PERFORM  f_format_alv_style.

* Enqueue table
  IF ( gv_flag_alv_upd IS NOT INITIAL ).
    PERFORM  f_enqueue_table.
  ENDIF.

* Call screen to display the requested data via an ALV grid
  CALL SCREEN 9000.

* Dequeue table
  IF ( gv_flag_alv_upd IS NOT INITIAL ).
    PERFORM  f_dequeue_table.
  ENDIF.

* Write error messages
  IF ( sp_rbg1u        IS NOT INITIAL ).

    PERFORM  f_write_messages.

  ENDIF.

*eject
*&---------------------------------------------------------------------*
*&      Module  pbo_9000  OUTPUT
*&---------------------------------------------------------------------*
*       Screen 9000 Process Before Output Module.  Performs code       *
*       to create the GUI ALV grid and display the report contents.    *
*----------------------------------------------------------------------*
MODULE pbo_9000 OUTPUT.

* Set the GUI status
  IF       ( gv_flag_alv_dsp IS NOT INITIAL ).
    SET      PF-STATUS 'MAIN9000' EXCLUDING 'SAVE'.
  ENDIF.
  IF       ( gv_flag_alv_upd IS NOT INITIAL ).
    SET      PF-STATUS 'MAIN9000'.
  ENDIF.

* Set the GUI titlebar
  SET        TITLEBAR  'MAIN9000'.

* Create the ALV grid
  PERFORM  f_create_alv_grid.

ENDMODULE.                 " pbo_9000  OUTPUT
*eject
*&---------------------------------------------------------------------*
*&      Module  pai_9000  INPUT
*&---------------------------------------------------------------------*
*       Screen 9000 Process After Input Module.  Performs code         *
*       to exit the screen and free space used by the ALV grid.        *
*----------------------------------------------------------------------*
MODULE pai_9000 INPUT.

* Process screen OK code
  CLEAR                  gv_ucomm.
  MOVE     gv_ok_code TO gv_ucomm.
  CLEAR    gv_ok_code.

  CASE     gv_ucomm.

    WHEN     'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM  f_exit_alv_screen.

    WHEN     'SAVE'.

      PERFORM  f_save_data.

  ENDCASE.

ENDMODULE.                 " pai_9000  INPUT
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_authority
*&---------------------------------------------------------------------*
*       Check the user's authority to execute the program
*----------------------------------------------------------------------*
FORM f_check_authority
  USING    iv_ind_dsp_upd TYPE char1
  CHANGING cv_subrc       TYPE sysubrc.

  CLEAR    cv_subrc.

  IF     ( iv_ind_dsp_upd EQ 'D' ). "display

    AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
                        ID 'ACTVT'     FIELD '03'
                        ID 'DICBERCLS' FIELD 'ARIB'.

    cv_subrc = sy-subrc.

  ELSEIF ( iv_ind_dsp_upd EQ 'U' ). "update

    AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
                        ID 'ACTVT'     FIELD '02'
                        ID 'DICBERCLS' FIELD 'ARIB'.

    cv_subrc = sy-subrc.

  ENDIF.

ENDFORM.                    " f_check_authority
*eject
*----------------------------------------------------------------------*
*       Form  F_ALV_VARIANT_DEFAULT_GET                                *
*----------------------------------------------------------------------*
*       Get the default ALV display variant.                           *
*----------------------------------------------------------------------*
FORM f_alv_variant_default_get.

  CLEAR                                     gs_variant.
  CLEAR                                     gs_variant_p.
  MOVE     gv_repid                      TO gs_variant-report.
  MOVE     gv_repid                      TO gs_variant_p-report.

  CHECK  ( sp_alvdv IS INITIAL ).

* Get the default ALV display variant

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = gs_variant_p
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF ( sy-subrc EQ 0 ).
    CLEAR                                   sp_alvdv.
    MOVE     gs_variant_p-variant        TO sp_alvdv.
  ENDIF.

ENDFORM.                    " f_alv_variant_default_get
*eject
*----------------------------------------------------------------------*
*       Form  F_ALV_VARIANT_F4                                         *
*----------------------------------------------------------------------*
*       Value request "drop-down" for the ALV display variant.         *
*----------------------------------------------------------------------*
FORM f_alv_variant_f4.

* Display list of ALV variants

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant
      i_save        = 'A'
    IMPORTING
      e_exit        = gv_flag_exit
      es_variant    = gs_variant_p
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc EQ 2 ).
    MESSAGE  ID sy-msgid TYPE 'S' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF ( gv_flag_exit IS INITIAL ).
      CLEAR                                 sp_alvdv.
      MOVE     gs_variant_p-variant      TO sp_alvdv.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_alv_variant_f4
*eject
*----------------------------------------------------------------------*
*       Form  F_ALV_VARIANT_EXISTS                                     *
*----------------------------------------------------------------------*
*       Validate that the ALV display variant exists.                  *
*----------------------------------------------------------------------*
FORM f_alv_variant_exists.

  IF ( NOT ( sp_alvdv IS INITIAL ) ).

    CLEAR                                   gs_variant_p.
    MOVE     gs_variant                  TO gs_variant_p.
    CLEAR                                   gs_variant_p-variant.
    MOVE     sp_alvdv                    TO gs_variant_p-variant.

* Check that the ALV display variant exists

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = 'A'
      CHANGING
        cs_variant    = gs_variant_p
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.
    IF ( sy-subrc EQ 0 ).
      CLEAR                                 gs_variant.
      MOVE     gs_variant_p              TO gs_variant.
    ELSE.
      CLEAR                                 gs_variant.
      MOVE     gv_repid                  TO gs_variant-report.
      MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CLEAR                                   gs_variant.
    MOVE       gv_repid                  TO gs_variant-report.
  ENDIF.

ENDFORM.                    " f_alv_variant_exists
*eject
*&---------------------------------------------------------------------*
*&      Form  f_f4_filename
*&---------------------------------------------------------------------*
*       Perform a GUI file select
*----------------------------------------------------------------------*
FORM f_f4_filename
  CHANGING cv_filename TYPE localfile.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = cv_filename.

ENDFORM.                    " f_f4_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

* Initial internal tables
  CLEAR    gt_aprvr[].
  CLEAR    gt_aribaaprv[].
  CLEAR    gt_aribaaprv_upload[].
  CLEAR    gt_aribaaprv_errors[].
  CLEAR    gt_aribaaprv_alv[].
  CLEAR    gt_aribaaprv_alv_del[].
  CLEAR    gt_field_cat[].
  CLEAR    gt_sort[].
  CLEAR    gt_toolbar_excl_cat[].

* Initial data elements
  CLEAR    gv_filename.
  CLEAR    gv_count_rows_deleted.
  CLEAR    gv_flag_alv_dsp.
  CLEAR    gv_flag_alv_upd.
  CLEAR    gv_flag_capture_delete.
  CLEAR    gv_flag_marked_delete.
  CLEAR    gv_flag_error_data.
  CLEAR    gv_flag_exit.
  CLEAR    gv_flag_save.
  CLEAR    gv_flag_sel_opt.
  CLEAR    gv_flag_sel_tab.

* Determine the ALV mode - display or update/edit
  IF ( ( sp_rbg1u     IS NOT INITIAL ) AND   "Update Table
       ( sp_rbg2a     IS NOT INITIAL ) AND   "Maintain From Screen (ALV)
       ( sy-batch         IS INITIAL )     )."Execute In Foreground
    MOVE     gc_x         TO gv_flag_alv_upd."Enter ALV in edit mode
  ELSE.
    MOVE     gc_x         TO gv_flag_alv_dsp."Enter ALV in display mode
  ENDIF.

* Determine the selection method - by option or by table
  IF ( ( sp_rbg1u     IS NOT INITIAL ) AND   "Update Table
       ( sp_rbg2f     IS NOT INITIAL )     )."Maintain From File
    MOVE     gc_x         TO gv_flag_sel_tab."Select by table
  ELSE.
    MOVE     gc_x         TO gv_flag_sel_opt."Select by option
  ENDIF.

* Set the filename
  MOVE       sp_upldf     TO gv_filename.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_file
*&---------------------------------------------------------------------*
*       Perform file processing
*----------------------------------------------------------------------*
FORM f_process_file.

  DATA: ls_aribaaprv LIKE zaribaaprv.

  CHECK  ( sp_rbg1u    IS NOT INITIAL ).
  CHECK  ( sp_rbg2f    IS NOT INITIAL ).
  CHECK  ( gv_filename IS NOT INITIAL ).

  CLEAR    gv_flag_save.
  CLEAR    gt_aprvr[].
  CLEAR    gt_aribaaprv[].
  CLEAR    gt_aribaaprv_upload[].
  CLEAR    gt_aribaaprv_errors[].
  CLEAR    gt_aribaaprv_alv[].
  CLEAR    gt_aribaaprv_alv_del[].

* Upload the file

  PERFORM  f_upload_file TABLES gt_aribaaprv_upload
                         USING  gv_filename.

* Delete all blank rows
  DELETE   gt_aribaaprv_upload WHERE zariba_approver  IS INITIAL
                                 AND zariba_email     IS INITIAL
                                 AND zsvr_conf_sap_id IS INITIAL
                                 AND flag_delete      IS INITIAL.

* Build a list of distinct approver IDs
  CLEAR                                gs_aribaaprv_upload.
  LOOP AT  gt_aribaaprv_upload    INTO gs_aribaaprv_upload.
    gv_tabix = sy-tabix.

    CONDENSE gs_aribaaprv_upload-zariba_approver.
    CONDENSE gs_aribaaprv_upload-zariba_email.
    CONDENSE gs_aribaaprv_upload-zsvr_conf_sap_id.
    CONDENSE gs_aribaaprv_upload-flag_delete.

    TRANSLATE gs_aribaaprv_upload-zariba_approver TO UPPER CASE.
    TRANSLATE gs_aribaaprv_upload-zariba_email TO UPPER CASE.
    TRANSLATE gs_aribaaprv_upload-zsvr_conf_sap_id TO UPPER CASE.
    TRANSLATE gs_aribaaprv_upload-flag_delete TO UPPER CASE.

*eject
* Build list of erred rows
    IF           ( gs_aribaaprv_upload-zariba_approver  IS INITIAL ) OR
                 ( gs_aribaaprv_upload-zariba_email     IS INITIAL ) OR
                 ( gs_aribaaprv_upload-zsvr_conf_sap_id IS INITIAL ). "BTBOUNDY
      APPEND       gs_aribaaprv_upload
                                    TO gt_aribaaprv_errors.
      DELETE                           gt_aribaaprv_upload
                                 INDEX gv_tabix.

* Build list of update rows
    ELSE.
      "Check for existence of svr_conf in usr02.
      CALL FUNCTION 'ZBBC_CHECK_VALID_USER'
        EXPORTING
          imp_uname      = gs_aribaaprv_upload-zsvr_conf_sap_id
        EXCEPTIONS
          user_not_valid = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        "Not valid userID.
        APPEND gs_aribaaprv_upload TO gt_aribaaprv_errors.
        DELETE gt_aribaaprv_upload INDEX gv_tabix.

      ELSE.
        "Valid ID
        MODIFY                           gt_aribaaprv_upload
                                    FROM gs_aribaaprv_upload
                                    INDEX gv_tabix.
        CLEAR                             gs_aprvr.
        MOVE         gs_aribaaprv_upload-zariba_approver
                                      TO gs_aprvr.
        APPEND       gs_aprvr         TO gt_aprvr.
      ENDIF.
    ENDIF.

    CLEAR  gs_aribaaprv_upload.
  ENDLOOP.

*eject
  SORT     gt_aribaaprv_upload      BY zariba_approver ASCENDING
                                       flag_delete     DESCENDING.
  DELETE   ADJACENT DUPLICATES    FROM gt_aribaaprv_upload
                             COMPARING zariba_approver.

  CHECK  ( gt_aribaaprv_upload[] IS NOT INITIAL ).

* Select the existing entries from the database
  SELECT   *
    INTO   TABLE gt_aribaaprv
    FROM   zaribaaprv FOR ALL ENTRIES IN gt_aprvr
   WHERE   zariba_approver = gt_aprvr-zariba_approver.
  IF ( sy-subrc EQ 0 ).
    SORT   gt_aribaaprv ASCENDING BY zariba_approver.
  ELSE.
    CLEAR  gt_aribaaprv[].
  ENDIF.

*eject
* Determine how the database is to be modified
  CLEAR                             gs_aribaaprv_upload.
  LOOP AT  gt_aribaaprv_upload INTO gs_aribaaprv_upload.

    CLEAR          gs_aribaaprv.
    READ     TABLE gt_aribaaprv
              INTO gs_aribaaprv
          WITH KEY zariba_approver =
                   gs_aribaaprv_upload-zariba_approver
                   BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).

      IF ( gs_aribaaprv_upload-flag_delete IS NOT INITIAL ). "Delete

        CLEAR                     gs_aribaaprv_alv.
        MOVE       gs_aribaaprv_upload-zariba_approver
                               TO gs_aribaaprv_alv-zariba_approver.
        MOVE       gs_aribaaprv_upload-zariba_email
                               TO gs_aribaaprv_alv-zariba_email.
        MOVE       gs_aribaaprv_upload-zsvr_conf_sap_id              "BTBOUNDY
                               TO gs_aribaaprv_alv-zsvr_conf_sap_id. "BTBOUNDY
        APPEND     gs_aribaaprv_alv
                               TO gt_aribaaprv_alv_del.

      ELSE. "Update

        IF       gs_aribaaprv-zariba_email      NE gs_aribaaprv_upload-zariba_email OR
                 gs_aribaaprv-zsvr_conf_sap_id  NE gs_aribaaprv_upload-zsvr_conf_sap_id .
          CLEAR                   gs_aribaaprv_alv.
          MOVE     gs_aribaaprv_upload-zariba_approver
                               TO gs_aribaaprv_alv-zariba_approver.
          MOVE     gs_aribaaprv_upload-zariba_email
                               TO gs_aribaaprv_alv-zariba_email.
          MOVE     gs_aribaaprv_upload-zsvr_conf_sap_id              "BTBOUNDY
                               TO gs_aribaaprv_alv-zsvr_conf_sap_id. "BTBOUNDY
          MOVE     gc_x        TO gs_aribaaprv_alv-flag_update.
          APPEND   gs_aribaaprv_alv
                               TO gt_aribaaprv_alv.
        ENDIF.

      ENDIF.

    ELSE.

      IF ( gs_aribaaprv_upload-flag_delete     IS INITIAL ). "Insert
        CLEAR                     gs_aribaaprv_alv.
        MOVE       gs_aribaaprv_upload-zariba_approver
                               TO gs_aribaaprv_alv-zariba_approver.
        MOVE       gs_aribaaprv_upload-zariba_email
                               TO gs_aribaaprv_alv-zariba_email.
        MOVE       gs_aribaaprv_upload-zsvr_conf_sap_id              "BTBOUNDY
                               TO gs_aribaaprv_alv-zsvr_conf_sap_id. "BTBOUNDY
        MOVE       space       TO gs_aribaaprv_alv-flag_update.
        APPEND     gs_aribaaprv_alv
                               TO gt_aribaaprv_alv.
      ENDIF.

    ENDIF.

    CLEAR  gs_aribaaprv_upload.
  ENDLOOP.

*eject
  CHECK  ( ( gt_aribaaprv_alv[]     IS NOT INITIAL ) OR
           ( gt_aribaaprv_alv_del[] IS NOT INITIAL )    ).

* Enqueue the database table
  PERFORM  f_enqueue_table.

* Delete rows from the database
  CLEAR                              gs_aribaaprv_alv.
  LOOP AT  gt_aribaaprv_alv_del INTO gs_aribaaprv_alv.

    DELETE   FROM zaribaaprv
     WHERE   zariba_approver = gs_aribaaprv_alv-zariba_approver.
    IF ( sy-subrc NE 0 ).
      ROLLBACK   WORK.
      PERFORM  f_dequeue_table.
      MESSAGE  e001 WITH text-321.
    ENDIF.

    MOVE     gc_x         TO gv_flag_save.

    CLEAR  gs_aribaaprv_alv.
  ENDLOOP.

* Insert or update rows in the database
  CLEAR                          gs_aribaaprv_alv.
  LOOP AT  gt_aribaaprv_alv INTO gs_aribaaprv_alv.

* Record exists - update
    IF ( gs_aribaaprv_alv-flag_update IS NOT INITIAL ).

      UPDATE   zaribaaprv
         SET   zariba_email    = gs_aribaaprv_alv-zariba_email
               zsvr_conf_sap_id = gs_aribaaprv_alv-zsvr_conf_sap_id "BTBOUNDY
       WHERE   zariba_approver = gs_aribaaprv_alv-zariba_approver.
      IF ( sy-subrc NE 0 ).
        ROLLBACK   WORK.
        PERFORM  f_dequeue_table.
        MESSAGE  e001 WITH text-322.
      ENDIF.

      MOVE     gc_x     TO gv_flag_save.

*eject
* Record does not exist - insert
    ELSE.

      CLEAR                            ls_aribaaprv.
      MOVE     sy-mandt             TO ls_aribaaprv-mandt.
      MOVE     gs_aribaaprv_alv-zariba_approver
                                    TO ls_aribaaprv-zariba_approver.
      MOVE     gs_aribaaprv_alv-zariba_email
                                    TO ls_aribaaprv-zariba_email.
      MOVE     gs_aribaaprv_alv-zsvr_conf_sap_id "BTBOUNDY
                                    TO ls_aribaaprv-zsvr_conf_sap_id.  "BTBOUNDY
      INSERT   into zaribaaprv  values ls_aribaaprv.
      IF ( sy-subrc NE 0 ).
        ROLLBACK   WORK.
        PERFORM  f_dequeue_table.
        MESSAGE  e001 WITH text-323.
      ENDIF.

      MOVE     gc_x       TO gv_flag_save.

    ENDIF.

    CLEAR  gs_aribaaprv_alv.
  ENDLOOP.

  IF ( gv_flag_save IS NOT INITIAL ).
    COMMIT   WORK AND WAIT.
    MESSAGE  i001 WITH text-325.
  ENDIF.

* Dequeue the database table
  PERFORM  f_dequeue_table.

ENDFORM.                    " f_process_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_upload_file
*&---------------------------------------------------------------------*
*       Upload the file
*----------------------------------------------------------------------*
FORM f_upload_file
  TABLES ct_aribaaprv_upload TYPE ty_gt_aribaaprv_upload
  USING  iv_filename         TYPE localfile.

  DATA:  lv_row_curr         TYPE kcd_ex_row_n.

  DATA:  ls_aribaaprv_upload TYPE ty_gs_aribaaprv_upload.

  DATA:  ls_excel            TYPE alsmex_tabline,
         lt_excel            TYPE STANDARD TABLE OF alsmex_tabline.

* Upload the Excel file
  CLEAR    lt_excel[].

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = iv_filename
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 4
      i_end_row               = 8192
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*eject
* Transfer the Excel file to the upload file
  CLEAR    lv_row_curr.

  CLEAR                  ls_excel.
  LOOP AT  lt_excel INTO ls_excel.

    IF         ( lv_row_curr             NE ls_excel-row ).
      IF       ( lv_row_curr         IS NOT INITIAL      ).
        APPEND   ls_aribaaprv_upload     TO ct_aribaaprv_upload.
      ENDIF.
      CLEAR                                 lv_row_curr.
      MOVE     ls_excel-row              TO lv_row_curr.
      CLEAR                                 ls_aribaaprv_upload.
    ENDIF.

    CASE   ls_excel-col.
      WHEN 1.
        CLEAR                     ls_aribaaprv_upload-zariba_approver.
        MOVE     ls_excel-value
                               TO ls_aribaaprv_upload-zariba_approver.
      WHEN 2.
        CLEAR                     ls_aribaaprv_upload-zariba_email.
        MOVE     ls_excel-value
                               TO ls_aribaaprv_upload-zariba_email.
      WHEN 3.
        CLEAR                     ls_aribaaprv_upload-zsvr_conf_sap_id. "BTBOUNDY
        MOVE     ls_excel-value
                               TO ls_aribaaprv_upload-zsvr_conf_sap_id. "BTBOUNDY
      WHEN 4.
        CLEAR                     ls_aribaaprv_upload-flag_delete.
        MOVE     ls_excel-value
                               TO ls_aribaaprv_upload-flag_delete.
    ENDCASE.

    AT LAST.
      APPEND     ls_aribaaprv_upload     TO ct_aribaaprv_upload.
    ENDAT.

    CLEAR  ls_excel.
  ENDLOOP.

  IF ( sp_del1r IS NOT INITIAL ).
    DELETE   ct_aribaaprv_upload INDEX 1.
  ENDIF.

ENDFORM.                    " f_upload_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_enqueue_table
*&---------------------------------------------------------------------*
*       Enqueue the database table
*----------------------------------------------------------------------*
FORM f_enqueue_table.

  CALL FUNCTION 'ENQUEUE_EZ_ZARIBAAPRV'
    EXPORTING
      mode_zaribaaprv         = 'E'
      mandt                   = sy-mandt
*     ZARIBA_APPROVER         =
*     X_ZARIBA_APPROVER       = ' '
      _scope                  = '1'
*     _WAIT                   = ' '
      _collect                = ' '
    EXCEPTIONS
      foreign_lock            = 1
      system_failure          = 2
      OTHERS                  = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE  i001 WITH text-341 text-342.
    LEAVE    LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " f_enqueue_table
*eject
*&---------------------------------------------------------------------*
*&      Form  f_dequeue_table
*&---------------------------------------------------------------------*
*       Dequeue the database table
*----------------------------------------------------------------------*
FORM f_dequeue_table.

  CALL FUNCTION 'DEQUEUE_EZ_ZARIBAAPRV'
    EXPORTING
      mode_zaribaaprv         = 'E'
      mandt                   = sy-mandt
*     ZARIBA_APPROVER         =
*     X_ZARIBA_APPROVER       = ' '
      _scope                  = '1'
      _synchron               = ' '
      _collect                = ' '.

ENDFORM.                    " f_dequeue_table
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_data
*&---------------------------------------------------------------------*
*       Select the data
*----------------------------------------------------------------------*
FORM f_select_data.

  CLEAR    gt_aribaaprv[].
  CLEAR    gt_aribaaprv_alv[].

  IF   ( gv_flag_sel_opt IS NOT INITIAL ). "Select by option

    SELECT   *
      INTO   TABLE gt_aribaaprv
      FROM   zaribaaprv
     WHERE   zariba_approver IN so_aprvr.
    IF ( sy-subrc NE 0 ).
      CLEAR gt_aribaaprv[].
    ENDIF.

  ENDIF.

  IF   ( gv_flag_sel_tab IS NOT INITIAL ). "Select by table

    IF ( gt_aprvr[] IS NOT INITIAL ).

      SELECT   *
        INTO   TABLE gt_aribaaprv
        FROM   zaribaaprv FOR ALL ENTRIES IN gt_aprvr
       WHERE   zariba_approver = gt_aprvr-zariba_approver.
      IF ( sy-subrc NE 0 ).
        CLEAR gt_aribaaprv[].
      ENDIF.

    ENDIF.

  ENDIF.

  SORT     gt_aribaaprv ASCENDING BY zariba_approver.

  CLEAR                      gs_aribaaprv.
  LOOP AT  gt_aribaaprv INTO gs_aribaaprv.
    CLEAR                         gs_aribaaprv_alv.
    MOVE   gs_aribaaprv-zariba_approver
                               TO gs_aribaaprv_alv-zariba_approver.
    MOVE   gs_aribaaprv-zariba_email
                               TO gs_aribaaprv_alv-zariba_email.
    MOVE   gs_aribaaprv-zsvr_conf_sap_id "BTBOUNDY
                               TO gs_aribaaprv_alv-zsvr_conf_sap_id. "BTBOUNDY
    MOVE   space               TO gs_aribaaprv_alv-flag_update.
    APPEND gs_aribaaprv_alv    TO gt_aribaaprv_alv.
    CLEAR  gs_aribaaprv.
  ENDLOOP.

ENDFORM.                    " f_select_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_alv_style
*&---------------------------------------------------------------------*
*       Format the ALV style data
*----------------------------------------------------------------------*
FORM f_format_alv_style.

  DATA: lv_style         TYPE raw4.

  DATA: ls_cellio        TYPE lvc_s_styl,
        lt_cellio        TYPE lvc_t_styl.

  lv_style = cl_gui_alv_grid=>mc_style_disabled.

  CLEAR                                gs_aribaaprv_alv.
  LOOP AT  gt_aribaaprv_alv       INTO gs_aribaaprv_alv.

    CLEAR    gs_aribaaprv_alv-cellio_tab[].
    CLEAR    gs_aribaaprv_alv-cellio_tab.
    CLEAR    lt_cellio[].
    CLEAR    ls_cellio.

    CLEAR                              ls_cellio.
    MOVE     'ZARIBA_APPROVER'      TO ls_cellio-fieldname.
    MOVE     lv_style               TO ls_cellio-style.
    INSERT   ls_cellio      INTO TABLE lt_cellio.

    INSERT                    LINES OF lt_cellio
                            INTO TABLE gs_aribaaprv_alv-cellio_tab.
    MODIFY                             gt_aribaaprv_alv
                                  FROM gs_aribaaprv_alv.

    CLEAR  gs_aribaaprv_alv.
  ENDLOOP.

ENDFORM.                    " f_format_alv_style
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_alv_grid
*&---------------------------------------------------------------------*
*       Create the GUI ALV grid for the summary report
*----------------------------------------------------------------------*
FORM f_create_alv_grid.

  IF ( go_custom_container IS INITIAL ).

    IF ( cl_gui_alv_grid=>offline( ) IS INITIAL ).

* Create an instance of the custom container for the ALV grid
      CREATE OBJECT go_custom_container
        EXPORTING
          container_name              = gc_alv_container
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      IF ( sy-subrc NE 0 ).
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = gv_repid
            txt1  = text-351
            txt2  = sy-subrc.
        LEAVE PROGRAM.
      ENDIF.

    ENDIF.

* Create an instance of the ALV grid
    CREATE OBJECT go_grid1
      EXPORTING
        i_parent = go_custom_container.

* Build the ALV field catalog
    PERFORM  f_build_field_cat  TABLES gt_field_cat.

* Exclude function codes
    IF     ( gv_flag_alv_dsp    IS NOT INITIAL ).
      PERFORM  f_exclude_function_display
                                TABLES gt_toolbar_excl_cat.
    ENDIF.
    IF     ( gv_flag_alv_upd    IS NOT INITIAL ).
      PERFORM  f_exclude_function_update
                                TABLES gt_toolbar_excl_cat.
    ENDIF.

*eject
* Build the ALV layout
    CLEAR                              gs_layout.
*   MOVE     gc_x                   TO gs_layout-zebra.
    MOVE     gc_x                   TO gs_layout-cwidth_opt.
    MOVE     text-101               TO gs_layout-grid_title.
    MOVE     'A'                    TO gs_layout-sel_mode.
    MOVE     'CELLIO_TAB'           TO gs_layout-stylefname.
    MOVE     'LINECOLOR'            TO gs_layout-info_fname.

    IF     ( gv_flag_alv_upd    IS NOT INITIAL ).
      MOVE   gc_x                   TO gs_layout-edit.
    ENDIF.

* At event "PRINT_END_OF_PAGE", two lines must be reserved
    CLEAR                              gs_print.
    MOVE     2                      TO gs_print-reservelns.

* Create an instance of the event receiver
    CREATE OBJECT go_event_receiver.

* Link event handler methods
* When ALV control raises the event, the method is automatically called
    SET HANDLER go_event_receiver->handle_data_changed
            FOR go_grid1.

    CALL METHOD go_grid1->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_print        = gs_print
        it_toolbar_excluding = gt_toolbar_excl_cat[]
*       IS_VARIANT      =
        i_save          = ' '
      CHANGING
        it_outtab       = gt_aribaaprv_alv[]
        it_fieldcatalog = gt_field_cat[]
        it_sort         = gt_sort[].

    IF     ( gv_flag_alv_upd    IS NOT INITIAL ).

      CALL METHOD go_grid1->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

      CALL METHOD go_grid1->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ENDIF.

*eject
* Do not refresh the ALV grid if bad data exists
  ELSEIF ( gv_flag_error_data IS INITIAL ).

    CALL METHOD go_grid1->refresh_table_display.

  ENDIF.

* Set focus to ensure that the cursor is active in the control
  IF ( cl_gui_alv_grid=>offline( ) IS INITIAL ).
    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = go_grid1.
  ENDIF.

ENDFORM.                    " f_create_alv_grid
*eject
*&---------------------------------------------------------------------*
*&      Form  f_build_field_cat
*&---------------------------------------------------------------------*
*       Build the ALV field catalog
*----------------------------------------------------------------------*
FORM f_build_field_cat
  TABLES ct_field_cat TYPE lvc_t_fcat.

* Build the field catalog according to the ALV structure
  CLEAR    ct_field_cat[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = gc_alv_structure
    CHANGING
      ct_fieldcat      = ct_field_cat[].

* Build the sort internal table
  CLEAR:   gt_sort[],                  gs_sort.
  MOVE     '1'                      TO gs_sort-spos.
  MOVE     'ZARIBA_APPROVER'        TO gs_sort-fieldname.
  MOVE     gc_x                     TO gs_sort-up.
  APPEND   gs_sort                  TO gt_sort.

ENDFORM.                    " f_build_field_cat
*eject
*&---------------------------------------------------------------------*
*&      Form  f_exclude_function_display
*&---------------------------------------------------------------------*
*       Exclude function codes - display
*----------------------------------------------------------------------*
FORM f_exclude_function_display
  TABLES ct_toolbar_excl_cat  TYPE ui_functions.

  DATA:  lv_toolbar_excl_code TYPE ui_func.

  CLEAR    ct_toolbar_excl_cat[].

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_detail.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_sum.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

*eject
  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_check.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

* CLEAR    lv_toolbar_excl_code.
* lv_toolbar_excl_code = cl_gui_alv_grid=>mc_mb_export.
* APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_mb_sum.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_mb_variant.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

* CLEAR    lv_toolbar_excl_code.
* lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_views.
* APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_graph.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

* CLEAR    lv_toolbar_excl_code.
* lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_info.
* APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

* CLEAR    lv_toolbar_excl_code.
* lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_print.
* APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

* CLEAR    lv_toolbar_excl_code.
* lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_refresh.
* APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

ENDFORM.                    " f_exclude_function_display
*eject
*&---------------------------------------------------------------------*
*&      Form  f_exclude_function_update
*&---------------------------------------------------------------------*
*       Exclude function codes - update
*----------------------------------------------------------------------*
FORM f_exclude_function_update
  TABLES ct_toolbar_excl_cat  TYPE ui_functions.

  DATA:  lv_toolbar_excl_code TYPE ui_func.

  CLEAR    ct_toolbar_excl_cat[].

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_detail.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_sum.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_check.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_mb_export.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_mb_sum.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

*eject
  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_mb_variant.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_views.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_filter.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_graph.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_info.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_print.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

  CLEAR    lv_toolbar_excl_code.
  lv_toolbar_excl_code = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND   lv_toolbar_excl_code TO ct_toolbar_excl_cat.

ENDFORM.                    " f_exclude_function_update
*eject
*&---------------------------------------------------------------------*
*&      Form  f_exit_alv_screen
*&---------------------------------------------------------------------*
*       Exit the ALV screen and return to list processing.             *
*----------------------------------------------------------------------*
FORM f_exit_alv_screen.

  IF ( go_custom_container IS NOT INITIAL ).

* Free space held by the ALV Grid
    CALL METHOD go_grid1->free.
    CLEAR       go_grid1.

* Free space held by the ALV Container
    CALL METHOD go_custom_container->free.
    CLEAR       go_custom_container.

  ENDIF.

* Synchronize the automation queue
  CALL METHOD cl_gui_cfw=>flush.
  IF ( sy-subrc NE 0 ).
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = gv_repid
        txt1  = text-352
        txt2  = text-353
        txt3  = sy-subrc.
  ENDIF.

  IF ( ( gv_ucomm EQ 'BACK' ) OR ( gv_ucomm EQ 'EXIT' ) ).
* Return to list processing
    SET   SCREEN 0.
    LEAVE SCREEN.
  ELSE.
* Exit program
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " f_exit_alv_screen
*eject
*&---------------------------------------------------------------------*
*&      Form  f_save_data
*&---------------------------------------------------------------------*
*       Save data to the database
*----------------------------------------------------------------------*
FORM f_save_data.

  DATA: lv_tabix     TYPE sytabix.

  DATA: ls_aribaaprv LIKE zaribaaprv.

  CLEAR    gv_flag_save.

  IF ( go_grid1 IS NOT INITIAL ).

    CALL METHOD go_grid1->check_changed_data.

  ENDIF.

* Delete all ALV rows where the approver is blank
  DELETE   gt_aribaaprv_alv     WHERE zariba_approver IS INITIAL.
  DELETE   gt_aribaaprv_alv_del WHERE zariba_approver IS INITIAL.

* Delete rows from the database
  CLEAR                              gs_aribaaprv_alv.
  LOOP AT  gt_aribaaprv_alv_del INTO gs_aribaaprv_alv.
    lv_tabix = sy-tabix.

    CLEAR    gv_zariba_approver.
    SELECT   SINGLE zariba_approver
      INTO   gv_zariba_approver
      FROM   zaribaaprv
     WHERE   zariba_approver   = gs_aribaaprv_alv-zariba_approver.
    IF ( sy-subrc EQ 0 ).

      DELETE   FROM zaribaaprv
       WHERE   zariba_approver = gs_aribaaprv_alv-zariba_approver.
      IF ( sy-subrc NE 0 ).
        ROLLBACK   WORK.
        PERFORM  f_dequeue_table.
        MESSAGE  e001 WITH text-321.
      ENDIF.

      MOVE   gc_x         TO gv_flag_save.

    ENDIF.

    DELETE   gt_aribaaprv_alv_del INDEX lv_tabix.

    CLEAR  gs_aribaaprv_alv.
  ENDLOOP.

*eject
* Insert or update rows in the database
  CLEAR                          gs_aribaaprv_alv.
  LOOP AT  gt_aribaaprv_alv INTO gs_aribaaprv_alv.

    CLEAR    ls_aribaaprv.
    SELECT   SINGLE *
      INTO   ls_aribaaprv
      FROM   zaribaaprv
     WHERE   zariba_approver = gs_aribaaprv_alv-zariba_approver.
    IF ( sy-subrc EQ 0 ).

* Record exists - update
      IF ( gs_aribaaprv_alv-flag_update IS NOT INITIAL ).

        UPDATE   zaribaaprv
           SET   zariba_email    = gs_aribaaprv_alv-zariba_email
                 zsvr_conf_sap_id = gs_aribaaprv_alv-zsvr_conf_sap_id "BTBOUNDY
         WHERE   zariba_approver = gs_aribaaprv_alv-zariba_approver.
        IF ( sy-subrc NE 0 ).
          ROLLBACK   WORK.
          PERFORM  f_dequeue_table.
          MESSAGE  e001 WITH text-322.
        ENDIF.

        MOVE     gc_x     TO gv_flag_save.

      ENDIF.

    ELSE.

* Record does not exist - insert
      CLEAR                            ls_aribaaprv.
      MOVE     sy-mandt             TO ls_aribaaprv-mandt.
      MOVE     gs_aribaaprv_alv-zariba_approver
                                    TO ls_aribaaprv-zariba_approver.
      MOVE     gs_aribaaprv_alv-zariba_email
                                    TO ls_aribaaprv-zariba_email.
      MOVE     gs_aribaaprv_alv-zsvr_conf_sap_id "BTBOUNDY
                                    TO ls_aribaaprv-zsvr_conf_sap_id. "BTBOUNDY
      INSERT   into zaribaaprv  values ls_aribaaprv.
      IF ( sy-subrc NE 0 ).
        ROLLBACK   WORK.
        PERFORM  f_dequeue_table.
        MESSAGE  e001 WITH text-323.
      ENDIF.

      MOVE     gc_x       TO gv_flag_save.

    ENDIF.

    CLEAR  gs_aribaaprv_alv.
  ENDLOOP.

*eject
  IF ( gv_flag_save IS NOT INITIAL ).
    COMMIT   WORK AND WAIT.
    MESSAGE  i001 WITH text-325.
  ENDIF.

* Re-select the approver data
  PERFORM  f_select_data.

* Re-format the ALV style data
  PERFORM  f_format_alv_style.

ENDFORM.                    " f_save_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_data_changed
*&---------------------------------------------------------------------*
*       Data changed in the ALV grid - process the event
*----------------------------------------------------------------------*
FORM f_data_changed
  USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells    TYPE lvc_s_modi.

  CLEAR    gv_count_rows_deleted.
  CLEAR    gv_flag_capture_delete.
  CLEAR    gv_flag_marked_delete.
  CLEAR    gv_flag_error_data.

  CLEAR                                          ls_mod_cells.
  LOOP AT  er_data_changed->mt_good_cells   INTO ls_mod_cells.

    PERFORM  f_check_duplicate_entry       USING er_data_changed
                                                 ls_mod_cells.

    PERFORM  f_check_modified_entry        USING ls_mod_cells.

    CLEAR  ls_mod_cells.
  ENDLOOP.

  IF ( gv_flag_capture_delete IS INITIAL ).

    PERFORM  f_check_deleted_entry         USING er_data_changed.

  ENDIF.

  IF ( gv_flag_error_data IS NOT INITIAL ).

    CALL METHOD er_data_changed->display_protocol.

  ENDIF.

ENDFORM.                    " f_data_changed
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_duplicate_entry
*&---------------------------------------------------------------------*
*       Check if a row in the ALV grid is a duplicate entry
*----------------------------------------------------------------------*
FORM f_check_duplicate_entry
  USING er_data_changed  TYPE REF TO cl_alv_changed_data_protocol
        is_mod_cells     TYPE lvc_s_modi.

  DATA: lv_zariba_approver TYPE z_ariba_approver.

* The check for a duplicate entry is only a test of the primary key.
* In this case the approver.  Because no updates can be performed on
* the primary key (and the screen does not allow it), the only change
* that can occur is an insert.  ER_DATA_CHANGED->MT_GOOD_CELLS does
* not identify deletes.
  IF     ( is_mod_cells-fieldname EQ 'ZARIBA_APPROVER' ).

    CLEAR                                   lv_zariba_approver.
    MOVE   is_mod_cells-value            TO lv_zariba_approver.

* Check if the approver being inserted already exists in the int. table
    CLEAR          gs_aribaaprv.
    READ     TABLE gt_aribaaprv
              INTO gs_aribaaprv
          WITH KEY zariba_approver = lv_zariba_approver
                   BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).

*eject
* Check if the approver is being deleted and inserted in the same event
      IF ( gv_flag_capture_delete IS INITIAL ).

        CLEAR    gv_flag_marked_delete.
        CLEAR                                          gs_del_row.
        LOOP AT  er_data_changed->mt_deleted_rows INTO gs_del_row.

          CLEAR          gs_aribaaprv_alv.
          READ     TABLE gt_aribaaprv_alv
                    INTO gs_aribaaprv_alv
                   INDEX gs_del_row-row_id.
          IF ( sy-subrc EQ 0 ).
            APPEND   gs_aribaaprv_alv    TO gt_aribaaprv_alv_del.
            CLEAR                           gv_flag_capture_delete.
            MOVE     gc_x                TO gv_flag_capture_delete.
            CLEAR                           gv_flag_error_data.
            MOVE     gc_x                TO gv_flag_error_data.
            CLEAR                           gv_flag_marked_delete.
            MOVE     gc_x                TO gv_flag_marked_delete.
            CALL METHOD er_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = '0K'
                i_msgno     = '000'
                i_msgty     = 'E'
                i_msgv1     = text-361
                i_msgv2     = text-362
                i_msgv3     = is_mod_cells-value
                i_fieldname = is_mod_cells-fieldname
                i_row_id    = is_mod_cells-row_id.
          ENDIF.

          CLEAR  gs_del_row.
        ENDLOOP.

*eject
* Check if the approver inserted had been deleted in a previous event
        DESCRIBE TABLE er_data_changed->mt_deleted_rows
                 LINES gv_count_rows_deleted.

        IF ( gv_count_rows_deleted EQ 0 ).
          CLEAR          gs_aribaaprv_alv.
          READ     TABLE gt_aribaaprv_alv_del
                    INTO gs_aribaaprv_alv
                WITH KEY zariba_approver = lv_zariba_approver.
          IF ( sy-subrc EQ 0 ).
            CLEAR                           gv_flag_error_data.
            MOVE     gc_x                TO gv_flag_error_data.
            CLEAR                           gv_flag_marked_delete.
            MOVE     gc_x                TO gv_flag_marked_delete.
            CALL METHOD er_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = '0K'
                i_msgno     = '000'
                i_msgty     = 'E'
                i_msgv1     = text-361
                i_msgv2     = text-362
                i_msgv3     = is_mod_cells-value
                i_fieldname = is_mod_cells-fieldname
                i_row_id    = is_mod_cells-row_id.
          ENDIF.
        ENDIF.

      ENDIF.

* Duplicate exists in the ALV list and it has not been deleted
      IF ( gv_flag_marked_delete IS INITIAL ).
        CLEAR                               gv_flag_error_data.
        MOVE     gc_x                    TO gv_flag_error_data.
        CALL METHOD er_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = text-363
            i_msgv2     = is_mod_cells-value
            i_fieldname = is_mod_cells-fieldname
            i_row_id    = is_mod_cells-row_id.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " f_check_duplicate_entry
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_modified_entry
*&---------------------------------------------------------------------*
*       Check if a row in the ALV grid has been modified
*----------------------------------------------------------------------*
FORM f_check_modified_entry
  USING is_mod_cells     TYPE lvc_s_modi.

  CLEAR          gs_aribaaprv_alv.
  READ     TABLE gt_aribaaprv_alv
            INTO gs_aribaaprv_alv
           INDEX is_mod_cells-row_id.
  IF ( sy-subrc EQ 0 ).

    IF ( ( is_mod_cells-fieldname   EQ 'ZARIBA_APPROVER' ) OR
         ( is_mod_cells-fieldname   EQ 'ZARIBA_EMAIL'    ) OR
         ( is_mod_cells-fieldname   EQ 'ZSVR_CONF_SAP_ID')    ). "BTBOUNDY

      CLEAR                            gs_aribaaprv_alv-flag_update.
      MOVE     gc_x                 TO gs_aribaaprv_alv-flag_update.

      MODIFY                           gt_aribaaprv_alv
                                  FROM gs_aribaaprv_alv
                                 INDEX is_mod_cells-row_id
                          TRANSPORTING flag_update.

    ENDIF.

  ENDIF.

ENDFORM.                    " f_check_modified_entry
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_deleted_entry
*&---------------------------------------------------------------------*
*       Check if a row in the ALV grid has been deleted
*----------------------------------------------------------------------*
FORM f_check_deleted_entry
  USING er_data_changed  TYPE REF TO cl_alv_changed_data_protocol.

  CLEAR                                          gs_del_row.
  LOOP AT  er_data_changed->mt_deleted_rows INTO gs_del_row.

    CLEAR          gs_aribaaprv_alv.
    READ     TABLE gt_aribaaprv_alv
              INTO gs_aribaaprv_alv
             INDEX gs_del_row-row_id.
    IF ( sy-subrc EQ 0 ).
      APPEND       gs_aribaaprv_alv TO gt_aribaaprv_alv_del.
    ENDIF.

    CLEAR  gs_del_row.
  ENDLOOP.

ENDFORM.                    " f_check_deleted_entry
*eject
*&---------------------------------------------------------------------*
*&      Form  f_write_messages
*&---------------------------------------------------------------------*
*       Write the error messages
*----------------------------------------------------------------------*
FORM f_write_messages.

  CHECK  ( gt_aribaaprv_errors[] IS NOT INITIAL ).

  MESSAGE  i001 WITH text-383.

  WRITE: /001 text-383.
  SKIP      1.

  WRITE: /001 text-211,
          022 text-212,
          063 text-213,
          079 text-214.

  LOOP AT gt_aribaaprv_errors INTO gs_aribaaprv_upload.
    IF gs_aribaaprv_upload-zariba_approver  IS INITIAL OR
       gs_aribaaprv_upload-zariba_email     IS INITIAL OR
       gs_aribaaprv_upload-zsvr_conf_sap_id IS INITIAL.

      WRITE: /001 gs_aribaaprv_upload-zariba_approver,
              022 gs_aribaaprv_upload-zariba_email,
              063 gs_aribaaprv_upload-zsvr_conf_sap_id,
              079 gs_aribaaprv_upload-flag_delete,
              089 text-391.
    ELSE.

      WRITE: /001 gs_aribaaprv_upload-zariba_approver,
               022 gs_aribaaprv_upload-zariba_email,
               063 gs_aribaaprv_upload-zsvr_conf_sap_id,
               079 gs_aribaaprv_upload-flag_delete,
               089 text-392.

    ENDIF.
    CLEAR gs_aribaaprv_upload.
  ENDLOOP.


*  WRITE: /001 text-381, text-382.
*  SKIP      1.
*  WRITE: /001 text-211,
*          023 text-212.
*
*  CLEAR                             gs_aribaaprv_upload.
*  LOOP AT  gt_aribaaprv_errors INTO gs_aribaaprv_upload.
*    WRITE: /001 gs_aribaaprv_upload-zariba_approver,
*            023 gs_aribaaprv_upload-zariba_email,
*            023 gs_aribaaprv_upload-zsvr_conf_sap_id, "BTBOUNDY
*           /023 gs_aribaaprv_upload-flag_delete.
*    CLEAR  gs_aribaaprv_upload.
*  ENDLOOP.

ENDFORM.                    " f_write_messages
