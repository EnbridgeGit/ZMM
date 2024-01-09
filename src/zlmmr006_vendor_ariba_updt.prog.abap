*&---------------------------------------------------------------------*
*& Report  ZLMMR006_VENDOR_ARIBA_UPDT
*&---------------------------------------------------------------------*
************************************************************************
*                                                                      *
*  Client:    Spectra Energy                                           *
*  Author:    John Hartung                                             *
*  Date:      April 06, 2011                                           *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*     - Maintain the Ariba Supplier ID in the Vendor Master Catalog    *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 04/06/11 0872 JRHARTU D30K916323 - Initial program development       *
* 04/06/11 0872 JRHARTU D30K916555 - Initial program development       *
*                                    Pre-go-live correction            *
*----------------------------------------------------------------------*
************************************************************************
REPORT  zlmmr006_vendor_ariba_updt  MESSAGE-ID  ZM
                                    NO STANDARD PAGE HEADING
                                    LINE-SIZE   132.

************************************************************************
*                                Tables                                *
************************************************************************
TABLES: lfa1.                                    "Vendor Master (General

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_gs_lifnr,                     "Vendor Account Number
        lifnr            TYPE lifnr,             "Vendor Account Number
       END   OF ty_gs_lifnr.

TYPES:  ty_gt_lifnr      TYPE STANDARD TABLE OF ty_gs_lifnr.

TYPES: BEGIN OF ty_gs_aribasupl_upload,          "Ariba Supplier Upload
        lifnr            TYPE char20,            "Vendor Account Number
        emnfr            TYPE char20,            "Ariba Supplier ID
       END   OF ty_gs_aribasupl_upload.

TYPES:  ty_gt_aribasupl_upload
                         TYPE STANDARD TABLE OF ty_gs_aribasupl_upload.

TYPES:  ty_gs_aribasupl_alv                      "Ariba Supplier ALV Dsp
                         TYPE zmms_vendor_ariba_alv.

TYPES:  ty_gt_aribasupl_alv
                         TYPE STANDARD TABLE OF ty_gs_aribasupl_alv.

TYPES: BEGIN OF ty_gs_aribasupl.                 "Ariba Supplier
INCLUDE TYPE ty_gs_aribasupl_alv.                "Ariba Supplier ALV Dsp
TYPES:  flag_update      TYPE flag,              "Flag-Update
       END   OF ty_gs_aribasupl.

TYPES:  ty_gt_aribasupl  TYPE STANDARD TABLE OF ty_gs_aribasupl.

************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE char1              "X, True, Yes
                         VALUE 'X',
        gc_asc           TYPE char10             "ASCII
                         VALUE 'ASC',
        gc_nodata        TYPE char1              "BDC No Data Character
                         VALUE '/',
        gc_alv_container TYPE scrfname           "ALV Container
                         VALUE 'SCREEN_9000_CONTAINER',
        gc_alv_structure TYPE tabname            "ALV Structure
                         VALUE 'ZMMS_VENDOR_ARIBA_ALV'.

*eject
************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_subrc         TYPE sysubrc,           "Return Code
        gv_tabix         TYPE sytabix,           "Internal Table Index
        gv_repid         TYPE sycprog,           "Report ID
        gv_ucomm         TYPE syucomm,           "Screen Function Code
        gv_ok_code       TYPE syucomm,           "Screen Function Code
        gv_ct_tcode      TYPE sytcode.           "Call Transactn Tcode

DATA:   gv_lifnr         TYPE lifnr,             "Vendor Account Number
        gv_filename      TYPE localfile.         "Filename

DATA:   gv_flag_exit     TYPE flag,              "Flag-Exit
        gv_flag_sel_opt  TYPE flag,              "Flag-Select By Option
        gv_flag_sel_tab  TYPE flag.              "Flag-Select By Table

************************************************************************
*                                Ranges                                *
************************************************************************

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_lifnr         TYPE ty_gs_lifnr,       "Vendor Account Number
        gs_aribasupl     TYPE ty_gs_aribasupl,   "Ariba Supplier
        gs_aribasupl_upload                      "Ariba Supplier Upload
                         TYPE ty_gs_aribasupl_upload,
        gs_aribasupl_alv                         "Ariba Supplier ALV Dsp
                         TYPE ty_gs_aribasupl_alv.

DATA:   gs_variant       TYPE disvariant,        "ALV Display Variant
        gs_variant_p     TYPE disvariant,        "ALV Display Variant
        gs_layout        TYPE lvc_s_layo,        "LVC Layout Structure
        gs_print         TYPE lvc_s_prnt,        "LVC Print Settings
        gs_sort          TYPE lvc_s_sort.        "LVC Sort

DATA:   gs_bdcdata       TYPE bdcdata,           "Batch Input
        gs_ct_options    TYPE ctu_params,        "Call Transactn Options
        gs_ct_messages   TYPE bdcmsgcoll.        "Call Transactn Mesgs

*eject
************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_lifnr         TYPE ty_gt_lifnr.       "Vendor Account Number

DATA:   gt_aribasupl     TYPE ty_gt_aribasupl.   "Ariba Supplier

DATA:   gt_aribasupl_upload                      "Ariba Supplier Upload
                         TYPE ty_gt_aribasupl_upload.

DATA:   gt_aribasupl_alv                         "Ariba Supplier ALV Dsp
                         TYPE ty_gt_aribasupl_alv.

DATA:   gt_field_cat     TYPE lvc_t_fcat.        "LVC Field Catalog

DATA:   gt_sort          TYPE lvc_t_sort.        "LVC Sort

DATA:   gt_bdcdata       TYPE TABLE OF bdcdata.  "Batch Input

DATA:   gt_ct_messages   TYPE TABLE OF bdcmsgcoll."Call Transactn Mesgs

DATA:   gt_ct_messages_p TYPE TABLE OF bdcmsgcoll."Call Transactn Mesgs

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
        go_grid1             TYPE REF TO cl_gui_alv_grid.
*       go_event_receiver    TYPE REF TO lcl_event_receiver.

****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
*===============================================================
* class c_event_receiver: local class to handle changed-data
*
CLASS lcl_event_receiver DEFINITION.
*
* PUBLIC SECTION.
*   METHODS:
*     handle_top_of_page
*       FOR EVENT print_top_of_page OF cl_gui_alv_grid,
*     handle_top_of_list
*       FOR EVENT print_top_of_list OF cl_gui_alv_grid.
*
* PRIVATE SECTION.
*
ENDCLASS.                    "lcl_event_receiver DEFINITION
*
* c_event_receiver (Definition)
*===============================================================

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class c_event_receiver (Implementation)
*
CLASS lcl_event_receiver IMPLEMENTATION.
*
* METHOD handle_top_of_page.
* ENDMETHOD.                           "handle_top_of_page
*
* METHOD handle_top_of_list.
* ENDMETHOD.                           "handle_top_of_list
*
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
SELECT-OPTIONS:   so_lifnr FOR  lfa1-lifnr.       "Vendor Account Number
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run options

SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-120.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       sp_rbg1d RADIOBUTTON GROUP RBG1 "Radio Grp 1-Display M
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       sp_rbg1u RADIOBUTTON GROUP RBG1."Radio Grp 1-Update Md
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  10(14) text-121.
SELECTION-SCREEN: POSITION 33.
PARAMETERS:       sp_upldf TYPE localfile.        "Upload File
SELECTION-SCREEN  END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 07.
PARAMETERS:       sp_del1r AS CHECKBOX.           "Delete First Row
SELECTION-SCREEN: COMMENT  10(24) text-122.
SELECTION-SCREEN  END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       sp_alvdv TYPE slis_vari.        "ALV Display Variant
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
    MESSAGE  E100 WITH text-371.
  ENDIF.

  PERFORM  f_alv_variant_default_get.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF  ( SCREEN-GROUP1 EQ 'DSP' ).
      SCREEN-INPUT     = 0.
      SCREEN-OUTPUT    = 1.
      SCREEN-INVISIBLE = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sp_upldf.

  PERFORM  f_f4_filename CHANGING sp_upldf.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sp_alvdv.

  PERFORM  f_alv_variant_f4.

AT SELECTION-SCREEN.

  PERFORM  f_alv_variant_exists.

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Initial data elements
  PERFORM  f_initial_data_elements.

  IF     ( sp_rbg1d   IS NOT INITIAL ).

    IF   ( so_lifnr[]     IS INITIAL ).
      MESSAGE  I100 WITH text-311 text-312.
      LEAVE    LIST-PROCESSING.
    ENDIF.

  ELSEIF ( sp_rbg1u   IS NOT INITIAL ).

    CLEAR    gv_subrc.

    PERFORM  f_check_authority  USING    'U'
                                CHANGING gv_subrc.

    IF ( gv_subrc NE 0 ).

      MESSAGE  I100 WITH text-372.
      LEAVE    LIST-PROCESSING.

    ENDIF.

    IF   ( so_lifnr[] IS NOT INITIAL ).
      MESSAGE  I100 WITH text-321 text-322.
      LEAVE    LIST-PROCESSING.
    ENDIF.

* Perform file processing

    PERFORM  f_process_file.

  ENDIF.

* Select the approver data
  PERFORM  f_select_data.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Call screen to display the requested data via an ALV grid
  CALL SCREEN 9000.

* Write error messages
  IF   ( sp_rbg1u   IS NOT INITIAL ).

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

* Set the GUI status and titlebar
  SET      PF-STATUS 'MAIN9000'.
  SET      TITLEBAR  'MAIN9000'.

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

    AUTHORITY-CHECK OBJECT 'F_LFA1_GRP'
                        ID 'ACTVT'     FIELD '03'
                        ID 'KTOKK'     FIELD 'VNDR'.

    cv_subrc = sy-subrc.

  ELSEIF ( iv_ind_dsp_upd EQ 'U' ). "update

    AUTHORITY-CHECK OBJECT 'F_LFA1_GRP'
                        ID 'ACTVT'     FIELD '02'
                        ID 'KTOKK'     FIELD 'VNDR'.

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
      I_SAVE        = 'A'
    CHANGING
      CS_VARIANT    = gs_variant_p
    EXCEPTIONS
      WRONG_INPUT   = 1
      NOT_FOUND     = 2
      PROGRAM_ERROR = 3
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
      others        = 3.

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
        I_SAVE        = 'A'
      CHANGING
        CS_VARIANT    = gs_variant_p
      EXCEPTIONS
        WRONG_INPUT   = 1
        NOT_FOUND     = 2
        PROGRAM_ERROR = 3
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
      FILE_NAME = cv_filename.

ENDFORM.                    " f_f4_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

* Initial internal tables
  CLEAR    gt_lifnr[].
  CLEAR    gt_aribasupl[].
  CLEAR    gt_aribasupl_upload[].
  CLEAR    gt_aribasupl_alv[].
  CLEAR    gt_field_cat[].
  CLEAR    gt_sort[].
  CLEAR    gt_ct_messages[].
  CLEAR    gt_ct_messages_p[].

* Initial data elements
  CLEAR    gv_filename.
  CLEAR    gv_flag_exit.
  CLEAR    gv_flag_sel_opt.
  CLEAR    gv_flag_sel_tab.

* Determine the selection method - by option or by table
  IF   ( sp_rbg1u     IS NOT INITIAL ).
    MOVE     gc_x         TO gv_flag_sel_tab."Select by table
  ELSE.
    MOVE     gc_x         TO gv_flag_sel_opt."Select by option
  ENDIF.

* Set the filename
  MOVE       sp_upldf     TO gv_filename.

* Set the call transaction tcode
  CLEAR                      gv_ct_tcode.
  MOVE     'MK02'         TO gv_ct_tcode.

* Set the call transaction option parameters
* Called synchronously because of follow-up ALV grid
  CLEAR                      gs_ct_options.
  MOVE     'N'            TO gs_ct_options-dismode.
  MOVE     'S'            TO gs_ct_options-updmode. "sync
  MOVE     SPACE          TO gs_ct_options-cattmode.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_file
*&---------------------------------------------------------------------*
*       Perform file processing
*----------------------------------------------------------------------*
FORM f_process_file.

  CHECK  ( sp_rbg1u    IS NOT INITIAL ).
  CHECK  ( gv_filename IS NOT INITIAL ).

  CLEAR    gt_lifnr[].
  CLEAR    gt_aribasupl[].
  CLEAR    gt_aribasupl_upload[].
  CLEAR    gt_aribasupl_alv[].

* Upload the file

  PERFORM  f_upload_file TABLES gt_aribasupl_upload
                         USING  gv_filename.

* Delete all rows where the approver is blank
  DELETE   gt_aribasupl_upload WHERE lifnr IS INITIAL.

* Build a list of distinct approver IDs
  CLEAR                                gs_aribasupl_upload.
  LOOP AT  gt_aribasupl_upload    INTO gs_aribasupl_upload.
    gv_tabix = sy-tabix.
    SHIFT    gs_aribasupl_upload-lifnr
                                  LEFT DELETING LEADING SPACE.
    TRANSLATE  gs_aribasupl_upload-lifnr
                                    TO UPPER CASE.
    CLEAR                              gv_lifnr.
    MOVE     gs_aribasupl_upload-lifnr(10)
                                    TO gv_lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = gv_lifnr
      IMPORTING
        OUTPUT = gv_lifnr.

    CLEAR                              gs_aribasupl_upload-lifnr.
    MOVE     gv_lifnr               TO gs_aribasupl_upload-lifnr.
    SHIFT    gs_aribasupl_upload-emnfr
                                  LEFT DELETING LEADING SPACE.
    TRANSLATE  gs_aribasupl_upload-emnfr
                                    TO UPPER CASE.
    MODIFY   gt_aribasupl_upload  FROM gs_aribasupl_upload
                                 INDEX gv_tabix.
    CLEAR                              gs_lifnr.
    MOVE     gs_aribasupl_upload-lifnr
                                    TO gs_lifnr.
    APPEND   gs_lifnr               TO gt_lifnr.
    CLEAR    gs_aribasupl_upload.
  ENDLOOP.

*eject
  SORT     gt_aribasupl_upload      BY lifnr ASCENDING
                                       emnfr ASCENDING.
  DELETE   ADJACENT DUPLICATES    FROM gt_aribasupl_upload
                             COMPARING lifnr.

  CHECK  ( gt_aribasupl_upload[] IS NOT INITIAL ).

* Select the existing entries from the database
  SELECT   lifnr name1 emnfr ktokk
           stras pfach ort01
           regio pstlz land1
    INTO   TABLE gt_aribasupl
    FROM   lfa1  FOR ALL ENTRIES IN gt_lifnr
   WHERE   lifnr = gt_lifnr-lifnr.
  IF ( sy-subrc EQ 0 ).
    SORT   gt_aribasupl ASCENDING BY lifnr.
  ELSE.
    CLEAR  gt_aribasupl[].
  ENDIF.

*eject
* Determine how the database is to be modified
  CLEAR                                gs_aribasupl_upload.
  LOOP AT  gt_aribasupl_upload    INTO gs_aribasupl_upload.

    CLEAR                              gv_lifnr.
    MOVE           gs_aribasupl_upload-lifnr(10)
                                    TO gv_lifnr.

    CLEAR          gs_aribasupl.
    READ     TABLE gt_aribasupl
              INTO gs_aribasupl
          WITH KEY lifnr = gv_lifnr
                   BINARY SEARCH.
    gv_subrc = sy-subrc.
    gv_tabix = sy-tabix.

    IF ( gv_subrc EQ 0 ).

      IF ( gs_aribasupl-emnfr NE gs_aribasupl_upload-emnfr ).

        CLEAR                          gs_aribasupl-emnfr.
        MOVE     gs_aribasupl_upload-emnfr
                                    TO gs_aribasupl-emnfr.
        CLEAR                          gs_aribasupl-flag_update.
        MOVE     gc_x               TO gs_aribasupl-flag_update.
        MODIFY                         gt_aribasupl
                                  FROM gs_aribasupl
                                 INDEX gv_tabix.
      ENDIF.

    ENDIF.

    CLEAR  gs_aribasupl_upload.
  ENDLOOP.

  CHECK  ( gt_aribasupl[] IS NOT INITIAL ).

* Update the vendor master

  PERFORM  f_update_vendor_master.

ENDFORM.                    " f_process_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_upload_file
*&---------------------------------------------------------------------*
*       Upload the file
*----------------------------------------------------------------------*
FORM f_upload_file
  TABLES ct_aribasupl_upload TYPE ty_gt_aribasupl_upload
  USING  iv_filename         TYPE localfile.

  DATA:  lv_row_curr         TYPE kcd_ex_row_n.

  DATA:  ls_aribasupl_upload TYPE ty_gs_aribasupl_upload.

  DATA:  ls_excel            TYPE alsmex_tabline,
         lt_excel            TYPE STANDARD TABLE OF alsmex_tabline.

* Upload the Excel file
  CLEAR    lt_excel[].

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = iv_filename
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 1
      I_END_COL               = 2
      I_END_ROW               = 8192
    TABLES
      INTERN                  = lt_excel
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE  ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*eject
* Transfer the Excel file to the upload file
  CLEAR    lv_row_curr.

  CLEAR                  ls_excel.
  LOOP AT  lt_excel INTO ls_excel.

    IF         ( lv_row_curr             NE ls_excel-row ).
      IF       ( lv_row_curr         IS NOT INITIAL      ).
        APPEND   ls_aribasupl_upload     TO ct_aribasupl_upload.
      ENDIF.
      CLEAR                                 lv_row_curr.
      MOVE     ls_excel-row              TO lv_row_curr.
      CLEAR                                 ls_aribasupl_upload.
    ENDIF.

    CASE   ls_excel-col.
      WHEN 1.
        CLEAR                               ls_aribasupl_upload-lifnr.
        MOVE     ls_excel-value          TO ls_aribasupl_upload-lifnr.
      WHEN 2.
        CLEAR                               ls_aribasupl_upload-emnfr.
        MOVE     ls_excel-value          TO ls_aribasupl_upload-emnfr.
    ENDCASE.

    AT LAST.
      APPEND     ls_aribasupl_upload     TO ct_aribasupl_upload.
    ENDAT.

    CLEAR  ls_excel.
  ENDLOOP.

  IF ( sp_del1r IS NOT INITIAL ).
    DELETE   ct_aribasupl_upload INDEX 1.
  ENDIF.

ENDFORM.                    " f_upload_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_update_vendor_master
*&---------------------------------------------------------------------*
*       Update the vendor master
*----------------------------------------------------------------------*
FORM f_update_vendor_master.

  CLEAR                                gs_aribasupl.
  LOOP AT  gt_aribasupl           INTO gs_aribasupl.

    CHECK  gs_aribasupl-flag_update EQ gc_x.

    CLEAR:   gt_bdcdata[],             gs_bdcdata.

    PERFORM  f_bdc_dynpro        USING 'SAPMF02K'
                                       '0108'.
    PERFORM  f_bdc_field         USING 'BDC_CURSOR'
                                       'RF02K-LIFNR'.
    PERFORM  f_bdc_field         USING 'RF02K-LIFNR'
                                       gs_aribasupl-lifnr.
    PERFORM  f_bdc_field         USING 'RF02K-D0120'
                                       gc_x.
    PERFORM  f_bdc_field         USING 'BDC_OKCODE'
                                       '/00'.

    PERFORM  f_bdc_dynpro        USING 'SAPMF02K'
                                       '0120'.
    PERFORM  f_bdc_field         USING 'BDC_CURSOR'
                                       'LFA1-EMNFR'.
    PERFORM  f_bdc_field         USING 'LFA1-EMNFR'
                                       gs_aribasupl-emnfr.
    PERFORM  f_bdc_field         USING 'BDC_OKCODE'
                                       '=UPDA'.

* Call transaction

    PERFORM  f_call_transaction TABLES gt_bdcdata
                                USING  gs_aribasupl-lifnr.

    CLEAR  gs_aribasupl.
  ENDLOOP.
  IF ( sy-subrc EQ 0 ).
    WAIT UP TO 2 SECONDS.
  ENDIF.

ENDFORM.                    " f_update_vendor_master
*eject
*&---------------------------------------------------------------------*
*&      Form  f_call_transaction
*&---------------------------------------------------------------------*
*       Call transaction
*----------------------------------------------------------------------*
FORM f_call_transaction
  TABLES it_ct_bdcdata TYPE bdcdata_tab
  USING  iv_lifnr      TYPE lifnr.

  DATA:  lv_subrc TYPE sysubrc.

  CLEAR  lv_subrc.

  CLEAR    gt_ct_messages[].

  CALL TRANSACTION gv_ct_tcode
             USING it_ct_bdcdata
      OPTIONS FROM gs_ct_options
     MESSAGES INTO gt_ct_messages.

  lv_subrc = sy-subrc.

  CLEAR                                gs_ct_messages.
  LOOP AT  gt_ct_messages         INTO gs_ct_messages.
    IF ( ( gs_ct_messages-msgtyp    EQ 'A' ) OR
         ( gs_ct_messages-msgtyp    EQ 'E' )    ).
      lv_subrc = 1.
      CLEAR                            gs_ct_messages-tcode.
      MOVE     iv_lifnr             TO gs_ct_messages-tcode.
      APPEND   gs_ct_messages       TO gt_ct_messages_p.
    ENDIF.
    CLEAR  gs_ct_messages.
  ENDLOOP.

  IF ( lv_subrc NE 0 ).

    CLEAR    lv_subrc.

  ENDIF.

ENDFORM.                    " f_call_transaction
*eject
*&---------------------------------------------------------------------*
*&      Form  f_bdc_dynpro
*&---------------------------------------------------------------------*
*       Start new screen                                              *
*----------------------------------------------------------------------*
FORM f_bdc_dynpro
  USING iv_program TYPE ANY
        iv_dynpro  TYPE ANY.

  CLEAR                                gs_bdcdata.
  MOVE     iv_program               TO gs_bdcdata-program.
  MOVE     iv_dynpro                TO gs_bdcdata-dynpro.
  MOVE     gc_x                     TO gs_bdcdata-dynbegin.
  APPEND   gs_bdcdata               TO gt_bdcdata.

ENDFORM.                    " f_bdc_dynpro
*eject
*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
*       Insert field                                                  *
*----------------------------------------------------------------------*
FORM f_bdc_field
  USING iv_fnam TYPE ANY
        iv_fval TYPE ANY.

  IF       ( iv_fval                NE gc_nodata ).
    CLEAR                              gs_bdcdata.
    MOVE     iv_fnam                TO gs_bdcdata-fnam.
    MOVE     iv_fval                TO gs_bdcdata-fval.
    APPEND   gs_bdcdata             TO gt_bdcdata.
  ENDIF.

ENDFORM.                    " f_bdc_field
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_data
*&---------------------------------------------------------------------*
*       Select the data
*----------------------------------------------------------------------*
FORM f_select_data.

  CLEAR    gt_aribasupl[].
  CLEAR    gt_aribasupl_alv[].

  IF   ( gv_flag_sel_opt IS NOT INITIAL ). "ALV Maintenance

    SELECT   lifnr name1 emnfr ktokk
             stras pfach ort01
             regio pstlz land1
      INTO   TABLE gt_aribasupl
      FROM   lfa1
     WHERE   lifnr IN so_lifnr.
    IF ( sy-subrc NE 0 ).
      CLEAR gt_aribasupl[].
    ENDIF.

  ENDIF.

  IF   ( gv_flag_sel_tab IS NOT INITIAL ). "File Maintenance

    IF ( gt_lifnr[] IS NOT INITIAL ).

      SELECT   lifnr name1 emnfr ktokk
               stras pfach ort01
               regio pstlz land1
        INTO   TABLE gt_aribasupl
        FROM   lfa1  FOR ALL ENTRIES IN gt_lifnr
       WHERE   lifnr = gt_lifnr-lifnr.
      IF ( sy-subrc NE 0 ).
        CLEAR gt_aribasupl[].
      ENDIF.

    ENDIF.

  ENDIF.

  SORT     gt_aribasupl ASCENDING BY lifnr.

  CLEAR                           gs_aribasupl.
  LOOP AT  gt_aribasupl      INTO gs_aribasupl.
    CLEAR                         gs_aribasupl_alv.
    MOVE-CORRESPONDING            gs_aribasupl
                               TO gs_aribasupl_alv.
    APPEND gs_aribasupl_alv    TO gt_aribasupl_alv.
    CLEAR  gs_aribasupl.
  ENDLOOP.

ENDFORM.                    " f_select_data
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
          CONTAINER_NAME              = gc_alv_container
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.

      IF ( sy-subrc NE 0 ).
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = gv_repid
            TXT1  = text-351
            TXT2  = sy-subrc.
        LEAVE PROGRAM.
      ENDIF.

    ENDIF.

* Create an instance of the ALV grid
    CREATE OBJECT go_grid1
      EXPORTING
        I_PARENT = go_custom_container.

* Build the ALV field catalog
    PERFORM  f_build_field_cat  TABLES gt_field_cat.

*eject
* Build the ALV layout
    CLEAR                              gs_layout.
    MOVE     gc_x                   TO gs_layout-zebra.
    MOVE     gc_x                   TO gs_layout-cwidth_opt.
    MOVE     text-101               TO gs_layout-grid_title.
    MOVE     'A'                    TO gs_layout-sel_mode.
    MOVE     'CELLIO_TAB'           TO gs_layout-stylefname.
    MOVE     'LINECOLOR'            TO gs_layout-info_fname.

* At event "PRINT_END_OF_PAGE", two lines must be reserved
    CLEAR                              gs_print.
    MOVE     2                      TO gs_print-reservelns.

* Create an instance of the event receiver
*   CREATE OBJECT go_event_receiver.

* Link event handler methods
* When ALV control raises the event, the method is automatically called
*   SET HANDLER go_event_receiver->handle_top_of_page
*           FOR go_grid1.
*   SET HANDLER go_event_receiver->handle_top_of_list
*           FOR go_grid1.

    CALL METHOD go_grid1->set_table_for_first_display
      EXPORTING
        IS_LAYOUT       = gs_layout
        IS_PRINT        = gs_print
*       IS_VARIANT      =
        I_SAVE          = ' '
      CHANGING
        IT_OUTTAB       = gt_aribasupl_alv[]
        IT_FIELDCATALOG = gt_field_cat[]
        IT_SORT         = gt_sort[].

* Do not refresh the ALV grid if bad data exists
  ELSE.

    CALL METHOD go_grid1->refresh_table_display.

  ENDIF.

* Set focus to ensure that the cursor is active in the control
  IF ( cl_gui_alv_grid=>offline( ) IS INITIAL ).
    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        CONTROL = go_grid1.
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
      I_STRUCTURE_NAME = gc_alv_structure
    CHANGING
      CT_FIELDCAT      = ct_field_cat[].

* Build the sort internal table
  CLEAR:   gt_sort[],                  gs_sort.
  MOVE     '1'                      TO gs_sort-spos.
  MOVE     'LIFNR'                  TO gs_sort-fieldname.
  MOVE     gc_x                     TO gs_sort-up.
  APPEND   gs_sort                  TO gt_sort.

ENDFORM.                    " f_build_field_cat
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
*&      Form  f_write_messages
*&---------------------------------------------------------------------*
*       Write the error messages to the spool
*----------------------------------------------------------------------*
FORM f_write_messages.

  DATA: lv_type   TYPE bapi_mtype,
        lv_cl     TYPE symsgid,
        lv_number TYPE symsgno,
        lv_par1   TYPE symsgv,
        lv_par2   TYPE symsgv,
        lv_par3   TYPE symsgv,
        lv_par4   TYPE symsgv.

  DATA: ls_return TYPE bapiret2.

  CLEAR                           gs_ct_messages.
  LOOP AT  gt_ct_messages_p  INTO gs_ct_messages.

    CLEAR                                   lv_type.
    MOVE     gs_ct_messages-msgtyp       TO lv_type.
    CLEAR                                   lv_cl.
    MOVE     gs_ct_messages-msgid        TO lv_cl.
    CLEAR                                   lv_number.
    MOVE     gs_ct_messages-msgnr        TO lv_number.
    CLEAR                                   lv_par1.
    MOVE     gs_ct_messages-msgv1(50)    TO lv_par1.
    CLEAR                                   lv_par2.
    MOVE     gs_ct_messages-msgv2(50)    TO lv_par2.
    CLEAR                                   lv_par3.
    MOVE     gs_ct_messages-msgv3(50)    TO lv_par3.
    CLEAR                                   lv_par4.
    MOVE     gs_ct_messages-msgv4(50)    TO lv_par4.
    CLEAR                                   ls_return.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        TYPE   = lv_type
        CL     = lv_cl
        NUMBER = lv_number
        PAR1   = lv_par1
        PAR2   = lv_par2
        PAR3   = lv_par3
        PAR4   = lv_par4
      IMPORTING
        RETURN = ls_return.

    WRITE: /001 text-211,
            008 gs_ct_messages-tcode.
    WRITE: /008 ls_return-message(122).

    CLEAR  gs_ct_messages.
  ENDLOOP.

ENDFORM.                    " f_write_messages
