*&---------------------------------------------------------------------*
*& Report  ZMMR_MATL_MASTERDATA_RPT
*&
*&---------------------------------------------------------------------*

REPORT  zmmr_matl_masterdata_rpt.
*&---------------------------------------------------------------------*
*& Program Name       :  ZMMR_MATL_MASTERDATA_RPT                      *
*& Author             :  Balaji Ganapathiraman                         *
*& Creation Date      :  20-JAN-2015                                   *
*& Object ID          :  SDP-80003                                     *
*& Application Area   :  SC-MM                                         *
*& Description        :  Material Master Data Report                   *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 20-JAN-2015                                          *
* Modified By   : Balaji G                                             *
* Correction No : D30K925073                                           *
* Description   : D30K925073 - Initial Version                         *
*----------------------------------------------------------------------*


DATA: gv_matnr   TYPE mara-matnr,
      gv_ersda   TYPE mara-ersda,
      gv_mstae   TYPE mara-mstae,
      gv_class   TYPE klah-class.

SELECTION-SCREEN BEGIN OF BLOCK b1a WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_matnr FOR gv_matnr,
                s_ersda FOR gv_ersda,
                s_mstae FOR gv_mstae NO INTERVALS NO-EXTENSION,
                s_class FOR gv_class NO INTERVALS NO-EXTENSION MATCHCODE OBJECT clas.
SELECTION-SCREEN END OF BLOCK b1a .
SELECTION-SCREEN BEGIN OF BLOCK b1b WITH FRAME TITLE text-t02.

PARAMETERS: rb_disp RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND uc1.
PARAMETERS: rb_dwld RADIOBUTTON GROUP r1 .

*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b2a WITH FRAME TITLE text-t05.
PARAMETERS: p_file TYPE rlgrap-filename DEFAULT 'H:\Matl_MD_Rpt.xlsx' MODIF ID m1.
SELECTION-SCREEN END OF BLOCK b2a.
SELECTION-SCREEN END OF BLOCK b1b .

*--------------------------------------------------------------------*
*               T Y P E    D E F I N I T I O N S                     *
*--------------------------------------------------------------------*

TYPES: BEGIN OF ty_output,
         material     TYPE mara-matnr,
         matl_type    TYPE mara-mtart,
         manu_partno  TYPE mara-mfrpn,
         manu_no      TYPE mara-mfrnr,
         manu_name    TYPE lfa1-name1,
         short_desc TYPE makt-maktx,
         long_desc  TYPE string,
       END OF ty_output.


*--------------------------------------------------------------------*
*     G L O B A L    V A R I A B L E    D E C L A R A T I O N S      *
*--------------------------------------------------------------------*

DATA:
      it_output TYPE STANDARD TABLE OF ty_output.           "#EC NEEDED


FIELD-SYMBOLS:
               <fs_output>  LIKE LINE OF it_output.         "#EC NEEDED

INITIALIZATION.
  SET PARAMETER ID 'KAR' FIELD '001'.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF  screen-group1 = 'M1'.
      IF  rb_disp NE space.
        screen-active = 0.
        MODIFY SCREEN.
      ELSE.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN .
  IF sy-ucomm EQ 'ONLI' AND rb_dwld NE space AND p_file EQ space .
    MESSAGE e100(zm) WITH 'Provide a filename for download.'(008).
  ENDIF.

START-OF-SELECTION.
  PERFORM f_fetch_data_from_db.

END-OF-SELECTION.
  PERFORM f_display_or_download.

*&---------------------------------------------------------------------*
*&      Form  F_FETCH_DATA_FROM_DB
*&---------------------------------------------------------------------*
*     Fetch Data from Database and Consolidate to Final Table
*----------------------------------------------------------------------*
FORM f_fetch_data_from_db .

  TYPES:
         BEGIN OF ty_makt,
           matnr  TYPE makt-matnr,
           maktx  TYPE makt-maktx,
         END OF ty_makt,

         BEGIN OF ty_mara,
           matnr  TYPE mara-matnr,
           mtart  TYPE mara-mtart,
           bmatn  TYPE mara-bmatn,
           mfrpn  TYPE mara-mfrpn,
           mfrnr  TYPE mara-mfrnr,
           objek  TYPE kssk-objek,
         END OF ty_mara,
         BEGIN OF ty_class,
           objek    TYPE kssk-objek,
           clint    TYPE kssk-clint,
           adzhl    TYPE kssk-adzhl,
           class    TYPE klah-class,
         END OF ty_class,
         BEGIN OF ty_lfa1,
           lifnr  TYPE lfa1-lifnr,
           name1  TYPE lfa1-name1,
         END OF ty_lfa1.

  DATA: lv_name TYPE thead-tdname,
        lv_text TYPE char255,
        lv_count  TYPE sy-tfill,
        lv_count1 TYPE char22,
        lv_index  TYPE sy-tabix,
        lv_name1  TYPE lfa1-name1.

  DATA:
        lit_mara   TYPE STANDARD TABLE OF ty_mara,
        lit_makt   TYPE STANDARD TABLE OF ty_makt,
        lit_class  TYPE STANDARD TABLE OF ty_class.

  DATA: lit_lines      TYPE idmx_di_t_tline,
        lit_mara_tmp   TYPE STANDARD TABLE OF ty_mara,
        lit_mara1      TYPE STANDARD TABLE OF ty_mara,
        lit_lfa1       TYPE STANDARD TABLE OF ty_lfa1.

  FIELD-SYMBOLS: <lfs_mara>   LIKE LINE OF lit_mara,
                 <lfs_lfa1>   LIKE LINE OF lit_lfa1,
                 <lfs_mara1>  LIKE LINE OF lit_mara,
                 <lfs_makt>   LIKE LINE OF lit_makt.

  DATA: lr_data TYPE REF TO data.

  CREATE DATA lr_data LIKE LINE OF lit_lfa1.
  ASSIGN lr_data->* TO <lfs_lfa1>.
  CLEAR: lit_mara[], lit_makt[], lit_class[], it_output[].

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Fetching Material Master Data...'(001).
  SELECT matnr
         mtart
         bmatn
         mfrpn
         mfrnr
         matnr
    FROM mara INTO TABLE lit_mara
    WHERE matnr  IN s_matnr
      AND ersda  IN s_ersda
      AND mstae  IN s_mstae.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 25
        text       = 'Fetching Material Short Desc...'(002).

    SELECT matnr
           maktx
      FROM makt INTO TABLE lit_makt
      FOR ALL ENTRIES IN lit_mara
      WHERE matnr = lit_mara-matnr
        AND spras = sy-langu.
    IF sy-subrc EQ 0.
      SORT lit_makt BY matnr.
    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 38
        text       = 'Fetching Manufacturer Details...'(010).

    lit_mara_tmp[] = lit_mara[].
    DELETE lit_mara_tmp WHERE bmatn NE space.
    IF lit_mara_tmp IS NOT INITIAL.
      SELECT matnr
             mtart
             bmatn
             mfrpn
             mfrnr
             matnr
        FROM mara INTO TABLE lit_mara1                  "#EC CI_NOFIELD
        FOR ALL ENTRIES IN lit_mara_tmp
        WHERE bmatn = lit_mara_tmp-matnr.
      IF sy-subrc EQ 0.
        SORT lit_mara1 BY bmatn.
        lit_mara_tmp[] = lit_mara1[].
        SORT lit_mara_tmp BY mfrnr.
        DELETE ADJACENT DUPLICATES FROM lit_mara_tmp COMPARING mfrnr.
        SELECT lifnr
               name1
          FROM lfa1 INTO TABLE lit_lfa1
          FOR ALL ENTRIES IN lit_mara_tmp
          WHERE lifnr = lit_mara_tmp-mfrnr.
        IF sy-subrc EQ 0  .
          SORT lit_lfa1 BY lifnr.
        ENDIF.

      ENDIF.
    ENDIF.
    IF s_class[] IS NOT INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = 'Fetching Material Classification Data...'(003).

      SELECT a~objek
             a~clint
             a~adzhl
             b~class
        INTO TABLE lit_class
        FROM kssk AS a INNER JOIN klah AS b
        ON a~clint = b~clint
        FOR ALL ENTRIES IN lit_mara
        WHERE a~objek = lit_mara-objek
          AND a~klart = '001'
          AND a~lkenz = space
          AND b~class IN s_class.
      IF sy-subrc EQ 0.
        SORT lit_class BY objek.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 70
        text       = 'Consolidating Data...'(004).

    LOOP AT  lit_mara ASSIGNING <lfs_mara>.
      READ TABLE lit_makt ASSIGNING <lfs_makt> WITH KEY matnr = <lfs_mara>-matnr  BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF s_class[] IS NOT INITIAL.
          READ TABLE lit_class TRANSPORTING NO FIELDS WITH KEY objek = <lfs_mara>-matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        APPEND INITIAL LINE TO it_output ASSIGNING <fs_output>.
        <fs_output>-material = <lfs_mara>-matnr.
        <fs_output>-matl_type = <lfs_mara>-mtart.
        IF <lfs_mara>-bmatn NE space.
          <fs_output>-manu_partno = <lfs_mara>-mfrpn.
          <fs_output>-manu_no     = <lfs_mara>-mfrnr.
        ELSE.
          READ TABLE lit_mara1 ASSIGNING <lfs_mara1> WITH KEY bmatn = <lfs_mara>-matnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            <fs_output>-manu_partno = <lfs_mara1>-mfrpn.
            <fs_output>-manu_no     = <lfs_mara1>-mfrnr.
          ENDIF.
        ENDIF.
        READ TABLE lit_lfa1 ASSIGNING <lfs_lfa1> WITH KEY lifnr = <fs_output>-manu_no BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_output>-manu_name = <lfs_lfa1>-name1.
        ELSE.
          lv_index = sy-tabix.
          SELECT SINGLE name1 FROM lfa1 INTO lv_name1
            WHERE lifnr = <fs_output>-manu_no.
          IF sy-subrc EQ 0.
            CLEAR: <lfs_lfa1>.
            <lfs_lfa1>-lifnr = <fs_output>-manu_no.
            <lfs_lfa1>-name1 = lv_name1.
            INSERT <lfs_lfa1> INTO lit_lfa1 INDEX lv_index.
          ENDIF.
        ENDIF.
        <fs_output>-short_desc = <lfs_makt>-maktx.
        lv_name = <fs_output>-material.
        CLEAR: lit_lines[].
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = 'GRUN'
            language                = sy-langu
            name                    = lv_name
            object                  = 'MATERIAL'
          TABLES
            lines                   = lit_lines[]
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ELSE.
          CALL FUNCTION 'IDMX_DI_TLINE_INTO_STRING'
            EXPORTING
              it_tline       = lit_lines[]
            IMPORTING
              ev_text_string = <fs_output>-long_desc.
        ENDIF.
        lv_count = lv_count + 1.
        lv_count1 = lv_count.
        CONDENSE lv_count1.
        CONCATENATE 'Building Final Table..'(005)
                    lv_count1
               INTO lv_text SEPARATED BY space.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = lv_text.

      ENDIF.
    ENDLOOP.


  ELSE.
    MESSAGE s100(zm) WITH 'No Data found for the input criteria'(006).
  ENDIF.

ENDFORM.                    " F_FETCH_DATA_FROM_DB
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_OR_DOWNLOAD
*&---------------------------------------------------------------------*
*       Based on User input either download or display the ALV
*----------------------------------------------------------------------*
FORM f_display_or_download .

  DATA: lr_salv_table TYPE REF TO cl_salv_table,
        lr_columns    TYPE REF TO cl_salv_columns_list,
        lr_column     TYPE REF TO cl_salv_column,
        lr_aggreg     TYPE REF TO cl_salv_aggregations,
        lit_fcat       TYPE lvc_t_fcat
        .

  DATA: lr_display   TYPE REF TO cl_salv_display_settings,
        lv_lines     TYPE char22,
        lv_title     TYPE lvc_title.


  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lr_salv_table
        CHANGING
          t_table      = it_output[].
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.
  lr_columns = lr_salv_table->get_columns( ).
  TRY.
      lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

      lr_column = lr_columns->get_column( columnname = 'LONG_DESC' ).
      lr_column->set_short_text( 'Long Desc' ).             "#EC NOTEXT
      lr_column->set_medium_text( 'Long Desc' ).            "#EC NOTEXT
      lr_column->set_long_text( 'Long Description' ).       "#EC NOTEXT
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
    CATCH cx_salv_not_found .                           "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'MANU_NAME' ).
      lr_column->set_short_text( 'Manufact.' ).             "#EC NOTEXT
      lr_column->set_medium_text( 'Manufacturer' ).         "#EC NOTEXT
      lr_column->set_long_text( 'Manufacturer Name' ).      "#EC NOTEXT
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
    CATCH cx_salv_not_found .                           "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'MANU_NO' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
    CATCH cx_salv_not_found .                           "#EC NO_HANDLER
  ENDTRY.

  lr_aggreg   = lr_salv_table->get_aggregations( ).
  lit_fcat    =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                r_columns      = lr_columns
                                r_aggregations = lr_aggreg ).

  DATA: lr_func TYPE REF TO cl_salv_functions_list.
  "Functions
  lr_func = lr_salv_table->get_functions( ).
  lr_func->set_all( ).

  lr_display = lr_salv_table->get_display_settings( ).
  lv_lines = lines( it_output[] ).
  CONDENSE lv_lines.
  CONCATENATE 'Material Master Data Report -'(011) lv_lines ' Records'(007) INTO lv_title SEPARATED BY space.
  lr_display->set_list_header( lv_title ).

  IF rb_dwld NE space AND p_file IS NOT INITIAL.
    CALL METHOD zcl_utilities=>create_xls_from_itab
      EXPORTING
        i_filename  = p_file
        it_fieldcat = lit_fcat[]
*       it_sort     = lit_sort_exl
**     it_filt     =
*       is_layout   = lwa_layout_exl
        i_filetype  = if_salv_bs_xml=>c_type_xlsx "lv_filetype  "IF_SALV_BS_XML=>C_TYPE_EXCEL_XML
      CHANGING
        ct_data     = it_output[].

  ENDIF.
  IF  rb_disp NE space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 99
        text       = 'Displaying ALV Output...'(009).
    WAIT UP TO 2 SECONDS.
    lr_salv_table->display( ).
  ENDIF.



ENDFORM.                    " F_DISPLAY_OR_DOWNLOAD
