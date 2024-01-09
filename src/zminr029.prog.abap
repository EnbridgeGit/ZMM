REPORT zminr029 NO STANDARD PAGE HEADING LINE-SIZE 100 LINE-COUNT 65
                                                       MESSAGE-ID zs.

************************************************************************
*  Client:     Spectra Energy.                                         *
*                                                                      *
*  Date:       July, 2010                                              *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
* This program produces a downloadable file listing the value of       *
* inventory.  The value will be of the material groups reported by the *
* plant and the storage location.  Parameters may be supplied to limit *
* the size of the report.                                              *
************************************************************************
*-----------------------------------------------------------------------
* Change History
*-----------------------------------------------------------------------
* Chng.Req#  |Date       |Developer   |Description
*-----------------------------------------------------------------------
* D30K928140/ 4/12/2017   PANUSURI    1.Added Price Per(MBEW-PEINH)
* D30K928142                            while computing value calculation
*                                     2.Included the Stock in Transit at
*                                       Plant level(MARC-UMLMC).
*                                     3.Fix is done for rounding the
*                                       stock issue.
************************************************************************

TABLES: mara,      "Material master, general data
        marc,      "Material master, plant code
        mard,      "Material master, storage location/batch segment
        mbew,      "Material valuation
        t001w,     "Plant Code Lookup
        t001l.     "Storage Location Lookup

DATA:   BEGIN OF mat_tab OCCURS 5000,             "Required data
          matkl    LIKE mara-matkl,               "Material Group
          werks    LIKE mard-werks,               "Plant
          name1    LIKE t001w-name1,              "Plant Name
          lgort    LIKE mard-lgort,               "Storage Location
          lgobe    LIKE t001l-lgobe,              "Storage Location Name
          value    LIKE mbew-salkv,               "Moving Average Value
        END OF mat_tab.

DATA:   BEGIN OF mat_tab_sum OCCURS 500,          "Required data
          matkl    LIKE mara-matkl,               "Material Group
          werks    LIKE mard-werks,               "Plant
          name1    LIKE t001w-name1,              "Plant Name
          lgort    LIKE mard-lgort,               "Storage Location
          lgobe    LIKE t001l-lgobe,              "Storage Location Name
          value    LIKE mbew-salkv,               "Moving Average Value
        END OF mat_tab_sum.

DATA:   BEGIN OF mgroup_list OCCURS 50,
          matkl    LIKE mara-matkl,
        END OF mgroup_list.

DATA:   mt_tab TYPE STANDARD TABLE OF string,
        m_rec      TYPE string,
        m_cnt      TYPE i.

DATA:   out_string  TYPE string.
DATA:   tabchar    TYPE x VALUE '09'.
DATA:   msg_text(50).
************************************************************************
*                   Selection Screen                                   *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK intro WITH FRAME.


SELECTION-SCREEN BEGIN OF BLOCK group WITH FRAME.
SELECT-OPTIONS:
  s_group     FOR mara-matkl                  "Material Group
              OBLIGATORY MODIF ID abc.
SELECTION-SCREEN END OF BLOCK group.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK plant WITH FRAME.
SELECT-OPTIONS:
  s_plant     FOR marc-werks                  "Plant Code
              OBLIGATORY MODIF ID abc.
SELECTION-SCREEN END OF BLOCK plant.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK store WITH FRAME.
SELECT-OPTIONS:
  s_store     FOR  mard-lgort                 "Storage Location
              OBLIGATORY MODIF ID abc.
SELECTION-SCREEN END OF BLOCK store.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK filename WITH FRAME TITLE text-000.
PARAMETERS: p_local RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_unix  RADIOBUTTON GROUP gr1.
SELECTION-SCREEN SKIP.
PARAMETERS: p_locfil LIKE filename-fileextern,
            p_outfil LIKE filename-fileextern.
SELECTION-SCREEN END OF BLOCK filename.

SELECTION-SCREEN END OF BLOCK intro.


************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid '/CFMM001'
              '/invmatgroup.dat' INTO p_outfil.
  CONCATENATE 'C:/SAPTEMP/' 'invmatgroup.dat' INTO p_locfil.


************************************************************************
*                 AT SELECTION-SCREEN                                  *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_locfil.
  PERFORM search_filename USING p_locfil.


************************************************************************
*                   START-OF-SELECTION                                 *
************************************************************************
START-OF-SELECTION.

*  DATA:   qtyonhand  TYPE i. "(-)PANUSURI Ticket ACR-4083
  DATA:   qtyonhand  TYPE labst.  "(+)PANUSURI Ticket ACR-4083

  SELECT DISTINCT matkl
  INTO CORRESPONDING FIELDS OF TABLE mgroup_list
  FROM mara
  WHERE matkl IN s_group.

  SELECT * FROM mara
  WHERE  matkl IN s_group.

    SELECT * FROM mard
    WHERE  matnr = mara-matnr
    AND    werks IN s_plant
    AND    lgort IN s_store.

      SELECT SINGLE * FROM mbew
      WHERE  matnr = mara-matnr
      AND    bwkey = mard-werks
      AND    bwtar = space.

      SELECT SINGLE * FROM t001w
      WHERE werks = mard-werks.

      SELECT SINGLE * FROM t001l
      WHERE werks = mard-werks
      AND lgort = mard-lgort.

      COMPUTE qtyonhand = ( mard-insme + mard-labst
                     + mard-umlme + mard-speme + mard-einme ).
      IF qtyonhand NE 0.
        CLEAR mat_tab.
        MOVE mara-matkl TO mat_tab-matkl.
        MOVE mard-werks TO mat_tab-werks.
        MOVE mard-lgort TO mat_tab-lgort.
        MOVE t001w-name1 TO mat_tab-name1.
        MOVE t001l-lgobe TO mat_tab-lgobe.
*        COMPUTE MAT_TAB-VALUE = MBEW-VERPR * QTYONHAND.  "(-)PANUSURI Ticket ACR-4083
        COMPUTE mat_tab-value = ( mbew-verpr / mbew-peinh ) * qtyonhand."(+)PANUSURI Ticket ACR-4083
        APPEND mat_tab.
      ENDIF.

    ENDSELECT.
*BOI by PANUSURI Ticket ACR-4083
    SELECT * FROM marc
    WHERE matnr = mara-matnr
    AND   werks IN s_plant.

      SELECT SINGLE * FROM mbew
      WHERE  matnr = mara-matnr
      AND    bwkey = marc-werks
      AND    bwtar = space.

      SELECT SINGLE * FROM t001w
      WHERE  werks = marc-werks.

      IF marc-umlmc IS NOT INITIAL.
        CLEAR mat_tab.
        MOVE mara-matkl TO mat_tab-matkl.
        MOVE marc-werks TO mat_tab-werks.
        MOVE t001w-name1 TO mat_tab-name1.
        COMPUTE mat_tab-value = ( mbew-verpr / mbew-peinh ) * marc-umlmc.
        APPEND mat_tab.
      ENDIF.
    ENDSELECT.
*EOI by PANUSURI Ticket ACR-4083
  ENDSELECT.

* Sort the table just extracted.
  SORT mgroup_list BY matkl.
  SORT mat_tab BY matkl werks lgort.

* Create summary table with total values
  LOOP AT mat_tab.

    CLEAR mat_tab_sum.
    MOVE mat_tab-matkl TO mat_tab_sum-matkl.
    MOVE mat_tab-werks TO mat_tab_sum-werks.
    MOVE mat_tab-lgort TO mat_tab_sum-lgort.
    MOVE mat_tab-name1 TO mat_tab_sum-name1.
    MOVE mat_tab-lgobe TO mat_tab_sum-lgobe.

    AT END OF lgobe.
      SUM.
      MOVE mat_tab-value TO mat_tab_sum-value.
      APPEND mat_tab_sum.
    ENDAT.

  ENDLOOP.

* Sort the table just created.
  SORT mat_tab_sum BY werks lgort matkl.


************************************************************************
*                END-OF-SELECTION                                      *
************************************************************************
END-OF-SELECTION.
  PERFORM create_file.


************************************************************************
*      CREATE FILE                                                     *
************************************************************************
FORM create_file.

  DATA: l_file TYPE string,
        l_subrc TYPE sy-subrc,
        cur_lgort TYPE string,
        cur_werks TYPE string,
        cur_line TYPE i,
        old_line TYPE i.

  DATA:   nvalue TYPE string.


* Output/Create Extract File
  IF NOT ( p_unix IS INITIAL ).
    OPEN DATASET p_outfil FOR OUTPUT IN TEXT MODE MESSAGE msg_text.
    IF sy-subrc NE 0.
      MESSAGE i002 WITH p_outfil msg_text.
      STOP.
    ENDIF.
  ENDIF.

  PERFORM generate_headers.

  CLEAR: cur_werks, cur_lgort, out_string.
  cur_line = 0.
  old_line = 1.

*****************************
* TRANSPOSE VALUES TO OUTPUT
*****************************
  LOOP AT mat_tab_sum.

*   Check if it is a new Storage Location or Plant
    IF cur_lgort NS mat_tab_sum-lgort OR cur_werks NS mat_tab_sum-werks.

*     Write the previous Storage Location and Plant
      IF out_string CN ''.
        PERFORM write_rec.
      ENDIF.

      CLEAR: out_string.
      old_line = 1.
      CONCATENATE mat_tab_sum-werks mat_tab_sum-name1 mat_tab_sum-lgort mat_tab_sum-lgobe
              INTO out_string SEPARATED BY tabchar.

    ENDIF.

    cur_line = 0.
*   Get the Current Material Group
    LOOP AT mgroup_list.
      cur_line = cur_line + 1.

      IF mgroup_list-matkl CS mat_tab_sum-matkl.
*       Exit the loop if Material Group is found
        EXIT.
      ENDIF.
    ENDLOOP.

    WHILE old_line < cur_line.
*     Add a TAB
      CONCATENATE out_string '' INTO out_string SEPARATED BY tabchar.
      old_line = old_line + 1.
    ENDWHILE.


*   Make the old line whatever line we are on, Plus 1 since we only want to loop when its not the NEXT one.
    old_line = cur_line + 1.

    cur_werks = mat_tab_sum-werks.
    cur_lgort = mat_tab_sum-lgort.


*   Convert to String Value to send to line output
    nvalue = mat_tab_sum-value.
    CONCATENATE out_string nvalue
            INTO out_string SEPARATED BY tabchar.

  ENDLOOP.

*Perform final writeout
  IF out_string CN ''.
    PERFORM write_rec.
  ENDIF.

*- Close Output File
  IF NOT ( p_unix IS INITIAL ).
    CLOSE DATASET p_outfil.
    IF NOT ( m_cnt IS INITIAL ).
      MESSAGE i034.
    ENDIF.

*- Create Local File
  ELSE.
    l_file = p_locfil.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = l_file
        filetype                = 'ASC'
      CHANGING
        data_tab                = mt_tab
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

    IF sy-subrc EQ 0.
      MESSAGE i034.
    ELSEIF sy-subrc EQ 15.
      MESSAGE i033.
    ELSEIF sy-subrc NE 0.
      MESSAGE i002 WITH p_locfil msg_text.
    ENDIF.

  ENDIF.
ENDFORM.                    "CREATE_FILE


************************************************************************
*      GENERATE_HEADERS                                                *
************************************************************************
FORM generate_headers.

  DATA: temp_matkl TYPE string.

  CLEAR: out_string.

  CONCATENATE 'Plnt' 'Plant Description' 'SLoc' 'SLoc Description'
    INTO out_string SEPARATED BY tabchar.

  LOOP AT mgroup_list.
    CONCATENATE '''' mgroup_list-matkl
            INTO temp_matkl.
    CONCATENATE out_string temp_matkl
            INTO out_string SEPARATED BY tabchar.
  ENDLOOP.

  PERFORM write_rec.

ENDFORM.                    "GENERATE_HEADERS


************************************************************************
*      SEARCH_FILENAME                                                 *
************************************************************************
FORM search_filename USING p_file.

  DATA: l_dir TYPE string,
        l_rc TYPE i,
        l_filetable TYPE filetable.

  l_dir = 'G:\'.

* Call Method to retrieve filename
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a file...'
      default_extension       = 'CSV'
      initial_directory       = l_dir
    CHANGING
      file_table              = l_filetable
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK sy-subrc EQ 0.
  READ TABLE l_filetable INDEX 1 INTO p_file.

ENDFORM.                    "SEARCH_FILENAME


************************************************************************
*      Form  WRITE_REC
************************************************************************
FORM write_rec.

*  WRITE: /1 OUT_STRING.

  IF NOT ( p_unix IS INITIAL ).
    TRANSFER out_string TO p_outfil.

    IF sy-subrc EQ 0.
      ADD 1 TO m_cnt.
    ENDIF.
  ELSE.
    m_rec = out_string.
    APPEND m_rec TO mt_tab.
  ENDIF.

ENDFORM.                    "WRITE_REC
