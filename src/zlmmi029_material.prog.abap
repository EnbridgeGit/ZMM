*&---------------------------------------------------------------------*
*& Report  ZLMMI029_MATERIAL
*&---------------------------------------------------------------------*

REPORT  zlmmi029_material.

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI029_MATERIAL                             *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 24, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts Material Master data and     *
*&                       Outbound CSV file send to SE FTP server.      *
*&                       This file is being used in ARIBA Spend Viz    *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : D30K925438                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

INCLUDE zlmmi029_material_top.

************************************************************************
*                           Selection Screen                           *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: rb_appl RADIOBUTTON GROUP grp1
                    USER-COMMAND cmd
                    DEFAULT 'X'.
PARAMETERS: rb_pres RADIOBUTTON GROUP grp1.

PARAMETERS: p_fpath  TYPE rlgrap-filename
                     DEFAULT gc_fpath MODIF ID csv.       " Application server path
PARAMETERS: p_ppath  TYPE rlgrap-filename
                     DEFAULT gc_ppath MODIF ID pre.       " Presentaion server path

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  IF NOT rb_pres IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'CSV'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF NOT rb_appl IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRE'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.

**  .CSV file name validations
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath.
  PERFORM f_directory_selection CHANGING p_fpath.

** Presentation server path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ppath.
  PERFORM f_directory_selection CHANGING p_ppath.

AT SELECTION-SCREEN.
  IF rb_appl IS NOT INITIAL.
    IF p_fpath IS INITIAL.
      CLEAR p_fpath.
      MOVE gc_fpath TO p_fpath.
    ENDIF.
  ENDIF.
  IF rb_pres IS NOT INITIAL.
    IF p_ppath IS INITIAL.
      CLEAR p_ppath.
      MOVE gc_ppath TO p_ppath.
    ENDIF.
  ENDIF.
************************************************************************
*                        START-OF-SELECTION                            *
************************************************************************
START-OF-SELECTION.
  PERFORM f_get_program_parms. "Get progam execution date details
  PERFORM f_prepare_header.     " Prepare header for Account.csv file
  PERFORM f_get_data.           " Get data from Master tables
  IF git_mara[] IS NOT INITIAL.
    PERFORM f_prepare_file.       " Prepare final internal table
    PERFORM f_send_file USING gc_part. " send file to location
  ELSE.
    MESSAGE 'NO Data Selected' TYPE 'E'.
  ENDIF.

  IF gv_flag EQ 'X'.
    MESSAGE 'File Downloaded Successfuly..' TYPE 'S'.
  ELSE.
    MESSAGE 'Error While Downloading' TYPE 'E'.
  ENDIF.

************************************************************************
*                        END-OF-SELECTION                              *
************************************************************************
END-OF-SELECTION.

**Inserting execution date&time
  CLEAR gwa_zprog.
  gwa_zprog-zclnt = sy-mandt.
  gwa_zprog-zprog = sy-cprog.
  gwa_zprog-zexec_date = sy-datum.
  gwa_zprog-zexec_time = sy-uzeit.
  MODIFY zexec_date FROM gwa_zprog.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_DIRECTORY_SELECTION
*&---------------------------------------------------------------------*
*      Select Directory for download
*----------------------------------------------------------------------*

FORM f_directory_selection  CHANGING p_p_path TYPE localfile.
  DATA lv_path TYPE string.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder = lv_path
    EXCEPTIONS
      OTHERS          = 0.
  IF sy-subrc EQ 0.
    CONCATENATE lv_path '\' INTO lv_path.
    p_p_path = lv_path.
    WRITE: p_p_path.
  ENDIF.
ENDFORM.                    " F_DIRECTORY_SELECTION

*&---------------------------------------------------------------------*
*&      Form  F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*      Get last execution date and finalise date for selection
*----------------------------------------------------------------------*
FORM f_get_program_parms .
  CLEAR:gv_ldate, gv_ltime.
** Get Last Execution data from custom table zexec_date
  SELECT zclnt zprog zexec_date zexec_time FROM zexec_date INTO TABLE git_zprog
    WHERE zprog EQ sy-cprog.
  IF sy-subrc EQ 0.
    SORT git_zprog BY zexec_date zexec_time DESCENDING.
    READ TABLE git_zprog INTO gwa_zprog INDEX 1.
    gv_ldate = gwa_zprog-zexec_date.
    gv_ltime = gwa_zprog-zexec_time.
    CLEAR gwa_zprog.
  ENDIF.
ENDFORM.                    " F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_HEADER
*&---------------------------------------------------------------------*
*       Prepare header for Account.csv file
*----------------------------------------------------------------------*
FORM f_prepare_header .
**Header Preparation
  CONCATENATE '"PartNumber"'
              '"RevisionNumber"'
              '"Description1"'
              '"Description2"'
              '"StandardCost"'
              '"StandardCostCurrency"'
              '"StandardCostDate"'
              '"StockIndicator"'
              '"ManufacturerName"'
              '"ManufacturerPartNumber"'
              '"LeadTimeInDays"'
              '"FlexField1"'
              '"FlexField2"'
              '"FlexField3"'
              '"PartFlexText1"'
              '"PartFlexText2"'
              '"PartFlexText3"'
              '"OriginCountry"'
              '"MaterialComposition"'
              '"ForecastUsage1"'
              '"ForecastUsage2"'
              '"ForecastUsage3"'
              INTO gwa_file-line SEPARATED BY ','.
  INSERT gwa_file INTO git_file INDEX 1.  " Insert as first line
  CLEAR gwa_file.
ENDFORM.                    " F_PREPARE_HEADER


*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Get data from Master tables
*----------------------------------------------------------------------*
FORM f_get_data .
**  Read data from database tables
  DATA: it_mbew TYPE STANDARD TABLE OF ty_mbew.

**  mara table data
  SELECT matnr
         mfrnr
         mfrpn
         przus
    INTO TABLE git_mara FROM mara
    WHERE ( ersda GT gv_ldate AND ersda LE sy-datum )  " Record Creation date
       OR ( laeda GE gv_ldate AND laeda LE sy-datum )  " Record Changed date
      AND lvorm EQ space.     " Deletion flag

  IF git_mara[] IS NOT INITIAL.
    SORT git_mara BY matnr.
**Get Valution details from MBEW table
    SELECT matnr
           bwkey " Valution Area
           verpr " Moving Avg price
           zkdat " Effective from date
      INTO TABLE git_mbew FROM mbew FOR ALL ENTRIES IN git_mara
      WHERE matnr EQ git_mara-matnr
        AND verpr GT 0.

    IF git_mbew[] IS NOT INITIAL.
** Get Marc data
      SELECT matnr werks plifz herkl FROM marc INTO TABLE git_marc FOR ALL ENTRIES IN git_mbew
        WHERE matnr EQ git_mbew-matnr
          AND werks EQ git_mbew-bwkey
          AND lvorm EQ space.
      SORT git_marc BY matnr werks.

      it_mbew[] = git_mbew[].
      SORT it_mbew BY bwkey.
      DELETE ADJACENT DUPLICATES FROM it_mbew COMPARING bwkey.
**      Get Company code for each plant
      IF it_mbew[] IS NOT INITIAL.
        SELECT bwkey
               bukrs "Company code
          INTO TABLE git_t001k FROM t001k FOR ALL ENTRIES IN it_mbew
          WHERE bwkey EQ it_mbew-bwkey.
      ENDIF.
**      Get Currency for Companycode
      IF git_t001k[] IS NOT INITIAL.
        SELECT bukrs
               waers  "Currency key
          INTO CORRESPONDING FIELDS OF TABLE git_t001 FROM t001 FOR ALL ENTRIES IN git_t001k
          WHERE bukrs = git_t001k-bukrs
           AND  spras EQ 'EN'.
      ENDIF.

    ENDIF.

** Get material descriptions
    SELECT matnr maktx maktg FROM makt INTO TABLE git_makt FOR ALL ENTRIES IN git_mara
      WHERE matnr EQ git_mara-matnr
        AND spras EQ 'EN'.
    SORT git_makt BY matnr.

  ENDIF.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_FILE
*&---------------------------------------------------------------------*
*       Prepare final internal table
*----------------------------------------------------------------------*
FORM f_prepare_file .

  LOOP AT git_mbew INTO gwa_mbew.
** Read Mara details
    READ TABLE git_mara INTO gwa_mara WITH KEY matnr = gwa_mbew-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gwa_final-matnr = gwa_mara-matnr.
      gwa_final-mfrnr = gwa_mara-mfrnr.
      gwa_final-mfrpn = gwa_mara-mfrpn.
      gwa_final-przus = gwa_mara-przus.
      CLEAR gwa_mara.
    ENDIF.

** Read descriptions
    READ TABLE git_makt INTO gwa_makt WITH KEY matnr = gwa_mbew-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gwa_final-maktx = gwa_makt-maktx.
      gwa_final-maktg = gwa_makt-maktg.
      CLEAR gwa_makt.
    ENDIF.

**Read plant level data
    READ TABLE git_marc INTO gwa_marc WITH KEY matnr = gwa_mbew-matnr
                                               werks = gwa_mbew-bwkey
                                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      gwa_final-plifz = gwa_marc-plifz. "Planned delivery
      gwa_final-herkl = gwa_marc-herkl. " Region
      CLEAR gwa_marc.
    ENDIF.

**    Read Currency key data
    READ TABLE git_t001k INTO gwa_t001k WITH KEY bwkey = gwa_mbew-bwkey.
    IF sy-subrc EQ 0.
      READ TABLE git_t001 INTO gwa_t001 WITH KEY bukrs = gwa_t001k-bukrs.

      IF sy-subrc EQ 0.
        gwa_final-stdcurr = gwa_t001-waers. " currency key
        CLEAR gwa_t001.
      ENDIF.
      CLEAR gwa_t001k.
    ENDIF.

    gwa_final-bunit   = gwa_mbew-bwkey. " valution Area
    gwa_final-stdcost = gwa_mbew-verpr. " Moving Avg.price

**     Effective from date  (YYYY-MM-DD)
    IF gwa_mbew-zkdat IS NOT INITIAL.
      CONCATENATE gwa_mbew-zkdat+0(4)
                  gwa_mbew-zkdat+4(2)
                  gwa_mbew-zkdat+6(2) INTO  gwa_final-stdcdat SEPARATED BY '-'.
    ELSE.
      gwa_final-stdcdat = ' '.
    ENDIF.

    APPEND gwa_final TO git_final.

    CLEAR: gwa_final, gwa_mbew.
  ENDLOOP.

  LOOP AT git_final INTO gwa_final.

    CONDENSE: gwa_final-matnr,
              gwa_final-bunit,
              gwa_final-maktx,
              gwa_final-maktg,
              gwa_final-stdcurr,
              gwa_final-stdcost,
              gwa_final-stdcdat,
              gwa_final-mfrnr,
              gwa_final-mfrpn,
              gwa_final-przus,
              gwa_final-plifz,
              gwa_final-herkl.

    IF gwa_final-matnr NE space.
      CONCATENATE '"' gwa_final-matnr '"' INTO gwa_final-matnr.
    ENDIF.

    IF gwa_final-bunit NE space. " Valution Area
      CONCATENATE '"' gwa_final-bunit '"' INTO gwa_final-bunit.
    ENDIF.

    IF gwa_final-maktx NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-maktx WITH '""'.
      CONCATENATE '"' gwa_final-maktx '"' INTO gwa_final-maktx.
    ENDIF.

    IF gwa_final-maktg NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-maktg WITH '""'.
      CONCATENATE '"' gwa_final-maktg '"' INTO gwa_final-maktg.
    ENDIF.

    IF gwa_final-stdcurr NE space.
      CONCATENATE '"' gwa_final-stdcurr '"' INTO gwa_final-stdcurr.
    ENDIF.

    IF gwa_final-stdcost NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-stdcost WITH '""'.
      CONCATENATE '"' gwa_final-stdcost '"' INTO gwa_final-stdcost.
    ENDIF.

    IF gwa_final-stdcdat NE space.
      CONCATENATE '"' gwa_final-stdcdat '"' INTO gwa_final-stdcdat.
    ENDIF.

    IF gwa_final-mfrnr NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-mfrnr WITH '""'.
      CONCATENATE '"' gwa_final-mfrnr '"' INTO gwa_final-mfrnr.
    ENDIF.

    IF gwa_final-mfrpn NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-mfrpn WITH '""'.
      CONCATENATE '"' gwa_final-mfrpn '"' INTO gwa_final-mfrpn.
    ENDIF.

    IF gwa_final-przus NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-przus WITH '""'.
      CONCATENATE '"' gwa_final-przus '"' INTO gwa_final-przus.
    ENDIF.

    IF gwa_final-plifz NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-plifz WITH '""'.
      CONCATENATE '"' gwa_final-plifz '"' INTO gwa_final-plifz.
    ENDIF.

    IF gwa_final-herkl NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-herkl WITH '""'.
      CONCATENATE '"' gwa_final-herkl '"' INTO gwa_final-herkl.
    ENDIF.

    CONCATENATE gwa_final-matnr
                gwa_final-bunit
                gwa_final-maktx
                gwa_final-maktg
                gwa_final-stdcost
                gwa_final-stdcurr
                gwa_final-stdcdat
                gwa_final-stk_ind
                gwa_final-mfrnr
                gwa_final-mfrpn
                gwa_final-plifz
                gwa_final-ff1
                gwa_final-ff2
                gwa_final-ff3
                gwa_final-pf1
                gwa_final-pf2
                gwa_final-pf3
                gwa_final-herkl
                gwa_final-przus
                gwa_final-fcu1
                gwa_final-fcu2
                gwa_final-fcu3
                INTO gwa_file-line SEPARATED BY ','.
    APPEND gwa_file TO git_file.
    CLEAR: gwa_final, gwa_file.

  ENDLOOP.

ENDFORM.                    " F_PREPARE_FILE

*&---------------------------------------------------------------------*
*&      Form  F_SEND_FILE
*&---------------------------------------------------------------------*
*       Send output CSV file
*----------------------------------------------------------------------*
FORM f_send_file USING p_name TYPE string.
  DATA: lv_fname  TYPE string,  "File name
        lv_ffname TYPE string,  "Full File name
        lv_msg         TYPE c,  "Error messages
        lv_date(10)    TYPE c,  " Preferred date formate
        lv_clnt        TYPE sy-mandt, " System client
        lv_sys         TYPE sy-sysid, " Systerm Name
        lv_name        TYPE string.   " Dynamic file name

  lv_name  = p_name. "file name
  lv_sys   = sy-sysid. " Logon System Name
  lv_clnt  = sy-mandt. " Lognon Client Name

** Date formate dd-mm-yyyy
  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4) INTO lv_date SEPARATED BY '-'.
**prepare file name
  CONCATENATE lv_sys lv_clnt lv_name lv_date '_' sy-uzeit '.CSV' INTO lv_fname.
  IF rb_appl IS NOT INITIAL
      AND p_fpath IS NOT INITIAL. " File download to Application server
** Combine path and file name
    CONCATENATE p_fpath lv_fname INTO lv_ffname.

**  Open Dataset to write data
    OPEN DATASET lv_ffname FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      LOOP AT git_file INTO gwa_file.
        TRANSFER gwa_file-line TO lv_ffname.
        CLEAR gwa_file.
      ENDLOOP.
      REFRESH git_file.
      gv_flag = 'X'.  " File download message flag
    ELSE.
**  Error handling here
      MESSAGE 'File open error' TYPE 'E'.
    ENDIF.
**  Close the Dataset
    CLOSE DATASET lv_ffname.
  ENDIF.

  IF rb_pres IS NOT INITIAL
    AND p_ppath IS NOT INITIAL. " File download to Presentation server
    CLEAR gv_flag.
** Combine path and file name
    CONCATENATE p_ppath lv_fname INTO lv_ffname.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_ffname
        filetype                = 'ASC'
      TABLES
        data_tab                = git_file
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
        OTHERS                  = 22.

    IF sy-subrc EQ 0.
      gv_flag = 'X'.
    ENDIF.
    REFRESH git_file.
  ENDIF.
ENDFORM.                    " F_SEND_FILE
