FUNCTION ZLMMI038_OPLK_PRICING_INDEX.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TAB_ZEMA_SAP STRUCTURE  ZMMS_OPLK_PRICING_INDEX
*"  EXCEPTIONS
*"      FILE_NAME
*"      UNABLE_WRITE
*"      UNABLE_CLOSE
*"      UNABLE_OPEN_JOB
*"      UNABLE_CLOSE_JOB
*"      UNABLE_PROG
*"      NO_DATA
*"----------------------------------------------------------------------
* Interface to get Pricing Index from Openlink (Copy of ZEMA Interface)

  TYPES: BEGIN OF ty_output,
          kschl TYPE kschl, "application
          ekorg TYPE ekorg, "Purchasing Organization
          inco1 TYPE inco1, "Inco Terms
          kbetr	TYPE kbetr, "Amount
          konwa	TYPE konwa, "Rate Unit / Currency
          kpein TYPE kpein, "condition Pricing Unit
          kmein(3), " type KMEIN, "Condition unit
          datab TYPE datab, "Validity Start date
         END OF ty_output.
  TYPES: BEGIN OF ty_output_text,
          kschl(4),
          ekorg(4),
          inco1(3),
          kbetr(13),
          konwa(5),
          kpein(5),
          kmein(3),
          datab(10),
         END OF ty_output_text.
  TYPES: BEGIN OF ty_output_header,
          kschl(5),
          ekorg(5),
          inco1(8),
          kbetr(8),
          konwa(8),
          kpein(8),
          kmein(8),
          datab(8),
         END OF ty_output_header.
  DATA: lt_output TYPE TABLE OF ty_output,
        ls_output LIKE LINE OF lt_output,
        ls_output_text TYPE ty_output_text,
        ls_zema_sap TYPE zmm_zema_sap,
        ls_zsap_map_zema TYPE zsap_map_zema,
        lt_zsap_map_zema TYPE TABLE OF zsap_map_zema,
        lv_string  TYPE string,
        ls_output_header TYPE ty_output_header.
************************************************************************
*  Variables for file output
************************************************************************
  DATA: lv_file LIKE filenameci-fileintern VALUE
                     'Z_ZEMA_INTERFACE'.
  DATA: lv_log_path LIKE filepath-pathintern, "used to determine log path
        lv_part_file TYPE string, " file physical path
        lv_part_file_f LIKE sapb-sappfad.
  DATA: lv_variant TYPE raldb-variant VALUE 'LOAD_ALL'.
**************************** variables for bakcground job
  DATA: number           TYPE tbtcjob-jobcount,
        name             TYPE tbtcjob-jobname VALUE 'Z_ZEMA_LOAD',
        print_parameters TYPE pri_params.
******************************************
  ls_output_header-kschl = 'KSCHL'.
  ls_output_header-ekorg = 'EKORG'.
  ls_output_header-inco1 = 'INCO1_01'.
  ls_output_header-kbetr = 'KBETR_01'.
  ls_output_header-konwa = 'KONWA_01'.
  ls_output_header-kpein = 'KPEIN_01'.
  ls_output_header-kmein = 'KMEIN_01'.
  ls_output_header-datab = 'DATAB_01'.
* Start of COG Changes
* Get Inco term from BODS and skip mapping table
*  SELECT * FROM zsap_map_zema INTO TABLE lt_zsap_map_zema.
*  SORT tab_zema_sap BY price_date zema_index.
*  LOOP AT tab_zema_sap INTO ls_zema_sap.
*    READ TABLE lt_zsap_map_zema INTO ls_zsap_map_zema
*         WITH KEY zema_index = ls_zema_sap-zema_index.
*    CHECK sy-subrc = 0.
*    ls_output-kschl = 'MP01'.
*    ls_output-ekorg = 'GASA'.
*    ls_output-inco1 = ls_zsap_map_zema-inco1.
*    ls_output-kbetr = ls_zema_sap-price.
*    ls_output-konwa = ls_zema_sap-currency.
*    ls_output-kpein = 1000. "ls_zema_sap-KPEIN.
*    ls_output-kmein = ls_zema_sap-uom.
*    ls_output-datab = ls_zema_sap-price_date.
*    APPEND ls_output TO lt_output.
*  ENDLOOP.
  LOOP AT tab_zema_sap INTO ls_zema_sap.
    ls_output-kschl = 'MP01'.
    ls_output-ekorg = 'GASA'.
    ls_output-inco1 = ls_zema_sap-zema_index.
    ls_output-kbetr = ls_zema_sap-price.
    ls_output-konwa = ls_zema_sap-currency.
    ls_output-kpein = 1000. "ls_zema_sap-KPEIN.
    ls_output-kmein = ls_zema_sap-uom.
    ls_output-datab = ls_zema_sap-price_date.
    APPEND ls_output TO lt_output.
  ENDLOOP.
*End of COG Change
  IF lt_output IS INITIAL.
    RAISE no_data.
  ENDIF.
*data: lv_check(1).
*check lv_check = 'X'.
************* CREATE OUTPUT FILE *****************************
* Get File name for to be used for FTP
**************************************************************
  CALL FUNCTION 'FILE_GET_NAME_AND_LOGICAL_PATH'
    EXPORTING
      client                     = sy-mandt
      logical_filename           = lv_file
      operating_system           = sy-opsys
    IMPORTING
      file_name                  = lv_file
      logical_path               = lv_log_path
    EXCEPTIONS
      file_not_found             = 1
      operating_system_not_found = 2
      file_system_not_found      = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
*   Logical Path invalid - &.
    RAISE file_name.
  ENDIF.

  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      logical_path               = lv_log_path
      file_name                  = lv_file
    IMPORTING
      file_name_with_path        = lv_part_file
    EXCEPTIONS
      path_not_found             = 1
      missing_parameter          = 2
      operating_system_not_found = 3
      file_system_not_found      = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
*   Logical File invalid - &.
    RAISE file_name.
  ENDIF.
*************************Write to file***********************
  OPEN DATASET lv_part_file FOR OUTPUT
      IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
*   Error opening file &.
    RAISE unable_write.
  ENDIF.
  CONCATENATE
          ls_output_header-kschl
          ls_output_header-ekorg
          ls_output_header-inco1
          ls_output_header-kbetr
          ls_output_header-konwa
          ls_output_header-kpein
          ls_output_header-kmein
          ls_output_header-datab
     INTO lv_string
     SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
*       Transfer data to file on application server
  TRANSFER lv_string TO lv_part_file.
  LOOP AT lt_output INTO ls_output.
    ls_output_text-kschl = ls_output-kschl.
    ls_output_text-ekorg = ls_output-ekorg.
    ls_output_text-inco1 = ls_output-inco1.
*             ls_output_text-KBETR = ls_output-KBETR.
    WRITE ls_output-kbetr TO ls_output_text-kbetr.
    CONDENSE ls_output_text-kbetr NO-GAPS.
    SHIFT ls_output_text-kbetr RIGHT.
    ls_output_text-konwa = ls_output-konwa.
    ls_output_text-kpein = ls_output-kpein.
    ls_output_text-kmein = ls_output-kmein.
    ls_output_text-datab = ls_output-datab.
    CONCATENATE
          ls_output_text-kschl
          ls_output_text-ekorg
          ls_output_text-inco1
          ls_output_text-kbetr
          ls_output_text-konwa
          ls_output_text-kpein
          ls_output_text-kmein
          ls_output_text-datab
     INTO lv_string
     SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
*       Transfer data to file on application server
    TRANSFER lv_string TO lv_part_file.
  ENDLOOP.
  CLOSE DATASET lv_part_file.
  IF sy-subrc <> 0.
    RAISE unable_close.
*   Data export aborted: error opening file &
  ENDIF.
*    data: lv_check(1).
*check lv_check = 'X'.

**************************************Trigger LSMW Background Job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = name
    IMPORTING
      jobcount         = number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc = 0.
*  SUBMIT submitable TO SAP-SPOOL
*                    SPOOL PARAMETERS print_parameters
*                    WITHOUT SPOOL DYNPRO
*                    VIA JOB name NUMBER number
*                    AND RETURN.

    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
*     ARCPARAMS                         =
        authcknam                         = sy-uname
*     COMMANDNAME                       = ' '
*     OPERATINGSYSTEM                   = ' '
*     EXTPGM_NAME                       = ' '
*     EXTPGM_PARAM                      = ' '
*     EXTPGM_SET_TRACE_ON               = ' '
*     EXTPGM_STDERR_IN_JOBLOG           = 'X'
*     EXTPGM_STDOUT_IN_JOBLOG           = 'X'
*     EXTPGM_SYSTEM                     = ' '
*     EXTPGM_RFCDEST                    = ' '
*     EXTPGM_WAIT_FOR_TERMINATION       = 'X'
        jobcount                          = number
        jobname                           = name
*     LANGUAGE                          = SY-LANGU
*     PRIPARAMS                         = ' '
       report                             =  '/SAPDMC/SAP_LSMW_INTERFACE'
      variant                           = lv_variant
*   IMPORTING
*     STEP_NUMBER                       =
*   EXCEPTIONS
*     BAD_PRIPARAMS                     = 1
*     BAD_XPGFLAGS                      = 2
*     INVALID_JOBDATA                   = 3
*     JOBNAME_MISSING                   = 4
*     JOB_NOTEX                         = 5
*     JOB_SUBMIT_FAILED                 = 6
*     LOCK_FAILED                       = 7
*     PROGRAM_MISSING                   = 8
*     PROG_ABAP_AND_EXTPG_SET           = 9
*     OTHERS                            = 10
              .

    IF sy-subrc = 0.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = number
          jobname              = name
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.
      IF sy-subrc <> 0.
        RAISE unable_close_job.
      ENDIF.
    ELSE.
      RAISE unable_prog.
    ENDIF.
  ELSE.
    RAISE unable_open_job.
  ENDIF.

*}   INSERT
ENDFUNCTION.
