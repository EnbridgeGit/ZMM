FUNCTION zedi_lpath_create_client_docnm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATATYPE) LIKE  EDIPO-ACTRIG
*"     VALUE(DIRECTORY) LIKE  EDIPO-OUTPUTDIR
*"     VALUE(FILENAME) LIKE  EDIPO-OUTPUTFILE
*"     VALUE(CONTROL) LIKE  EDIDC STRUCTURE  EDIDC
*"  EXPORTING
*"     VALUE(PATHNAME) LIKE  EDI_PATH-PTHNAM
*"  EXCEPTIONS
*"      LOGICAL_PATH_ERROR
*"----------------------------------------------------------------------
  CONSTANTS: c_supdir TYPE edi_drctry VALUE 'COG_SUP_DEAL',
             c_tsddir TYPE edi_drctry VALUE 'COG_TS_DEAL',
             c_sup TYPE char8 VALUE 'SupplyD_',
             c_tsd TYPE char7 VALUE 'TSDeal_',
             c_dat TYPE char4 VALUE '.dat'.

  DATA: logical_path LIKE  filepath-pathintern.

  DATA: BEGIN OF path,
          directory LIKE edfil-directory,
          filename  LIKE edfil-filename,
        END OF path.

  logical_path = directory.
  SHIFT control-docnum LEFT DELETING LEADING '0'.
  IF logical_path = c_supdir.
    CONCATENATE c_sup control-docnum c_dat INTO path-filename.
  ELSEIF logical_path = c_tsddir.
    CONCATENATE c_tsd control-docnum c_dat INTO path-filename.
  ENDIF.

  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
*     CLIENT                     = SY-MANDT
      logical_path               = logical_path
*     OPERATING_SYSTEM           = SY-OPSYS
*     PARAMETER_1                = ' '
*     PARAMETER_2                = ' '
*     USE_BUFFER                 = ' '
      file_name                  = path-filename
*     USE_PRESENTATION_SERVER    = ' '
    IMPORTING
      file_name_with_path        = pathname
    EXCEPTIONS
      path_not_found             = 1
      missing_parameter          = 2
      operating_system_not_found = 3
      file_system_not_found      = 4
      OTHERS                     = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          RAISING logical_path_error.
  ENDIF.

*  move directory to path-directory.
*  condense path no-gaps.
*  move path to pathname .
ENDFUNCTION.
