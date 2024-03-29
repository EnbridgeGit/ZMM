*&---------------------------------------------------------------------*
*& Report  ZTEST_IDOCFILE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTEST_IDOCFILE line-size 275.
************************************************************************
* This tool reads an existing Idoc and dispays the contents in a       *
* spreadsheet format. The spreadsheet (MS-EXCEL) will be automatically *
* created if D_EXCEL = 'X'.                                            *
************************************************************************

data: idoc_control like EDIDC,
      NUMBER_OF_DATA_RECORDS like sy-dbcnt,
      NUMBER_OF_STATUS_RECORDS like sy-dbcnt,
      INT_EDIDS like edids occurs 0 with header line,
      INT_EDIDD like edidd occurs 0 with header line.

TYPE-POOLS :  LEDID.

data: STRUCT_TYPE TYPE  LEDID_STRUCT_TYPE ,
      IDOC_STRUCT TYPE  LEDID_T_IDOC_STRUCT,
      SEGMENTS TYPE  LEDID_T_SEGMENT,
      SEGMENT_STRUCT TYPE  LEDID_T_SEGMENT_STRUCT,
      excel_tab(2000) occurs 0 with header line.

parameter: DOCNUM like edidc-docnum obligatory, ""Idoc Number
           sap_rel like SY-SAPRL default SY-SAPRL," obligatory,
           pi_ver like EDI_VERREC-VERSION default '3'," obligatory,
           d_excel as checkbox default 'X'. ""Download ?

start-of-selection.
  perform read_idoc.
  perform process_idoc.
  if d_excel = 'X'.
    perform download_to_excel.
  endif.

end-of-selection.

FORM read_idoc.
  CALL FUNCTION 'IDOC_READ_COMPLETELY'
       EXPORTING
            DOCUMENT_NUMBER          = docnum
       IMPORTING
            IDOC_CONTROL             = idoc_control
            NUMBER_OF_DATA_RECORDS   = NUMBER_OF_DATA_RECORDS
            NUMBER_OF_STATUS_RECORDS = NUMBER_OF_STATUS_RECORDS
       TABLES
            INT_EDIDS                = INT_EDIDS
            INT_EDIDD                = INT_EDIDD
       EXCEPTIONS
            DOCUMENT_NOT_EXIST       = 1
            DOCUMENT_NUMBER_INVALID  = 2
            OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "" read_idoc

FORM process_idoc.
  perform read_idoc_structure.
  perform display_data_records.
ENDFORM.                    "" process_idoc

FORM display_data_records.

  data: PE_seg_HEADER like EDI_SAPI01,
        segname like EDI_IAPI12-SEGMENTTYP,
        prev_segname like EDI_IAPI12-SEGMENTTYP value ' ',
        pt_fields2 like EDI_IAPI12 occurs 0 with header line,
        PT_FVALUES2 like EDI_IAPI14 occurs 0 with header line,
        byte_first type i,
        byte_last type i,
        field_val(50),
        tmp_str(15),
        tmp_str3(15),
        seg_repeats type i value 0,
        tmp_str2(15),
        tab_cr(2) type C value CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
        tot_ctr type i value 0,
        ctr type i value 0,
        msg(40) type c.

  data: IDOC_STRUCT_wa TYPE  LEDID_IDOC_STRUCT.

  sort int_edidd by segnum.
  describe table int_edidd lines tot_ctr.
  loop at int_edidd.
    move int_edidd-segnam to segname.
    clear msg.
    concatenate 'Reading segment ' segname
                into msg separated by space.
    if tot_ctr <> 0.
      ctr = ( 100 * sy-tabix ) / tot_ctr.
    endif.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = ctr
              TEXT       = msg.
    add 1 to seg_repeats.
    clear tmp_str2.
    if int_edidd-segnam <> prev_segname.
      seg_repeats = 1.
      clear: pe_seg_header, pt_fields2, pt_fvalues2.
      refresh: pt_fields2, pt_fvalues2.
      CALL FUNCTION 'SEGMENT_READ_COMPLETE'
           EXPORTING
                PI_SEGTYP                 = segname
                PI_RELEASE                = sap_rel
                PI_VERSION                = pi_ver
           IMPORTING
                PE_HEADER                 = pe_seg_header
           TABLES
                PT_FIELDS                 = pt_fields2
                PT_FVALUES                = pt_fvalues2
           EXCEPTIONS
                SEGMENT_UNKNOWN           = 1
                SEGMENT_STRUCTURE_UNKNOWN = 2
                OTHERS                    = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      prev_segname = int_edidd-segnam.
    endif.
    read table idoc_struct into idoc_struct_wa with key
                           segment_type = int_edidd-segnam.
    if sy-subrc = 0.
      IF IDOC_STRUCT_WA-SYNTAX_ATTRIB-MUSTFL = 'X'.
        TMP_STR = 'Mandatory'.                  ""Mandatory
      ELSE.
        TMP_STR = 'Optional'.                  ""Optional
      ENDIF.
      if IDOC_STRUCT_wa-SEGMENT_TYPE_ATTRIB-QUALIFIER = 'X'.
        tmp_str3 = 'Qualified'.
      else.
        tmp_str3 = 'Non-Qualified'.
      endif.
      shift IDOC_STRUCT_wa-SYNTAX_ATTRIB-OCCMAX
                                 left deleting leading '0'.
      move seg_repeats to tmp_str2.
      condense: IDOC_STRUCT_wa-SYNTAX_ATTRIB-OCCMAX, tmp_str2.
      concatenate tmp_str2 'of'  IDOC_STRUCT_wa-SYNTAX_ATTRIB-OCCMAX
          into tmp_str2 separated by space.

      write :/ IDOC_STRUCT_wa-SEGMENT_TYPE,
           tmp_str,
           TMP_STR3,
           tmp_str2,
           IDOC_STRUCT_wa-SYNTAX_ATTRIB-HLEVEL,
           IDOC_STRUCT_wa-SEGMENT_TYPE_ATTRIB-plast,
           IDOC_STRUCT_wa-SEGMENT_TYPE_ATTRIB-DESCRP.
      if d_excel = 'X'.
        concatenate 'Segment Name' tab_cr
                    'Mand / Opt ' tab_cr
                    'Qual / non-Qual' tab_cr
                    'Seq of Max' tab_cr
                    'Level' tab_cr
                    'Owner' tab_cr
                    'Description'
                    into excel_tab.
        append excel_tab.
        concatenate IDOC_STRUCT_wa-SEGMENT_TYPE tab_cr
              tmp_str tab_cr
              TMP_STR3 tab_cr
              tmp_str2 tab_cr
              IDOC_STRUCT_wa-SYNTAX_ATTRIB-HLEVEL tab_cr
              IDOC_STRUCT_wa-SEGMENT_TYPE_ATTRIB-plast tab_cr
              IDOC_STRUCT_wa-SEGMENT_TYPE_ATTRIB-DESCRP
              into excel_tab.
        append excel_tab.
        concatenate tab_cr
                    'Field Nma' tab_cr
                    'Type' tab_cr
                    'Length' tab_cr
                    'Byte From' tab_cr
                    'Byte To' tab_cr
                    'Description' tab_cr
                    'Value' tab_cr
                    'Qualifier Meaning'
                    into excel_tab.
        append excel_tab.
      endif.
    endif.
    sort pt_fields2 by field_pos.
    byte_first = 0.
    loop at pt_fields2.
      clear: field_val.
      byte_last = pt_fields2-EXTLEN.
      write int_edidd-sdata+byte_first(byte_last) to
            field_val left-justified.
      shift pt_fields2-EXTLEN left deleting leading '0'.
      shift pt_fields2-byte_first left deleting leading '0'.
      shift pt_fields2-byte_last left deleting leading '0'.
      write:/ '   ', pt_fields2-fieldname,
              pt_fields2-datatype,
              pt_fields2-EXTLEN,
              pt_fields2-byte_first ,
              pt_fields2-byte_last,
              pt_fields2-descrp,
              field_val.
      read table pt_fvalues2 with key fieldname = pt_fields2-fieldname
                    fldvalue_l = field_val.
      add byte_last to byte_first.
      if sy-subrc = 0.
        write : pt_fvalues2-descrp.
      else.
        clear pt_fvalues2-descrp.
      endif.
      if d_excel = 'X'.
        concatenate tab_cr pt_fields2-fieldname tab_cr
                pt_fields2-datatype tab_cr
                pt_fields2-EXTLEN tab_cr
                pt_fields2-byte_first tab_cr
                pt_fields2-byte_last tab_cr
                pt_fields2-descrp tab_cr
                field_val tab_cr
                pt_fvalues2-descrp
                into excel_tab.
        append excel_tab.
      endif.
    endloop.
  endloop.
ENDFORM.                    "" display_data_records

FORM read_idoc_structure.
  data: idoctype type LEDID_IDOCTYPE.

  if not idoc_control-cimtyp is initial.
    STRUCT_TYPE = 'E'. ""Extended
    idoctype = idoc_control-cimtyp.
  else.
    STRUCT_TYPE = 'B'. ""Basic
    idoctype = idoc_control-idoctp.
  endif.

  CALL FUNCTION 'IDOC_TYPE_COMPLETE_READ'
       EXPORTING
            RELEASE              = sap_rel
            STRUCT_TYPE          = STRUCT_TYPE
            IDOCTYPE             = idoctype
            VERSION              = pi_ver
*       IMPORTING
*            IDOC_TYPE            = idoctype
       TABLES
            IDOC_STRUCT          = idoc_struct
            SEGMENTS             = segments
            SEGMENT_STRUCT       = segment_struct
       EXCEPTIONS
            IDOCTYPE_UNKNOWN     = 1
            IDOCSTRUCT_UNKNOWN   = 2
            SEGMENT_DATA_MISSING = 3
            ILLEGAL_STRUCT_TYPE  = 4
            OTHERS               = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "" read_idoc_structure

FORM download_to_excel.
  data: name type string value '\\Client\C$\Users\nagirir\Desktop\file1.xls'." RLGRAP-FILENAME.

*  shift docnum left deleting leading '0'.
*  concatenate docnum '-' idoc_control-idoctp '.xls'
*              into name.

CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
*   BIN_FILESIZE                    =
    FILENAME                        = name
   FILETYPE                        = 'ASC'
*   APPEND                          = ' '
   WRITE_FIELD_SEPARATOR           = 'X'
* IMPORTING
*   FILELENGTH                      =
  TABLES
    DATA_TAB                        = excel_tab
*   FIELDNAMES                      =
 EXCEPTIONS
   FILE_WRITE_ERROR                = 1
   NO_BATCH                        = 2
   GUI_REFUSE_FILETRANSFER         = 3
   INVALID_TYPE                    = 4
   NO_AUTHORITY                    = 5
   UNKNOWN_ERROR                   = 6
   HEADER_NOT_ALLOWED              = 7
   SEPARATOR_NOT_ALLOWED           = 8
   FILESIZE_NOT_ALLOWED            = 9
   HEADER_TOO_LONG                 = 10
   DP_ERROR_CREATE                 = 11
   DP_ERROR_SEND                   = 12
   DP_ERROR_WRITE                  = 13
   UNKNOWN_DP_ERROR                = 14
   ACCESS_DENIED                   = 15
   DP_OUT_OF_MEMORY                = 16
   DISK_FULL                       = 17
   DP_TIMEOUT                      = 18
   FILE_NOT_FOUND                  = 19
   DATAPROVIDER_EXCEPTION          = 20
   CONTROL_FLUSH_ERROR             = 21
   OTHERS                          = 22
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "" download_to_excel
