*----------------------------------------------------------------------*
*       CLASS ZLMM_INVOICE_ATTACH_DWNLD DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zlmm_invoice_attach_dwnld DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZLMM_INVOICE_ATTACH_DWNLD
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF gty_rbkp,
         belnr TYPE re_belnr,
         gjahr TYPE gjahr,
         objkey TYPE sibfboriid,
      END OF gty_rbkp .
    TYPES:
      tt_rbkp TYPE STANDARD TABLE OF gty_rbkp INITIAL SIZE 0 .

    DATA:
      gt_rbkp TYPE STANDARD TABLE OF gty_rbkp INITIAL SIZE 0 .
    DATA wa_rbkp TYPE gty_rbkp .
    CONSTANTS:gc_set   TYPE flag VALUE 'X',                 "#EC NOTEXT
              gc_slash TYPE flag VALUE '\',
              gc_dot   TYPE flag VALUE '.',
              gc_dash  TYPE flag VALUE '_'.
    METHODS validations
      IMPORTING
        !invoices TYPE idfiwt_t_belnr
        !fiscal_yr TYPE gjahr
      EXPORTING
        !return_code TYPE subrc .
    METHODS dwnld_attachments
      IMPORTING
        !filepath TYPE localfile .
    METHODS get_archive_attachment
      IMPORTING
        !it_rbkp TYPE tt_rbkp
        !filepath TYPE localfile .
    METHODS get_nonarchive_attachment
      IMPORTING
        !filepath TYPE localfile
        !it_rbkp TYPE tt_rbkp .
protected section.
*"* protected components of class ZLMM_INVOICE_ATTACH_DWNLD
*"* do not include other source files here!!!
private section.
*"* private components of class ZLMM_INVOICE_ATTACH_DWNLD
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZLMM_INVOICE_ATTACH_DWNLD IMPLEMENTATION.


METHOD dwnld_attachments.
  DATA:lv_filepath TYPE localfile.

*Get archived attachments as PDF files and download the
*same in PC
  IF filepath IS NOT INITIAL.
    lv_filepath = filepath.
    CALL METHOD me->get_archive_attachment
      EXPORTING
        it_rbkp  = gt_rbkp
        filepath = lv_filepath.
*Get non-archived attachments as PDF files and download the
*same in PC
    CALL METHOD me->get_nonarchive_attachment
      EXPORTING
        filepath = lv_filepath
        it_rbkp  = gt_rbkp.
  ENDIF.
ENDMETHOD.


METHOD get_archive_attachment.
  TYPES: BEGIN OF lty_inv_link,
                instid_a       TYPE sibfboriid,
                instid_b       TYPE sibfboriid,
                has_attachment TYPE flag,
             END OF lty_inv_link,
             BEGIN OF lty_sood,
              objtp    TYPE so_obj_tp,
              objyr    TYPE so_obj_yr,
              objno    TYPE so_obj_no,
              objdes   TYPE so_obj_des,
              file_ext TYPE so_fileext,
              objlen   TYPE so_obj_len,
              extct    TYPE so_extct,
            END OF lty_sood,
            BEGIN OF lty_docid,
              docid   TYPE sofolenti1-doc_id,
              invoice TYPE re_belnr,
            END OF lty_docid.
  DATA: lt_inv_link TYPE STANDARD TABLE OF lty_inv_link INITIAL SIZE 0,
        lt_sood     TYPE STANDARD TABLE OF lty_sood     INITIAL SIZE 0,
        lt_objcont  TYPE STANDARD TABLE OF soli         INITIAL SIZE 0.
  DATA: lt_object_header    TYPE STANDARD TABLE OF  solisti1   INITIAL SIZE 0,
        lt_object_content   TYPE STANDARD TABLE OF  solisti1   INITIAL SIZE 0,
        lt_object_para      TYPE STANDARD TABLE OF  soparai1   INITIAL SIZE 0,
        lt_object_parb      TYPE STANDARD TABLE OF  soparbi1   INITIAL SIZE 0,
        lt_attachment_list  TYPE STANDARD TABLE OF  soattlsti1 INITIAL SIZE 0,
        lt_receiver_list    TYPE STANDARD TABLE OF  soreclsti1 INITIAL SIZE 0,
        lt_contents_hex     TYPE STANDARD TABLE OF  solix      INITIAL SIZE 0,
        lt_docid            TYPE STANDARD TABLE OF lty_docid   INITIAL SIZE 0.
  DATA:lwa_inv_link       TYPE lty_inv_link,
       lwa_document_data  TYPE sofolenti1,
       lwa_object_content TYPE solisti1,
       lwa_docid          TYPE lty_docid,
       lwa_objcont        TYPE soli,
       lwa_object_header  TYPE solisti1.
  DATA: lv_index       TYPE sy-tabix,
        lv_file        TYPE string,
        lv_datfilename TYPE string,
        lv_result      TYPE abap_bool,
        lv_path(255),
        lv_str1(255),
        lv_str2(255),
        lv_type(3).
  FIELD-SYMBOLS:<lfs_sood>     TYPE lty_sood,
                <lfs_rbkp>     TYPE gty_rbkp,
                <lfs_inv_link> TYPE lty_inv_link.
  LOOP AT gt_rbkp ASSIGNING <lfs_rbkp>.
    CONCATENATE <lfs_rbkp>-belnr <lfs_rbkp>-gjahr INTO <lfs_rbkp>-objkey.
    CONDENSE <lfs_rbkp>-objkey.
  ENDLOOP.
  IF gt_rbkp IS NOT INITIAL.
    " Retrieve list of Archived SAP Attachment for invoice
    SELECT instid_a instid_b
     INTO CORRESPONDING FIELDS OF TABLE lt_inv_link
     PACKAGE SIZE 5000
     FROM srgbtbrel
     FOR ALL ENTRIES IN gt_rbkp
     WHERE instid_a EQ gt_rbkp-objkey
       AND reltype  = 'ATTA'
       AND typeid_a = 'BUS2081'
       AND typeid_b = 'MESSAGE'.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      SORT lt_inv_link BY instid_a.
    ENDIF.
  ENDIF.
  " Filter out the attachment info for which invoice number is not among the ones entered
  LOOP AT gt_rbkp INTO wa_rbkp.
    READ TABLE lt_inv_link INTO lwa_inv_link
                           WITH KEY instid_a = wa_rbkp-objkey
                           BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CLEAR lv_index.
      lv_index = sy-tabix.
      LOOP AT lt_inv_link ASSIGNING <lfs_inv_link> FROM lv_index.
        IF <lfs_inv_link>-instid_a NE wa_rbkp-objkey.
          EXIT. "parallel cursor exit criteria
        ENDIF.
        <lfs_inv_link>-has_attachment = gc_set.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  DELETE lt_inv_link WHERE has_attachment IS INITIAL.
  IF lt_inv_link IS NOT INITIAL.
    SORT lt_inv_link BY instid_b.
    DELETE ADJACENT DUPLICATES FROM lt_inv_link COMPARING instid_b.
  ENDIF.
  IF lt_inv_link IS NOT INITIAL.
    " Retrieve File Attributs of SAP Attachments
    SELECT objtp
           objyr
           objno
           objdes
           file_ext
           objlen extct
      FROM sood
      INTO TABLE lt_sood
      PACKAGE SIZE 5000
      FOR ALL ENTRIES IN lt_inv_link
      WHERE objtp = lt_inv_link-instid_b+17(3)
      AND   objyr = lt_inv_link-instid_b+20(2)
      AND   objno = lt_inv_link-instid_b+22(12).
    ENDSELECT.
    CHECK lt_sood IS NOT INITIAL.
    SORT lt_sood BY objtp objyr objno.
    DELETE ADJACENT DUPLICATES FROM lt_sood COMPARING objtp objyr objno.
  ENDIF.
  LOOP AT gt_rbkp INTO wa_rbkp.
    READ TABLE lt_inv_link INTO lwa_inv_link
                           WITH KEY instid_a = wa_rbkp-objkey
                           BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CLEAR lv_index.
      lv_index = sy-tabix.
      LOOP AT lt_inv_link ASSIGNING <lfs_inv_link> FROM lv_index.
        IF <lfs_inv_link>-instid_a NE wa_rbkp-objkey.
          EXIT. "parallel cursor exit criteria
        ENDIF.
        LOOP AT lt_sood ASSIGNING <lfs_sood>
          WHERE objlen GT 0
          AND  objtp  = <lfs_inv_link>-instid_b+17(3)
          AND  objyr  = <lfs_inv_link>-instid_b+20(2)
          AND  objno  = <lfs_inv_link>-instid_b+22(12)." Check VBELN
          lwa_docid-docid   = <lfs_inv_link>-instid_b.
          lwa_docid-invoice = wa_rbkp-belnr.
          APPEND lwa_docid TO lt_docid.
          CLEAR lwa_docid.
          EXIT.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_docid INTO lwa_docid.
*Get file data
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = lwa_docid-docid
      IMPORTING
        document_data              = lwa_document_data
      TABLES
        object_header              = lt_object_header
        object_content             = lt_object_content
        object_para                = lt_object_para
        object_parb                = lt_object_parb
        attachment_list            = lt_attachment_list
        receiver_list              = lt_receiver_list
        contents_hex               = lt_contents_hex
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.
    IF sy-subrc = 0.
* Implement suitable error handling here
      LOOP AT lt_object_content INTO lwa_object_content.
        lwa_objcont-line = lwa_object_content-line.
        APPEND lwa_objcont TO lt_objcont.
        CLEAR lwa_objcont.
      ENDLOOP.
*Build file names and file type
      LOOP AT lt_object_header INTO lwa_object_header.
        IF lwa_object_header-line CS 'SO_FILENAME'.
          SPLIT lwa_object_header-line AT '=' INTO lv_str1 lv_str2.
          CONCATENATE filepath
                      gc_slash
                      text-t01
                      gc_dash
                      lwa_docid-invoice
                      text-t02
                      gc_slash
                      lv_str2
                 INTO lv_file.
        ELSEIF lwa_object_header-line CS 'SO_FORMAT'.
          SPLIT lwa_object_header-line AT '=' INTO lv_str1 lv_str2.
          lv_type = lv_str2.
        ENDIF.
        CLEAR lwa_object_header.
      ENDLOOP.
    ENDIF.

*Check if the filepath already exists
    lv_datfilename = lv_file.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = lv_datfilename
      RECEIVING
        result               = lv_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    IF lv_result IS INITIAL.
    ENDIF.
    lv_path = lv_file.
    " Download SAP Attachment
    CALL FUNCTION 'SO_OBJECT_DOWNLOAD'
      EXPORTING        "bin_filesize     = lv_bin_filesize
        filetype         = lv_type
        path_and_file    = lv_path
        no_dialog        = gc_set
      TABLES
        objcont          = lt_objcont
      EXCEPTIONS
        file_write_error = 1
        invalid_type     = 2
        x_error          = 3
        kpro_error       = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4.
      CONTINUE.
    ENDIF.
    REFRESH:lt_objcont,lt_inv_link,lt_object_header, lt_object_content, lt_object_para,
            lt_object_parb, lt_attachment_list,lt_receiver_list,lt_contents_hex.
  ENDLOOP.
ENDMETHOD.


METHOD get_nonarchive_attachment.
  TYPES:BEGIN OF lty_archive_link,
             belnr      TYPE re_belnr,
             archiv_id  TYPE saearchivi,
             arc_doc_id TYPE saeardoid,
             ar_object  TYPE saeobjart,
             reserve    TYPE saereserve,
             filedesc   TYPE saearoname,
             END OF lty_archive_link,
             BEGIN OF lty_toasp,
               ar_object  TYPE saeobjart,
               language   TYPE spras,
               objecttext TYPE saearoname,
               END OF lty_toasp.
  DATA lwa_rbkp TYPE gty_rbkp.
  DATA: lv_objectkey         TYPE saeobjid,
        lv_length            TYPE i,
        lv_count             TYPE i,
        lv_cnt               TYPE i,
        lv_archived_filename TYPE char255.
  DATA:lt_connect_info  TYPE STANDARD TABLE OF toav0 INITIAL SIZE 0,
       lwa_connect_info TYPE toav0,
       lt_archive_link  TYPE STANDARD TABLE OF lty_archive_link INITIAL SIZE 0,
       lwa_archive_link TYPE lty_archive_link,
       lt_archive_temp  TYPE STANDARD TABLE OF lty_archive_link INITIAL SIZE 0,
       lt_toasp         TYPE STANDARD TABLE OF lty_toasp INITIAL SIZE 0,
       lwa_toasp        TYPE lty_toasp,
       lt_tbl1024 TYPE STANDARD TABLE OF tbl1024 INITIAL SIZE 0.
  FIELD-SYMBOLS:<lfs_archive_link> TYPE lty_archive_link.
*Get non archived data
  LOOP AT gt_rbkp INTO lwa_rbkp.
    CONCATENATE lwa_rbkp-belnr lwa_rbkp-gjahr INTO lv_objectkey.
    CONDENSE lv_objectkey.
    CALL FUNCTION 'ARCHIV_CONNECTINFO_GET_META'
      EXPORTING
        object_id             = lv_objectkey
        sap_object            = 'BUS2081'
      IMPORTING
        count                 = lv_count
      TABLES
        connect_info          = lt_connect_info
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.
    IF sy-subrc = 0.
* Implement suitable error handling here
      IF lv_count GE 1.
        LOOP AT lt_connect_info INTO lwa_connect_info.
          lwa_archive_link-belnr          = lwa_rbkp-belnr.
          lwa_archive_link-archiv_id      = lwa_connect_info-archiv_id.
          lwa_archive_link-arc_doc_id     = lwa_connect_info-arc_doc_id.
          lwa_archive_link-ar_object      = lwa_connect_info-ar_object.
          lwa_archive_link-reserve        = lwa_connect_info-reserve.
          APPEND lwa_archive_link TO lt_archive_link.
          CLEAR:lwa_connect_info,lwa_archive_link.
        ENDLOOP.
      ENDIF.
    ENDIF.
    CLEAR:lv_objectkey,lv_count.
    REFRESH lt_connect_info.
  ENDLOOP.
  IF lt_archive_link IS NOT INITIAL.
    lt_archive_temp[] = lt_archive_link[].
    DELETE ADJACENT DUPLICATES FROM lt_archive_temp COMPARING ar_object.
    IF lt_archive_temp IS NOT INITIAL.
      SELECT ar_object language objecttext
        FROM toasp
        INTO TABLE lt_toasp
        FOR ALL ENTRIES IN lt_archive_temp
        WHERE ar_object = lt_archive_temp-ar_object
          AND language = sy-langu.
      IF sy-subrc IS INITIAL.
        SORT lt_toasp BY ar_object.
        LOOP AT lt_archive_link ASSIGNING <lfs_archive_link>.
          READ TABLE lt_toasp INTO lwa_toasp
                              WITH KEY ar_object = <lfs_archive_link>-ar_object
                              BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <lfs_archive_link>-filedesc = lwa_toasp-objecttext.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  LOOP AT lt_archive_link INTO lwa_archive_link.
    lv_cnt = lv_cnt + 1.
    CLEAR: lv_archived_filename, lv_length, lt_tbl1024.
    CONCATENATE filepath
                gc_slash
                text-t01
                gc_dash
                lwa_archive_link-belnr
                text-t02
                gc_slash
                lwa_archive_link-filedesc
                gc_dot
                lwa_archive_link-reserve
           INTO lv_archived_filename.
    CONDENSE lv_archived_filename NO-GAPS.
    " Replace unsupported caracters in windows filename
    "REPLACE ALL OCCURRENCES OF '/' IN lv_file WITH '_'. " for example
    CALL FUNCTION 'SCMS_AO_TABLE_GET'
      EXPORTING
        arc_id       = lwa_archive_link-archiv_id
        doc_id       = lwa_archive_link-arc_doc_id
      IMPORTING
        length       = lv_length
      TABLES
        data         = lt_tbl1024
      EXCEPTIONS
        error_http   = 1
        error_archiv = 2
        error_kernel = 3
        error_config = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'SCMS_DOWNLOAD'
      EXPORTING
        filename = lv_archived_filename "'C:\DATA\DOCUMENTUM\....
        filesize = lv_length
      TABLES
        data     = lt_tbl1024
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    CLEAR :lwa_archive_link.
  ENDLOOP.
ENDMETHOD.


METHOD validations.
  IF fiscal_yr IS NOT INITIAL.
    SELECT belnr gjahr
      FROM rbkp
      INTO TABLE gt_rbkp
      WHERE belnr IN invoices
        AND gjahr = fiscal_yr.
  ELSE.
    SELECT belnr gjahr
      FROM rbkp
      INTO TABLE gt_rbkp
      WHERE belnr IN invoices.
  ENDIF.
  return_code = sy-subrc.
ENDMETHOD.
ENDCLASS.
