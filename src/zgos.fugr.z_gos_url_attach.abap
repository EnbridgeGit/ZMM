FUNCTION z_gos_url_attach .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BO_TYPE) TYPE  BORIDENT-OBJTYPE DEFAULT 'BUS1001006'
*"     VALUE(IV_BO_ID) TYPE  BORIDENT-OBJKEY DEFAULT
*"       '000000000000112613'
*"     VALUE(IV_TITLE) TYPE  STRING DEFAULT 'TITLE'
*"     VALUE(IV_URL) TYPE  STRING DEFAULT 'HTTP://'
*"  EXCEPTIONS
*"      RELATION_CREATE_ERROR
*"      MATERIAL_NOT_FOUND
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
*  Author    : Brian Boundy                         SAP : East
*  Date      : March, 2007                Program Type : Function Module
*  Issue Log : TR835: SCE-MDM Project
*----------------------------------------------------------------------*
*  Title : Z_GOS_URL_ATTACH - Add a URL to a GOS object.
*----------------------------------------------------------------------*
*  Description:
*     - Function to be used in IDOC processing to add a url to a GOS
*       object.
*----------------------------------------------------------------------
* Changes:
* YYYY/MM/DD btboundy - TR### - DESCRIPTION
*----------------------------------------------------------------------*


  INCLUDE : <cntn01>.

  DATA: lv_docty    LIKE borident-objtype,
        lv_msgtyp   LIKE sofm-doctp,
        lv_reltyp   LIKE breltyp-reltype,
        lv_bapiret  LIKE bapireturn1,
        lv_matnum   LIKE bapimatall-material.

  TYPES: BEGIN OF ty_message_key,
        foltp TYPE so_fol_tp,
        folyr TYPE so_fol_yr,
        folno TYPE so_fol_no,
        doctp TYPE so_doc_tp,
        docyr TYPE so_doc_yr,
        docno TYPE so_doc_no,
        fortp TYPE so_for_tp,
        foryr TYPE so_for_yr,
        forno TYPE so_for_no,
       END OF ty_message_key.
  DATA : lv_message_key TYPE ty_message_key.
  DATA : lo_message     TYPE swc_object.
  DATA : lt_doc_content TYPE STANDARD TABLE OF soli-line
                             WITH HEADER LINE.


*----------------------------------------------------------------------*

* First derive the Attachment's ( MESSAGE )document type.
  lv_docty = 'MESSAGE'.
  lv_msgtyp = 'URL'.
  lv_reltyp = 'URL'.
*----------------------------------------------------------------*





* If the Buisness Object is a material check if it exists.
  IF iv_bo_type = 'BUS1001006'.

    lv_matnum = iv_bo_id.

    CALL FUNCTION 'BAPI_MATERIAL_EXISTENCECHECK'
      EXPORTING
        material      = lv_matnum
      IMPORTING
*        deletion_flag =
        return        = lv_bapiret.

    IF lv_bapiret-type = 'E'.
      RAISE material_not_found.
    ENDIF.

  ENDIF.

* Create an initial instance of BO 'MESSAGE' - to call the
* instance-independent method 'Create'.
  swc_create_object lo_message 'MESSAGE' lv_message_key.

* define container to pass the parameter values to the method call
* in next step.
  swc_container lt_message_container.

* Populate container with parameters for method
  swc_set_element lt_message_container 'DOCUMENTTITLE' iv_title.
  swc_set_element lt_message_container 'DOCUMENTLANGU' 'E'.
  swc_set_element lt_message_container 'NO_DIALOG'     'X'.
  swc_set_element lt_message_container 'DOCUMENTNAME'  lv_docty.
  swc_set_element lt_message_container 'DOCUMENTTYPE'  lv_msgtyp.


* In case of URLs..it should be concatenated with &KEY& in the begining.
  CONCATENATE '&KEY&' iv_url INTO lt_doc_content.
  APPEND lt_doc_content.

* 'DocumentContent' is a multi-line element ( itab ).
  swc_set_table lt_message_container 'DocumentContent' lt_doc_content.

* Size is required in case of File attachments
  DATA : lv_doc_size TYPE i.
  DATA : l_file_lines TYPE i.

  DESCRIBE TABLE lt_doc_content LINES l_file_lines.

  READ TABLE lt_doc_content INDEX l_file_lines.

  lv_doc_size = ( 255 * ( l_file_lines - 1 ) ) +
              STRLEN( lt_doc_content ).

  swc_set_element lt_message_container 'DOCUMENTSIZE'   lv_doc_size .

* Refresh to get the reference of create 'MESSAGE' object for attachment
  swc_refresh_object lo_message.
  swc_call_method lo_message 'CREATE' lt_message_container.

* Get Key of new object
  swc_get_object_key lo_message lv_message_key.

* Now we have attachment as a business object instance. We can now
* attach it to our main business object instance.

* Create main BO object_a
* data: LO_IS_OBJECT_A type SIBFLPORB.  "type SIBFLPORB is unknown, so I
  DATA: lo_is_object_a TYPE borident.

  lo_is_object_a-objkey = iv_bo_id.
  lo_is_object_a-objtype = iv_bo_type.
*  LO_IS_OBJECT_A-CATID  = 'BO'.

* Create attachment BO object_b
* data: LO_IS_OBJECT_B type SIBFLPORB.    "type SIBFLPORB is unknown
  DATA: lo_is_object_b TYPE borident.

  lo_is_object_b-objkey = lv_message_key.
  lo_is_object_b-objtype = lv_docty.
*  LO_IS_OBJECT_B-CATID  = 'BO'.

*TRY.
*CALL METHOD CL_BINARY_RELATION=&gtCREATE_LINK
*  EXPORTING
*    IS_OBJECT_A            = LO_IS_OBJECT_A
*    IS_OBJECT_B            = LO_IS_OBJECT_B
*    IP_RELTYPE             = P_RELTYP.

  CALL FUNCTION 'BINARY_RELATION_CREATE'
    EXPORTING
      obj_rolea    = lo_is_object_a
      obj_roleb    = lo_is_object_b
      relationtype = lv_reltyp
    EXCEPTIONS
      OTHERS       = 1.

* Check if everything OK
  IF sy-subrc <> 0.
    RAISE relation_create_error.
  ENDIF.


ENDFUNCTION.
