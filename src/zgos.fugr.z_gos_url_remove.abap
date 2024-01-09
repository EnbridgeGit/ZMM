FUNCTION z_gos_url_remove .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BO_TYPE) TYPE  BORIDENT-OBJTYPE DEFAULT 'BUS1001006'
*"     VALUE(IV_BO_ID) TYPE  BORIDENT-OBJKEY DEFAULT
*"       '000000000000112613'
*"     VALUE(IV_TITLE) TYPE  STRING DEFAULT 'TITLE'
*"  EXCEPTIONS
*"      RELATION_CREATE_ERROR
*"      MATERIAL_NOT_FOUND
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
*  Author    : Brian Boundy                         SAP : East
*  Date      : March, 2007                Program Type : Function Module
*  Issue Log : TR835: SCE-MDM Project
*----------------------------------------------------------------------*
*  Title : Z_GOS_URL_REMOVE - Delete a URL from a GOS object.
*----------------------------------------------------------------------*
*  Description:
*     - Function to be used in IDOC processing to remove url from a GOS
*       object with a given TITLE.
*----------------------------------------------------------------------
* Changes:
* YYYY/MM/DD btboundy - TR### - DESCRIPTION
*----------------------------------------------------------------------*

  INCLUDE : <cntn01>.

  TYPES: BEGIN OF lty_message_key,
         instid_b LIKE srgbtbrel-instid_b,
         objtp    LIKE sood-objtp,
         objyr    LIKE sood-objyr,
         objno    LIKE sood-objno,
         objdes   LIKE sood-objdes,
         END OF lty_message_key.


  DATA: lv_docty        LIKE borident-objtype,
        lv_reltyp       LIKE breltyp-reltype,
        lv_bapiret      LIKE bapireturn1,
        lv_matnum       LIKE bapimatall-material,
        lv_message_key  LIKE srgbtbrel-instid_b,
        lt_message_keys TYPE TABLE OF lty_message_key,
        ls_message_keys LIKE LINE OF lt_message_keys.

**----------------------------------------------------------------------*
** First derive the Attachment's ( MESSAGE )document type.
  lv_docty = 'MESSAGE'.
  lv_reltyp = 'URL'.
**----------------------------------------------------------------*

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


  SELECT instid_b
    FROM srgbtbrel
    INTO CORRESPONDING FIELDS OF TABLE lt_message_keys
    WHERE instid_a = iv_bo_id
    AND   typeid_a = iv_bo_type.

  LOOP AT lt_message_keys INTO ls_message_keys.
    ls_message_keys-objtp = ls_message_keys-instid_b+17(3).
    ls_message_keys-objyr = ls_message_keys-instid_b+20(2).
    ls_message_keys-objno = ls_message_keys-instid_b+22(12).
    MODIFY lt_message_keys INDEX sy-tabix FROM ls_message_keys.
  ENDLOOP.

  LOOP AT lt_message_keys INTO ls_message_keys.
    SELECT SINGLE objdes
      FROM sood
      INTO ls_message_keys-objdes
      WHERE objtp = ls_message_keys-objtp
      AND   objyr = ls_message_keys-objyr
      AND   objno = ls_message_keys-objno.
    MODIFY lt_message_keys INDEX sy-tabix FROM ls_message_keys.
  ENDLOOP.

  LOOP AT lt_message_keys INTO ls_message_keys.
    IF ls_message_keys-objdes <> iv_title.
      DELETE lt_message_keys INDEX sy-tabix.
    ENDIF.
  ENDLOOP.





*Using iv_bo_id, and iv_bo_type find list of lv_message_key from SRGBTBREL-INSTID_B
*CONCAT the list and use it to look up SOOD information to match IV_TITLE



* Create main BO object_a
* data: LO_IS_OBJECT_A type SIBFLPORB.  "type SIBFLPORB is unknown, so I
  DATA: lo_is_object_a TYPE borident.

  lo_is_object_a-objkey = iv_bo_id.
  lo_is_object_a-objtype = iv_bo_type.
*  LO_IS_OBJECT_A-CATID  = 'BO'.

* Create attachment BO object_b
* data: LO_IS_OBJECT_B type SIBFLPORB.    "type SIBFLPORB is unknown
  DATA: lo_is_object_b TYPE borident.

  LOOP AT lt_message_keys INTO ls_message_keys.
    lv_message_key = ls_message_keys-instid_b.
    lo_is_object_b-objkey = lv_message_key.
    lo_is_object_b-objtype = lv_docty.
*   LO_IS_OBJECT_B-CATID  = 'BO'.

    CALL FUNCTION 'BINARY_RELATION_DELETE'
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

  ENDLOOP.

ENDFUNCTION.
