FUNCTION z_idoc_input_gos_url .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"     VALUE(NO_APPLICATION_LOG) TYPE  SY-DATAR OPTIONAL
*"     VALUE(MASSAVEINFOS) TYPE  MASSSAVINF OPTIONAL
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWF_PARAM-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWF_PARAM-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"----------------------------------------------------------------------


*----------------------------------------------------------------------*
*  Author    : Brian Boundy                         SAP : East
*  Date      : March, 2007                Program Type : Function Module
*  Issue Log : TR835: SCE-MDM Project
*----------------------------------------------------------------------*
*  Title : Z_IDOC_INPUT_GOS_URL - Process inbound IDOC ZGOSURL01
*----------------------------------------------------------------------*
*  Description:
*     - ABAP code to process message type ZGOSURL and idoc ZGOSURL01.
*----------------------------------------------------------------------
* Changes:
* YYYY/MM/DD btboundy - TR### - DESCRIPTION
*----------------------------------------------------------------------*


* Include File containing ALE constants
  INCLUDE mbdconwf.


***Data
  DATA : lv_bo_type   LIKE borident-objtype,
         lv_bo_id     LIKE borident-objkey,
         lv_title     TYPE string,
         lv_url       TYPE string,
         lv_errors   TYPE integer,
         lt_z1gosurl  TYPE TABLE OF z1gosurl,
         ls_z1gosurl  LIKE LINE OF lt_z1gosurl.



***********************************************************************
  LOOP AT idoc_contrl.


*   CHECK TO ENSURE PROPER MESSAGE TYPE
    IF idoc_contrl-mestyp NE 'ZGOSURL_MSG'.
      idoc_status-docnum = idoc_contrl-docnum.
      idoc_status-status = '51'.
      idoc_status-msgty = 'E'.
      idoc_status-msgid = 'ZMDM'.
      idoc_status-msgno = '002'.
      idoc_status-msgv1 = 'Wrong Message Type: Use Message Type ZGOSURL_MSG.'.
      APPEND idoc_status.
      CLEAR idoc_status.
      return_variables-wf_param = 'Error_Idocs'.
      return_variables-doc_number = idoc_contrl-docnum.
      APPEND return_variables.
      CLEAR return_variables.
      EXIT.
    ENDIF.


    CLEAR ls_z1gosurl.
    lv_errors = 0.


*   LOOP AND FILL INTERNAL TABLE WITH IDOC INFO.
    LOOP AT idoc_data WHERE docnum EQ idoc_contrl-docnum.
      CASE idoc_data-segnam.

          "Read in the segment and test it, if any fail raise exception
        WHEN 'Z1GOSURL'.
          ls_z1gosurl = idoc_data-sdata.
          APPEND ls_z1gosurl TO lt_z1gosurl.
      ENDCASE.
    ENDLOOP.

*   MAKE SURE DELETES ARE PROCESSED FIRST!!
    SORT lt_z1gosurl BY msgfn ASCENDING.

*   LOOP AND ATTEMPT TO PROCESS
    LOOP AT lt_z1gosurl INTO ls_z1gosurl.


      lv_bo_type = ls_z1gosurl-bo_type.
      lv_bo_id = ls_z1gosurl-bo_id.
      lv_title = ls_z1gosurl-title.
      lv_url = ls_z1gosurl-url.

      IF ls_z1gosurl-msgfn = '003'.
        CALL FUNCTION 'Z_GOS_URL_REMOVE'
          EXPORTING
            iv_bo_type            = lv_bo_type
            iv_bo_id              = lv_bo_id
            iv_title              = lv_title
          EXCEPTIONS
            relation_create_error = 1
            material_not_found    = 2
            OTHERS                = 3.
        IF sy-subrc = 1 OR sy-subrc = 3.
          idoc_status-docnum = idoc_contrl-docnum.
          idoc_status-status = '52'.
          idoc_status-msgty = 'I'.
          idoc_status-msgid = 'ZMDM'.
          idoc_status-msgno = '001'.
          idoc_status-msgv1 = 'Relation could not be created.'.
          idoc_status-msgv2 = lv_bo_id.
          idoc_status-msgv3 = lv_title.
          APPEND idoc_status.
          CLEAR idoc_status.
          ADD 1 TO lv_errors.
        ELSEIF sy-subrc = 2.
          idoc_status-docnum = idoc_contrl-docnum.
          idoc_status-status = '52'.
          idoc_status-msgty = 'I'.
          idoc_status-msgid = 'ZMDM'.
          idoc_status-msgno = '001'.
          idoc_status-msgv1 = 'Material could not be found'.
          idoc_status-msgv2 = lv_bo_id.
          idoc_status-msgv3 = lv_title.
          APPEND idoc_status.
          CLEAR idoc_status.
          ADD 1 TO lv_errors.
        ENDIF.

      ELSEIF ls_z1gosurl-msgfn = '009'.

        CALL FUNCTION 'Z_GOS_URL_ATTACH'
          EXPORTING
            iv_bo_type            = lv_bo_type
            iv_bo_id              = lv_bo_id
            iv_title              = lv_title
            iv_url                = lv_url
          EXCEPTIONS
            relation_create_error = 1
            material_not_found    = 2
            OTHERS                = 3.

        IF sy-subrc = 1 OR sy-subrc = 3.
          idoc_status-docnum = idoc_contrl-docnum.
          idoc_status-status = '52'.
          idoc_status-msgty = 'I'.
          idoc_status-msgid = 'ZMDM'.
          idoc_status-msgno = '001'.
          idoc_status-msgv1 = 'Relation could not be created.'.
          idoc_status-msgv2 = lv_bo_id.
          idoc_status-msgv3 = lv_title.
          APPEND idoc_status.
          CLEAR idoc_status.
          ADD 1 TO lv_errors.
        ELSEIF sy-subrc = 2.
          idoc_status-docnum = idoc_contrl-docnum.
          idoc_status-status = '52'.
          idoc_status-msgty = 'I'.
          idoc_status-msgid = 'ZMDM'.
          idoc_status-msgno = '001'.
          idoc_status-msgv1 = 'Material could not be found'.
          idoc_status-msgv2 = lv_bo_id.
          idoc_status-msgv3 = lv_title.
          APPEND idoc_status.
          CLEAR idoc_status.
          ADD 1 TO lv_errors.
        ENDIF.

      ELSE.
        idoc_status-docnum = idoc_contrl-docnum.
        idoc_status-status = '52'.
        idoc_status-msgty = 'I'.
        idoc_status-msgid = 'ZMDM'.
        idoc_status-msgno = '001'.
        idoc_status-msgv1 = 'Wrong Control Code'.
        idoc_status-msgv2 = lv_bo_id.
        idoc_status-msgv3 = lv_title.
        APPEND idoc_status.
        CLEAR idoc_status.
        ADD 1 TO lv_errors.
      ENDIF.
    ENDLOOP.



*   CHECK FOR ERRORS AND EXIT.
    IF lv_errors > 0.
      ROLLBACK WORK.
      idoc_status-docnum = idoc_contrl-docnum.
      idoc_status-status = '51'.
      idoc_status-msgty = 'E'.
      idoc_status-msgid = 'ZMDM'.
      idoc_status-msgno = '002'.
      idoc_status-msgv1 = 'Errors were encounted in the IDOC'.
      APPEND idoc_status.
      CLEAR idoc_status.
      return_variables-wf_param = 'Error_Idocs'.
      return_variables-doc_number = idoc_contrl-docnum.
      APPEND return_variables.
      CLEAR return_variables.
      EXIT.
    ELSE.
      COMMIT WORK.
      idoc_status-docnum = idoc_contrl-docnum.
      idoc_status-status = '53'.
      idoc_status-msgty = 'I'.
      idoc_status-msgid = 'ZMDM'.
      idoc_status-msgno = '000'.
      APPEND idoc_status.
      CLEAR idoc_status.
    ENDIF.



  ENDLOOP.
ENDFUNCTION.
