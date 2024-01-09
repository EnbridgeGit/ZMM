*----------------------------------------------------------------------*
***INCLUDE ZXM06F01 .
*----------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM06F01                                                 *
*  Func Grp:  XM06                                                     *
*  Author:    Brian Boundy                                             *
*  Date:      December 01, 2010                                        *
*  Track #:   TR872 Release 1                                          *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MM06E001 - user exits for EDI purchasing docs      *
*                                                                      *
*     This is a user exit within the framework of outbound IDoc        *
*     processing for purchasing documents.                             *
*                                                                      *
************************************************************************
*------------------------ CHANGE LOG ----------------------------------*
*  Date     TR # By      Description                                   *
* --------- ---- ------- --------------------------------------------- *
* 01-Dec-10 0872 BTBOUND D30K915866 - ARIBA R1 - Initial development   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    :  004                                                 *
* Date          :  04-Jun-2014                                         *
* Modified By   :  JRHARTUNG                                           *
* Correction No :  D30K923669                                          *
* Description   :  Create segment Z1ARBA1 and populate with            *
*                  references to PO item attachments                   *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_po_attachments
*&---------------------------------------------------------------------*
*       Get the PO attachments
*----------------------------------------------------------------------*
FORM f_get_po_attachments
  TABLES   cit_z1arba1            TYPE ty_it_z1arba1
  USING    iv_ebeln               TYPE ebeln
           iv_ebelp               TYPE ebelp.

  DATA:    lwa_z1arba1            TYPE ty_wa_z1arba1,
           lit_z1arba1            TYPE ty_it_z1arba1,
           lwa_return             TYPE bapiret2,
           lwa_documentlist       TYPE bapi_doc_keys,
           lit_documentlist       TYPE STANDARD TABLE OF bapi_doc_keys,
           lwa_documentfiles      TYPE bapi_doc_files2,
           lit_documentfiles      TYPE STANDARD TABLE
                                    OF bapi_doc_files2.

  DATA:    lv_objecttype          TYPE dokob,
           lv_objectkey           TYPE objky,
           lv_zinterface_ver      TYPE scms_crvht,
           lv_zcontent_rep        TYPE scms_crep.

  CLEAR    cit_z1arba1[].

  IF     ( iv_ebeln IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_z1arba1[].

* Read the interface version value
  CLEAR                                     gwa_zvar.
  READ     TABLE git_zvar              INTO gwa_zvar
                                   WITH KEY varname = 'SCMS_CRVHT'.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_zinterface_ver.
  MOVE     gwa_zvar-value1               TO lv_zinterface_ver.

* Read the content repository value
  CLEAR                                     gwa_zvar.
  READ     TABLE git_zvar              INTO gwa_zvar
                                   WITH KEY varname = 'SCMS_CREP'.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_zcontent_rep.
  MOVE     gwa_zvar-value1               TO lv_zcontent_rep.

*eject
* Get the PO header attachments
  IF     ( iv_ebelp IS INITIAL ).

* Get the PO item attachements
  ELSE.

* Get the document objects
    CLEAR                                   lv_objecttype.
    MOVE     'EKPO'                      TO lv_objecttype.
    CLEAR                                   lv_objectkey.
    CONCATENATE                             iv_ebeln
                                            iv_ebelp
                                       INTO lv_objectkey.

    CLEAR    lwa_return.
    CLEAR    lit_documentlist[].

    CALL FUNCTION 'BAPI_DOCUMENT_GETOBJECTDOCS'
      EXPORTING
        OBJECTTYPE          = lv_objecttype
        OBJECTKEY           = lv_objectkey
*       CURRENTVERSIONSONLY =
*       DATE                =
      IMPORTING
        RETURN              = lwa_return
      TABLES
        DOCUMENTLIST        = lit_documentlist.

  ENDIF.

  IF     ( lwa_return-type CA 'EA' ).
    RETURN.
  ENDIF.

  IF     ( lit_documentlist[] IS INITIAL ).
    RETURN.
  ENDIF.

*eject
* Get the document files
  CLEAR                                     lwa_documentlist.
  LOOP AT  lit_documentlist            INTO lwa_documentlist.

    CLEAR          gwa_zvar.
    READ     TABLE git_zvar
              INTO gwa_zvar
          WITH KEY varname = 'DOCUMENTTYPE'
                   value1  = lwa_documentlist-documenttype.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lwa_documentlist.
      CONTINUE.
    ENDIF.

    CLEAR    lwa_return.
    CLEAR    lit_documentfiles[].

    CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
      EXPORTING
        DOCUMENTTYPE         = lwa_documentlist-documenttype
        DOCUMENTNUMBER       = lwa_documentlist-documentnumber
        DOCUMENTPART         = lwa_documentlist-documentpart
        DOCUMENTVERSION      = lwa_documentlist-documentversion
        GETDOCFILES          = 'X'
      IMPORTING
*       DOCUMENTDATA         =
        RETURN               = lwa_return
      TABLES
*       OBJECTLINKS          =
*       DOCUMENTDESCRIPTIONS =
*       LONGTEXTS            =
*       STATUSLOG            =
        DOCUMENTFILES        = lit_documentfiles
*       COMPONENTS           =
*       CHARACTERISTICVALUES =
*       CLASSALLOCATIONS     =
*       DOCUMENTSTRUCTURE    =
*       WHEREUSEDLIST        =
      .

    IF     ( lwa_return-type CA 'EA' ).
      CLEAR    lwa_documentlist.
      CONTINUE.
    ENDIF.

    IF     ( lit_documentfiles[] IS INITIAL ).
      CLEAR    lwa_documentlist.
      CONTINUE.
    ENDIF.

*eject
    CLEAR                                   lwa_documentfiles.
    LOOP AT  lit_documentfiles         INTO lwa_documentfiles.

      CLEAR          gwa_zvar.
      READ     TABLE git_zvar
                INTO gwa_zvar
            WITH KEY varname = 'STORAGECATEGORY'
                     value1  = lwa_documentfiles-storagecategory.
      IF     ( sy-subrc NE 0 ).
        CLEAR  lwa_documentfiles.
        CONTINUE.
      ENDIF.

      CLEAR                                 lwa_z1arba1.
      MOVE     lv_zinterface_ver         TO lwa_z1arba1-zinterface_ver.
      MOVE     lv_zcontent_rep           TO lwa_z1arba1-zcontent_rep.
      MOVE     lwa_documentfiles-file_id TO lwa_z1arba1-zdoc_phys.
      MOVE     lwa_documentfiles-docfile TO lwa_z1arba1-zdoc_orig.
      APPEND                                lwa_z1arba1
                                         TO lit_z1arba1.

      CLEAR  lwa_documentfiles.
    ENDLOOP.

    CLEAR  lwa_documentlist.
  ENDLOOP.

  IF     ( lit_z1arba1[] IS INITIAL ).
    RETURN.
  ENDIF.

  cit_z1arba1[] = lit_z1arba1[].

ENDFORM.                    " f_get_po_attachments
