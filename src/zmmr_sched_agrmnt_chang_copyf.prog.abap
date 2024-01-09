*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHANG_COPYF
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHANG_FCAT
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Program Name       :  ZMMR_SCHED_AGRMNT_CHNG_HISTORY                 *
*& Include            :  ZMMR_SCHED_AGRMNT_CHNAG_FCAT                   *
*& Author             :  Sudheer Kumar Chinnam                          *
*& Creation Date      :  20/05/2021                                     *
*& Application Area   :  SCM                                            *
*& Description        :  By based on EKKO,CDHDR and CDPOS tables        *
*&                       CDHDR values are passed to the function module *
*&                       CHANGEDOCUMENT_READ_POSITIONS and to get the   *
*&                       Scheduling agreemnt history records            *
*&--------------------------------------------------------------------- *
*&----------------------------------------------------------------------*
*&                      Modification Log                                *
*&                                                                      *
*& Changed On   Changed By  CTS        Description                      *
*& ---------------------------------------------------------------------*
*& 05-Mar-2018  KUMARCHS   D30K930966  CHG0212611 : Scheduling Agreement*
*&                                     Change History Report            *
*&----------------------------------------------------------------------*

************Fieldcatalog Design*****************
  ls_fcat-fieldname = 'OBJID'.
  ls_fcat-seltext_l = 'Scheduling Agreement'.
  ls_fcat-COL_POS = '1'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'USERNAME'.
  ls_fcat-seltext_l = 'User ID'.
  ls_fcat-COL_POS = '2'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'NAME1'.
  ls_fcat-seltext_l = 'First Name'.
  ls_fcat-COL_POS = '3'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'NAME2'.
  ls_fcat-seltext_l = 'Last Name'.
  ls_fcat-COL_POS = '4'.
*ls_fcat-EMPHASIZE = 'C510'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'USDATE'.
  ls_fcat-seltext_l = 'Release Date'.
  ls_fcat-COL_POS = '5'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.


  ls_fcat-fieldname = 'USTIME'.
  ls_fcat-seltext_l = 'Release Time'.
  ls_fcat-COL_POS = '6'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'TCODE'.
  ls_fcat-seltext_l = 'T Code'.
  ls_fcat-COL_POS = '7'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'UDATE'.
  ls_fcat-seltext_l = 'Change Date'.
  ls_fcat-COL_POS = '8'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'UTIME'.
  ls_fcat-seltext_l = 'Change Time'.
  ls_fcat-COL_POS = '9'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'TABKEY'.
  ls_fcat-seltext_l = 'Table Key'.
  ls_fcat-COL_POS = '10'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'CHANGENR'.
  ls_fcat-seltext_l = 'Change number'.
  ls_fcat-COL_POS = '11'.
  ls_fcat-NO_OUT = 'X'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'CHNGIND'.
  ls_fcat-seltext_l = 'Change Id'.
  ls_fcat-COL_POS = '12'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'TABNAME'.
  ls_fcat-seltext_l = 'Table'.
  ls_fcat-COL_POS = '13'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'FNAME'.
  ls_fcat-seltext_l = 'Field'.
  ls_fcat-COL_POS = '14'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

  ls_fcat-fieldname = 'FTEXT'.
  ls_fcat-seltext_l = 'Field Description'.
  ls_fcat-COL_POS = '15'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.


  ls_fcat-fieldname = 'F_OLD'.
  ls_fcat-seltext_l = 'Old Value'.
  ls_fcat-COL_POS = '16'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.


  ls_fcat-fieldname = 'F_NEW'.
  ls_fcat-seltext_l = 'New Value'.
  ls_fcat-COL_POS = '17'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.



  ls_fcat-fieldname = 'TEXT_CASE'.
  ls_fcat-seltext_l = 'Text Case name'.
  ls_fcat-COL_POS = '18'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.


  ls_fcat-fieldname = 'TEXTART'.
  ls_fcat-seltext_l = 'Text Type'.
  ls_fcat-COL_POS = '19'.
  append ls_fcat to lt_fcat.
  clear:ls_fcat.

*******************Field catalog Design *************************8
