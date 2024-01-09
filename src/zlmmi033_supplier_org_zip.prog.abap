*&---------------------------------------------------------------------*
*& Report  ZLMMI033_SUPPLIER_ORG_ZIP
*&
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI033_SUPPLIER_ORG_ZIP                     *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 27, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program Zip the supplier Organization/ID Part *
*&                       data file and places the Outbound CSV files in*
*&                       SE FTP server. These files are used in        *
*&                       ARIBA Upstream for Supplier data update       *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : D30K925223                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*
REPORT  zlmmi033_supplier_org_zip.
DATA: lt_data TYPE TABLE OF x255,
      ls_data LIKE LINE OF lt_data,
      lo_zip    TYPE REF TO cl_abap_zip,
      lo_zipdel TYPE REF TO cl_abap_zip,
      lv_zip_content TYPE xstring ,
      lv_file_length TYPE i ,
      lv_content     TYPE xstring,
      lv_date(8)  TYPE c,
      lv_time     TYPE sy-uzeit,
      lv_timestamp TYPE string,
      lv_org      TYPE i, " Lines import from Memory
      lv_orgdel   TYPE i. " Lines import from Memory

DATA: lv_wpath TYPE rlgrap-filename,
      lv_rpath TYPE rlgrap-filename.

CONSTANTS: lc_zip(50)     TYPE c VALUE 'SupplierOrganization',
           lc_zipdel(50)  TYPE c VALUE 'SupplierOrganization_Delete',
           lc_org(50)     TYPE c VALUE 'SupplierOrganization.csv',
           lc_orgpart(50) TYPE c VALUE 'SupplierOrganizationOrganizationIDPart.csv',
           lc_orgdel(50)  TYPE c VALUE 'SupplierOrganization_Delete.csv',
           lc_orgpartdel(50) TYPE c VALUE 'SupplierOrganizationOrganizationIDPart_Delete.csv'.

**Read Memory values
*IMPORT gv_org     TO lv_org     FROM MEMORY ID 'GV_ORG'.
*IMPORT gv_orgdel  TO lv_orgdel  FROM MEMORY ID 'GV_ORGDEL'.
IMPORT p_fpath    TO lv_rpath   FROM MEMORY ID 'CSV_PATH'.
IMPORT p_zpath    TO lv_wpath   FROM MEMORY ID 'ZIP_PATH'.
**Clear Memory values
*FREE MEMORY ID 'GV_ORG'.
*FREE MEMORY ID 'GV_ORGDEL'.
FREE MEMORY ID 'CSV_PATH'.
FREE MEMORY ID 'ZIP_PATH'.

lv_time = sy-uzeit.
CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+0(4) INTO lv_date.  " Time stamp
CONCATENATE '_' lv_date '_' lv_time INTO lv_timestamp.

IF lv_rpath IS NOT INITIAL AND lv_wpath IS NOT INITIAL.
*  IF lv_org IS NOT INITIAL.
    PERFORM f_prepare_orgzip.      "SupplierOrganization.zip
*  ENDIF.
*  IF lv_orgdel IS NOT INITIAL.
    PERFORM f_prepare_orgdelzip.   "SupplierOrganization_Delete.zip
*  ENDIF.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_ORGZIP
*&---------------------------------------------------------------------*
*      SupplierOrganization.zip
*----------------------------------------------------------------------*
FORM f_prepare_orgzip .

  DATA: lv_so      TYPE string,
        lv_sopart  TYPE string,
        lv_sozip   TYPE string.

  CONCATENATE lv_rpath lc_org INTO lv_so.         " SupplierOrganization.csv
  CONCATENATE lv_rpath lc_orgpart INTO lv_sopart. " SupplierOrganizationIDPart.csv
  CONCATENATE lv_wpath lc_zip '_U' lv_timestamp '.zip' INTO lv_sozip.  " SupplierOrganization.zip

  CLEAR :lv_zip_content, lv_file_length.
  CREATE OBJECT lo_zip.

  CLEAR lv_content .
  OPEN DATASET lv_so FOR INPUT IN BINARY MODE.
  READ DATASET lv_so INTO lv_content .
  CLOSE DATASET lv_so.

  lo_zip->add( name = 'SupplierOrganization.csv'  content = lv_content ).

  CLEAR lv_content .

  OPEN DATASET lv_sopart FOR INPUT IN BINARY MODE.
  READ DATASET lv_sopart INTO lv_content .
  CLOSE DATASET lv_sopart.

  lo_zip->add( name = 'SupplierOrganizationOrganizationIDPart.csv' content = lv_content ).

  lv_zip_content = lo_zip->save( ).

* Conver the xstring content to binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_zip_content
    IMPORTING
      output_length = lv_file_length
    TABLES
      binary_tab    = lt_data.


  OPEN DATASET lv_sozip FOR OUTPUT IN BINARY MODE.
  LOOP AT lt_data INTO ls_data.
    TRANSFER ls_data TO lv_sozip.
  ENDLOOP.
  CLOSE DATASET lv_sozip.
  REFRESH lt_data. CLEAR ls_data.
ENDFORM.                    " F_PREPARE_ORGZIP

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_ORGDELZIP
*&---------------------------------------------------------------------*
*   SupplierOrganizationIDPart_Delete.zip
*----------------------------------------------------------------------*
FORM f_prepare_orgdelzip .
  DATA: lv_sodel      TYPE string,
        lv_sopartdel  TYPE string,
        lv_sodelzip   TYPE string.

  CONCATENATE lv_rpath lc_orgdel INTO lv_sodel.         " SupplierOrganization_Delete.csv
  CONCATENATE lv_rpath lc_orgpartdel INTO lv_sopartdel. " SupplierOrganizationIDPart_Delete.csv
  CONCATENATE lv_wpath lc_zipdel lv_timestamp '.zip' INTO lv_sodelzip.     " SupplierOrganizationIDPart_Delete.zip
  CLEAR :lv_zip_content, lv_file_length.
  CREATE OBJECT lo_zipdel.

  CLEAR lv_content .
  OPEN DATASET lv_sodel FOR INPUT IN BINARY MODE.
  READ DATASET lv_sodel INTO lv_content .
  CLOSE DATASET lv_sodel.

  lo_zipdel->add( name = 'SupplierOrganization_Delete.csv' content = lv_content ).

  CLEAR lv_content .

  OPEN DATASET lv_sopartdel FOR INPUT IN BINARY MODE.
  READ DATASET lv_sopartdel INTO lv_content .
  CLOSE DATASET lv_sopartdel.

  lo_zipdel->add( name = 'SupplierOrganizationOrganizationIDPart_Delete.csv' content = lv_content ).

  lv_zip_content = lo_zipdel->save( ).

* Conver the xstring content to binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_zip_content
    IMPORTING
      output_length = lv_file_length
    TABLES
      binary_tab    = lt_data.

  OPEN DATASET lv_sodelzip FOR OUTPUT IN BINARY MODE.
  LOOP AT lt_data INTO ls_data.
    TRANSFER ls_data TO lv_sodelzip.
  ENDLOOP.
  CLOSE DATASET lv_sodelzip.
  REFRESH lt_data. CLEAR ls_data.

ENDFORM.                    " F_PREPARE_ORGDELZIP
