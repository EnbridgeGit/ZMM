*&---------------------------------------------------------------------*
*&  Include           ZLMMI033_SUPPLIER_ORG_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI033_SUPPLIER_ORG_TOP                     *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 09, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  TOP Include - Outbound - Supplier Data        *
*&                       For Ariba Upstream Integration                *
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

TABLES : lfa1,         " Vendor Master general data
         lfb1,         " Vendor Master Company data
         lfm1.

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:BEGIN OF ty_lfa1,
      lifnr TYPE lfa1-lifnr,   "SupplierId
      name1 TYPE lfa1-name1,   "SupplierName
      stras TYPE lfa1-stras,   "StreetAddress
      ort01 TYPE lfa1-ort01,   " City
      regio TYPE lfa1-regio,   " State
      land1 TYPE lfa1-land1,   " Country
      pstlz TYPE lfa1-pstlz,   " Postal Code
      telf1 TYPE lfa1-telf1,   " Telephone Number
      telfx TYPE lfa1-telfx,   " Fax Number
      stceg TYPE lfa1-stceg,   " VAT Registrastion ID
      adrnr TYPE lfa1-adrnr,   " Address Number
      stcd1 TYPE lfa1-stcd1,   " Tax Number1
      END OF ty_lfa1.

TYPES:BEGIN OF ty_adr6,
      addrnumber TYPE adr6-addrnumber, " Address Nubmer
      smtp_addr   TYPE adr6-smtp_addr, " Email address
      END OF ty_adr6.

TYPES :BEGIN OF ty_lfb1,
       lifnr TYPE lfa1-lifnr,   "SupplierId
       bukrs TYPE lfb1-bukrs,   "Company code
      END OF ty_lfb1.
TYPES :BEGIN OF ty_lfm1,
        lifnr TYPE lfm1-lifnr,   "SupplierId
        ekorg TYPE lfm1-ekorg,   "Purchase.Org
       END OF ty_lfm1.

**Finla table structure
TYPES:BEGIN OF ty_final,
      taxid(100) TYPE c,   "TAXID "BussinessUnit_SystemID_lfa1-stcd1
      stin(20)  TYPE c,    "StateTIN
      rtin(20)  TYPE c,    "RegionalTIN
      plun(20)  TYPE c,    "PreferredLanguage.UniqueName
      lifnr(25) TYPE c,    "SystemID
      stceg(30) TYPE c,    "VatID
      armax(20) TYPE c,    "AnnualRevenueMaximum.Amount
      armaxcu(20) TYPE c,  "AnnualRevenueMaximum.Currency.UniqueName
      armin(20) TYPE c,    "AnnualRevenueMinimum.Amount
      armincu(20) TYPE c,  "AnnualRevenueMinimum.Currency.UniqueName
      ort01(50) TYPE c,    "City
      land1(40) TYPE c,    "Country
      stras(50) TYPE c,    "StreetAddress
      pstlz(30) TYPE c,    "PostalCode
      regio(30) TYPE c,    "State
      bukrs(25) TYPE c,    "CorporateAddress.UniqueName
      smtp_addr(241) TYPE c, "ContactEmail
      telfx(35) TYPE c,    "FaxNumber
      telf1(35) TYPE c,    "ContactPhoneNumber
      curl(50)  TYPE c,    " Corporate URL
      hsr(20)   TYPE c,    "HasSyncRelationship
      htr(20)   TYPE c,    "HasTradingRelationship
      iscust(20) TYPE c,   "ISCustomer
      ismng(20) TYPE c,    "ISManaged
      isorga(20) TYPE c,   "IsOrgApproved
      issup(20) TYPE c,    "IsSupplier
      name1(100) TYPE c,    "SupplierName
      noe(30)    TYPE c,   "NumberOfEmployees
      orgtyp(20) TYPE c,   "OrganizationType
      pcun(20)   TYPE c,   "PreferredCurrency.UniqueName
      stoi(20)   TYPE c,   "StateOfIncorporation
      yfound(20) TYPE c,   "YearFounded
     END OF ty_final.

TYPES:BEGIN OF ty_final1,
      domain(20) TYPE c,   "Domain
      value(30)  TYPE c,   "Value
      psys(30)   TYPE c,   "Parent.SystemID
  END OF ty_final1.

TYPES: BEGIN OF ty_file,
       line TYPE string,  "record in a file
       END OF ty_file.

**  Program last executed time
TYPES: BEGIN OF ty_zprog,
       zclnt        TYPE mandt,
       zprog        TYPE zprog_name,      " Program Name
       zexec_date   TYPE zdate,           "Last Execution date
       zexec_time   TYPE ztime,           "Time of Execution
       END OF ty_zprog.

** CDPOS structure
TYPES: BEGIN OF ty_cdpos,
       objectid LIKE cdpos-objectid,
       lifnr    LIKE lfa1-lifnr,
       changenr LIKE cdpos-changenr,
       fname    LIKE cdpos-fname,
       value_new LIKE cdpos-value_new,
       END OF ty_cdpos.

** CDHDR structure
TYPES: BEGIN OF ty_cdhdr,
       objectid LIKE cdhdr-objectid,
       changenr LIKE cdhdr-changenr,
       udate    LIKE cdhdr-udate,
       tcode    LIKE cdhdr-tcode,
       END OF ty_cdhdr.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: git_lfa1 TYPE STANDARD TABLE OF ty_lfa1, " Internal Table for structure ty_lfa1
      gwa_lfa1 TYPE ty_lfa1,                   "  Work Area for git_lfa1
      git_adr6 TYPE STANDARD TABLE OF ty_adr6, " Internal Table for structure ty_adr6
      gwa_adr6 TYPE ty_adr6,                   " Work Area for git_adr6
      git_final TYPE STANDARD TABLE OF ty_final, "Internal Table for structure ty_final
      gwa_final TYPE ty_final,                 " Work Area for git_final
      git_file TYPE STANDARD TABLE OF ty_file, "  Internal Table for structure ty_file
      gwa_file TYPE ty_file,                   " Work Area for git_file
      git_lfb1 TYPE STANDARD TABLE OF ty_lfb1, "  Internal Table for structure ty_lfb1
      gwa_lfb1 TYPE ty_lfb1,                   " Work Area git_lfb1
      git_lfm1 TYPE STANDARD TABLE OF ty_lfm1, "  Internal Table for structure ty_lfm1
      gwa_lfm1 TYPE ty_lfm1,                   " Work Area git_lfm1
      git_final1 TYPE STANDARD TABLE OF ty_final1, " Internal table for structure ty_final1
      gwa_final1 TYPE ty_final1,                " Work Area for git_final1
      git_zprog TYPE STANDARD TABLE OF ty_zprog,"  Internal Table for structure ty_zprog
      gwa_zprog TYPE ty_zprog,                  " Work Area for git_zprog
      git_cdhdr TYPE STANDARD TABLE OF ty_cdhdr, " Internal Table for structure ty_cdhdr
      gwa_cdhdr LIKE LINE OF git_cdhdr,          " Work Area for git_cdhdr
      git_cdpos TYPE STANDARD TABLE OF ty_cdpos, " Internal Table for structure ty_cdpos
      gwa_cdpos LIKE LINE OF git_cdpos,           " Work Area for git_cdpos
      git_lifnr TYPE STANDARD TABLE OF ty_cdhdr.

************************************************************************
*                          Custom Data Types                           *
************************************************************************
DATA: gv_ldate TYPE sy-datum,  " Last execution date
      gv_ltime TYPE sy-uzeit,  " Last Execution time
      gv_path  TYPE string,    " Selected download path
      gv_flag  TYPE c,         " File send status flag
      gv_org    TYPE i,        " SupplierOrg lines
      gv_orgdel TYPE i,        " SupplierOrg_Delete lines
      gv_act(1).

************************************************************************
*                             Ranges                                   *
************************************************************************
RANGES: gr_actsupp FOR lfa1-lifnr,  "Range Table for Vendors
        gr_tcode   FOR cdhdr-tcode,  " T-codes
        gr_fname   FOR cdpos-fname.  " Field Names


************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS: gc_buyer(20)  TYPE c VALUE 'buyersystemid',
           gc_bunit(2)   TYPE c VALUE  'UG',
           gc_no(2)      TYPE c VALUE 'No',
           gc_yes(3)     TYPE c VALUE 'Yes',
           gc_0(1)       TYPE c VALUE '0',
           gc_2(1)       TYPE c VALUE '2',
           gc_x(1)       TYPE c VALUE 'X',
           gc_org        TYPE string VALUE 'SupplierOrganization',
           gc_idpart     TYPE string VALUE 'SupplierOrganizationOrganizationIDPart',
           gc_org_del    TYPE string VALUE 'SupplierOrganization_Delete',
           gc_idpart_del TYPE string VALUE 'SupplierOrganizationOrganizationIDPart_Delete'.
CONSTANTS: gc_fpath      TYPE localfile VALUE
           '/usr/sap/interfaces/D30/ARIBA/Supplier/Staging/',  " All .CSV files
           gc_zpath      TYPE localfile VALUE
           '/usr/sap/interfaces/D30/ARIBA/Supplier/Zip/',  " final .zip file
           gc_ppath      TYPE localfile VALUE 'H:\my documents\'.  " All .CSV files to Presentations server
