*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHNAG_COPYT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHNAG_TOP
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Program Name       :  ZMMR_SCHED_AGRMNT_CHNG_HISTORY                 *
*& Include            :  ZMMR_SCHED_AGRMNT_CHNAG_TOP                    *
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

TABLES: ekko,ekpo,cdhdr,cdpos.

types: begin of ty_ekko,
    ebeln type CDOBJECTV,
    client type CDTABKEY,
  end of ty_ekko.

data:ls_ekko type ty_ekko,
     lt_ekko type STANDARD TABLE OF ty_ekko.

data:lv_string(150) type c.


types: BEGIN OF TY_CDHDR,
        OBJECTCLAS TYPE CDHDR-OBJECTCLAS,
        OBJECTID TYPE CDHDR-OBJECTID,
        CHANGENR TYPE CDHDR-CHANGENR,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,
        change_ind type CDCHNGINDH,
      END OF TY_CDHDR.

data:ls_cdhdr type ty_cdhdr,
     lt_cdhdr_A1 type TABLE OF ty_cdhdr,
     lt_cdhdr type TABLE OF ty_cdhdr.

types: BEGIN OF TY_CDHDR1,
        OBJECTCLAS TYPE CDHDR-OBJECTCLAS,
        OBJECTID TYPE CDHDR-OBJECTID,
        CHANGENR TYPE CDHDR-CHANGENR,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,
        change_ind type CDCHNGINDH,
      END OF TY_CDHDR1.

data:ls_cdhdr1 type ty_cdhdr1,
     lt_cdhdr1_A1 type TABLE OF ty_cdhdr1,
     lt_cdhdr1 type TABLE OF ty_cdhdr1.
types: BEGIN OF TY_CDHDR2,

        OBJECTID TYPE CDHDR-OBJECTID,
        CHANGENR TYPE CDHDR-CHANGENR,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,

      END OF TY_CDHDR2.
data:ls_cdhdr2 type ty_cdhdr2,
     lt_cdhdr2_A1 type TABLE OF ty_cdhdr2,
     lt_cdhdr2 type TABLE OF ty_cdhdr2.
types: BEGIN OF TY_CDHDR3,

        OBJECTID TYPE CDHDR-OBJECTID,
        CHANGENR TYPE CDHDR-CHANGENR,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,

      END OF TY_CDHDR3.
data:ls_cdhdr3 type ty_cdhdr3,
     lt_cdhdr3 type TABLE OF ty_cdhdr3.


types: BEGIN OF TY_CDPOS,
        OBJECTCLAS TYPE CDPOS-OBJECTCLAS,
        OBJECTID TYPE CDPOS-OBJECTID,
        CHANGENR TYPE CDPOS-CHANGENR,

      END OF TY_CDPOS.

data:ls_cdpos type ty_cdpos,
     lt_cdpos_A1 type TABLE OF ty_cdpos,
     lt_cdpos type TABLE OF ty_cdpos.

types: BEGIN OF TY_CDPOS1,
        OBJECTCLAS TYPE CDPOS-OBJECTCLAS,
        OBJECTID TYPE CDPOS-OBJECTID,
        CHANGENR TYPE CDPOS-CHANGENR,

      END OF TY_CDPOS1.

data:ls_cdpos1 type ty_cdpos1,
     lt_cdpos1 type table of ty_cdpos1.

types: BEGIN OF TY_CDPOS2,
        OBJECTCLAS TYPE CDPOS-OBJECTCLAS,
        OBJECTID TYPE CDPOS-OBJECTID,
        CHANGENR TYPE CDPOS-CHANGENR,

      END OF TY_CDPOS2.

data:ls_cdpos2 type ty_cdpos2,
     lt_cdpos2_A1 type table of ty_cdpos2,
    lt_cdpos2 type table of ty_cdpos2.

types: BEGIN OF TY_CDPOS3,

        OBJECTID TYPE CDPOS-OBJECTID,
        CHANGENR TYPE CDPOS-CHANGENR,
        fname type cdpos-fname,
        value_new type cdpos-value_new,

      END OF TY_CDPOS3.


data:ls_cdpos3 type Ty_cdpos3,
     lt_cdpos3_A1 type table of ty_cdpos3,
     lt_cdpos3 type table of ty_cdpos3.

data:lt_hdr type cdhdr,
      lt_EDITPOS  type TABLE OF CDSHW,
*        ls_editpos1 type CDSHW,
      ls_editpos  type cdred,
      ls_editpos1 type cdred,
      lt_EDITPOS_WITH_HEADER  type table of   CDRED.


types:begin of ty_final,

  objid type CDOBJECTV,
  username type CDUSERNAME,
  name1 type XUBNAME,
  name2 type XUBNAME,
  usdate type CDDATUM,
  ustime type CDUZEIT,
  tcode type CDTCODE,
  udate type CDDATUM,
  utime type cduzeit,
  tabkey type CDTABKEY,
  CHANGENR TYPE CDCHANGENR,
  chngind type cdchngind,
  tabname type cdtabname,
  fname type fieldname,
  ftext type ddtext,
  f_old type CDFLDVALO,
  f_new type CDFLDVALN,
  TEXT_CASE Type  CDTEXTIND,
  TEXTART Type  CDTEXTART,

  end of ty_final.

data:lt_final type TABLE OF ty_final,
     ls_final type ty_final,
     ls_final1 type ty_final,
     wa_cellcolor       TYPE lvc_s_scol.


data:lt_fcat type slis_t_fieldcat_alv,
     ls_fcat like LINE OF lt_fcat.

DATA:LS_USER type USR03.

DATA:LT_ADRS  TYPE table of   ADDR3_VAL,
      ls_adrs type ADDR3_VAL.

DATA:LV_OBJECTCLAS TYPE CDOBJECTCL VALUE 'EINKBELEG',
     LV_TABNAME TYPE TABNAME VALUE 'EKKO',
     LV_FNAME TYPE FIELDNAME VALUE 'FRGKE',
     LV_VALUE TYPE CDFLDVALN VALUE 'Y'.
