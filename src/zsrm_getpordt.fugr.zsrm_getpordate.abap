FUNCTION zsrm_getpordate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_EBELN) TYPE  EBELN
*"  EXPORTING
*"     VALUE(EX_PORELDATE) TYPE  CDDATUM
*"     VALUE(EX_PORELTIME) TYPE  CDUZEIT
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Program Name       : ZSRM_GETPORDATE                                *
*& Author             : CKUMAR                                         *
*& Creation Date      : 20-Jan-2017                                    *
*& Object ID          : ACR-3186                                       *
*& Application Area   : SRM                                            *
*& Description        : To get PO release date from ECC and retrun     *
*&                      PO release date to SRM system                  *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Object ID     :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*

************************************************************************
*Data Type Declaration
************************************************************************
  TYPES: BEGIN OF ty_cdpos,
           objectclas TYPE cdobjectcl,
           objectid   TYPE cdobjectv,
           changenr   TYPE cdchangenr,
         END OF ty_cdpos.

************************************************************************
*Internal Table and work area Declaration
************************************************************************
  DATA: wa_cdpos     TYPE ty_cdpos,
        lv_poreldate TYPE cddatum,
        lv_poreltime TYPE cduzeit.

************************************************************************
*Constant Declaration
************************************************************************
  CONSTANTS: lc_einkbeleg  TYPE cdobjectcl   VALUE 'EINKBELEG',
             lc_ekko       TYPE tabname      VALUE 'EKKO',
             lc_fname      TYPE fieldname    VALUE 'MEMORYTYPE',
             lc_u          TYPE cdchngind    VALUE 'U',
             lc_h          TYPE cdfldvalo    VALUE 'H'.

*To get the value from CDPOS
  SELECT SINGLE objectclas
         objectid
         changenr
         FROM cdpos
         INTO wa_cdpos
         WHERE objectclas EQ lc_einkbeleg AND
               objectid   EQ im_ebeln     AND
               tabname    EQ lc_ekko      AND
               fname      EQ lc_fname     AND
               chngind    EQ lc_u         AND
               value_old  EQ lc_h.
  IF sy-subrc EQ 0.
*To get the value from CDHDR
    SELECT SINGLE udate
           utime
           FROM cdhdr
           INTO (lv_poreldate, lv_poreltime)
           WHERE objectclas EQ wa_cdpos-objectclas AND
                 objectid   EQ wa_cdpos-objectid   AND
                 changenr   EQ wa_cdpos-changenr.
    IF sy-subrc EQ 0.
      ex_poreldate = lv_poreldate.
      ex_poreltime = lv_poreltime.
    ENDIF.
  ENDIF.

ENDFUNCTION.
