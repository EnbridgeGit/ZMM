*&---------------------------------------------------------------------*
*& Report  ZLMMC001_DEALIDOC
*&
*&---------------------------------------------------------------------*
*Project:   COG.
*Date:      3 Aug 2021
*Author:    Durgaprakash biruduraju
*Program Description: Wrapper to process Deal IDOC's and send to
*                     BODS as individual file for each IDOC
*&---------------------------------------------------------------------*
*Changes Log
* RLSE0017325 NIKAMT July 11, 2023 D30K932557
* Output control for SAs to openlink
*&---------------------------------------------------------------------*
REPORT  zlmmc001_dealidoc MESSAGE-ID zm.

TYPES: BEGIN OF ty_idoc,
       docnum TYPE edidc-docnum,
       mestyp TYPE edidc-mestyp,
       END OF ty_idoc.

TYPES: BEGIN OF ty_nast,
       objky TYPE nast-objky,
       kschl TYPE nast-kschl,
       parnr TYPE nast-parnr,
       parvw TYPE nast-parvw,
       manue TYPE nast-manue,
       datvr TYPE nast-datvr,
       uhrvr TYPE nast-uhrvr,
       vstat TYPE nast-vstat,
      END OF ty_nast.

TYPES: BEGIN OF ty_cdhdr,
       objectclas TYPE cdhdr-objectclas,
       objectid   TYPE cdhdr-objectid,
       changenr   TYPE cdhdr-changenr,
       udate      TYPE cdhdr-udate,
       utime      TYPE cdhdr-utime,
       tcode      TYPE cdhdr-tcode,
       END OF ty_cdhdr.

TYPES: BEGIN OF ty_cdpos,
      objectclas TYPE cdpos-objectclas,
      objectid   TYPE cdpos-objectid,
      changenr   TYPE cdpos-changenr,
      tabname    TYPE cdpos-tabname,
      tabkey     TYPE cdpos-tabkey,
      fname      TYPE cdpos-fname,
      value_new  TYPE cdpos-value_new,
      value_old  TYPE cdpos-value_old,
      END OF ty_cdpos.

TYPES : BEGIN OF ty_nast1,
        objky(90) TYPE c,
        datvr TYPE nast-datvr,
        uhrvr TYPE nast-uhrvr,
        END OF ty_nast1.

TYPES: BEGIN OF ty_out_list,
        msg(72) TYPE c,
       END OF ty_out_list .

TYPES : BEGIN OF ty_zfit,
        key1 TYPE zfit_xparam-key1,
        END OF ty_zfit.

DATA :   gs_fieldn TYPE ty_zfit,
         gt_fieldn TYPE TABLE OF ty_zfit.

DATA :   gs_cdhdr TYPE ty_cdhdr,
         gt_cdhdr TYPE TABLE OF ty_cdhdr.

DATA: t_idoc TYPE STANDARD TABLE OF ty_idoc,
      w_idoc TYPE ty_idoc,
      v_lines TYPE i,
      lv_lines TYPE i,
      v_count TYPE i,
      lv_msg TYPE string,
      v_docnum TYPE edi_docnum.
* Following lines Added   D30K932557
DATA: gs_xparam TYPE zfit_xparam,
      gt_xparam TYPE TABLE OF zfit_xparam,
      gs_orders TYPE zfit_xparam,
      gs_others TYPE zfit_xparam,
      gs_cndnty TYPE zfit_xparam,
      gs_svalue TYPE zfit_xparam .

DATA: gt_out_list TYPE STANDARD TABLE OF ty_out_list,
      gw_out_list TYPE ty_out_list .

DATA: gs_cdpos TYPE ty_cdpos,
      gt_cdpos TYPE TABLE OF ty_cdpos.

DATA: gs_edid4 TYPE edid4,
      gs_nast TYPE ty_nast,
      gt_nast TYPE TABLE OF ty_nast,
      gt_nast1 TYPE TABLE OF ty_nast1,
      gs_nast1 TYPE ty_nast1.

DATA: gv_sanum(10) TYPE c,
      gv_idoc_processed,
      gv_amntd TYPE p DECIMALS 4 ,
      gv_amntn TYPE p DECIMALS 4 .

DATA : lv_object TYPE nast-objky,
       lv_date TYPE nast-datvr,
       lv_time TYPE nast-uhrvr.
DATA : lv_count TYPE i.
DATA : lv_flag TYPE c,
 ls_flag TYPE c,
 gs_flag TYPE c.

CONSTANTS: gc_doctyp TYPE zparamsubtype VALUE 'DOCTYP',
           gc_orders TYPE zparamvalue VALUE 'ORDERS',
           gc_cndnty TYPE zparamsubtype VALUE 'KSCHL',
           gc_ineu   TYPE zparamvalue VALUE 'INEU',
           gc_cfield TYPE zparamsubtype VALUE 'CFIELD',
           gc_svalue TYPE zparamsubtype VALUE 'SVALUE',
           gc_progrm TYPE zparamtype VALUE 'ZLMMC001_DEALIDOC',
           gc_eiedk2 TYPE edi_segnam VALUE 'E1EDK02' .
* End of addition D30K932557
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
SELECT-OPTIONS: s_docnum FOR v_docnum.
PARAMETERS:
p_status    TYPE edi_status OBLIGATORY,
p_idoctp    TYPE edi_idoctp OBLIGATORY,
p_msgtp     TYPE edi_mestyp OBLIGATORY,
p_rcvpor    TYPE edi_rcvpor OBLIGATORY,
p_date      TYPE sy-datum     OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.

START-OF-SELECTION.
* Following lines Added   D30K932557
  CLEAR: gs_xparam, gs_orders, gs_others, gs_cndnty ,gs_svalue.
  SELECT SINGLE *
    INTO gs_xparam
    FROM zfit_xparam
   WHERE paramtype = gc_progrm
     AND subtype   = gc_doctyp .
  gs_orders = gs_xparam .

  SELECT SINGLE *
    INTO gs_cndnty
    FROM zfit_xparam
   WHERE paramtype = gc_progrm
     AND subtype   = gc_cndnty .
  SELECT SINGLE *
      INTO gs_svalue
       FROM zfit_xparam
       WHERE paramtype = gc_progrm
       AND subtype   = gc_svalue .
** End of addition D30K932557
  REFRESH: t_idoc.
  CLEAR: v_lines, v_count, w_idoc,lv_lines,lv_flag,ls_flag.
** select docnum from edidc into table t_idoc  <== Delete D30K932557
** Following line Added   D30K932557
  SELECT docnum mestyp FROM edidc INTO TABLE t_idoc
    WHERE docnum IN s_docnum
      AND status = p_status
      AND rcvpor = p_rcvpor
      AND mestyp = p_msgtp
      AND idoctp = p_idoctp
      AND upddat = p_date.

  IF sy-subrc = 0.
    DESCRIBE TABLE t_idoc LINES v_lines.
*    CLEAR: gt_out_list[] .        " <== Insert D30K932557
    LOOP AT t_idoc INTO w_idoc.
** Following Lines Added D30K932557
      CLEAR: gv_idoc_processed,gw_out_list .
      IF gs_orders-value1 = w_idoc-mestyp .
        CLEAR: gs_edid4 .
        SELECT SINGLE *
          INTO gs_edid4
          FROM edid4
         WHERE docnum = w_idoc-docnum
           AND segnam = gc_eiedk2 .

        IF syst-subrc EQ 0 .
          CLEAR: gv_sanum .
          gv_sanum = gs_edid4-sdata+3(10) .
*
          SELECT objky
                  kschl
                  parnr
                  parvw
                  manue
                  datvr
                  uhrvr
                  vstat
            INTO  TABLE gt_nast
            FROM nast
           WHERE objky = gv_sanum
             AND kschl = gs_cndnty-value1
           ORDER BY datvr DESCENDING
                    uhrvr DESCENDING .

          LOOP AT gt_nast INTO gs_nast.
            DESCRIBE TABLE gt_nast LINES lv_lines.

            IF gs_nast-manue = abap_true .
              IF  sy-tabix = 1.
                SUBMIT rseout00 WITH docnum = w_idoc-docnum
                     AND RETURN .
                v_count = v_count + 1 .
                gv_idoc_processed = abap_true .
              ENDIF.
            ELSEIF gs_nast-vstat EQ 1 .
              lv_flag = abap_true.
              IF lv_lines = 1.
                SUBMIT rseout00 WITH docnum = w_idoc-docnum
                     AND RETURN.
                v_count = v_count + 1 .
                gv_idoc_processed = abap_true.
              ELSEIF
                 sy-tabix GE 2.
                IF sy-subrc = 0.
                  gs_nast1-objky = gs_nast-objky.
                  gs_nast1-datvr = gs_nast-datvr.
                  gs_nast1-uhrvr = gs_nast-uhrvr.
                  APPEND gs_nast1 TO gt_nast1.
                  CLEAR : gs_nast1 , gs_nast.
                ENDIF.
              ENDIF.
            ENDIF.

          ENDLOOP.
          IF lv_flag IS INITIAL.
            SUBMIT rseout00 WITH docnum = w_idoc-docnum
                    AND RETURN .
            v_count = v_count + 1 .
            gv_idoc_processed = abap_true .
          ENDIF.
          IF gt_nast1[] IS NOT INITIAL.
            loop at gt_nast1 into gs_nast1.
            SELECT objectclas
                   objectid
                   changenr
                   udate
                   utime
                   tcode FROM cdhdr
               INTO TABLE gt_cdhdr FOR ALL ENTRIES IN gt_nast1
                WHERE objectid = gt_nast1-objky
             AND  udate ge gt_nast1-datvr
*               and utime ge gt_nast1-uhrvr   " gv_sanum
             AND objectclas EQ 'EINKBELEG'.
            DELETE gt_cdhdr[] WHERE udate le gs_nast1-datvr and utime < gs_nast1-uhrvr.
            endloop.
*
            IF gt_cdhdr[] IS NOT INITIAL.
              SELECT  objectclas
                      objectid
                      changenr
                      tabname
                      tabkey
                      fname
                      value_new
                      value_old INTO TABLE gt_cdpos
                FROM cdpos FOR ALL ENTRIES IN gt_cdhdr
               WHERE objectclas EQ gt_cdhdr-objectclas    " 'EINKBELEG'
                 AND objectid   EQ gt_cdhdr-objectid      " gv_sanum
                AND changenr EQ gt_cdhdr-changenr .
              SORT gt_cdpos BY fname.   "changenr
            ENDIF.
          ENDIF.

          IF sy-subrc = 0.
            SELECT key1
              INTO TABLE gt_fieldn
              FROM zfit_xparam
             WHERE paramtype = gc_progrm
               AND subtype   = gc_cfield .
            SORT gt_fieldn.

            CONCATENATE text-s01 w_idoc-docnum text-s02 INTO gw_out_list-msg SEPARATED BY ' ' .
            IF gt_cdpos[] IS NOT INITIAL.
              LOOP AT gt_cdpos INTO gs_cdpos.
                READ TABLE gt_fieldn INTO gs_fieldn WITH KEY key1 = gs_cdpos-fname.

                IF sy-subrc = 0.
                  ls_flag = abap_true.

                ELSEIF gs_cdpos-fname = 'RLWRT' .
                  gv_amntd = gs_cdpos-value_old - gs_cdpos-value_new .
                  MOVE gv_amntd TO gv_amntn.
                  IF gv_amntn LT 0.
                    gv_amntn = gv_amntd * -1.
                  ELSE.
                    gv_amntn = gv_amntd.
                  ENDIF.

                  IF gv_amntn GT gs_svalue-value1 .
*                    SUBMIT rseout00 WITH docnum = w_idoc-docnum
*                     AND RETURN .
*                    v_count = v_count + 1 .
*                    gv_idoc_processed = abap_true.
                    gs_flag = abap_true.
                  ENDIF .
                ENDIF.
                CLEAR : gs_cdpos,gv_amntn.
              ENDLOOP.
              IF ls_flag IS INITIAL AND gs_flag IS INITIAL.
                WRITE : gw_out_list-msg.
              ELSE.
                SUBMIT rseout00 WITH docnum = w_idoc-docnum
                       AND RETURN .
                v_count = v_count + 1 .
                gv_idoc_processed = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        SUBMIT rseout00 WITH docnum = w_idoc-docnum
                    AND RETURN .
        v_count = v_count + 1 .
        gv_idoc_processed = abap_true.

      ENDIF.
    ENDLOOP..
    MESSAGE i000 WITH 'IDOC Count'(002) v_lines 'and processed'(003) v_count.
  ELSE.
    MESSAGE i000 WITH 'No IDOCS to process'(001) .
  ENDIF.
