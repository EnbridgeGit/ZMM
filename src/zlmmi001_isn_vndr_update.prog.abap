REPORT  zlmmi001_isn_vndr_update MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will load isnetworl vendor information.                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date    Description                                   *
*CHG0161300 KMB 09.10.19 Isnetworld file transfer feed needs to be     *
*                        updated going to SAPMMEAST - 5194365215       *
************************************************************************

TYPE-POOLS: abap.

TYPES:  BEGIN OF ty_isnworld,
          comp_name   TYPE char200,
          isnvdr      LIKE zisnqual-isnvdr,
*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
          province(45) TYPE c,
          grade(10)   TYPE c,
          expired(3)  TYPE c,
          blocked(5)  TYPE c,
*          province    LIKE t005u-bezei,
*          grade(1)    TYPE c,
*          expired(1)  TYPE c,
*          blocked(1)  TYPE c,
*          deleted(1)  TYPE c,
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
        END OF ty_isnworld.



DATA:   msg(80)       TYPE          c,
        lv_input(400) TYPE          c,
        lv_lines      TYPE          integer,
        lv_string     TYPE          string,
        ls_isnworld   TYPE          ty_isnworld,
        lt_isnworld   LIKE TABLE OF ls_isnworld,
        ls_zisnqual   TYPE          zisnqual,
        ls_zisnqualdb TYPE          zisnqual,
        lt_zisnqual   LIKE TABLE OF ls_zisnqual,
        lv_objid      TYPE          cdobjectv,

        ls_splits     TYPE          string,
        lt_splits     LIKE TABLE OF ls_splits,

        ls_t005u      LIKE          t005u,

        lv_error(1)   TYPE          c VALUE abap_false.


FIELD-SYMBOLS:  <curcol>      TYPE          any.

CONSTANTS:  delimtr(1) TYPE c VALUE '|'.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
PARAMETERS: p_infile  LIKE filenameci-fileextern OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE: '/usr/sap/interfaces/' sy-sysid+0(3) '/S2C/'
              'isnvendorqual.dat' INTO p_infile.


*************************************************************************
*************************************************************************
START-OF-SELECTION.
  PERFORM read_input_file.
  PERFORM translate_isnworld.
  PERFORM update_database.

  IF lv_error = abap_false.
    WRITE: / 'Program completed with no errors'.
  ELSE.
    WRITE: / 'Program completed with errors'.
  ENDIF.

*----------------------------------------------------------------------*
*  This routine reads the tab-delimited input file sent by the
*  client community and splits it into its various components.
*----------------------------------------------------------------------*
FORM  read_input_file.

  OPEN DATASET p_infile  FOR INPUT  IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_infile msg.
    STOP.
  ENDIF.


  CLEAR lt_isnworld.

  DO.
    READ DATASET p_infile INTO lv_input.

    IF sy-subrc <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.
    CLEAR lt_splits.
    SPLIT lv_input AT delimtr INTO TABLE lt_splits.

    CLEAR ls_isnworld.
    LOOP AT lt_splits INTO ls_splits.
      ASSIGN COMPONENT sy-tabix
             OF STRUCTURE ls_isnworld
             TO <curcol>.
      MOVE ls_splits TO <curcol>.
    ENDLOOP.

    APPEND ls_isnworld TO lt_isnworld.
  ENDDO.



  CLOSE DATASET p_infile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_infile msg.
    STOP.
  ENDIF.
ENDFORM.                    "read_input_file


*----------------------------------------------------------------------*
*  This routine reads the isnworld input and will translate to be
*  loaded into the program.
*----------------------------------------------------------------------*
FORM translate_isnworld.
*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
  TYPES:BEGIN OF lty_zmm_isn_con,
      zvarname TYPE zvarname,
      country  TYPE zcountry,
      region   TYPE zregion,
      END OF lty_zmm_isn_con.
  DATA: ls_zmm_isn_con TYPE lty_zmm_isn_con.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215

  CLEAR lt_zisnqual.

  LOOP AT lt_isnworld INTO ls_isnworld.
    CLEAR ls_zisnqual.

    MOVE ls_isnworld-isnvdr TO ls_zisnqual-isnvdr.

    CLEAR: ls_t005u.

*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
*    SELECT SINGLE land1 bland
*      FROM t005u
*      INTO CORRESPONDING FIELDS OF ls_t005u
*      WHERE land1 = 'CA'
*        AND spras = 'E'
*        AND bezei = ls_isnworld-province
*    .
*
*    IF sy-subrc <> 0.
*      WRITE: /'Vendor: ', ls_zisnqual-isnvdr, '  Region: ', ls_isnworld-province.
*      WRITE: /'  Invalid region'.
*      lv_error = abap_true.
*      CONTINUE.
*    ENDIF.
*
*    ls_zisnqual-land1 = ls_t005u-land1.
*    ls_zisnqual-regio = ls_t005u-bland.
    TRANSLATE ls_isnworld-expired TO UPPER CASE.
    TRANSLATE ls_isnworld-blocked TO UPPER CASE.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
    IF ls_isnworld-expired+0(3) NE 'YES'.
      IF ls_isnworld-blocked+0(3) NE 'YES'.
        SELECT SINGLE ZVARNAME COUNTRY REGION FROM ZMM_ISN_CON INTO LS_ZMM_ISN_CON
                 WHERE  ZPROGRAMM = SY-CPROG
                 AND    ZVARNAME  = ls_isnworld-province
                 AND    ZVARNUM   = '1'.
        IF SY-SUBRC IS INITIAL.
          ls_zisnqual-land1 = LS_ZMM_ISN_CON-COUNTRY.
          ls_zisnqual-regio = LS_ZMM_ISN_CON-REGION.
          CLEAR:LS_ZMM_ISN_CON.
        ELSE.
          TRANSLATE ls_isnworld-province TO UPPER CASE.
          SELECT SINGLE ZVARNAME COUNTRY REGION FROM ZMM_ISN_CON INTO LS_ZMM_ISN_CON
                           WHERE  ZPROGRAMM = SY-CPROG
                           AND    ZVARNAME  = ls_isnworld-province
                           AND    ZVARNUM   = '1'.
          IF sy-subrc IS INITIAL.
            ls_zisnqual-land1 = ls_zmm_isn_con-country.
            ls_zisnqual-regio = ls_zmm_isn_con-region.
            CLEAR:ls_zmm_isn_con.
          ELSE.
            WRITE: / 'Region and Country code not Maintained'.
            lv_error = abap_true.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


*    IF ls_isnworld-deleted = 'X' OR ls_isnworld-deleted = 'x'.
*      ls_zisnqual-isnqual = ''.
*    ELSEIF ls_isnworld-expired = 'X' OR ls_isnworld-blocked = 'X' OR ls_isnworld-expired = 'x' OR ls_isnworld-blocked = 'x'.
*      ls_zisnqual-isnqual = 'F'.
    IF ls_isnworld-expired+0(3) = 'YES' OR ls_isnworld-blocked+0(3) = 'YES'.
      ls_zisnqual-isnqual = 'Z'.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
    ELSE.
*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMM
*      CASE ls_isnworld-grade.
*        WHEN 'A' OR 'B' OR 'C' OR 'D'.
*          ls_zisnqual-isnqual = 'P'.
*        WHEN 'F' OR 'R'.
*          ls_zisnqual-isnqual = 'F'.
      CASE ls_isnworld-grade+0(1).
        WHEN 'A' OR 'B'.
          ls_zisnqual-isnqual = 'P'.
        WHEN 'C' OR 'D' OR 'F'.
          ls_zisnqual-isnqual = 'F'.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMM
        WHEN OTHERS.
          WRITE: /'Vendor: ', ls_zisnqual-isnvdr, '  Region: ', ls_isnworld-province.
          WRITE: /'  Invalid grade specified: ', ls_isnworld-grade.
          lv_error = abap_true.
          CONTINUE.
      ENDCASE.
    ENDIF.


    APPEND ls_zisnqual TO lt_zisnqual.
  ENDLOOP.
ENDFORM.                    "translate_isnworld

*----------------------------------------------------------------------*
*  This routine reads the database and compairs to the records supplied
*  and updates as required.
*----------------------------------------------------------------------*
FORM update_database.
*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
  DATA: lt_zisnqual_temp TYPE TABLE OF zisnqual,
        lt_zisnqual_new  TYPE TABLE OF zisnqual,
        ls_zisnqual_new  TYPE zisnqual,
        lv_index         TYPE sy-tabix,
        lv_flag          TYPE flag.
  lt_zisnqual_temp[] = lt_zisnqual[].
  DELETE lt_zisnqual_temp WHERE isnqual NE 'Z'.
  IF lt_zisnqual_temp[] IS NOT INITIAL.
    SELECT * FROM zisnqual INTO TABLE lt_zisnqual_new
                           FOR ALL ENTRIES IN lt_zisnqual_temp
                           WHERE isnvdr = lt_zisnqual_temp-isnvdr.
    IF sy-subrc IS INITIAL.
      SORT  lt_zisnqual_new BY isnvdr.
    ENDIF.
  ENDIF.
  SORT lt_zisnqual BY isnvdr.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215

  LOOP AT lt_zisnqual INTO ls_zisnqual.
    CALL FUNCTION 'ENQUEUE_EZ_ZISNQUAL'
      EXPORTING
        isnvdr         = ls_zisnqual-isnvdr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
    ELSEIF sy-subrc = 1.
      WRITE: /'Vendor: ', ls_zisnqual-isnvdr, '  Region: ', ls_isnworld-province.
      WRITE: /'  Vendor was locked.'.
      lv_error = abap_true.
      CONTINUE.
    ELSE.
      WRITE: /'Vendor: ', ls_zisnqual-isnvdr, '  Region: ', ls_isnworld-province.
      WRITE: /'  Error during locking of vendor.'.
      lv_error = abap_true.
      CONTINUE.
    ENDIF.

*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
    CLEAR:lv_flag.
    READ TABLE lt_zisnqual_new INTO ls_zisnqual_new WITH KEY isnvdr = ls_zisnqual-isnvdr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      lv_index = sy-tabix.
      LOOP AT lt_zisnqual_new INTO ls_zisnqual_new FROM lv_index.
        IF ls_zisnqual_new-isnvdr <> ls_zisnqual-isnvdr.
          EXIT.
        ENDIF.
        DELETE zisnqual FROM ls_zisnqual_new.
        lv_objid = ls_zisnqual_new-isnvdr.
        CALL FUNCTION 'ZISNQUAL_WRITE_DOCUMENT'
          EXPORTING
            objectid                = lv_objid
            tcode                   = sy-tcode
            utime                   = sy-uzeit
            udate                   = sy-datum
            username                = sy-uname
            object_change_indicator = 'U'
            n_zisnqual              = ls_zisnqual_new
            o_zisnqual              = ls_zisnqual_new
            upd_zisnqual            = 'U'.
        CLEAR: ls_zisnqual_new.
      ENDLOOP.
    ENDIF.
    IF ls_zisnqual-isnqual EQ 'Z'.
      lv_flag = 'X'.
    ENDIF.
    IF lv_flag NE 'X'.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215


      SELECT SINGLE *
        FROM zisnqual
        INTO ls_zisnqualdb
        WHERE isnvdr = ls_zisnqual-isnvdr
          AND land1  = ls_zisnqual-land1
          AND regio  = ls_zisnqual-regio
      .

      IF sy-subrc <> 0.
        "Record not found insert.
        ls_zisnqual-erdat = sy-datum.
        ls_zisnqual-ernam = sy-uname.

        INSERT zisnqual FROM ls_zisnqual.
        lv_objid = ls_zisnqual-isnvdr.
        CALL FUNCTION 'ZISNQUAL_WRITE_DOCUMENT'
          EXPORTING
            objectid                = lv_objid
            tcode                   = sy-tcode
            utime                   = sy-uzeit
            udate                   = sy-datum
            username                = sy-uname
            object_change_indicator = 'U'
            n_zisnqual              = ls_zisnqual
            o_zisnqual              = ls_zisnqual
            upd_zisnqual            = 'I'.
      ELSE.
        "Record exists.
        IF ls_zisnqual-isnqual <> ls_zisnqualdb-isnqual.
          "Different values: update
          ls_zisnqual-erdat = ls_zisnqualdb-erdat.
          ls_zisnqual-ernam = ls_zisnqualdb-ernam.
          ls_zisnqual-laeda = sy-datum.
          ls_zisnqual-aenam = sy-uname.

          MODIFY zisnqual FROM ls_zisnqual.

          lv_objid = ls_zisnqual-isnvdr.
          CALL FUNCTION 'ZISNQUAL_WRITE_DOCUMENT'
            EXPORTING
              objectid                = lv_objid
              tcode                   = sy-tcode
              utime                   = sy-uzeit
              udate                   = sy-datum
              username                = sy-uname
              object_change_indicator = 'U'
              n_zisnqual              = ls_zisnqual
              o_zisnqual              = ls_zisnqualdb
              upd_zisnqual            = 'U'.
        ELSE.
          "Do nothing, record is same.
        ENDIF.
      ENDIF.
*BOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
    ENDIF.
*EOC by KMB CHG0161300 09.10.19 Isnetworld file transfer feed needs to be pdated going to SAPMMEAST - 5194365215
    CALL FUNCTION 'DEQUEUE_EZ_ZISNQUAL'
      EXPORTING
        isnvdr = ls_zisnqual-isnvdr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      WRITE: /'Vendor: ', ls_zisnqual-isnvdr, '  Region: ', ls_isnworld-province.
      WRITE: /'  Error during unlocking of vendor.'.
      lv_error = abap_true.
      CONTINUE.
    ENDIF.


  ENDLOOP.
ENDFORM.                    "update_database
