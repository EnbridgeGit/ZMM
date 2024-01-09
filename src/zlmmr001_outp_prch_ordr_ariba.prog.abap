*&---------------------------------------------------------------------*
*& Report  ZLMMR001_OUTP_PRCH_ORDR_ARIBA
*&---------------------------------------------------------------------*
************************************************************************
*                                                                      *
*  Client:    Spectra Energy.                                          *
*  Author:    John Hartung                                             *
*  Date:      February 21, 2011                                        *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*     - Output purchase orders to ARIBA via SONIC using trans. ME9F.   *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 02/21/11 0872 JRHARTU D30K916001 - Initial program development       *
*----------------------------------------------------------------------*
************************************************************************
REPORT  zlmmr001_outp_prch_ordr_ariba.

************************************************************************
*                                Tables                                *
************************************************************************
TABLES: nast,                                    "Message Status
        ekko,                                    "Purchasing Doc Header
        cdhdr.                                   "Change Document Header

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_gs_nast.                      "Message Status
        INCLUDE STRUCTURE nast.
TYPES: END   OF ty_gs_nast.

TYPES:  ty_gt_nast       TYPE STANDARD TABLE OF ty_gs_nast.

TYPES: BEGIN OF ty_gs_cdhdr.                     "Change Document Header
        INCLUDE STRUCTURE cdhdr.
TYPES: END   OF ty_gs_cdhdr.

TYPES:  ty_gt_cdhdr      TYPE STANDARD TABLE OF ty_gs_cdhdr.

TYPES: BEGIN OF ty_gs_results,                   "Results
         ebeln           TYPE ebeln,             "Purchasing Doc Number
         flag_send       TYPE flag,              "Send Flag
       END   OF ty_gs_results.

TYPES:  ty_gt_results    TYPE STANDARD TABLE OF ty_gs_results.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE char1              "X, True, Yes
                         VALUE 'X',
        gc_kappl_po      TYPE sna_kappl          "Message Application-
                         VALUE 'EF',             "Purchase Order
        gc_kschl_ariba   TYPE sna_kschl          "Message Type-ARIBA
                         VALUE 'ZARI',
        gc_vstat_np      TYPE na_vstat           "Msg Processing Status-
                         VALUE '0',              "Not Processed
        gc_usnam_ariba   TYPE usnam              "Username - ARIBA
                         VALUE 'SONIC',
        gc_objectclas_po TYPE cdobjectcl         "Object Class - PO
                         VALUE 'EINKBELEG',
        gc_ekorg_mm      TYPE ekorg              "Purchasing Organizatn-
                         VALUE 'MATL'.           "Materials Management

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_objectid      TYPE cdobjectv.         "Object Value

************************************************************************
*                                Ranges                                *
************************************************************************
RANGES: gr_objky         FOR  nast-objky,        "Object Key-Purchas Ord
        gr_ebeln         FOR  ekko-ebeln,        "Purchasing Doc Number
        gr_ekorg         FOR  ekko-ekorg,        "Purchasing Organizatn
        gr_kschl         FOR  t685b-kschl.       "Output Type

************************************************************************
*                              Structures                              *
************************************************************************

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_nast          TYPE ty_gt_nast         "Message Status
                         WITH HEADER LINE.

DATA:   gt_cdhdr         TYPE ty_gt_cdhdr        "Change Document Header
                         WITH HEADER LINE.

DATA:   gt_results       TYPE ty_gt_results      "Results
                         WITH HEADER LINE.

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select options
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-110.
SELECT-OPTIONS:   so_ebeln FOR ekko-ebeln.       "Purcahase Order Number
PARAMETER:        sp_kappl TYPE sna_kappl        "Message Application
                           MODIF ID dsp
                           DEFAULT gc_kappl_po.
PARAMETER:        sp_kschl TYPE sna_kschl        "Message Type-ARIBA
                           MODIF ID dsp
                           DEFAULT gc_kschl_ariba.
PARAMETER:        sp_vstat TYPE na_vstat         "Msg Processing Status
                           MODIF ID dsp
                           DEFAULT gc_vstat_np.
PARAMETER:        sp_uname TYPE usnam
                           MODIF ID dsp
                           DEFAULT gc_usnam_ariba.
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run options
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-120.
PARAMETER:        sp_test  AS CHECKBOX           "Test Run
                           DEFAULT 'X'.
SELECTION-SCREEN: END   OF BLOCK ssb2.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF  ( screen-group1 EQ 'DSP' ).
      screen-input     = 0.
      screen-output    = 1.
      screen-invisible = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.

START-OF-SELECTION.

* Initial data elements
  PERFORM  f_initial_data_elements.

* Select the report data
  PERFORM  f_select_data  TABLES gt_results.

END-OF-SELECTION.

* Submit output
  PERFORM  f_submit_output  TABLES gt_results.

* Write report to spool
  WRITE: /001 'Pur.Doc.   Output'(201).
  CLEAR    gt_results.
  LOOP AT  gt_results.
    WRITE: /001 gt_results-ebeln, gt_results-flag_send.
    CLEAR  gt_results.
  ENDLOOP.

*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

* Initial internal tables
  CLEAR:   gt_nast[],             gt_nast.
  CLEAR:   gt_cdhdr[],            gt_cdhdr.
  CLEAR:   gt_results[],          gt_results.

* Initial range tables
  CLEAR:   gr_objky[],            gr_objky.
  CLEAR:   gr_ebeln[],            gr_ebeln.
  CLEAR:   gr_ekorg[],            gr_ekorg.
  CLEAR:   gr_kschl[],            gr_kschl.

* Set the object key range table to the range of purchase ords selected
  CLEAR    so_ebeln.
  LOOP AT  so_ebeln.
    CLEAR                         gr_objky.
    MOVE   so_ebeln-sign       TO gr_objky-sign.
    MOVE   so_ebeln-option     TO gr_objky-option.
    MOVE   so_ebeln-low        TO gr_objky-low.
    MOVE   so_ebeln-high       TO gr_objky-high.
    APPEND                        gr_objky.
    CLEAR  so_ebeln.
  ENDLOOP.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_data
*&---------------------------------------------------------------------*
*       Select the report data
*----------------------------------------------------------------------*
FORM f_select_data
  TABLES et_results STRUCTURE gt_results.

  DATA:  lv_subrc TYPE sysubrc.

* Select all entries in table NAST that have not been processed for
* message type ARIBA
  SELECT   *
    INTO   TABLE gt_nast
    FROM   nast
   WHERE   vstat  = sp_vstat
     AND   kappl  = sp_kappl
     AND   kschl  = sp_kschl
     AND   objky IN gr_objky.
  IF ( sy-subrc NE 0 ).
    CLEAR: gt_nast[],   gt_nast.
    RETURN.
  ENDIF.

  SORT     gt_nast ASCENDING BY objky.

* If the message was generated as a result of activity performed by
* ARIBA (user SONIC), then check if the last change performed was
* a userid other than SONIC.
  CLEAR    gt_nast.
  LOOP AT  gt_nast.

    CLEAR  lv_subrc.

    IF   ( gt_nast-usnam EQ sp_uname ).

      PERFORM  f_check_change_header USING    gt_nast-objky
                                     CHANGING lv_subrc.

    ENDIF.

    CLEAR                              et_results.
    MOVE     gt_nast-objky          TO et_results-ebeln.

    IF ( lv_subrc EQ 0 ).
      MOVE     gc_x                 TO et_results-flag_send.
    ELSE.
      CLEAR                            et_results-flag_send.
    ENDIF.

    APPEND                             et_results.

    CLEAR  gt_nast.
  ENDLOOP.

ENDFORM.                    " f_select_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_change_header
*&---------------------------------------------------------------------*
*       Check if the last change to a purchase order was performed by
*       a userid other than SONIC
*----------------------------------------------------------------------*
FORM f_check_change_header
  USING    iv_objky TYPE na_objkey
  CHANGING ev_subrc TYPE sysubrc.

  DATA:    lv_tabix TYPE sytabix.

  CLEAR                            ev_subrc.
  MOVE     1                    TO ev_subrc.

  CLEAR                            gv_objectid.
  MOVE     iv_objky             TO gv_objectid.

  CLEAR:   gt_cdhdr[], gt_cdhdr.
  SELECT   *
    INTO   TABLE gt_cdhdr
    FROM   cdhdr
   WHERE   objectclas = gc_objectclas_po
     AND   objectid   = gv_objectid.
  IF ( sy-subrc NE 0 ).
    CLEAR: gt_cdhdr[], gt_cdhdr.
    RETURN.
  ENDIF.

  SORT     gt_cdhdr ASCENDING BY udate utime.

  lv_tabix = LINES( gt_cdhdr ).

* Read the last item in the table and check the userid
  CLEAR          gt_cdhdr.
  READ     TABLE gt_cdhdr INDEX lv_tabix.
  IF           ( sy-subrc EQ 0 ).
    IF         ( gt_cdhdr-username NE sp_uname ).
      CLEAR      ev_subrc.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_check_change_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_submit_output
*&---------------------------------------------------------------------*
*       Submit output
*----------------------------------------------------------------------*
FORM f_submit_output
  TABLES et_results STRUCTURE gt_results.

* Initial the range tables

* Purchasing document number
  CLEAR    et_results.
  LOOP AT  et_results WHERE flag_send = gc_x.
    CLEAR                         gr_ebeln.
    MOVE   'I'                 TO gr_ebeln-sign.
    MOVE   'EQ'                TO gr_ebeln-option.
    MOVE   et_results-ebeln    TO gr_ebeln-low.
    APPEND                        gr_ebeln.
    CLEAR  et_results.
  ENDLOOP.

  CHECK  ( gr_ebeln[] IS NOT INITIAL ).

* Material group
  CLEAR                           gr_ekorg.
  MOVE     'I'                 TO gr_ekorg-sign.
  MOVE     'EQ'                TO gr_ekorg-option.
  MOVE     gc_ekorg_mm         TO gr_ekorg-low.
  APPEND                          gr_ekorg.

* Message type
  CLEAR                           gr_kschl.
  MOVE     'I'                 TO gr_kschl-sign.
  MOVE     'EQ'                TO gr_kschl-option.
  MOVE     sp_kschl            TO gr_kschl-low.
  APPEND                          gr_kschl.


  IF ( sp_test IS INITIAL ).

    SUBMIT rm06endr_alv
     USING SELECTION-SCREEN '1000'
      WITH s_ebeln IN gr_ebeln
      WITH s_ekorg IN gr_ekorg
      WITH p_kappl EQ sp_kappl
      WITH s_kschl IN gr_kschl
      WITH p_vstat EQ sp_vstat
       AND RETURN.

  ENDIF.

ENDFORM.                    " f_submit_output
