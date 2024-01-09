*&---------------------------------------------------------------------*
*& Program Name       :  ZMM_ARIBA_INV_REJECT                          *
*& Include Name       :  ZRSEIDOC_DAT1                                  *
*& Author             :  Praveena Anusuri                              *
*& Creation Date      :  07/03/2013                                    *
*& Object ID          :  E_PTP_MM (Ticket 30830)                       *
*& Transport Request  :  D30K921628                                    *
*& Application Area   :  PTP-MM                                        *
*& Description        :  An outbound invoice IDoc is sent to Ariba with*
*                        status 'DELETE'and will update the failed     *
*                        inbound invoice IDoc to '68'.                 *
*                        Copied from standard program RSEIDOC2.       *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*

TABLES: edidc, teds2.

TABLES: edpp1.

TABLES: edoc_stat.
TABLES: edoc_stli.

TABLES: stacust, stalight, t100.

DATA: is_variant LIKE disvariant.

DATA: need_refresh(1).
DATA: sel_index LIKE sy-tabix.
DATA: number TYPE i.
DATA: select_all_use(1).
DATA: direction(1).
DATA: port_partner(1).
DATA: segnam LIKE dntab-tabname.
DATA: offset TYPE p VALUE 0,
      string(255).
*BOI PANUSURI Ticket 30830
DATA: gv_count          TYPE i.
CONSTANTS: c_zarbinv01 TYPE char9 VALUE 'ZARBINV01',
           c_zarbinv   TYPE char7 VALUE 'ZARBINV',
           c_51        TYPE char2 VALUE '51',
           c_68        type char2 value '68'.
*EOI PANUSURI Ticket 30830
FIELD-SYMBOLS: <feld>.

DATA: time_0 LIKE edidc-updtim VALUE '000000',
      time_24 LIKE edidc-updtim VALUE '235959'.
DATA: BEGIN OF i_stacust OCCURS 0,
        status LIKE stacust-status,
        statva LIKE stacust-statva,
      END OF i_stacust.

DATA: BEGIN OF i_stalight OCCURS 0,
        statva LIKE stalight-statva,
        stalight LIKE stalight-stalight,
      END OF i_stalight.

DATA: BEGIN OF int_edidc OCCURS 50.
        INCLUDE STRUCTURE edidc.
DATA: END OF int_edidc.

**BOI PANUSURI Ticket 30830
DATA: BEGIN OF int_edidd OCCURS 0.
        INCLUDE STRUCTURE edidd.
DATA: END OF int_edidd.
DATA: wa_listedidd LIKE int_edidd.

DATA: BEGIN OF int_dfies OCCURS 50.
        INCLUDE STRUCTURE dfies.
DATA: END OF int_dfies.
DATA: wa_dfies LIKE int_dfies.

DATA: BEGIN OF int_seg OCCURS 100.
        INCLUDE STRUCTURE int_seg.
DATA: END OF int_seg.
DATA: wa_seg LIKE int_seg.
*EOC PANUSURI Ticket 30830

DATA: BEGIN OF int_edids OCCURS 0.
        INCLUDE STRUCTURE edids.
DATA: END OF int_edids.

DATA: i_listedidc LIKE listedidc OCCURS 0.
DATA: wa_listedidc LIKE listedidc.

DATA: BEGIN OF choose_edidc OCCURS 0,
        docnum LIKE edidc-docnum,
      END OF choose_edidc.

DATA: BEGIN OF i_edidc OCCURS 0.
        INCLUDE STRUCTURE edidc.
DATA: END OF i_edidc.

DATA: BEGIN OF inb_edidc OCCURS 0,
        mestyp LIKE edidc-mestyp,
        status LIKE edidc-status,
        docnum LIKE edidc-docnum,
      END OF inb_edidc.

DATA: BEGIN OF out_edidc OCCURS 0,
        mestyp LIKE edidc-mestyp,
        status LIKE edidc-status,
        docnum LIKE edidc-docnum,
      END OF out_edidc.

data: status_01 like edfi2-lstrec,
      status_02 like edfi2-lstrec,
      status_03 like edfi2-lstrec,
      status_04 like edfi2-lstrec,
      status_05 like edfi2-lstrec,
      status_06 like edfi2-lstrec,
      status_07 like edfi2-lstrec,
      status_08 like edfi2-lstrec,
      status_09 like edfi2-lstrec,
      status_10 like edfi2-lstrec,
      status_11 like edfi2-lstrec,
      status_12 like edfi2-lstrec,
      status_13 like edfi2-lstrec,
      status_14 like edfi2-lstrec,
      status_15 like edfi2-lstrec,
      status_16 like edfi2-lstrec,
      status_17 like edfi2-lstrec,
      status_18 like edfi2-lstrec,
      status_19 like edfi2-lstrec,
      status_20 like edfi2-lstrec,
      status_21 like edfi2-lstrec,
      status_22 like edfi2-lstrec,
      status_23 like edfi2-lstrec,
      status_24 like edfi2-lstrec,
      status_25 like edfi2-lstrec,
      status_26 like edfi2-lstrec,
      status_27 like edfi2-lstrec,
      status_28 like edfi2-lstrec,
      status_29 like edfi2-lstrec,
      status_30 like edfi2-lstrec,
      status_31 like edfi2-lstrec,
      status_32 like edfi2-lstrec,
      status_33 like edfi2-lstrec,
      status_34 like edfi2-lstrec,
      status_35 like edfi2-lstrec,
      status_36 like edfi2-lstrec,
      status_37 like edfi2-lstrec,
      status_38 like edfi2-lstrec,
      status_39 like edfi2-lstrec,
      status_40 like edfi2-lstrec,
      status_41 like edfi2-lstrec,
      status_42 like edfi2-lstrec.

data: status_50 like edfi2-lstrec,
      status_51 like edfi2-lstrec,
      status_52 like edfi2-lstrec,
      status_53 like edfi2-lstrec,
      status_54 like edfi2-lstrec,
      status_55 like edfi2-lstrec,
      status_56 like edfi2-lstrec,
      status_57 like edfi2-lstrec,
      status_58 like edfi2-lstrec,
      status_60 like edfi2-lstrec,
      status_61 like edfi2-lstrec,
      status_62 like edfi2-lstrec,
      status_63 like edfi2-lstrec,
      status_64 like edfi2-lstrec,
      status_65 like edfi2-lstrec,
      status_66 like edfi2-lstrec,
      status_68 like edfi2-lstrec,
      status_69 like edfi2-lstrec,
      status_70 like edfi2-lstrec,
      status_71 like edfi2-lstrec,
      status_73 like edfi2-lstrec,
      status_74 like edfi2-lstrec,
      status_75 like edfi2-lstrec.


DATA: BEGIN OF int_teds2 OCCURS 30.
        INCLUDE STRUCTURE teds2.
DATA: END OF int_teds2.

*parameters: docking like edi_help-onl_option default 'O' no-display.
*BOC PANUSURI Ticket 30830
*SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-018.
SELECT-OPTIONS: cretim  FOR edidc-cretim DEFAULT time_0 TO time_24 .
SELECT-OPTIONS: credat  FOR edidc-credat DEFAULT sy-datum TO sy-datum .
*                UPDTIM  FOR EDIDC-UPDTIM DEFAULT TIME_0 TO TIME_24,
*                UPDDAT  FOR EDIDC-UPDDAT.
*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: DIRECT  FOR EDIDC-DIRECT no-extension no intervals,
*                DOCNUM  FOR EDIDC-DOCNUM,
*                STATUS  FOR EDIDC-STATUS.
*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: IDOCTP  FOR EDIDC-IDOCTP,
*                CIMTYP  FOR EDIDC-CIMTYP,
*                MESTYP  FOR EDIDC-MESTYP,
*                mescod  for edidc-mescod,
*                mesfct  for edidc-mesfct.
*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: ppPOR  FOR EDOC_STli-rcvPOR,
*                ppPRN  FOR EDOC_STAT-rcvPRN,
*                ppPRT  FOR edoc_stli-rcvprt,
*                pppfc  for edoc_stli-rcvpfc.
SELECTION-SCREEN END OF BLOCK a.
*selection-screen begin of screen 1200 as subscreen.
*SELECT-OPTIONS: ownPOR  FOR EDOC_STLI-sndPOR,
*                ownPRN  FOR EDOC_STli-sndPRN,
*                ownPRT  FOR edoc_stli-sndprt,
*                ownpfc  for edoc_stli-sndpfc,
*                TEST    FOR EDIDC-TEST no-extension no intervals.
*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN END OF SCREEN 1200.
*SELECTION-SCREEN BEGIN OF SCREEN 1300 AS SUBSCREEN.
*SELECT-OPTIONS: REFINT  FOR EDIDC-REFINT,
*                REFGRP  FOR EDIDC-REFGRP,
*                REFMES  FOR EDIDC-REFMES,
*                ARCKEY  FOR EDIDC-ARCKEY.
*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: STD     FOR EDIDC-STD,
*                STDVRS  FOR EDIDC-STDVRS,
*                STDMES  FOR EDIDC-STDMES.
*SELECTION-SCREEN END OF SCREEN 1300.
*
*SELECTION-SCREEN BEGIN OF TABBED BLOCK idoctabbl FOR 20 LINES.
*SELECTION-SCREEN TAB (15) sos_tabl USER-COMMAND sos_tab
*                 DEFAULT SCREEN 1100.
*SELECTION-SCREEN TAB (15) SOS_TAB2 USER-COMMAND SOS_TAB2
*                 DEFAULT SCREEN 1200.
*SELECTION-SCREEN TAB (15) SOS_TAB3 USER-COMMAND SOS_TAB3
*                 DEFAULT SCREEN 1300.
*SELECTION-SCREEN END OF BLOCK idoctabbl.
*EOC PANUSURI Ticket 30830
