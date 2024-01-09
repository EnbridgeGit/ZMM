*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHANG_SCR
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Program Name       :  ZMMR_SCHED_AGRMNT_CHNG_HISTORY                 *
*& Include            :  ZMMR_SCHED_AGRMNT_CHNAG_SCR                    *
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


*****************Selection Screen **************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:p_ekorg TYPE ekko-ekorg OBLIGATORY DEFAULT 'GASA' .
SELECT-OPTIONS:s_ebeln FOR ekko-ebeln OBLIGATORY.
SELECT-OPTIONS:s_udate FOR cdhdr-udate NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1 .
*****************Selection Screen ***************************************
