*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHANG_COPYS
*&---------------------------------------------------------------------*
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
SELECTION-SCREEN begin of block b1 WITH FRAME.
PARAMETERS:p_ekorg type ekko-ekorg OBLIGATORY DEFAULT 'GASA' .
Select-OPTIONS:s_ebeln for ekko-ebeln OBLIGATORY.
parameters:p_udate type cdhdr-udate.
SELECTION-SCREEN end of block b1 .
*****************Selection Screen ***************************************
