*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHNG_COPYD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZMMR_SCHED_AGRMNT_CHNG_DISP
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Program Name       :  ZMMR_SCHED_AGRMNT_CHNG_HISTORY                 *
*& Include            :  ZMMR_SCHED_AGRMNT_CHNAG_DISP                   *
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
***************************Display the data**************
sort lt_final by objid ASCENDING udate DESCENDING utime DESCENDING .
if LT_FINAL IS not INITIAL.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = sy-repid
      IT_FIELDCAT        = lt_fcat
    TABLES
      T_OUTTAB           = lt_final
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
else.
  message 'No Records Found' TYPE 'I' .
endif.

*****************************Display the data**************
