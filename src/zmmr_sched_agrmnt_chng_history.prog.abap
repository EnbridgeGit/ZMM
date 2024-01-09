*&----------------------------------------------------------------------*
*& Report  ZMMR_SCHED_AGRMNT_CHNG_HISTORY
*&*&--------------------------------------------------------------------*
*&
*&----------------------------------------------------------------------*
*&
*&
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Program Name       :  ZMMR_SCHED_AGRMNT_CHNG_HISTORY                 *
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
*&                         D30K931026            Change History Report  *
*&                         D30K931070
*&                         D30K931076
*&                         D30K931078
*&                         D30K931080
*&                         D30K931138
*&----------------------------------------------------------------------*

REPORT  zmmr_sched_agrmnt_chng_history MESSAGE-ID zfi01
                                       NO STANDARD PAGE HEADING.

******data declaration ************
  INCLUDE zmmr_sched_agrmnt_chnag_top.

******selection screen*************
  INCLUDE zmmr_sched_agrmnt_chang_scr.

********program logic**************
  INCLUDE zmmr_sched_agrmnt_chang_lgc.

*******Display the fieldcatalog****
  INCLUDE zmmr_sched_agrmnt_chang_fcat.

******Display the data*************
  INCLUDE zmmr_sched_agrmnt_chng_disp.
