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
*&                                     Change History Report            *
*&----------------------------------------------------------------------*

REPORT  ZMMR_SCHED_AGRMNT_CHNG_COPY MESSAGE-ID ZFI01
                                       NO STANDARD PAGE HEADING.

******data declaration ************
INCLUDE ZMMR_SCHED_AGRMNT_CHNAG_COPYT.

******selection screen*************
INCLUDE ZMMR_SCHED_AGRMNT_CHANG_COPYS.

********program logic**************
INCLUDE ZMMR_SCHED_AGRMNT_CHANG_COPYL.

*******Display the fieldcatalog****
INCLUDE ZMMR_SCHED_AGRMNT_CHANG_COPYF.

******Display the data*************
INCLUDE ZMMR_SCHED_AGRMNT_CHNG_COPYD.
