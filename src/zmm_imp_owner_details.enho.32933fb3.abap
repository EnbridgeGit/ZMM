"Name: \PR:SAPFSSO6\FO:SOFD_SEL003\SE:END\EI
ENHANCEMENT 0 ZMM_IMP_OWNER_DETAILS.
*&---------------------------------------------------------------------*
*& Program Name       : ZMM_IMP_OWNER_DETAILS                          *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 09-Jul-2014                                    *
*& Object ID          : SDP67946                                       *
*& Application Area   : MM                                             *
*& Description        : User details were passed to Owner to overcome  *
*&                      Authorization issue.                           *
*&---------------------------------------------------------------------*
data: lwa_souc type souc.
if sy-cprog = 'ZFAPR019_PARKEDINV_NOWORKFLOW'.
  if sofd_dat-folrg eq pr_fol      " PR_FOL is private folders
     or sofd_dat-folrg eq pw_fol   " PW_FOL is private waste basket
     or sofd_dat-folrg eq pv_fol   " PV_FOL is resubmission folder
     or sofd_dat-folrg eq ou_fol   " OU_FOL is the outbox
     or sofd_dat-folrg eq in_fol.  " IN_FOL is the inbox
    if sy-uname is not initial.
      select single * from souc into lwa_souc where sapnam = sy-uname.
      if sy-subrc = 0.
        sofd_dat-owntp = lwa_souc-usrtp.
        sofd_dat-ownyr = lwa_souc-usryr.
        sofd_dat-ownno = lwa_souc-usrno.
      endif.
    endif.
  endif.
endif.
ENDENHANCEMENT.
