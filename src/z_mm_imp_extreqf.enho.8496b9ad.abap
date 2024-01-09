"Name: \PR:SAPLBBP_EXTREQ\FO:SEND_PR_TO_PROCSYSTEM\SE:END\EI
ENHANCEMENT 0 Z_MM_IMP_EXTREQF.
*-----------------------------------------------------------------------
*Modification details                                                  *
*Version No    : 01                                                    *
*Date          : 29/09/2011                                            *
*Modified by   : RBHATT                                                *
*Correction No : EE038                                                 *
*Transport No  : D30K917873 SL:LMM:MM:EE0038_Request Fails Notification*
*Description   : The desired functionality is to send an Email         *
*                notification whenever we delete the Error Queue with  *
*                queue details and PR details.                         *
*-----------------------------------------------------------------------
*Data declaration
  DATA: LWA_TID TYPE  ARFCTID,
       LWA_BANFN_QUEUE TYPE ZMMT_QUEUE.
* Constant declaration
  CONSTANTS : CO_MM_IMP_QUEUE(14) TYPE C VALUE 'Z_MM_IMP_QUEUE'.

* Importing value of TID and senddata from abap memeory
  IMPORT TID TO LWA_TID FROM MEMORY ID CO_MM_IMP_QUEUE.

  LWA_BANFN_QUEUE-BANFN = IS_HEADER-EXT_DEMID.
  LWA_BANFN_QUEUE-ARFCIPID = LWA_TID+0(8).
  LWA_BANFN_QUEUE-ARFCPID = LWA_TID+8(4).
  LWA_BANFN_QUEUE-ARFCTIME = LWA_TID+12(8).
  LWA_BANFN_QUEUE-ARFCTIDCNT = LWA_TID+20(4).
  LWA_BANFN_QUEUE-QNAME = iv_queuename.
  LWA_BANFN_QUEUE-DEST = lv_destination.

* Update database table ZMMT_QUEUE form work area
  INSERT ZMMT_QUEUE FROM LWA_BANFN_QUEUE .

ENDENHANCEMENT.
