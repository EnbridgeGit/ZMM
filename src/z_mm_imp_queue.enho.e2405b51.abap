"Name: \FU:ARFC_INIT\SE:END\EI
ENHANCEMENT 0 Z_MM_IMP_QUEUE.
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
* Data decleration
CONSTANTS : CO_MM_IMP_QUEUE(14) TYPE C VALUE 'Z_MM_IMP_QUEUE'.


* Export value of send status and TID into abap memory

EXPORT TID TO MEMORY ID CO_MM_IMP_QUEUE.

ENDENHANCEMENT.
