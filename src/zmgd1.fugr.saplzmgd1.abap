*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LZMGD1TOP.      " Global Data
  INCLUDE LZMGD1UXX.      " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************

* DIESES INCLUDE NICHT MEHR AENDERN!                                 *
* NEUE INCLUDES IN BESTEHENDE INCLUDES AUFNEHMEN!                    *

*------------------------------------------------------------------
*           PBO-Module
*------------------------------------------------------------------
INCLUDE LZMGD1OXX.
*INCLUDE LMGD1OXX.     "zentrale PBO-Module Bildbausteine
INCLUDE LZMGD1O01.
*INCLUDE LMGD1O01.     "PBO-Module für Kurztexthandling
INCLUDE LZMGD1O02.
*INCLUDE LMGD1O02.     "PBO-Module für Steuerhandling
INCLUDE LZMGD1O03.
*INCLUDE LMGD1O03.     "PBO-Module für Verbrauchswerte
INCLUDE LZMGD1O04.
*INCLUDE LMGD1O04.     "PBO-Mdoule Mengeneinheiten
INCLUDE LZMGD1O05.
*INCLUDE LMGD1O05.     "PBO-Module für Prognosewerte
INCLUDE LZMGD1O06.
*INCLUDE LMGD1O06.     "PBO-Module für EAN
INCLUDE LZMGD1O07.
*INCLUDE LMGD1O07.     "PBO-Module für Langtexte
INCLUDE LZMGD1O08.
*INCLUDE LMGD1O08.     "PBO-Module für Table-Control Steuerung
INCLUDE LZMGD1O1K.
*INCLUDE LMGD1O1K.     "PBO-Modul für Klassif.-Subscreen
*------------------------------------------------------------------
*           PAI-Module
*------------------------------------------------------------------
INCLUDE LZMGD1IXX.
*INCLUDE LMGD1IXX.     "zentrale PAI-Module Bildbausteine
INCLUDE LZMGD1IYY.
*INCLUDE LMGD1IYY.     "Gemeinsame PAI-Module Bildbaustein/Trägerprogramm
INCLUDE LZMGD1I01.
*INCLUDE LMGD1I01.     "Prüfmodule Datenbilder  MARA, MAKT (Kopfbaustein)
INCLUDE LZMGD1I02.
*INCLUDE LMGD1I02.     "Prüfmodule Datenbilder  MARC, MARD, MPGD
INCLUDE LZMGD1I03.
*INCLUDE LMGD1I03.     "Prüfmodule Datenbilder  QM-Daten (MARA/MARC)
INCLUDE LZMGD1I04.
*INCLUDE LMGD1I04.     "Prüfmodule Datenbilder  MBEW
INCLUDE LZMGD1I05.
*INCLUDE LMGD1I05.     "Prüfmodule Datenbilder  MFHM
INCLUDE LZMGD1I06.
*INCLUDE LMGD1I06.     "Prüfmodule Datenbilder  MLGN, MLGT
INCLUDE LZMGD1I07.
*INCLUDE LMGD1I07.     "Prüfmodule Datenbilder  MPOP
INCLUDE LZMGD1I08.
*INCLUDE LMGD1I08.     "Prüfmodule Datenbilder  MVKE
INCLUDE LZMGD1I09.
*INCLUDE LMGD1I09.     "Prüfmodule für Kurztexthandling
INCLUDE LZMGD1I10.
*INCLUDE LMGD1I10.     "PAI-Module für Steuerhandling
INCLUDE LZMGD1I11.
*INCLUDE LMGD1I11.     "PAI-Module für Verbrauchswerte
INCLUDE LZMGD1I12.
*INCLUDE LMGD1I12.     "PAI-Module Mengeneinheiten
INCLUDE LZMGD1I13.
*INCLUDE LMGD1I13.     "PAI-Module für Prognosewerte
INCLUDE LZMGD1I14.
*INCLUDE LMGD1I14.     "PAI-Module EAN
INCLUDE LZMGD1I15.
*INCLUDE LMGD1I15.     "PAI-Module für Langtexte
INCLUDE LZMGD1I17.
*INCLUDE LMGD1I17.     "PAI-Module für TC-Steuerung
INCLUDE LZMGD1I7O.
*INCLUDE LMGD1I7O.     "PAI-Module für Klassif.-Subscreen
INCLUDE LZMGD1IHX.
*INCLUDE LMGD1IHX.     "Eingabehilfen Bildbausteine
*------------------------------------------------------------------
*
*           FORM-Routinen
*
*------------------------------------------------------------------
INCLUDE LZMGD1FXX.
*INCLUDE LMGD1FXX.        "zentrale Formroutinen Bildbausteine
INCLUDE LZMGD1FYY.
*INCLUDE LMGD1FYY.        "Gemeinsame Form-Routinen Bildbaustein/Tägerpr.
INCLUDE LZMGD1FSC.
*INCLUDE LMGD1FSC.        "zentrale Blätterroutinen   Bildbausteine
INCLUDE LZMGD1F01.
*INCLUDE LMGD1F01.        "Form-Routinen Kurztexthandling
INCLUDE LZMGD1F02.
*INCLUDE LMGD1F02.        "Form-Routinen Steuerhandling
INCLUDE LZMGD1F03.
*INCLUDE LMGD1F03.        "Form-Routinen I Verbrauchswerte/Prognosewerte
INCLUDE LZMGD1F06.
*INCLUDE LMGD1F06.        "Form-Routinen II Verbrauchswerte/Prognosewerte
INCLUDE LZMGD1F04.
*INCLUDE LMGD1F04.        "Form-Routinen Mengeneinheiten
INCLUDE LZMGD1F05.
*INCLUDE LMGD1F05.        "Form-Routinen EAN

*
INCLUDE LZMGD1FHX.
*INCLUDE LMGD1FHX.       "spezielle Eingabehilfen Bildbausteine
INCLUDE LMGMMFHX.       "allg. Routinen Eingabehilfen
* generierte Form-Routinen für Bildbausteine
 INCLUDE MMMGXGUW.        "Holen der Daten auf den Bildbaustein
 INCLUDE MMMGXSUW.        "Übergeben der Daten vom Bildbaustein
 INCLUDE MMMGXRBD.        "Zus. Vorschlagshandling before  Dialog
 INCLUDE MMMGXRAD.        "Zus. Vorschlagshandling after   Dialog

INCLUDE LZMGD1I7K.
*INCLUDE LMGD1I7K.

*INCLUDE LMGD1F2F.

INCLUDE LZMGD1O1J.
*INCLUDE LMGD1O1J.

INCLUDE LZMGD1I7Q.
*INCLUDE LMGD1I7Q.

INCLUDE LZMGD1OV0.
*INCLUDE LMGD1OV0.

INCLUDE LZMGD1I7T.
*INCLUDE LMGD1I7T.

* DIESES INCLUDE NICHT MEHR AENDERN!                                 *
* NEUE INCLUDES IN BESTEHENDE INCLUDES AUFNEHMEN!                    *
