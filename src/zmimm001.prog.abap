* 2000/10/10 mdemeest 4.6B Copied SAPM07DR to ZMIMM001             "UGL
*                          Changed includes:                       "UGL
*                          M07DRTOP to ZNMIM001                    "UGL
*                          M07DRA01 to ZNMIM003                    "UGL
*                          M07DRA03 to ZNMIM002                    "UGL
*                          M07DRAUS to ZNMIM005                    "UGL
* 2013/01/28 btboundy 600 to 605 Upgrade, re-coppied "UGL" code into SAP Standard

*INCLUDE: M07DRTOP.              "Tabellen und Datendeklarationen  "UGL
include ZNMIM001.                                                  "UGL

INCLUDE: M07DRMTA.                   "Zugriffsroutinen für ATAB-Tabellen

INCLUDE: M07DRMMA.                   "Zugriffsroutinen Material

INCLUDE: M07DRMBE.                   "Zugriffsroutine Bestellung

INCLUDE: M07DRMFA.                   "Zugriffsroutine FauftrKopf

INCLUDE: M07DRKON.                   "Zugriffsroutine Kontierung

INCLUDE: M07DRF01.                   "Druck WE Schein  FertAuftr. Vers1

INCLUDE: M07DRF02.                   "Druck WE Schein  FertAuftr. Vers2

INCLUDE: M07DRE01.                   "Druck Wareneingangsschein Vers.1

INCLUDE: M07DRE02.                   "Druck Wareneingangsschein Vers.2

INCLUDE: M07DRE03.                   "Druck Wareneingangsschein Vers.3

*INCLUDE: M07DRA01.              "Druck Warenausgangsschein Vers.1   UGL
include znmim003.                                                   "UGL

INCLUDE: M07DRA02.                   "Druck Warenausgangsschein Vers.2

*INCLUDE: M07DRA03.              "Druck Warenausgangsschein Vers.3   UGL
include znmim002.                                                   "UGL

INCLUDE: M07DRLB3.                   "Druck Warenausg.schein LB Vers.3

INCLUDE: M07DRETI.                   "Druck Etiketten Warenausgang

INCLUDE: M07DRKTO.                   "Druck Mehrfachkontierungsblatt

INCLUDE: M07DRENT.                   "Entries für Druck

INCLUDE: M07DRLOB.                   "Leseroutine Adresse LB-Lieferant

*iNCLUDE: M07DRAUS.                   "Ausgaberoutinen              "UGL
include znmim005.                                                   "UGL

INCLUDE: M07DRSON.                   "sonstige Routinen

* eject
