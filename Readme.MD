# Verwendung

Dies ist ein kleines Hilfsprogramm zur Aufbereitung der Trades in IBKR, so dass man die Ausgabe zur Prüfung der 
eigenen Steuererklärung nutzen kann.

Dies stellt keine Steuerberatung dar. Dieses Tool ist nicht von Interactive Brokers und steht in keiner Verbindung zu IBKR.
Es wird keine Haftung übernommen.

## Referenzkurse

Hier: die Euro-Referenzkurse runterladen:
https://www.bundesbank.de/de/statistiken/wechselkurse/-/wechselkursstatistik-804110
Unter II. Euro-Referenzkurse der Europäischen Zentralbank
und darunter 1. Monatsendstände und Tageskurse
Gibt es ein PDF. Scrollt man ganz ans Ende zum USD, kann man auf die Spalte
klicken, man landet bei einem Link der so ähnlich wie der folgende aussieht:

https://www.bundesbank.de/dynamic/action/de/statistiken/zeitreihen-datenbanken/zeitreihen-datenbank/723452/723452?tsId=BBEX3.D.USD.EUR.BB.AC.000

Aktuell werden die ersten paar Zeilen und die letzten Zeilen weggeworfen, da nicht benötigt. 
Sollte sich das ändern, muss man den Code anpassen.

## CSV-Datei bei IBKR herunterladen

Bei Interactive-Brokers eine Flex-Query definieren zu Trades.
Ausführungen auswählen.
Diese beinhaltet bei mir Currency, Date/Time,
Trade Date, Quantity, TradePrice, Trade Money, Taxes, IB Commission,
IB Commission Currency, Net Cash, Buy/Sell, Order Time, Order Type,
Symbol, Change In Quantity.

Andere Einstellungen:

Modelle: Optional
Format: CSV
Überschrift und Trailer-Daten miteinbeziehen? Nein
Spaltenüberschriften miteinbeziehen? Ja
Abschnittscode und Zeilenbeschriftung miteinbeziehen? Nein
Zeitraum Jahresbeginn bis heute

Datumsformat yyyyMMdd
Zeitformat HHmmss
Datum/Uhrzeit-Trennzeichen ; (Strichpunkt)
Gewinn und Verlust Standard
Gelöschte Transaktionen miteinbeziehen? Nein
Wechselkurse miteinbeziehen? Nein
Prüfpfadfelder einbeziehen?  Nein
Konto-Alias anstelle der Konto-ID anzeigen? Nein
Breakout by Day? Nein

Bei mir gab es auch noch einige cancelled orders, d.h. Zeilen, in denen
Ca. stand. Diese löscht der Code automatisch.

## Anschließend

Anschließend kann man das jar etwa wiefolgt aufrufen:

java -jar steuermann.jar /home/username/steuer/BBEX3.D.USD.EUR.BB.AC.000.csv /home/username/steuer/Test_Steuermann.csv 1000 /home/username/steuer/gesamt.csv

Anschließend erhält man eine Reihe von Trades in EUR, die man in Excel importieren kann. 
Nach Belieben kann man anschließend filtern, um die Anlageklassen zu unterscheiden und Summen bilden.

## TODOs

- Dividende
- ETFs
- Futures
- Funktioniert das jahresübergreifend?
- Zinsen?  
- so korrekt?  
- Optionen richtig prüfen  
- andere Währungen als USD und EUR
- Versionen hochziehen  
- ...

## License

Copyright © 2021

Licensed GPLv3
