# HRTime
## Założenia aplikacji rejestru czasu pracy

1. Rejestrator czasu pracy:

    - rejestrator `wyświetla godzinę i datę` na LCD przy antenie RFID
  
    - Zielona `dioda` led zaświeci się na chwilę po `prawidłowym odczytaniu` karty
 
    - `Buzzer` zabrzęczy po `prawidłowym odczytaniu` karty
 
    - od strony wejścia zapisuje: <br> 
    `[in, nr_RFID, czas(data i godzina)`<br> 
    `[in, 666, 13-02-2020 6:57]`
 
    - od strony wyjścia zapisuje: <br> 
    `[ou, nr_RFID, czas(data i godzina)`<br> 
    `[ou, 666, 13-02-2020 15:03]`
 
    - `zapis` odbywa się `do bazy` danych na urządzeniu RPi
 
    - w razie `niezarejestrowanej karty brak reakcji` od strony urządzenia
    
    - Każde przyłożenie karty ma być rejestrowane

    - tryb dev - czyli czytanie kart, włączenie tego trybu umożliwia dodawanie po kolei nowych kart bez konieczności używania aplikacji, w aplikacji jedynie uruchamiamy i zakańczamy ten tryb. W razie zczytania TAGu istniejącego już w bazie, wyświetlony zostanie komunikat oraz odpowiedni sygnał. Może dorzucić fizyczny przycisk będący potwierdzeniem startu rejestracji nowych kart? Takie zabezpieczenie on/off trybu dev po aktywacji z aplikacji, by ktoś inny w międzyczasie nie przyłożył karty.

#
2. Aplikacja komputerowa - `Dane w bazie`:
   - Tabela - Rejestr użytkowników: <br>
   `[Nr_akt_osobowych_TETA, Imię, Nazwisko, Dział, Typ_pracownika, aktywny_w_systemie?]`<br>
   `[4269, Aleksander, Sinkowski, TME, Umysłowy, true]`

   - Tabela - Rejestr kart RFID oraz powiązania z numerem akt osobowych w TETA:<br>
   `[Nr_akt_osobowych_TETA, nr_RFID, aktywna_karta?]`<br>
   `[4268, 333, false]`<br>
   `[4269, 666, true]`<br>
   `[4270, empty, false]`

#
3. Aplikacja komputerowa - ` Wyświetlanie`:
   - Dodanie nowej karty do bazy

   - Dodanie nowego pracownika
   
   - Deaktywacja pracownika z możliwością przypisania karty RFID innemu

   - Wyświetlony ma być czas przepracowany każdego dnia, czyli jak ktoś wszedł o 07:10 i wyszedł o 15:00 to przepracowane ma 7h i 50min, analogicznie dla wejścia o 07:00 i wyjścia 15:10, czas przepracowany wykaże 8h i 10min. Dodatkowo można zaznaczyć kolorem brakujące godziny i nadgodziny.

   - `Wszystko` - wyświetlenie pozycji dla `wszystkich` osób, pozycje  wejścia-wyjścia sortując po dacie od najstarszej lub odwrotnie, `osoba po osobie`
  
   - `W okresie dla jednego` - zestawienie wejścia-wyjścia sortując po dacie od najstarszej lub odwrotnie dla osoby `inwidualnej` w okresie `od - do`, przykład: <br>
    `[Aleksander Sinkowski] [13-02-2020] [16-02-2020]`

   - `W okresie dla wielu` - zestawienie wejścia-wyjścia sortując po dacie od najstarszej lub odwrotnie dla `wielu` osób w okresie `od - do`:
     - Możliwość wyboru, dla którego działu
     - Możliwość wybory typu pracownika
     - Dział + Typ

   - `Brak możliwości` wprowadzenia korekty w godzinach.
