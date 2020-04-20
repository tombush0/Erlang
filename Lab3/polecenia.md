

## Laboratorium nr 3 - programowanie procesów w Erlangu

#### Przed zajęciami

<div class="level4">

Zapoznaj się z:

1.  <div class="li">Tworzeniem procesów.</div>

2.  <div class="li">Rejestracja procesów.</div>

3.  <div class="li">Komunikacja między procesami.</div>

</div>

#### Cele zajęć

<div class="level4">

1.  <div class="li">Poszerzenie wiedzy dotyczącej Erlangowych procesów.</div>

2.  <div class="li">Poznanie sposobów łączenia węzłów zdalnych</div>

Celem zajęć jest zapoznanie się z procesami w Erlangu, poznanie mechanizmów ich tworzenia, rejestracji, i komunikacji.

</div>

#### Przebieg zajęć

##### Ping - Pong

<div class="level5">

1.  <div class="li">Napisz moduł pingpong, który będzie eksportował funkcje:</div>

    *   <div class="li">_start/0_, która utworzy 2 procesy i zarejestruje je pod nazwami ping i pong,</div>

    *   <div class="li">_stop/0_, która zakończy oba procesy,</div>

    *   <div class="li">_play/1_, która wyśle wiadomość z liczbą całkowitą N do procesu ping.</div>

2.  <div class="li">Po otrzymaniu wiadomości, proces ping ma rozpocząć wymianę N wiadomości z procesem pong. Przy odebraniu każdej wiadomości procesy mają wypisać na standardowe wyjście informację o przebiegu odbijania.</div>

3.  <div class="li">Dla zwiększenia czytelności działania warto użyć funkcji _timer:sleep(Milisec)_.</div>

4.  <div class="li">Procesy ping i pong powinny samoczynnie kończyć działanie po 20 sekundach bezczynności.</div>

5.  <div class="li">Zmodyfikuj proces ping by przechowywał stan - sumę wszystkich liczb z komunikatów, które otrzymał. Dodaj tę sumę do informacji wypisywanej po odebraniu komunikatu.</div>

</div>

##### Obliczenia równoległe

<div class="level5">

Dane są 2 listy punktów na płaszczyźnie 2D:

1.  <div class="li">lista lokalizacji paczkomatów, których jest np. 1000</div>

2.  <div class="li">lista rozmieszczenia osób, które chcą odwiedzić paczkomat - jest ich np. 10000</div>

Każda osoba chce wiedzieć, który paczkomat ma najbliżej.

1.  <div class="li">Przygotuj dane - 2 listy par liczb całkowitych z zakresu 0-10000, wygenerowane losowo. Użyj list comprehensions.</div>

2.  <div class="li">Napisz funkcję _findMyParcelLocker(PersonLocation, LockerLocations)_ wyszukującą najbliższy paczkomat dla danej osoby.</div>

Wersja sekwencyjna:

1.  <div class="li">Uruchom funkcję _findMyParcelLocker_ dla każdej osoby, wyniki zbierz w liście par.</div>

Wersja bardzo równoległa:

1.  <div class="li">Uruchom funkcję _findMyParcelLocker_ w osobnym procesie dla każdej osoby. Będzie to wymagało dodania mechanizmu odsyłania wyniku do rodzica.</div>

2.  <div class="li">Odbierz komunikaty, zbierz wszystkie wyniki od dzieci w liście par.</div>

3.  <div class="li">Porównaj czas obliczeń.</div>

Wersja mniej równoległa:

1.  <div class="li">Uruchom tyle procesów wywołujących funkcję _findMyParcelLocker_ ile rdzeni ma twój procesor. Każdy powinien dostać odpowiednią część zadania - podzbiór osób. Wyniki możesz odesłać w osobnych komunikatach, dzięki czemu wcześniejszy kod agregujący dane zadziała.</div>

2.  <div class="li">Porównaj czas obliczeń.</div>

</div>

##### Distributed Erlang

<div class="level5">

TEN PUNKT POMIJAMY - wrócimy do niego po przywróceniu normalnego trybu zajęć.

<del>W tej części trzeba będzie skorzystać z maszyn w laboratorium. - uruchom węzeł erlanga, spróbuj połączyć się z węzłem uruchomionym przez prowadzącego * przyda się funkcja net_adm:ping(Node) oraz BIF nodes(). - napisz… szczegóły na laboratorium…</del>

</div>

##### Serwer zanieczyszczeń

<div class="level5">

1.  <div class="li">Zaimplementuj moduł _pollution_server_, który będzie startował proces obsługujący funkcjonalność modułu _pollution_. Powinien działać analogicznie do serwera zmiennej globalnej - o bogatszej funkcjonalności.</div>

2.  <div class="li">Dodatkowe funkcje eksportowane: _start/0_ i _stop/0_.</div>

3.  <div class="li">Dodatkowa funkcja: _init/0_, która będzie wykonywana już w kontekście nowego procesu.</div>

4.  <div class="li">Serwer powinien dostarczyć funkcje analogiczne do modułu _pollution_ - ale każda z nich będzie miała o jeden argument mniej. Serwer ma wołać funkcje z modułu _pollution_.</div>

</div>

##### Zadanie domowe

<div class="level5">

1.  <div class="li">Dokończ moduł _pollution_server_</div>

2.  <div class="li">Napisz testy modułu _pollution_ i _pollution_server_.</div>

