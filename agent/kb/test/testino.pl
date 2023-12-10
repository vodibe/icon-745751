% Predicato per calcolare la media di una lista di numeri
media(Lista, Media) :-
    length(Lista, Lunghezza),
    sum_list(Lista, Somma),
    Media is Somma / Lunghezza.

% Predicato per calcolare la deviazione standard di una lista di numeri
deviazione_standard(Lista, Deviazione) :-
    media(Lista, Media),
    length(Lista, Lunghezza),
    maplist(differenza_quadrata(Media), Lista, DifferenzeQuadrate),
    sum_list(DifferenzeQuadrate, SommaDifferenzeQuadrate),
    Deviazione is sqrt(SommaDifferenzeQuadrate / Lunghezza).

% Predicato ausiliario per calcolare il quadrato della differenza tra due numeri
differenza_quadrata(X, Y, QuadratoDiff) :-
    QuadratoDiff is (X - Y) * (X - Y).

% Predicato per individuare gli outlier in una lista di numeri
individua_outlier(Lista, Outlier) :-
    deviazione_standard(Lista, Deviazione),
    media(Lista, Media),
    SogliaSuperiore is Media + 10 * Deviazione,
    SogliaInferiore is Media - 10 * Deviazione,
    include(outlier(SogliaSuperiore, SogliaInferiore), Lista, Outlier).

% Predicato per verificare se un numero è un outlier
outlier(SogliaSuperiore, SogliaInferiore, Numero) :-
    Numero > SogliaSuperiore, !.
outlier(SogliaSuperiore, SogliaInferiore, Numero) :-
    Numero < SogliaInferiore.