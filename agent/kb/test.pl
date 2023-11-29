% disattivo warning per variabili che compaiono solo una volta nella clausola.
:- style_check(-singleton).

% islist/1 (predicato)
% Vero quando il suo termine è semanticamente una lista. 
islist([]).
islist([_ | Tail]) :- islist(Tail).

% listlen/2 (predicato)
% Vero quando il primo termine è semanticamente una lista e la sua lunghezza è pari al secondo termine.
listlen([], 0).
listlen([_ | Tail], L) :-
    listlen(Tail, LT),
    L is LT + 1. 

% page/9 (predicato)
% Vero quando tutti i termini sono semanticamente riferiti alle caratteristiche della pagina.
% Si suppone che la correttezza semantica per tutti i termini (tranne NDOM_Tasks) sia garantita.
% Il termine NDOM_tasks deve essere una lista di 8 elementi.
page(
    schoolassoc(Url, School_ID),
    Width,
    Height,
    Load_time_ms,
    Template_ID,
    Menu_or,
    Ungrouped_multim,
    ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks),
    Metric
) :-
    islist(NDOM_Tasks),
    listlen(NDOM_Tasks, 8).
    
% pagewronglyredirects/1 (predicato)
% Vero quando il primo termine (che è un simbolo di funzione) è riferito a una pagina
% il cui indirizzo che è presente nel dataset fa un redirect non standard al sito aggiornato.
% "Non standard" significa che il server non fa un redirect permanente (status code 301), ma restituisce
% un messaggio HTTP con status code diverso. I redirect permanenti sono infatti già gestiti nella fase di preprocessing.
pagewronglyredirects(schoolassoc(Url, School_ID)) :- 
    page(
        schoolassoc(Url, School_ID),
        _,
        Height,
        _,
        _,
        _,
        _,
        ndom(NDOM_Nodes, NDOM_Height, _),
        _
    ),
    Height == 0,
    NDOM_Height =< 1,
    NDOM_Nodes =< 2.
