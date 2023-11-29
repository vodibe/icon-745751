:- style_check(-singleton).

% page/4 (predicato)
page(
    schoolassoc(Url, School_ID),
    details(Width, Height, Load_time_ms, Template_ID, Menu_or, Ungrouped_multim),
    ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks),
    Metric
) :-
    Url,
    School_ID,
    Width,
    Height,
    Load_time_ms,
    Template_ID,
    Menu_or,
    Ungrouped_multim,
    NDOM_Nodes,
    NDOM_Height,
    Metric,
    NDOM_Tasks = [Task1, Task2, Task3, Task4, Task5, Task6, Task7, Task8].


% --- goal 1 ---
% pagewronglyredirects/1 (predicato)
% Vero quando il primo termine (che è un simbolo di funzione) è riferito a una pagina il cui indirizzo
% presente nel dataset fa un redirect NON permanente (cioè con status code != 301) al sito aggiornato
% (che dovrebbe essere la norma). I redirect permanenti sono infatti già gestiti nella fase di preprocessing.
pagewronglyredirects(schoolassoc(Url, School_ID)) :- 
    page(
        schoolassoc(Url, School_ID),
        details(_, Height, _, _, _, _),
        ndom(NDOM_Nodes, NDOM_Height, _),
        _
    ),
    Height == 0,
    NDOM_Height =< 1,
    NDOM_Nodes =< 2.

% per ogni risultato della query pagewronglyredirects(schoolassoc(X, Y)) ottieni una lista di codice scuola
% visto che nel dataset ds3_gt_no_noise non ci sono siti duplicati.

% per ogni codice scuola, aggiungere a una lista il codice istituto riferimento

