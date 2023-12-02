% ----- fatti per stylecheck

% disabilito warning per variabili presenti una sola volta all'interno di una clausola.
:- style_check(-singleton).

% ----- fatti utili in generale

% predicato listfirstelelem. Vero quando FirstElem è il primo elemento della lista.
%listfirstelem([FirstElem|_], FirstElem).

% ----- regole

% l'atomo page è definito (cioè presente nella testa di una clausola) nel file kb_facts.pl
:- discontiguous page/4.

% pagewronglyredirects/1 (predicato)
% Vero quando il primo termine schoolassoc (simbolo di funzione) è riferito a una pagina il cui indirizzo
% fa un redirect NON permanente (cioè con status code != 301) al sito aggiornato.
page_wrongly_redirects(schoolassoc(Url, School_ID)) :- 
    page(schoolassoc(Url, School_ID), _, ndom(NDOM_Nodes, NDOM_Height, _), _),
    NDOM_Height =< 1,
    NDOM_Nodes =< 2.
    

% l'atomo instituteexists è definito (cioè presente nella testa di una clausola) nel file kb_facts.pl
:- discontiguous institute_is_related/3.

% institute_is_related_for_job1/2 (predicato)
% Vero quando il primo termine è l'istituto a cui la scuola (secondo termine) appartiene e inoltre la
% scuola ha un sito con redirect errato. 
institute_is_related_for_job1(institute(Institute_ID, Institute_Name, Institute_Schools_IDs), schoolassoc(Url, School_ID)) :-
    page_wrongly_redirects(schoolassoc(Url, School_ID)),
    institute_is_related(Institute_ID, Institute_Name, School_ID),
    findall(S, institute_is_related(Institute_ID, _, S), Institute_Schools_IDs).


    
    
    
