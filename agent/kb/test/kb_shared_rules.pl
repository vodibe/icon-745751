% ----- fatti per stylecheck

% disabilito warning per variabili presenti una sola volta all'interno di una clausola.
:- style_check(-singleton).

% ----- fatti utili in generale

% is_list_length/2.
% Vero quando il secondo termine è la lunghezza della lista (primo termine).
is_list_length([], 0).
is_list_length([_ | Tail], Length) :-
    is_list_length(Tail, Length_Tail),
    Length is Length_Tail + 1.



% ----- regole

% l'atomo page è definito (cioè presente nella testa di una clausola) nel file kb_facts.pl
:- discontiguous page/4.
% page(schoolassoc(Url, School_ID), details(Width, Height, Load_time_ms, Template, Menu_or, Ungrouped_multim), ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks), Metric).


% %%%%%% JOB1

% l'atomo institute_has_school compare nei file job1_facts_input.pl e job2_facts_input.pl
:- multifile institute_has_school/2.

% page_wrongly_redirects/1 (predicato)
% Vero quando il primo termine schoolassoc (simbolo di funzione) è riferito a una pagina il cui indirizzo
% fa un redirect NON permanente (cioè con status code != 301) al sito aggiornato.
page_wrongly_redirects(schoolassoc(Url, School_ID)) :- 
    page(schoolassoc(Url, School_ID), _, ndom(NDOM_Nodes, NDOM_Height, _), _),
    NDOM_Height =< 1,
    NDOM_Nodes =< 2.
    
% is_partial_report1/2
% Vero quando il primo termine è l'istituto (id, nome e lista scuole ad esso associate)
% a cui la scuola (secondo termine) appartiene. 
is_partial_report1(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID)) :-
    page_wrongly_redirects(schoolassoc(Url, School_ID)),
    institute_has_school(institute(Institute_ID, Institute_Name), School_ID),
    findall(S, institute_has_school(institute(Institute_ID, _), S), Institute_Schools_IDs).


% %%%%%% JOB2

% is_good_template/1
% Vero quando il primo termine è una lista indicante le pagine per le quali è molto probabile
% abbiano una buona interfaccia.
is_good_template([1, 4, 5, 7]).

% page_needs_improvement/2
% Vero quando il primo termine è associato a una scuola avente pagina che necessita un miglioramento grafico.
page_needs_improvement(schoolassoc(Url, School_ID)) :-
    page_needs_improvement(schoolassoc(Url, School_ID), 15, 3).

page_needs_improvement(schoolassoc(Url, School_ID), Treshold1, Treshold2) :-
    page(schoolassoc(Url, School_ID), details(_, _, _, Template, _, Ungrouped_multim), _, Metric),
    Ungrouped_multim >= Treshold1,
    Metric =< Treshold2,
    is_good_template(Good_Templates),
    \+ member(Template, Good_Templates).
    
% is_partial_report2/2
% Vero quando il primo termine è l'istituto (id, nome e lista scuole ad esso associate)
% a cui la scuola (secondo termine) appartiene. 
is_partial_report2(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID)) :-
    page_needs_improvement(schoolassoc(Url, School_ID)),
    institute_has_school(institute(Institute_ID, Institute_Name), School_ID),
    findall(S, institute_has_school(institute(Institute_ID, _), S), Institute_Schools_IDs).


% %%%%%% JOB3

:- discontiguous school_is_in_place/2.


% page_has_good_metric/1
% Vero quando il primo termine si riferisce a una pagina con buona metrica.
page_has_good_metric(schoolassoc(Url, School_ID)) :-
    page(schoolassoc(Url, School_ID), _, _, Metric),
    Metric > 3.8.

% is_relative_frequency_for_place/2
% Vero quando il secondo termine è la frequenza relativa dei siti con buona metrica dell'area di ricerca Place.
is_relative_frequency_for_place(Place, Relative_Frequency) :-

    findall(_, (school_is_in_place(School_ID, Place), page_has_good_metric(schoolassoc(_, School_ID))), List_Good_In_Place),
    is_list_length(List_Good_In_Place, Numerator),

    findall(_, (school_is_in_place(_, Place)), List_All_In_Place),
    is_list_length(List_All_In_Place, Denominator),
    
    Relative_Frequency is Numerator / Denominator.

% is_rank_of_places/1
% Vero quando Rank indica una lista (ordinata in modo decrescente) di simboli di funzione place_rf
is_rank_of_places(Rank) :-
    findall(X, school_is_in_place(_, X), List_Places_W_Dups),
    setof(Y, member(Y, List_Places_W_Dups), List_Places_WO_Dups),
    %member(Place, List_Places_WO_Dups),

    bagof(place_rf(Place, Relative_Frequency), (member(Place, List_Places_WO_Dups), is_relative_frequency_for_place(Place, Relative_Frequency)), Unordered_Rank),
    predsort(place_order, Unordered_Rank, Rank_Ascendant),
    reverse(Rank_Ascendant, Rank).

% place_order/3
% Criterio per stabilire se il simbolo di funzione place_rf è < o > di un altro.
place_order(<, place_rf(_, Relative_Frequence1), place_rf(_, Relative_Frequence2)) :-
	Relative_Frequence1 =< Relative_Frequence2.
	
place_order(>, place_rf(_, Relative_Frequence1), place_rf(_, Relative_Frequence2)) :-
	Relative_Frequence1 > Relative_Frequence2.




    
    
