% ----- fatti per stylecheck

% disabilito warning per variabili presenti una sola volta all'interno di una clausola.
:- style_check(-singleton).

% ----- fatti utili in generale

% is_list_length/2. Vero quando il secondo termine è la lunghezza della lista (primo termine).
is_list_length([], 0).
is_list_length([_ | Tail], Length) :-
    is_list_length(Tail, Length_Tail),
    Length is Length_Tail + 1.

% ----- regole

% l'atomo page è definito (cioè presente nella testa di una clausola) nel file kb_facts.pl
:- discontiguous page/4.

% l'atomo institute_is_related_for_job1 è definito separatamente.
:- discontiguous institute_is_related_for_job1/3.

% page_wrongly_redirects/1 (predicato)
% Vero quando il primo termine schoolassoc (simbolo di funzione) è riferito a una pagina il cui indirizzo
% fa un redirect NON permanente (cioè con status code != 301) al sito aggiornato.
page_wrongly_redirects(schoolassoc(Url, School_ID)) :- 
    page(schoolassoc(Url, School_ID), _, ndom(NDOM_Nodes, NDOM_Height, _), _),
    NDOM_Height =< 1,
    NDOM_Nodes =< 2.
    
% is_valid_report_for_job1/2 (predicato)
% Vero quando il primo termine è l'istituto a cui la scuola (secondo termine) appartiene e inoltre la
% scuola ha un sito con redirect errato. 
is_valid_report_for_job1(institute(Institute_ID, Institute_Name, Institute_Schools_IDs), schoolassoc(Url, School_ID)) :-
    page_wrongly_redirects(schoolassoc(Url, School_ID)),
    institute_is_related_for_job1(Institute_ID, Institute_Name, School_ID),
    findall(S, institute_is_related_for_job1(Institute_ID, _, S), Institute_Schools_IDs).



:- discontiguous institute_is_related_for_job2/3.

% is_good_template/1 (predicato)
% Vero quando il primo termine è una lista indicante le pagine per le quali è molto probabile
% abbiano una buona interfaccia.
is_good_template([1, 4, 5, 7]).

% page_needs_graphical_improvement/2 (predicato)
% Vero quando il primo termine è associato a una scuola avente pagina che necessita un miglioramento grafico.
page_needs_graphical_improvement(schoolassoc(Url, School_ID)) :-
    page_needs_graphical_improvement(schoolassoc(Url, School_ID), 20, 3).

page_needs_graphical_improvement(schoolassoc(Url, School_ID), Treshold1, Treshold2) :-
    page(schoolassoc(Url, School_ID), details(_, _, _, Template, _, Ungrouped_multim), _, Metric),
    Ungrouped_multim >= Treshold1,
    Metric =< Treshold2,
    is_good_template(Good_Templates),
    \+ member(Template, Good_Templates).

% is_valid_report_for_job2/2 (predicato)
% Vero quando il primo termine è l'istituto a cui la scuola (secondo termine) appartiene e inoltre la
% scuola ha un sito da migliorare. 
is_valid_report_for_job2(institute(Institute_ID, Institute_Name, Institute_Schools_IDs), schoolassoc(Url, School_ID)) :-
    page_needs_graphical_improvement(schoolassoc(Url, School_ID)),
    institute_is_related_for_job2(Institute_ID, Institute_Name, School_ID),
    findall(S, institute_is_related_for_job2(Institute_ID, _, S), Institute_Schools_IDs).

% page(schoolassoc(Url, School_ID), details(Width, Height, Load_time_ms, Template, Menu_or, Ungrouped_multim), ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks), Metric).


%%%%%%%%%%%%%%%%%
:- discontiguous page_has_geoinfo/2.

page_has_good_metric(schoolassoc(Url, School_ID)) :-
    page(schoolassoc(Url, School_ID), _, _, Metric),
    Metric > 3.8.



is_relative_frequency_for_region(Region, Relative_Frequency) :-
    findall(_, (page_has_geoinfo(schoolassoc(_, School_ID), Region), page_has_good_metric(schoolassoc(_, School_ID))), List_Good_Metric),
    is_list_length(List_Good_Metric, Numerator),
    findall(_, (page_has_geoinfo(_, Region)), List_Pages_Region),
    is_list_length(List_Pages_Region, Denominator),
    Relative_Frequency is Numerator / Denominator.

is_rank_of_regions(Rank) :-
    findall(Region-Relative_Frequency, (is_relative_frequency_for_region(Region, Relative_Frequency)), Lista),
    predsort(compare_percentuale, Lista, ClassificaReversed),
    reverse(ClassificaReversed, Classifica),
    length(Classifica, Len),
    Len >= 3,
    Classifica = [_, _, _].

    
    
