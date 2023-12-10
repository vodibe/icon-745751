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
:- discontiguous schoolcontact/6.
% page(schoolassoc(Url, School_ID), details(Width, Height, Load_time_ms, Template, Menu_or, Ungrouped_multim), ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks), Metric).



% %%%%%% JOB1


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

% l'atomo institute_has_school compare nei file job1_clauses.pl e job2_clauses.pl
:- multifile institute_has_school/2.

is_partial_report1(schoolassoc(Url, School_ID), institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs)) :-
    page_wrongly_redirects(schoolassoc(Url, School_ID)),
    institute_has_school(institute(Institute_ID, Institute_Name), School_ID),
    findall(S, institute_has_school(institute(Institute_ID, _), S), Institute_Schools_IDs).




% %%%%%% JOB2

% is_good_template/1
% Vero quando il primo termine è una lista indicante le pagine per le quali è molto probabile
% abbiano una buona interfaccia.
is_good_template([1, 5, 7]).

% page_needs_improvement/2
% Vero quando il primo termine è associato a una scuola avente pagina che necessita un miglioramento grafico.
page_needs_improvement(schoolassoc(Url, School_ID)) :-
    page_needs_improvement(schoolassoc(Url, School_ID), 15, 3).

page_needs_improvement(schoolassoc(Url, School_ID), Treshold1, Treshold2) :-
    page(schoolassoc(Url, School_ID), details(_, _, _, Template, _, Ungrouped_multim), _, Metric),
    Ungrouped_multim >= Treshold1,
    Metric < Treshold2,
    is_good_template(Good_Templates),
    \+ member(Template, Good_Templates).
    
% is_partial_report2/2
% Vero quando il primo termine è l'istituto (id, nome e lista scuole ad esso associate)
% a cui la scuola (secondo termine) appartiene. 
is_partial_report2(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID)) :-
    page_needs_improvement(schoolassoc(Url, School_ID)),
    institute_has_school(institute(Institute_ID, Institute_Name), School_ID),
    findall(S, institute_has_school(institute(Institute_ID, _), S), Institute_Schools_IDs).




% %%%%%% JOB3, JOB4

:- discontiguous school_is_in_place/2.
:- multifile school_is_in_place/2.


% page_has_good_metric/1
% Vero quando il primo termine si riferisce a una pagina con buona metrica.
page_has_good_metric(schoolassoc(Url, School_ID)) :-
    page(schoolassoc(Url, School_ID), _, _, Metric),
    Metric >= 3.8.


% is_relative_frequency_for_place/2
% Vero quando il secondo termine è la frequenza relativa dei siti con buona metrica dell'area di ricerca Place.
is_relative_frequency_for_place(Place, Relative_Frequency) :-

    findall(_, (school_is_in_place(School_ID, Place), page_has_good_metric(schoolassoc(_, School_ID))), List_Good_In_Place),
    is_list_length(List_Good_In_Place, Numerator),

    findall(_, (school_is_in_place(_, Place)), List_All_In_Place),
    is_list_length(List_All_In_Place, Denominator),
    
    % non viene gestito il caso Denominator = 0 perchè prima di eseguire questa regola vengono creati i fatti school_is_in_place.
    Relative_Frequency is Numerator / Denominator.

% is_rank_of_places/1
% Vero quando Rank indica una lista (ordinata in modo decrescente) di simboli di funzione place_rf
is_rank_of_places(Rank) :-
    findall(X, school_is_in_place(_, X), List_Places_W_Dups),
    setof(Y, member(Y, List_Places_W_Dups), List_Places_WO_Dups),

    findall(place_rf(Place, Relative_Frequency), (member(Place, List_Places_WO_Dups), is_relative_frequency_for_place(Place, Relative_Frequency)), Unordered_Rank),
    predsort(place_order, Unordered_Rank, Rank_Ascendant),
    reverse(Rank_Ascendant, Rank).

% place_order/3
% Criterio per stabilire se il simbolo di funzione place_rf è < o > di un altro.
place_order(<, place_rf(_, Relative_Frequence1), place_rf(_, Relative_Frequence2)) :-
	Relative_Frequence1 =< Relative_Frequence2.
	
place_order(>, place_rf(_, Relative_Frequence1), place_rf(_, Relative_Frequence2)) :-
	Relative_Frequence1 > Relative_Frequence2.



% %%%%% JOB 5

%page(schoolassoc(Url, School_ID), details(Width, Height, Load_time_ms, Template, Menu_or, Ungrouped_multim), ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks), Metric).

% is_rtc_tuple/1
% Vero quando il simbolo di funzione rtc è corretto, cioè indicante per la regione italiana Region
% il numero di siti (Count) che usano il template Template_ID.
is_rtc_tuple(rtc(Region, Template_ID, Count)) :-

    findall(
        School_ID,
        (school_is_in_place(School_ID, Region), page(schoolassoc(_, School_ID), details(_, _, _, Template_ID, _, _), _, _)),
        L
    ),
    is_list_length(L, Count).

% rtc_tuples_grouped_by_region/4
% Vero quando RTC_grouped_by_region è un simbolo di funzione rtc_grouped associato alla regione Region.
% rtc_grouped(Region, TC_Rank) ove TC_Rank è la classifica dei template più impiegati nelle scuole della regione.
rtc_tuples_grouped_by_region(List_RTC_Tuples, Region, List_Templates, RTC_grouped_by_region) :-

    findall(
        tc(Template_ID, Count),

        (
            member(Template_ID, List_Templates),
            is_rtc_tuple(rtc(Region, Template_ID, Count))
        ),

        TC_Rank_Unordered
    ),

    predsort(tc_order, TC_Rank_Unordered, TC_Rank_Ascendant),
    reverse(TC_Rank_Ascendant, TC_Rank),

    RTC_grouped_by_region = rtc_grouped(Region, TC_Rank).

% place_order/3
% Criterio per stabilire se il simbolo di funzione tc è < o > di un altro.
tc_order(<, tc(_, Count1), tc(_, Count2)) :-
	Count1 =< Count2.
	
tc_order(>, tc(_, Count1), tc(_, Count2)) :-
	Count1 > Count2.

% most_popular_templates_for_region/1
% Vero quando Template_Rank_For_Each_Region è la lista di predicati rtc_grouped.
most_popular_templates_for_region(Template_Rank_For_Each_Region) :-

    findall(X, school_is_in_place(_, X), List_Regions_W_Dups),
    setof(R, member(R, List_Regions_W_Dups), List_Regions_WO_Dups),

    findall(TT, page(_, details(_, _, _, TT, _, _), _, _), List_Templates_W_Dups),
    setof(T, member(T, List_Templates_W_Dups), List_Templates_WO_Dups),

    findall(
        rtc(Region, Template_ID, Count),
        (
            member(Region, List_Regions_WO_Dups),
            member(Template_ID, List_Templates_WO_Dups),
            is_rtc_tuple(rtc(Region, Template_ID, Count))
        ),
        List_RTC_Tuples
    ),

    findall(
        Z,
        (
            member(Region1, List_Regions_WO_Dups),
            rtc_tuples_grouped_by_region(List_RTC_Tuples, Region1, List_Templates_WO_Dups, Z)
        ),
        Template_Rank_For_Each_Region
    ).

    %page(schoolassoc(), details, ndom, metric)
    %school_geofact(ID, city, province, region)




% %%%%% JOB 6

:- use_module(library(plstat)).

%page(schoolassoc(Url, School_ID), details(Width, Height, Load_time_ms, Template, Menu_or, Ungrouped_multim), ndom(NDOM_Nodes, NDOM_Height, NDOM_Tasks), Metric).


% value_is_outlier/3
% Vero quando Value non è compreso nell'intervallo [Lower_Bound, Upper_Bound]
value_is_outlier(Value, Lower_Bound, Upper_Bound) :-
    Value =< Lower_Bound;
    Value >= Upper_Bound.

% feature_is_outlier/4
% Vero quando il valore della feature non è ..........................
feature_is_outlier(Value, List_Feature_Asc, P1_, P2_) :-

    percentile(List_Page_Load_Time_Asc, P1_, P1),
    percentile(List_Page_Load_Time_Asc, P2_, P2),
    Iqr is P2 - P1,
    Lower_Bound is P1 - (1.5 * Iqr),
    Upper_Bound is P2 + (1.5 * Iqr),

    value_is_outlier(Value, Lower_Bound, Upper_Bound).


% page_is_outlier/2
% Vero quando la pagina (primo argomento) ha almeno una feature avente valore outlier, tenendo conto che l'insieme
% di tutti i valori assunti dalle pagine del ds (per ciascuna feature) sono presenti nel secondo argomento. (lista di liste)
page_is_outlier(
    page(schoolassoc(Url, School_ID), details(Width, Height, Load_time_ms, Template, Menu_or, Ungrouped_multim), ndom(NDOM_Nodes, NDOM_Height, [Task1, Task2, Task3, Task4, Task5, Task6, Task7, Task8]), Metric),
    [List_Load_time_ms_Asc, List_Width_Asc, List_Height_Asc, List_NDOM_Nodes_Asc, List_Task1_Asc, List_Task2_Asc, List_Task3_Asc, List_Task4_Asc, List_Task5_Asc, List_Task6_Asc, List_Task7_Asc, List_Task8_Asc]
) :-
    feature_is_outlier(Load_time_ms, List_Load_time_ms_Asc, 25, 75);
    feature_is_outlier(Width, List_Width_Asc, 25, 75);
    feature_is_outlier(Height, List_Height_Asc, 25, 75);
    feature_is_outlier(NDOM_Nodes, List_NDOM_Nodes_Asc, 25, 75);
    feature_is_outlier(Task1, List_Task1_Asc, 25, 75);
    feature_is_outlier(Task2, List_Task2_Asc, 25, 75);
    feature_is_outlier(Task3, List_Task3_Asc, 25, 75);
    feature_is_outlier(Task4, List_Task4_Asc, 25, 75);
    feature_is_outlier(Task5, List_Task5_Asc, 25, 75);
    feature_is_outlier(Task6, List_Task6_Asc, 25, 75);
    feature_is_outlier(Task7, List_Task7_Asc, 25, 75);
    feature_is_outlier(Task8, List_Task8_Asc, 25, 75).


% are_detected_outliers/1
% Vero quando Outliers è la lista di tutte le pagine outlier del dataset.
are_detected_outliers(Outliers) :-

    findall(Load_time_ms, page(_, details(_, _, Load_time_ms, _, _, _), _, _), List_Load_time_ms),
    findall(Width, page(_, details(Width, _, _, _, _, _), _, _), List_Width),
    findall(Height, page(_, details(_, Height, _, _, _), _, _), List_Height),
    findall(NDOM_Nodes, page(_, _, ndom(NDOM_Nodes, _, _), _), List_NDOM_Nodes),
    findall(Task1, page(_, _, ndom(_, _, [Task1, _, _, _, _, _, _, _]), _), List_Task1),
    findall(Task2, page(_, _, ndom(_, _, [_, Task2, _, _, _, _, _, _]), _), List_Task2),
    findall(Task3, page(_, _, ndom(_, _, [_, _, Task3, _, _, _, _, _]), _), List_Task3),
    findall(Task4, page(_, _, ndom(_, _, [_, _, _, Task4, _, _, _, _]), _), List_Task4),
    findall(Task5, page(_, _, ndom(_, _, [_, _, _, _, Task5, _, _, _]), _), List_Task5),
    findall(Task6, page(_, _, ndom(_, _, [_, _, _, _, _, Task6, _, _]), _), List_Task6),
    findall(Task7, page(_, _, ndom(_, _, [_, _, _, _, _, _, Task7, _]), _), List_Task7),
    findall(Task8, page(_, _, ndom(_, _, [_, _, _, _, _, _, _, Task8]), _), List_Task8),

    sort(0, @=<, List_Load_time_ms, List_Load_time_ms_Asc),
    sort(0, @=<, List_Width, List_Width_Asc),
    sort(0, @=<, List_Height, List_Height_Asc),
    sort(0, @=<, List_NDOM_Nodes, List_NDOM_Nodes_Asc),
    sort(0, @=<, List_Task1, List_Task1_Asc),
    sort(0, @=<, List_Task2, List_Task2_Asc),
    sort(0, @=<, List_Task3, List_Task3_Asc),
    sort(0, @=<, List_Task4, List_Task4_Asc),
    sort(0, @=<, List_Task5, List_Task5_Asc),
    sort(0, @=<, List_Task6, List_Task6_Asc),
    sort(0, @=<, List_Task7, List_Task7_Asc),
    sort(0, @=<, List_Task8, List_Task8_Asc),

    findall(
        page(schoolassoc(Url1, School_ID1), details(Width1, Height1, Load_time_ms1, Template1, Menu_or1, Ungrouped_multim1), ndom(NDOM_Nodes1, NDOM_Height1, [Task11, Task21, Task31, Task41, Task51, Task61, Task71, Task81]), Metric1),
        (
            page(schoolassoc(Url1, School_ID1), details(Width1, Height1, Load_time_ms1, Template1, Menu_or1, Ungrouped_multim1), ndom(NDOM_Nodes1, NDOM_Height1, [Task11, Task21, Task31, Task41, Task51, Task61, Task71, Task81]), Metric1),
            page_is_outlier(
                page(schoolassoc(Url1, School_ID1), details(Width1, Height1, Load_time_ms1, Template1, Menu_or1, Ungrouped_multim1), ndom(NDOM_Nodes1, NDOM_Height1, [Task11, Task21, Task31, Task41, Task51, Task61, Task71, Task81]), Metric1),
                [List_Load_time_ms_Asc, List_Width_Asc, List_Height_Asc, List_NDOM_Nodes_Asc, List_Task1_Asc, List_Task2_Asc, List_Task3_Asc, List_Task4_Asc, List_Task5_Asc, List_Task6_Asc, List_Task7_Asc, List_Task8_Asc]
            )
        ),
        Outliers_W_Dups
    ),
    setof(O, member(O, Outliers_W_Dups), Outliers).








    
    
