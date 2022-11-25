:- use_module(library(lists)).

% Almacena: nombre de equipo, posición temporada pasada, seguidores, ciudad, estadio

equipo([arsenal, 5, 20, londres, 'Emirates Stadium']).
equipo([man_city, 1, 14, manchester, 'Estadio de la Ciudad de Manchester']).
equipo([newcastle, 11, 2.3, newcastle, 'St James Park']).
equipo([tottenham, 4, 8.2, londres, 'Tottenham Hotspur Stadium']).
equipo([man_united, 6, 34, manchester, 'Old Trafford']).
equipo([brighton, 9, 0.68, brighton, 'Falmer Stadium']).
equipo([chelsea, 3, 23, londres, 'Stamford Bridge']).
equipo([liverpool, 2, 22, liverpool, 'Anfield']).
equipo([fulham, 18, 0.69, londres, 'Craven Cottage']).
equipo([brighton, 9, 0.68, brighton, 'Falmer Stadium']).
equipo([crystal_palace, 12, 1.3, londres, 'Selhurst Park Stadium']).
equipo([brentford, 12, 0.29, londres, 'Gtech Community Stadium']).
equipo([aston_villa, 14, 2.2, birmingham, 'Villa Park']).
equipo([leicester_city, 8, 2.6, leicester, 'King Power Stadium']).
equipo([west_ham, 7, 2.5, londres, 'Estadio Olimpico de Londres']).
equipo([everton, 16, 2.9, liverpool, 'Goodison Park']).
equipo([bournemouth, 19, 0.64, bournemouth, 'Vitality Stadium']).
equipo([southampton, 15, 1.5, southampton, 'St Mary Stadium']).
equipo([wolves, 10, 1.1, wolverhampton, 'Molineux Stadium']).
equipo([nottm_forest, 20, 0.549, west_bridgford, 'City Ground']).
equipo([leeds_united, 17, 0.99, leeds, 'Elland Road']).


derby([arsenal, tottenham]).
derby([arsenal, chelsea]).
derby([brentford, chelsea]).
derby([brentford, fulham]).
derby([leicester_city, nottm_forest]).
derby([man_city, man_united]).
derby([everton, liverpool]).
derby([liverpool, man_united]).
derby([man_city, liverpool]).
derby([leeds_united, man_united]).
derby([bournemouth, southampton]).
derby([leeds_united, brentford]).
derby([leeds_united, chelsea]).
derby([chelsea, liverpool]).
derby([brighton, crystal_palace]).
derby([arsenal, man_united]).
derby([chelsea, man_united]).

/*Función heurística de partidos.*/
/*Evalúa entre 0 y 1 un partido individual*/

rating_partido([Eq1, Eq2], Rating):-
    es_derby([Eq1, Eq2], Derby),
    seguidores([Eq1, Eq2], Seg),
    dif_lugares([Eq1, Eq2], Dif),
    prom_lugares([Eq1, Eq2], Prom),
    Rating is (((Derby*0.1) + (Seg*0.2) + (Dif*0.3) + (Prom*0.4))).
    /*Evalúe el promedio de lugar en la temporada pasada de ambos equipos*/

rating_jornada(Jornada, Res):-
    suma_jornada(Jornada, Sum),
    prom(Sum, 10, Prom), 
    may_65(Jornada, M65),
    may_50(Jornada, E50),
    if_cont_1(M65, Cont1),
    if_cont_2(E50, Cont2),
    Res is ((((Cont1 + Cont2)/2)*0.6) + (Prom*0.4)).


/*Regresa una lista ordenada del valor de cada jornada considerando la fecha*/

rating_vuelta_lista([], []).
rating_vuelta_lista([Cab|Resto], [CabRes|Cola]):-
    rating_jornada(Cab, Rat),
    get_head(Cab,[[_,_], NumJornada,_,_]),
    check_numjor(NumJornada, Z),
    CabRes is (Rat*Z),
    rating_vuelta_lista(Resto, Cola).   
    

/*Evalúa si el partido es un derby o no*/
es_derby([Eq1, Eq2], Res):-
    (derby([Eq1, Eq2]);
    derby([Eq2, Eq1])) ->
        Res is 1;
        Res is 0.

/*Rating de 0 a 1 de seguidores*/
seguidores([Eq1, Eq2], Res):-
    equipo([Eq1, _, X, _, _]),
    equipo([Eq2, _, Y, _, _]),
    Z is X + Y, 
    Res is (Z*1/57).

/*Diferencia entre lugares*/
/*Evalúa como más relevante entre menor sea la diferencia*/
dif_lugares([Eq1, Eq2], Res):-
    equipo([Eq1, X, _, _, _]),
    equipo([Eq2, Y, _, _, _]),
    abs(X-Y, Z),
    Res is ((20/19) - (Z/19)).

prom_lugares([Eq1, Eq2], Res):-
    equipo([Eq1, X, _, _, _]),
    equipo([Eq2, Y, _, _, _]),
    Res is ((41/39) - ((X+Y)/39)).

/*Contar partidos mayores a 6.5*/
may_65([],0).
may_65([Cab|Cola], Res) :-
    get_head(Cab, Partido),
    rating_partido(Partido, Rat),
    may_65(Cola, Resto),
    (  Rat >= 0.65
    -> Res is Resto + 1
    ;  Res = Resto
    ).

/*Contar partidos entre 5 y 6.5*/
may_50([],0).
may_50([Cab|Cola], Res) :-
    get_head(Cab, Partido),
    rating_partido(Partido, Rat),
    may_50(Cola, Resto),
    (  (Rat >= 0.5)
    -> Res is Resto + 1
    ;  Res = Resto
    ).

if_cont_1(Num, Res):-
    Num >= 1,
    Res is 1.
if_cont_1(Num, Res):-
    Num =:= 0,
    Res is 0.

if_cont_2(Num, Res):-
    Num >= 2,
    Res is 1.
if_cont_2(Num, Res):-
    Num < 2,
    Res is 0.



/*Regresa la suma de los ratings de los 10 partidos de una jornada*/
suma_jornada([], 0).

suma_jornada([Cab|Cola], Res):-
    get_head(Cab, Partido),
    rating_partido(Partido, Rat),
    get_tail(Cab, Hora), /*Hora del partido*/
    dia(Cab, Dia),
    check_hora(Hora, H1),
    check_dia(Dia, D1),
    RatingFinal is (0.8*Rat + (H1 + D1)*0.2),
    suma_jornada(Cola, Resto),
    Res is RatingFinal+Resto.

check_hora(Hora, Res):-
    (  Hora >= 9
    -> Res is 0.5
    ;  Res = 0
    ).

check_dia(Dia, Res):-
    (  Dia == viernes
    -> Res is 0
    ;  Res = 0.5
    ).



/*Función para sacar promedios, en general*/
prom(Suma, Term, Res):-
    Res is Suma/Term.


/*Función de valor absoluto*/
abs(X, X):-
    X >= 0.
abs(X,Y):-
    X<0, 
    Y is (-1*X).

 % Pequeña función para obtener la cabeza y cola de una lista de forma sencilla.
 get_head([A|_],A).
 get_tail(List,B):-
    reverse(List,ListR),
    get_head(ListR,B).

/*Da el día del partido*/

dia([_, _, Dia, _], X):-
    X = Dia.


/*Devuelve 1 si es un número de jornada regular, y 1.5 si es importante*/
check_numjor(NumJornada, Res):-
    NumJornada =:= 1,
    Res is 1.5.

check_numjor(NumJornada, Res):-
    NumJornada =:= 19,
    Res is 1.5.
check_numjor(NumJornada, Res):-
    NumJornada =:= 20,
    Res is 1.5.
check_numjor(NumJornada, Res):-
    NumJornada =:= 38,
    Res is 1.5.
check_numjor(NumJornada, Res):-
    NumJornada =\= 1,
    NumJornada =\= 19,
    NumJornada =\= 20,
    NumJornada =\= 38,
    Res is 1.
