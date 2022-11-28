:- use_module(library(lists)).
:- dynamic to_be_moved/1.
:- dynamic partido/1.
:- dynamic partido_aux/1.
:- dynamic partido_aux_dos/1.
:- dynamic jornada/1.
:- dynamic horario/1.
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
/*Hola*/
% Almacena: nombre de equipo, posición temporada pasada, seguidores, ciudad, estadio

equipo([arsenal, 5, 20, londres, 'Emirates Stadium']).
equipo([man_city, 1, 14, manchester, 'Estadio de la Ciudad de Manchester']).
equipo([newcastle, 11, 2.3, newcastle, 'St James Park']).
equipo([tottenham, 4, 8.2, londres, 'Tottenham Hotspur Stadium']).
equipo([man_united, 6, 34, manchester, 'Old Trafford']).
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


partido([[man_city, brentford], 16, viernes, 5]).
partido([[liverpool, southampton], 16, viernes, 5]).
partido([[bournemouth, everton], 16, viernes, 5]).
partido([[west_ham, leicester_city], 16, viernes, 5]).
partido([[tottenham, leeds_united], 16, viernes, 5]).
partido([[nottm_forest, crystal_palace], 16, viernes, 5]).
partido([[newcastle, chelsea], 16, viernes, 5]).
partido([[wolves, arsenal], 16, viernes, 5]).
partido([[brighton, aston_villa], 16, viernes, 5]).
partido([[fulham, man_united], 16, viernes, 5]). 

partido([[brentford, tottenham], 17, viernes, 5]).
partido([[southampton, brighton], 17, viernes, 5]).
partido([[leicester_city, newcastle], 17, viernes, 5]).
partido([[crystal_palace, fulham], 17, viernes, 5]).
partido([[everton, wolves], 17, viernes, 5]).
partido([[aston_villa, liverpool], 17, viernes, 5]).
partido([[arsenal, west_ham], 17, viernes, 5]).
partido([[chelsea, bournemouth], 17, viernes, 5]).
partido([[man_united, nottm_forest], 17, viernes, 5]).
partido([[leeds_united, man_city], 17, viernes, 5]).


/*Función que te regresa la temporada como una lista */

temporada_lista(Lista):-
    assert_jornadas,
    findall(X, jornada(X), Lista).

/*Agrega todas las jornadas como auxiliares*/
assert_jornadas:-
    assert_jornadas(16).

assert_jornadas(18):-!.
assert_jornadas(Cont):-
    partido([_, Cont,_,_]),
    jornada_a_lista(Cont, Jornada),
    assert(jornada(Jornada)),
    Z is (Cont+1),
    assert_jornadas(Z).




/*Devuelve jornada como lista*/
jornada_a_lista(NumJornada, Lista):-
    dame_partidos_jornada(NumJornada, Partidos),
    Lista = Partidos.

aux_vuelta(X):-
    jornada_a_lista(16,Y),
    jornada_a_lista(17,Z),
    X = [Y,Z].

/*Función heurística de partidos.*/
/*Evalúa entre 0 y 1 un partido individual*/

rating_partido([Eq1, Eq2], Rating):-
    es_derby([Eq1, Eq2], Derby),
    seguidores([Eq1, Eq2], Seg),
    dif_lugares([Eq1, Eq2], Dif),
    prom_lugares([Eq1, Eq2], Prom),
    Rating is (((Derby*0.1) + (Seg*0.2) + (Dif*0.3) + (Prom*0.4))).

/*Evalúa entre 0 y 1 una jornada*/
/*Recibe número de la jornada y regresa el rating*/
rating_jornada_numjornada(NumJornada, Res):-
    jornada_a_lista(NumJornada, Jornada),
    suma_jornada(Jornada, Sum),
    prom(Sum, 10, Prom), 
    may_65(Jornada, M65),
    may_50(Jornada, E50),
    if_cont_1(M65, Cont1),
    if_cont_2(E50, Cont2),
    partido_rep_horario(Jornada, Rep),
    AuxRes is ((((Cont1 + Cont2)/2)*0.6) + (Prom*0.4)),
    Res is ((AuxRes + Rep)/2).



/*Regresa una lista ordenada del valor de cada jornada considerando la fecha*/

rating_vuelta(Vuelta, Res):-
    rating_vuelta_lista(Vuelta, Lista),
    sumlist(Lista, Suma),
    length(Lista, Len),
    Res is ((Suma)/Len).

rating_vuelta_lista([], []):- !.
rating_vuelta_lista([Cab|Resto], [CabRes|Cola]):-
    rating_vuelta_lista(Resto, Cola),
    rating_jornada(Cab, Rat),
    get_head(Cab,[[_,_], NumJornada,_,_]),
    check_numjor(NumJornada, Z),
    CabRes is (Rat*Z).
      

rating_jornada(Jornada, Res):-
    suma_jornada(Jornada, Sum),
    prom(Sum, 10, Prom), 
    may_65(Jornada, M65),
    may_50(Jornada, E50),
    if_cont_1(M65, Cont1),
    if_cont_2(E50, Cont2),
    partido_rep_horario(Jornada, Rep),
    AuxRes is ((((Cont1 + Cont2)/2)*0.6) + (Prom*0.4)),
    Res is ((AuxRes + Rep)/2). 

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
    W is X-Y,
    abs(W, Z),
    Res is ((20/19) - (Z/19)).

prom_lugares([Eq1, Eq2], Res):-
    equipo([Eq1, X, _, _, _]),
    equipo([Eq2, Y, _, _, _]),
    Res is ((41/39) - ((X+Y)/39)).

/*Contar partidos mayores a 6.5*/
may_65([],0):-!.
may_65([Cab|Cola], Res) :-
    get_head(Cab, Partido),
    rating_partido(Partido, Rat),
    may_65(Cola, Resto),
    (  Rat >= 0.65
    -> Res is Resto + 1
    ;  Res = Resto
    ).

/*Contar partidos entre 5 y 6.5*/
may_50([],0):-!.
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
    Res is 1,
    !.
if_cont_1(Num, Res):-
    Num =:= 0,
    Res is 0.

if_cont_2(Num, Res):-
    Num >= 2,
    Res is 1,
    !.

if_cont_2(Num, Res):-
    Num < 2,
    Res is 0.



/*Regresa la suma de los ratings de los 10 partidos de una jornada*/
suma_jornada([], 0):-!.

suma_jornada([Cab|Cola], Res):-
    suma_jornada(Cola, Resto),
    get_head(Cab, Partido),
    rating_partido(Partido, Rat),
    get_tail(Cab, Hora), /*Hora del partido*/
    dia(Cab, Dia),
    check_hora(Hora, H1),
    check_dia(Dia, D1),
    RatingFinal is (0.8*Rat + (H1 + D1)*0.2),
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
    X >= 0,
    !.
abs(X,Y):-
    Y is (-1*X).

 % Pequeña función para obtener la cabeza y cola de una lista de forma sencilla.
 get_head([A|_],A).
 get_tail(List,B):-
    reverse(List,ListR),
    get_head(ListR,B).

/*Da el día del partido*/

dia([_, _, Dia, _], X):-
    X = Dia.

hora([_, _,_, Hora], X):-
    X = Hora.


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


/*Encuentra la mejor jornada de una vuelta*/

mejor_jornada([Cab], Cab):-!.
mejor_jornada([CabVuelta|ColaVuelta], Mejor):-
    mejor_jornada(ColaVuelta, MejorCola),
    mejor_entre_jornadas(CabVuelta, MejorCola, MejActual),
    Mejor = MejActual.


/*Devuelve la mejor jornada entre dos jornadas*/
mejor_entre_jornadas(Jorn1, Jorn2, Jorn1):-
    rating_jornada(Jorn1, RatJ1),
    rating_jornada(Jorn2, RatJ2),
    RatJ1>RatJ2,
    !.

mejor_entre_jornadas(_,Jorn2, Jorn2).


/*Encuentra la peor jornada de una vuelta*/

peor_jornada([Cab], Cab).
peor_jornada([CabVuelta|ColaVuelta], Peor):-
    peor_jornada(ColaVuelta, PeorCola),
    peor_entre_jornadas(CabVuelta, PeorCola, PeorActual),
    Peor = PeorActual.


/*Devuelve la mejor jornada entre dos jornadas*/
peor_entre_jornadas(Jorn1, Jorn2, Jorn1):-
    rating_jornada(Jorn1, RatJ1),
    rating_jornada(Jorn2, RatJ2),
    RatJ1<RatJ2,
    !.

peor_entre_jornadas(_,Jorn2, Jorn2).

/*Encuentra el peor partido dado una jornada*/

peor_partido([Partido], Partido).
peor_partido([CabJor|ColaJor], PeorPartido):-
    peor_partido(ColaJor, PeorPartidoCola),
    peor_entre_partidos(CabJor, PeorPartidoCola, PeorPartidoActual),
    PeorPartido = PeorPartidoActual.

/*Da el peor de dos partidos*/
peor_entre_partidos(Part1, Part2, Part1):-
    rating_partido(Part1, RatP1),
    rating_partido(Part2, RatP2),
    RatP1<RatP2,
    !.

peor_entre_partidos(_,Part2, Part2).


/*Dada una jornada, regresa el número de la jornada*/
num_jornada([[_,X|_]|_], X).

/*Regresa un equipo de un partido*/
equipo_partido([[X,_]|_],X).


/*Dada una jornada, regresa 0 si al menos un partido se repite en hora y día, y 1 si no*/

partido_rep_horario(Jornada, X):-
    assert_horario(Jornada),
    findall(Z, distinct(horario(Z)), ListaDistinct),
    length(ListaDistinct, Len),
    (  Len < 10
    -> X is 0
    ;  X = 1
    ),
    retractall(horario(_)).


assert_horario([]):-!.
assert_horario([Cab|Cola]):-
    dia(Cab, Dia),
    hora(Cab, Hora),
    asserta(horario([Dia,  Hora])),
    assert_horario(Cola).

/*MUTACIÓN*/

/*Cambia la fecha y hora de un partido en una jornada dada*/
/*Checa que no haya más de un partido a la misma hora y mismo día*/
/*Si hay un partido a la misma hora y mismo día, repite la mutación*/
/*Al azar*/


mutacion([[E1, E2], Jor, Dia, Hora]):-
    random_member(DiaR, [viernes, sabado, domingo]),
    random_member(HoraR, [14, 15, 16, 17, 18, 19, 20, 21, 22]),
    retract(partido([[E1, E2], Jor, Dia, Hora])),
    asserta(partido([[E1, E2], Jor, DiaR, HoraR])).



/*MARIANO: funciones que uso*/


/* dame_num_jornada_partido(input, output)
   donde:
      input: el partido del cual se desea extraer el número de jornada
      output: el número de jornada del partido en cuestión
*/
dame_num_jornada_partido([_, Num_jornada, _, _], Num_jornada).


/* dame_partidos_jornada(input, output)
   donde:
      input: el número de la jornada que se busca
      outuput: los partidos que se juegan en esa jornada
*/
dame_partidos_jornada_aux(Numero_jornada, Partido):-
   partido(Partido),
   dame_num_jornada_partido(Partido, Numero_jornada_aux),
   Numero_jornada_aux == Numero_jornada.

dame_partidos_jornada(Numero_jornada, Partidos):-
   findall(Partido, dame_partidos_jornada_aux(Numero_jornada, Partido), Partidos).

/* dame_partidos_jornada_individual(input, output)
   donde:
      input: el número de la jornada que se busca
      output: los partidos que se juegan en esa jornada enlistados de uno por uno
*/

