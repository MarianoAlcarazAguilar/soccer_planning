:- use_module(library(lists)).
:- dynamic to_be_moved/1.
:- dynamic partido/1.
:- dynamic partido_aux/1.
:- dynamic partido_aux_dos/1.
:- dynamic jornada/1.
:- dynamic horario/1.
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
:- dynamic combinacion_equipos/1.
:- dynamic combinacion_usada_jornada/2.


/*************************************************
Estas funciones se tienen que eliminar
*************************************************/
saca_equipos_una_jornada(Jornada, Equipos):-
   dame_partidos_jornada(Jornada, Partidos),
   recorre_partidos(Partidos, Equipos).


recorre_partidos([], []):- !.
recorre_partidos([[Equipos, _, _, _] | Resto_partidos], [Equipos | Resto_equipos]):-
   recorre_partidos(Resto_partidos, Resto_equipos).

dame_partidos_jornada(Numero_jornada, Partidos):-
   findall(Partido, dame_partidos_jornada_aux(Numero_jornada, Partido), Partidos).

dame_partidos_jornada_aux(Numero_jornada, Partido):-
   partido(Partido),
   dame_num_jornada_partido(Partido, Numero_jornada_aux),
   Numero_jornada_aux == Numero_jornada.

dame_num_jornada_partido([_, Num_jornada, _, _], Num_jornada).

/*************************************************
Eliminar hasta aquí
*************************************************/


/*************************
PASO 1: Meter todos los datos de los equipos
Almacena: nombre de equipo, posición temporada pasada, seguidores, ciudad, estadio
*************************/

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


/************************
PASO 2: Generar vuelta inicial
************************/

dias([viernes, sabado, domingo]).
horarios([14, 15, 16, 17, 18, 19, 20, 21, 22]).

/* Regresa una lista con los nombres de todos los equipos */
dame_nombres_equipos(Equipos):-
   findall(X, equipo([X, _, _, _, _]), Equipos).

/* Quiero generar todas las combinaciones de partidos entre los equipos */
genera_contrincantes([]):- !.
genera_contrincantes([Equipo | Resto]):-
   genera_contrincantes(Resto),
   combina_equipos(Equipo, Resto).

combina_equipos(_, []):- !.
combina_equipos(Equipo_uno, [Otro_equipo | Resto_equipos]):-
   combina_equipos(Equipo_uno, Resto_equipos),
   asserta(combinacion_equipos([Equipo_uno, Otro_equipo])).

dame_lista_combinaciones_equipos(Combinaciones):-
   findall(X, combinacion_equipos(X), Combinaciones).

/* Esta función crea todas las combinaciones. Solo correr una vez. */
genera_combinaciones:-
   dame_nombres_equipos(Eq),
   genera_contrincantes(Eq).

elimina_combinaciones:-
   retractall(combinacion_equipos(_)).

elimina_partidos:-
   retractall(partido(_)).

elimina_combinacion_usada_jornada:-
   retractall(combinacion_usada_jornada(_)).

/* Esta función regresa los nombres de los equipos que ya están en una jornada, para asegurarnos que un nuevo partido sí puede entrar */
dame_nombres_equipos_jornada(Num_jornada, Equipos):-
   saca_equipos_una_jornada(Num_jornada, Aux_equipos),
   flatten(Aux_equipos, Equipos).

/* Funcion para elegir un dia de forma aleatoria */
elige_dia_aleatorio(Dia):-
   dias(Dias),
   random_member(Dia, Dias).

/* Funcion para elegir un horario de forma aleatoria */
elige_hora_aleatorio(Hora):-
   horarios(Horarios),
   random_member(Hora, Horarios).

/* Funcion para elegir una combinación aleatoria de equipos */
elige_combinacion_aleatoria(Combinacion_aleatoria):-
   dame_lista_combinaciones_equipos(X),
   random_member(Combinacion_aleatoria, X).

/* Función que regresa los nombres de los equipos dada una combinación */
dame_equipos_de_combinacion([Equipo_1, Equipo_2], Equipo_1, Equipo_2).

limpia:-
   elimina_partidos,
   elimina_combinaciones,
   elimina_combinacion_usada_jornada,
   genera_combinaciones.

/* Si es 11, sabemos que ya metimos 10 partidos, entonces ya acabamos esa jornada */
agrega_partidos_a_jornada(_, 11):- !.
agrega_partidos_a_jornada(Num_jornada, Num_partido):-
   dame_nombres_equipos_jornada(Num_jornada, Equipos_ya_participantes),
   /* Primero elegimos un partido de forma aleatoria */
   elige_combinacion_aleatoria(Combinacion_aleatoria),
   dame_equipos_de_combinacion(Combinacion_aleatoria, Equipo_1, Equipo_2),
   /* Segundo, vemos si esos equipos ya están jugando en la jornada especificada */
   (not(member(Equipo_1, Equipos_ya_participantes)),
    not(member(Equipo_2, Equipos_ya_participantes)) ->
      /* Si ninguno de los dos equipos está en la jornada, entonces sí los metemos */
      elige_dia_aleatorio(Dia),
      elige_hora_aleatorio(Hora),
      assertz(partido([[Equipo_1, Equipo_2], Num_jornada, Dia, Hora])),
      /* Eliminamos la combinación de las combinaciones disponibles */
      retract(combinacion_equipos([Equipo_1, Equipo_2])),
      /* Llamamos a la función otra vez */
      Otro_otro_num is Num_partido + 1,
      agrega_partidos_a_jornada(Num_jornada, Otro_otro_num),
      !
      ;
         /* Si alguno de los dos equipos sí esta en la jornada ya, no los podemos meter, 
         entonces volvemos a llamar a la función sin aumentar el número de partidos */
         agrega_partidos_a_jornada(Num_jornada, Num_partido)
   ).

/* Vamos a optimizar esta madre */
agrega_optimizado(_, 11, _, _):- !.
agrega_optimizado(Num_jornada, Num_partido, Num_iteracion, Num_desatoracion):-
   (Num_desatoracion == 50 -> fail; true),
   /* Vemos qué equipos YA ESTÁN jugando en la jornada: Equipos_ya_participantes */
   dame_nombres_equipos_jornada(Num_jornada, Equipos_ya_participantes),
   /* Vemos qué equipos FALTAN en la jornada: Equipos_faltantes */
   dame_nombres_equipos(All_teams),
   subtract(All_teams, Equipos_ya_participantes, Equipos_faltantes),
   /* Elegimos DOS de los equipos faltantes: Equipo_1 y Equipo_2 */
   random_member(Equipo_1, Equipos_faltantes),
   delete(Equipos_faltantes, Equipo_1, Equipos_faltantes_2),
   random_member(Equipo_2, Equipos_faltantes_2),
   length(Equipos_faltantes, Size),
   /* Ahora tenemos que ver si esa combinación se puede meter todavía;
   Si existe la combinación, meto el partido, sino no */
   (existe_combinacion_equipos(Equipo_1, Equipo_2) ->
      elige_dia_aleatorio(Dia),
      elige_hora_aleatorio(Hora),
      assertz(partido([[Equipo_1, Equipo_2], Num_jornada, Dia, Hora])),
      assertz(combinacion_usada_jornada([[Equipo_1, Equipo_2], Num_jornada])),
      elimina_combinacion(Equipo_1, Equipo_2),
      Otro_num is Num_partido + 1,
      Aux_iter_1 is Num_iteracion + 1,
      agrega_optimizado(Num_jornada, Otro_num, Aux_iter_1, Num_desatoracion),
      !
      ;  
         (Size == 2 ->
            /* En este punto ya estamos atorados, por lo que es necesario reinciar la jornada dada */
            reinicia_combinaciones_usadas(Num_jornada),
            Aux_iter_2 is Num_iteracion + 1,
            agrega_optimizado(Num_jornada, 1, Aux_iter_2, Num_desatoracion)
            ;
               (Num_iteracion == 1000 ->
                  reinicia_combinaciones_usadas(Num_jornada),
                  Aux_desatoracion is Num_desatoracion + 1,
                  agrega_optimizado(Num_jornada, 1, 1, Aux_desatoracion)
                  ;
                     Aux_iter_3 is Num_iteracion + 1,
                     agrega_optimizado(Num_jornada, Num_partido, Aux_iter_3, Num_desatoracion)
               )
         )
   ).

/* Esta función te regresa true si existe una combinación disponible de los equipos dados */
existe_combinacion_equipos(Equipo_1, Equipo_2):-
   combinacion_equipos([Equipo_1, Equipo_2]),
   !.

existe_combinacion_equipos(Equipo_1, Equipo_2):-
   combinacion_equipos([Equipo_2, Equipo_1]),
   !.

/* Esta función elimina una combinación de dos equipos dados si esta existe */
elimina_combinacion(Equipo_1, Equipo_2):-
   existe_combinacion_equipos(Equipo_1, Equipo_2),
   combinacion_equipos([Equipo_1, Equipo_2]),
   retract(combinacion_equipos([Equipo_1, Equipo_2])),
   !.
elimina_combinacion(Equipo_1, Equipo_2):-
   existe_combinacion_equipos(Equipo_1, Equipo_2),
   combinacion_equipos([Equipo_2, Equipo_1]),
   retract(combinacion_equipos([Equipo_2, Equipo_1])),
   !.

/*
Necesito una función que reinicie los partidos usados en una jornada dada
*/
dame_combinaciones_usadas_en_jornada(Num_jornada, Usadas):-
   findall(X, combinacion_usada_jornada([X, Num_jornada]), Usadas).

reinicia_combinaciones_usadas(_, []):- !.
reinicia_combinaciones_usadas(Num_jornada, [Combinacion_usada | Resto_combinaciones]):-
   reinicia_combinaciones_usadas(Num_jornada, Resto_combinaciones),
   assertz(combinacion_equipos(Combinacion_usada)),
   retract(combinacion_usada_jornada([Combinacion_usada, Num_jornada])),
   retract(partido([Combinacion_usada, Num_jornada, _, _])).

reinicia_combinaciones_usadas(Num_jornada):-
   dame_combinaciones_usadas_en_jornada(Num_jornada, Usadas),
   reinicia_combinaciones_usadas(Num_jornada, Usadas).

/* Métod para generar una vuelta aleatorioa */
genera_vuelta:-
   limpia,
   genera_vuelta(1),
   write('Vuelta Terminada'),nl.

genera_vuelta(20):- !.
genera_vuelta(Num_jornada):-
   agrega_optimizado(Num_jornada, 1, 1, 0),
   write(Num_jornada),nl,
   Sig_jornada is Num_jornada + 1,
   (genera_vuelta(Sig_jornada) -> true; write('Repitiendo Vuelta'),nl,genera_vuelta, !).


/**********************
PASO 3: Calificar una vuelta
**********************/
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

rating_vuelta_lista([], []).
rating_vuelta_lista([Cab|Resto], [CabRes|Cola]):-
    rating_jornada(Cab, Rat),
    get_head(Cab,[[_,_], NumJornada,_,_]),
    check_numjor(NumJornada, Z),
    CabRes is (Rat*Z),
    rating_vuelta_lista(Resto, Cola).   

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

mejor_jornada([Cab], Cab).
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


assert_horario([]).
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
