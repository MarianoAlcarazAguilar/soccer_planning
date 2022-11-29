:- use_module(library(lists)).
:- dynamic to_be_moved/1.
:- dynamic partido/1.
:- dynamic partido_aux_dos/1.
:- dynamic jornada/1.
:- dynamic horario/1.
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
:- dynamic combinacion_equipos/1.
:- dynamic combinacion_usada_jornada/1.
:- dynamic to_be_moved/1.
:- dynamic partido_aux/1.


%Programa realizado por Javier Nieto Merodio y Mariano Alcaraz Aguilar

/****************************************************************************/

%Generador de un calendario de partidos para la Premier League 21/22

% Este programa es un algoritmo evolutivo que busca generar un calendario 
% de partidos óptimo para toda la temporada 2021/2022 de la Premier League.
% Genera una población inicial aleatorizada de un calendario factible.
% Luego, evalúa con una función de aptitud que considera: posición de un 
% equipo en la temporada pasada, seguidores, si es un derby. De no tener el 
% rating arriba del minimo deseado, realiza cruzamientos y mutaciones. 
% Finalmente, al alcanzar el rating óptimo, guarda la temporada y la regresa 
% en un archivo csv

/****************************************************************************/



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
num_jornadas([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]).

/* Regresa una lista con los nombres de todos los equipos */
dame_nombres_equipos(Equipos):-
   findall(X, equipo([X, _, _, _, _]), Equipos).

/* Dame nombre de equipo aleatoriamente */
elige_equipo_al_azar(Equipo):-
   dame_nombres_equipos(All_teams),
   random_member(Equipo, All_teams).

/* Dame dos números de jornadas aleatoriamente */
elige_dos_jornadas_al_azar(Jornada_a, Jornada_b):-
   num_jornadas(All_jornadas),
   random_member(Jornada_a, All_jornadas),
   delete(All_jornadas, Jornada_a, All_jornadas_b),
   random_member(Jornada_b, All_jornadas_b).

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
   genera_vuelta(1).

genera_vuelta(20):- !.
genera_vuelta(Num_jornada):-
   agrega_optimizado(Num_jornada, 1, 1, 0),
   Sig_jornada is Num_jornada + 1,
   (genera_vuelta(Sig_jornada) -> true; write('Repitiendo Vuelta'),nl,genera_vuelta, !).

/* dame_num_jornada_partido(input, output)
   donde:
      input: el partido del cual se desea extraer el número de jornada
      output: el número de jornada del partido en cuestión
*/
dame_num_jornada_partido([_, Num_jornada, _, _], Num_jornada).


/**********************
PASO 3: Calificar una vuelta
**********************/

/*Devuelve jornada como lista*/
jornada_a_lista(NumJornada, Lista):-
   dame_partidos_jornada(NumJornada, Partidos),
   Lista = Partidos.

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



/*Recibe la vuelta como una lista de jornadas
  Regresa un número entre el 0 y el 1 evaluando la vuelta. */
rating_vuelta(Vuelta, Res):-
   rating_vuelta_lista(Vuelta, Lista),
   sumlist(Lista, Suma),
   length(Lista, Len),
   Res is ((Suma)/Len).

/*Regresa una lista ordenada del valor de cada jornada considerando la fecha*/
rating_vuelta_lista([], []):- !.
rating_vuelta_lista([Cab|Resto], [CabRes|Cola]):-
   rating_vuelta_lista(Resto, Cola),
   rating_jornada(Cab, Rat),
   get_head(Cab,[[_,_], NumJornada,_,_]),
   check_numjor(NumJornada, Z),
   CabRes is (Rat*Z).
     
/*Evalúa una jornada recibida como lista*/
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

/*Si el número es mayor igual a 1, regresa 1. Si no, 0*/
if_cont_1(Num, Res):-
   Num >= 1,
   Res is 1,
   !.
if_cont_1(Num, Res):-
   Num =:= 0,
   Res is 0.
/*Si el número es mayor igual a 2, regresa 1. Si no, 0*/
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

/*Valora mejor a los partidos despues de las 9*/
check_hora(Hora, Res):-
   (  Hora >= 9
   -> Res is 0.5
   ;  Res = 0
   ).
/*Da menos valor a los partidos que suceden en viernes*/
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
/*Hora del partido*/
hora([_, _,_, Hora], X):-
   X = Hora.


/*Devuelve 1 si es un número de jornada regular, y 1.5 si es importante*/
check_numjor(NumJornada, Res):-
   NumJornada =:= 1,
   Res is 1.5,
   !.

check_numjor(NumJornada, Res):-
   NumJornada =:= 19,
   Res is 1.5,
   !.
check_numjor(NumJornada, Res):-
   NumJornada =:= 20,
   Res is 1.5,
   !.
check_numjor(NumJornada, Res):-
   NumJornada =:= 38,
   Res is 1.5,
   !.
check_numjor(_, Res):-
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

/*Da el número de la peor jornada*/

num_peor_jornada(Num):-
   dame_jornadas_en_lista(Vuelta),
   peor_jornada(Vuelta, Jornada),
   num_jornada(Jornada, Num).


/*Encuentra la peor jornada de una vuelta*/

peor_jornada([Cab], Cab):- !.
peor_jornada([CabVuelta|ColaVuelta], Peor):-
   peor_jornada(ColaVuelta, PeorCola),
   peor_entre_jornadas(CabVuelta, PeorCola, PeorActual),
   Peor = PeorActual.

/* Dado el número de la peor jornada, dame otro número de jornada */
dame_otro_num_jornada(Jornada_a, Jornada_b):-
   num_jornadas(All_jornadas),
   delete(All_jornadas, Jornada_a, All_aux),
   random_member(Jornada_b, All_aux).

/* Encontrar el número de la peor jornada, y otro random */
dame_peor_jornada_y_al_azar(Peor_jornada, Otra_jornada):-
   num_peor_jornada(Peor_jornada),
   dame_otro_num_jornada(Peor_jornada, Otra_jornada).


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

/***********************************
PASO 4: Algoritmo evolutivo
***********************************/

/*Cambia la fecha y hora de un partido en una jornada dada*/
/*Checa que no haya más de un partido a la misma hora y mismo día*/
/*Si hay un partido a la misma hora y mismo día, repite la mutación*/
/*Al azar*/


mutacion([[E1, E2], Jor, Dia, Hora]):-
   random_member(DiaR, [viernes, sabado, domingo]),
   random_member(HoraR, [14, 15, 16, 17, 18, 19, 20, 21, 22]),
   retract(partido([[E1, E2], Jor, Dia, Hora])),
   assertz(partido([[E1, E2], Jor, DiaR, HoraR])),
   !.


/* dame_hora_partido(input, output)
   donde:
      input: el partido del cual se desea extraer la hora
      output: la hora del partido en cuestión
*/
dame_hora_partido([_, _, _, Hora], Hora).

/* dame_equipos_partido(input, output)
   donde:
      input: el partido del cual se desea extraer los equipos
      output: los equipos del partido en cuestión
*/
dame_equipos_partido([Equipos, _, _, _], Equipos).

/* dame_dia_partido(input, output)
   donde:
      input: el partido del cual se desea extraer el dia
      output: el dia del partido en cuestión
*/
dame_dia_partido([_, _, Dia, _], Dia).

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
dame_partidos_jornada_individuales(Numero_jornada, Partido):-
   partido(Partido),
   dame_num_jornada_partido(Partido, Numero_jornada_aux),
   Numero_jornada_aux == Numero_jornada.

dame_partidos_jornada_individuales(_,_).


/* saca_equipos_una_jornada(input, output)
   donde:
      input: el número de jornada que se busca
      output: Los equipos que jugarán cada partido en la jornada en cuestión
*/
saca_equipos_una_jornada(Jornada, Equipos):-
   dame_partidos_jornada(Jornada, Partidos),
   recorre_partidos(Partidos, Equipos).


recorre_partidos([], []):- !.
recorre_partidos([[Equipos, _, _, _] | Resto_partidos], [Equipos | Resto_equipos]):-
   recorre_partidos(Resto_partidos, Resto_equipos).


/* encuentra_partido_local(input, output)
   encuentra_partido_visitante(input, output) (basicamente funciona igual)
   donde:
      input: una lista con los equipos que disputarán el partido
      output: el equipo local (básicamente es el primer equipo de la lista)
*/
encuentra_equipo_local([Equipo_local, _], Equipo_local):- !.

encuentra_equipo_local(Partido, Equipo_local):-
   dame_equipos_partido(Partido, Equipos),
   encuentra_equipo_local(Equipos, Equipo_local).

encuentra_equipo_visitante([_, Equipo_visitante], Equipo_visitante):- !.

encuentra_equipo_visitante(Partido, Equipo_visitante):-
   dame_equipos_partido(Partido, Equipos),
   encuentra_equipo_visitante(Equipos, Equipo_visitante).


/*
Dado un número de jornada y un equipo local, encuentra el equipo visitante
dame_contrincante(input_1, input_2, output)
donde:
   input_1: nombre del equipo local
   input_2: numero de jornada que se busca
   output: el nombre del equipo visitante contra el que juega el equipo local en la jornada dada
*/
dame_contrincante(Equipo_dado, Num_jornada, Equipo_contrincante):-
   dame_partidos_jornada_individuales(Num_jornada, Partido_aux),
   encuentra_equipo_local(Partido_aux, Local_aux),
   encuentra_equipo_visitante(Partido_aux, Visitante_aux),
   (
      Equipo_dado == Local_aux -> 
         encuentra_equipo_visitante(Partido_aux, Equipo_contrincante),
         !;
      (
         Equipo_dado == Visitante_aux ->
            encuentra_equipo_local(Partido_aux, Equipo_contrincante),
            !;
            true
      )
   ).

/* busca_contrincante(input_1, input_2, output)
   donde:
      input_1: el nombre del equipo buscado
      input_2: la lista con los equipos
      output: el equipo contra el que está jugando el equipo buscado
Dada una LISTA DE EQUIPOS y el NOMBRE DE EQUIPO, encontrar a su contrincante.
*/
busca_contrincante(_, [], _):- !.
busca_contrincante(Equipo_buscado, [[Un_equipo, Otro_equipo] | Resto_equipos], Contrincante):-
   busca_contrincante(Equipo_buscado, Resto_equipos, Contrincante),
   (Equipo_buscado == Un_equipo -> evalua_contrincate(Equipo_buscado, [Un_equipo, Otro_equipo], Contrincante);
      (Equipo_buscado == Otro_equipo -> evalua_contrincate(Equipo_buscado, [Un_equipo, Otro_equipo], Contrincante); true)
   ).

evalua_contrincate(Equipo_buscado, [Equipo_buscado, Otro_equipo], Otro_equipo):- !.
evalua_contrincate(Equipo_buscado, [Otro_equipo, Equipo_buscado], Otro_equipo).

/*
busca_hasta_regresar(input_1, input_2, input_3, input_4, input_5, output)
donde:
   input_1: El equipo inicial y final (cuando se encuentra en la otra jornada se acaba la búsqueda)
   input_2: Equipo auxiliar que va cambiando, es el contrincante de equipo inicial, y así sucesivamente
   input_3: Una lista con los equipos de la jornada a
   input_4: Una lista con los equipos de la jornada b
   output: El primer contrincante. Al chile esto casi ni se necesita
Dado un nombre de equipo inicial, encontrar todos los equipos que se necesitan iterar hasta llegar de nuevo al inicial.
Esta función recibe el nombre del equipo inicial, y las listas con los equipos de dos jornadas
*/
busca_hasta_regresar(Equipo_inicial, Equipo_aux, Equipos_jornada_a, Equipos_jornada_b, Par_impar, Contrincante):-
   Z is Par_impar mod 2,
   (Z == 0 -> 
      busca_contrincante(Equipo_aux, Equipos_jornada_a, Contrincante),
      asserta(to_be_moved(Contrincante)),
      Equipo_inicial \== Contrincante,
      busca_hasta_regresar(Equipo_inicial, Contrincante, Equipos_jornada_a, Equipos_jornada_b, Par_impar + 1, _)
      ; 
         busca_contrincante(Equipo_aux, Equipos_jornada_b, Contrincante),
         asserta(to_be_moved(Contrincante)),
         (Equipo_inicial \== Contrincante ->
            busca_hasta_regresar(Equipo_inicial, Contrincante, Equipos_jornada_a, Equipos_jornada_b, Par_impar + 1, _)
            ;
               true
         )
   ).


/* find_teams_to_move(input_1, input_2, input_3, output)
   donde:
      input_1: nombre de un equipo del partido que provoca los movimientos
      input_2: el número de la peor jornada
      input_3: el número de la otra jornada
      output: una lista con los nombres de los equipos que se necesitan mover
Necesito encontrar el contrincante dada la lista de equipos 
*/
find_teams_to_move(Equipo_inicial, Jornada_a, Jornada_b, Equipos):-
   encuentra_equipos_a_mover([Equipo_inicial, _], Jornada_a, Jornada_b, Equipos).

encuentra_equipos_a_mover([Equipo_inicial, _], Jornada_a, Jornada_b, Equipos):-
   saca_equipos_una_jornada(Jornada_a, Equipos_jornada_a),
   saca_equipos_una_jornada(Jornada_b, Equipos_jornada_b),
   busca_hasta_regresar(Equipo_inicial, Equipo_inicial, Equipos_jornada_a, Equipos_jornada_b, 0, _),
   findall(Y, to_be_moved(Y), Equipos),
   retractall(to_be_moved(_)).

/* dame_nueva_jornada(input_1, input_2, input_3, output)
   donde:
      input_1: el número de jornada original
      input_2: el número de la jornada a
      input_3: el número de la jornada b
      output: el nuevo número de jornada
Básicamente te da el contrario al número de jornada original, pues lo que se busca es cambiarlas.
*/
dame_nueva_jornada(Jornada_original, Jornada_original, Jornada_b, Jornada_b):- !.
dame_nueva_jornada(Jornada_original, Jornada_a, Jornada_original, Jornada_a).

/* crea_partidos_auxiliares(input_1, input_2, input_3)
   donde:
      input_1: el nombre del equipo que provoca todos los cambios
      input_2: el número de la jornada a
      input_3: el número de la jornada b
Esto esta MAL! 
Ahora funciona porque solo tengo datos de dos jornadas, pero si tengo más me va a cambiar todos.
Lo que hace es que crea partidos auxiliares con los nuevos números de jornada. También elimina los partidos originales para luego meterlos otra vez.
*/
crea_partidos_auxiliares(Equipo_inicial, Jornada_a, Jornada_b):-
   find_teams_to_move(Equipo_inicial, Jornada_a, Jornada_b, Equipos),
   partido(Partido),
   dame_equipos_partido(Partido, [Equipo_1, Equipo_2]),
   member(Equipo_1, Equipos),
   dame_num_jornada_partido(Partido, Num_jornada),
   dame_hora_partido(Partido, Hora),
   dame_dia_partido(Partido, Dia),
   dame_nueva_jornada(Num_jornada, Jornada_a, Jornada_b, Nueva_jornada),
   retract(partido(Partido)),
   asserta(partido_aux([[Equipo_1, Equipo_2], Nueva_jornada, Dia, Hora])).

crea_partidos_auxiliares(_, _, _).



/* dame_partidos_dos_jornadas(input_1, input_2, output)
   donde:
      input_1: el número de la jornada a
      input_2: el número de la jornada b
      output: los partidos de ambas jornadas
Necesito una función que me regrese en una lista los partidos de dos jornadas
*/
dame_partidos_dos_jornadas(Jornada_a, Jornada_b, Partidos):-
   dame_partidos_jornada(Jornada_a, Partidos_a),
   dame_partidos_jornada(Jornada_b, Partidos_b),
   append(Partidos_a, Partidos_b, Partidos).

escribe_partidos_auxiliares(_, _, _, _, []):- !.
escribe_partidos_auxiliares(Equipo_inicial, Jornada_a, Jornada_b, Equipos_to_move, [Partido | Resto_partidos]):-
   escribe_partidos_auxiliares(Equipo_inicial, Jornada_a, Jornada_b, Equipos_to_move, Resto_partidos),
   dame_equipos_partido(Partido, [Equipo_1, Equipo_2]),
   (member(Equipo_1, Equipos_to_move) ->
      dame_num_jornada_partido(Partido, Num_jornada),
      dame_hora_partido(Partido, Hora),
      dame_dia_partido(Partido, Dia),
      dame_nueva_jornada(Num_jornada, Jornada_a, Jornada_b, Nueva_jornada),
      retract(partido(Partido)),
      asserta(partido_aux([[Equipo_1, Equipo_2], Nueva_jornada, Dia, Hora]))
      ;
      true
   ).

/*
Esta función estática se encarga de reemplazar los partidos auxiliares por partidos normales.
Al final elimina los partidos que no se necesitan.
*/
add_partidos_aux_to_partidos:-
   dame_lista_partidos_aux(Partidos_auxiliares),
   elimina_partidos_auxiliares(Partidos_auxiliares).

dame_lista_partidos_aux(Partidos_auxiliares):-
   findall(X, partido_aux(X), Partidos_auxiliares).

elimina_partidos_auxiliares([]):- !.
elimina_partidos_auxiliares([Partido_aux | Resto_partidos_auxiliares]):-
   elimina_partidos_auxiliares(Resto_partidos_auxiliares),
   asserta(partido(Partido_aux)),
   retract(partido_aux(Partido_aux)).


cambia_partidos_entre_jornadas_oficial(Equipo_inicial, Jornada_a, Jornada_b):-
   /* Saco los partidos de las jornadas que nos interesan */
   dame_partidos_dos_jornadas(Jornada_a, Jornada_b, Partidos_a_cambiar),
   find_teams_to_move(Equipo_inicial, Jornada_a, Jornada_b, Equipos),
   escribe_partidos_auxiliares(Equipo_inicial, Jornada_a, Jornada_b, Equipos, Partidos_a_cambiar),
   add_partidos_aux_to_partidos,
   !.

cambia_partidos_entre_jornadas(Equipo_inicial, Jornada_a, Jornada_b):-
   crea_partidos_auxiliares(Equipo_inicial, Jornada_a, Jornada_b),
   add_partidos_aux_to_partidos.
   
cambia_partidos_entre_jornadas(_, _, _).


/********************
PASO 5: Implementación final
*********************/


/*Función principal main: con solo escribir main, se ejecuta el programa y regresa en un csv
el calendario optimo*/

main:-
   genera_vuelta,
   write('Vuelta ha sido generada'),nl,
   dame_jornadas_en_lista(Lista),
   rating_vuelta(Lista, Rating),
   write(Rating),
   (  Rating < 10
   -> mainmut
   ;  fin(Lista, Rating)
   ).

/*Que realice una mutación y continúe haciendo cambios si es necesario*/
mainmut:-
   dame_jornadas_en_lista(Lista),
   random_member(RandomJ, Lista),
   random_member(RandomP, RandomJ),
   mutacion(RandomP),
   dame_jornadas_en_lista(NuevaLista),
   rating_vuelta(NuevaLista, Rating),
   (  Rating < 0.75
   -> maincruz
   ;  fin(NuevaLista, Rating), !
   ).

/*Para que si no ha terminado realice un cruzamiento y cambie el rating*/
maincruz:-
   elige_equipo_al_azar(Equipo),
   dame_peor_jornada_y_al_azar(Jornada_a, Jornada_b),
   cambia_partidos_entre_jornadas_oficial(Equipo, Jornada_a, Jornada_b),
   dame_jornadas_en_lista(NuevaLista),
   rating_vuelta(NuevaLista, Rating),
   (  Rating < 0.75
   -> mainmut
   ;  fin(NuevaLista, Rating), !
   ).

fin(_, Rating):-
   write('Rating final'),
   write(" "),
   write(Rating),
   write_partidos.
   
/*Función que regresa todas las jornadas de una vuelta como lista*/

dame_jornadas_en_lista(Lista):-
   crea_jornadas_listas,
   findall(X, jornada(X), Lista),
   retractall(jornada(X)).

/*Con asserta, agrega elementos tipo jornada para luego poder usar findall*/
crea_jornadas_listas:-
   crea_jornadas_listas(1).

crea_jornadas_listas(20):- !.
crea_jornadas_listas(Num_jornada):-
   dame_partidos_jornada(Num_jornada, Jornada),
   asserta(jornada(Jornada)),
   Aux_num is Num_jornada + 1,
   crea_jornadas_listas(Aux_num).

/* Función que escribe todos los partidos a un archivo CSV */ 
dame_lista_todos_los_partidos(All_partidos):-
   findall(X, partido(X), All_partidos).

write_partidos(_, []):- !.
write_partidos(Out, [Partido | Resto]):-
   write_partidos(Out, Resto),
   write(Out, Partido),
   write(Out, '\n').

write_partidos:-
   dame_lista_todos_los_partidos(Partidos),
   open('vuelta_encontrada.csv', write, Out),
   write(Out, 'Partido\n'),
   write_partidos(Out, Partidos),
   close(Out).

imprime_lista([]):- !.
imprime_lista([H | T]):-
   imprime_lista(T),
   write(H),nl.
