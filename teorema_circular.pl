/* partido: [equipo_local, equipo_visitante], num_jornada, dia_juego, hora */
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

/* dame_num_jornada_partido(input, output)
   donde:
      input: el partido del cual se desea extraer el número de jornada
      output: el número de jornada del partido en cuestión
*/
dame_num_jornada_partido([_, Num_jornada, _, _], Num_jornada).


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
   dame_partidos_jornada_individuales(Jornada, Equipo_aux),
   dame_equipos_partido(Equipo_aux, Equipos).

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


/* El partido que ocasiona el movimiento es: New Castle vs Chelsea */

/* dame_partidos_mover(input_1, input_2, input_3, output)
   donde:
      input_1: el partido de la primera jornada que ocasiona el movimiento; puede ser un partido [ equipo, equipo ] o partido de la forma completa
      input_2: la jornada a
      input_3: la jornada b
      output: los partidos que se deben mover en ambas jornadas para asegurarnos de que los equipos se mantienen iguales 
      NOTA: posteriormente esto se va a cambiar a dos outputs, los partidos separados de cada jornada que hay que cambiar
*/
auxiliar(Partido, _, _, _):-
   /* Equipo_local es el equipo con el que se pretende terminar el ciclo */
   encuentra_equipo_local(Partido, Equipo_local),
   encuentra_equipo_visitante(Partido, Equipo_visitante).
   


