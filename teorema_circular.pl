/* partido: [equipo_local, equipo_visitante], num_jornada, hora */
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

/* dame_fecha_partido(input, output)
   donde:
      input: el partido del cual se desea extraer la fecha
      output: la fecha del partido en cuestión
*/
dame_fecha_partido([_, Num_jornada, _, _], Num_jornada).


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
dame_partidos_jornada_aux(Fecha_jornada, Partido):-
   partido(Partido),
   dame_fecha_partido(Partido, Fecha_partido_aux),
   Fecha_partido_aux == Fecha_jornada.

dame_partidos_jornada(Fecha_jornada, Partidos):-
   findall(Partido, dame_partidos_jornada_aux(Fecha_jornada, Partido), Partidos).

/* dame_partidos_jornada_individual(input, output)
   donde:
      input: el número de la jornada que se busca
      output: los partidos que se juegan en esa jornada enlistados de uno por uno
*/
dame_partidos_jornada_individuales(Fecha_jornada, Partido):-
   partido(Partido),
   dame_fecha_partido(Partido, Fecha_partido_aux),
   Fecha_partido_aux == Fecha_jornada.

dame_partidos_jornada_individuales(_,_).


/* El partido que ocasiona el movimiento es: New Castle vs Chelsea */

/* dame_partidos_mover(input_1, input_2, input_3, output)
   donde:
      input_1: el partido de la primera jornada que ocasiona el movimiento
      input_2: la jornada a
      input_3: la jornada b
      output: los partidos que se deben mover en ambas jornadas para asegurarnos de que los equipos se mantienen iguales 
      NOTA: posteriormente esto se va a cambiar a dos outputs, los partidos separados de cada jornada que hay que cambiar
*/
%dame_partidos_mover(Partido, Jornada_1, Jornada_2, Partidos_mover):-

