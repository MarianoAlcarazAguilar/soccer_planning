/*
Métodos que tengo:
dame_num_jornada_partido(input, output)
dame_hora_partido(input, output)
dame_equipos_partido(input, output)
dame_partidos_jornada(input, output)
dame_partidos_jornada_individual(input, output)
saca_equipos_una_jornada(input, output)
encuentra_partido_local(input, output)
encuentra_partido_visitante(input, output)
busca_contrinante(input, input, output)
*/

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
Dado un nombre de equipo inicial, encontrar todos los equipos que se necesitan iterar hasta llegar de nuevo al inicial.
Esta función recibe el nombre del equipo inicial, y las listas con los equipos de dos jornadas
*/
busca_hasta_regresar(Equipo_inicial, Equipo_aux, Equipos_jornada_a, Equipos_jornada_b, Par_impar, Contrincante):-
   /*Tenemos que saber en qué lista de equipos tenemos que encontrar el contrincante */
   /* Si Z es 0, significa que estamos en iteración par, por lo tanto, hay que buscar en la lista de equipos a; sino en la b */
   Z is Par_impar mod 2,
   (Z == 0 -> 
      busca_contrincante(Equipo_aux, Equipos_jornada_a, Contrincante),
      write(Contrincante),nl,
      Equipo_inicial \== Contrincante,
      busca_hasta_regresar(Equipo_inicial, Contrincante, Equipos_jornada_a, Equipos_jornada_b, Par_impar + 1, Otro)
      ; 
         busca_contrincante(Equipo_aux, Equipos_jornada_b, Contrincante),
         write(Contrincante),nl,
         (Equipo_inicial \== Contrincante ->
            busca_hasta_regresar(Equipo_inicial, Contrincante, Equipos_jornada_a, Equipos_jornada_b, Par_impar + 1, Otro)
            ;
               !
         )
         %Equipo_inicial \== Contrincante,
         %busca_hasta_regresar(Equipo_inicial, Contrincante, Equipos_jornada_a, Equipos_jornada_b, Par_impar + 1, _)
   ).


/* Necesito encontrar el contrincante dada la lista de equipos */
main([Equipo_inicial, _], Jornada_a, Jornada_b, Contrincante):-
   saca_equipos_una_jornada(Jornada_a, Equipos_jornada_a),
   saca_equipos_una_jornada(Jornada_b, Equipos_jornada_b),
   busca_hasta_regresar(Equipo_inicial, Equipo_inicial, Equipos_jornada_a, Equipos_jornada_b, 0, Contrincante).      