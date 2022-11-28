:- dynamic combinacion_equipos/1.
:- dynamic partido/1.

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

dias([viernes, sabado, domingo]).
horarios([14, 15, 16, 17, 18, 19, 20, 21, 22]).

equipo([arsenal, 5, 20, londres, 'Emirates Stadium']).
equipo([man_city, 1, 14, manchester, 'Estadio de la Ciudad de Manchester']).
equipo([newcastle, 11, 2.3, newcastle, 'St James Park']).
equipo([tottenham, 4, 8.2, londres, 'Tottenham Hotspur Stadium']).
equipo([man_united, 6, 34, manchester, 'Old Trafford']).
equipo([brighton, 9, 0.68, brighton, 'Falmer Stadium']).
equipo([chelsea, 3, 23, londres, 'Stamford Bridge']).
equipo([liverpool, 2, 22, liverpool, 'Anfield']).
equipo([fulham, 18, 0.69, londres, 'Craven Cottage']).
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

dame_equipos_de_combinacion([Equipo_1, Equipo_2], Equipo_1, Equipo_2).

limpia:-
   elimina_partidos,
   elimina_combinaciones,
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
      write('Equipos distintos'),nl,
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
         write('Equipos iguales'),nl,
         /* Si alguno de los dos equipos sí esta en la jornada ya, no los podemos meter, 
         entonces volvemos a llamar a la función sin aumentar el número de partidos */
         agrega_partidos_a_jornada(Num_jornada, Num_partido)
   ).

/* Vamos a optimizar esta madre */
agrega_optimizado(_, 11):- !.
agrega_optimizado(Num_jornada, Num_partido):-
   /* Vemos qué equipos YA ESTÁN jugando en la jornada: Equipos_ya_participantes */
   dame_nombres_equipos_jornada(Num_jornada, Equipos_ya_participantes),
   /* Vemos qué equipos FALTAN en la jornada: Equipos_faltantes */
   dame_nombres_equipos(All_teams),
   subtract(All_teams, Equipos_ya_participantes, Equipos_faltantes),
   /* Elegimos DOS de los equipos faltantes: Equipo_1 y Equipo_2 */
   random_member(Equipo_1, Equipos_faltantes),
   delete(Equipos_faltantes, Equipo_1, Equipos_faltantes_2),
   random_member(Equipo_2, Equipos_faltantes_2),
   write(Equipo_1),write(' '),
   write(Equipo_2),nl,
   /* Ahora tenemos que ver si esa combinación se puede meter todavía;
   Si existe la combinación, meto el partido, sino no */
   (existe_combinacion_equipos(Equipo_1, Equipo_2) ->
      elige_dia_aleatorio(Dia),
      elige_hora_aleatorio(Hora),
      assertz(partido([[Equipo_1, Equipo_2], Num_jornada, Dia, Hora])),
      elimina_combinacion(Equipo_1, Equipo_2),
      Otro_num is Num_partido + 1,
      agrega_optimizado(Num_jornada, Otro_num),
      !
      ;
         agrega_optimizado(Num_jornada, Num_partido)
   ).

genera_vuelta_aleatoria:-
   limpia,
   genera_vuelta_aleatoria(1).
genera_vuelta_aleatoria(20):- !.
genera_vuelta_aleatoria(Num_jornada):-
   agrega_partidos_a_jornada(Num_jornada, 1),
   Sig_jornada is Num_jornada + 1,
   genera_vuelta_aleatoria(Sig_jornada).

%dame_contrincantes_restantes(Equipo_buscado, Contrincantes):-

existe_combinacion_equipos(Equipo_1, Equipo_2):-
   combinacion_equipos([Equipo_1, Equipo_2]),
   !.

existe_combinacion_equipos(Equipo_1, Equipo_2):-
   combinacion_equipos([Equipo_2, Equipo_1]),
   !.

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