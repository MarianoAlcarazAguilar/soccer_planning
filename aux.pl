agrega_optimizado(_, 11, _, _):- !.
agrega_optimizado(Num_jornada, Num_partido, Num_iteracion, Num_desatoracion):-
   (Num_desatoracion == 50 -> fail; true),
   dame_nombres_equipos_jornada(Num_jornada, Equipos_ya_participantes),
   dame_nombres_equipos(All_teams),
   subtract(All_teams, Equipos_ya_participantes, Equipos_faltantes),
   random_member(Equipo_1, Equipos_faltantes),
   delete(Equipos_faltantes, Equipo_1, Equipos_faltantes_2),
   random_member(Equipo_2, Equipos_faltantes_2),
   length(Equipos_faltantes, Size),
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