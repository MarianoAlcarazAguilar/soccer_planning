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

