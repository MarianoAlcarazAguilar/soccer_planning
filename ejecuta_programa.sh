#! /bin/bash

echo -e 'Por favor espera, estamos crendo tu nueva temporada.\nEsto puede tardar unos minutos.'
swipl -c ./prolog_files/calendario_partidos_ai.pl

echo -e 'Graficando Resultados\n'
python ./python_files/grafica_progreso.py

rm a.out

echo 'Guardando Cambios en Git'
sleep 1
git add .
sleep 1
git commit -m 'Commit automático de script ejecuta_programa.sh'