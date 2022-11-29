#! /bin/bash

swipl -c ./prolog_files/calendario_partidos_ai.pl

echo -e 'Graficando Resultados\n'
python ./python_files/grafica_progreso.py

echo 'Eliminando cosas innecesarias (a.out)'
rm a.out

echo 'Guardando Cambios en Git'
sleep 1
git add .
sleep 1
git commit -m 'Commit automático de script ejecuta_programa.sh'