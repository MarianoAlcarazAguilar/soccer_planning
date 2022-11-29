#! /bin/bash

swipl -c calendario_partidos_ai.pl

echo -e 'Graficando Resultados\n'
python grafica_progreso.py

echo 'Eliminando cosas innecesarias (a.out)'
rm a.out