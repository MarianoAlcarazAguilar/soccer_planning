import pandas as pd
import matplotlib.pyplot as plt

datos = pd.read_csv('../other_files/progreso_calificacion.csv')

fig, ax = plt.subplots(figsize=(15,5))
limite_superior = .8

ax.spines.top.set_visible(False)
ax.spines.right.set_visible(False)

ax.plot(datos.Calificacion, zorder=3)
ax.hlines(limite_superior, 0, datos.Calificacion.size, color='gray', zorder=1)
ax.set_title('Progreso de Calificación', size=18, fontname='Arial')
ax.set_xlabel('Número de Iteración', size=13, fontname='Arial')
ax.set_ylabel('Calificación', size=13, fontname='Arial')

fig.tight_layout()
fig.savefig('../other_files/progreso_calificacion.jpeg', dpi=300)

plt.close()