# Plantilla de la evaluación final Ciencia Social Abierta 2026

## Enlace del reporte renderizado

El reporte está disponible en GitHub Pages. Puedes acceder a él [aquí](https://renatodeluca.github.io/evaluacion-final-grupo-6/)

## Requisitos previos

- Quarto 1.5 o superior instalado en el sistema (https://quarto.org).
- Motor LaTeX disponible para la salida PDF (TinyTeX, TeX Live o MiKTeX).
- Git para clonar y versionar (opcional pero recomendado).
- R con los paquetes necesarios si se ejecutan analisis embebidos en los capitulos.

## Estructura principal del repositorio


El repositorio organiza el contenido en capítulos base (`00–07`), apéndices, e includes para portadas y metadatos. La identidad visual se controla en `assets/styles.scss` y el diseño PDF en `reportes-facso-plantilla.tex`. La carpeta `refs/` aloja `apa.csl` y `referencias.bib`. Se sigue el [protocolo IPO](https://lisacoes.com/protocolos/a-ipo-rep/) con `input/` (datos), `procesamiento/` (scripts/notebooks) y `output/` (tablas/figuras listas).

La plantilla permite integrar tablas y gráficos que se actualizan al cambiar los datos. Las citas se gestionan con Pandoc usando claves de `refs/referencias.bib` (formato BibTeX/CSL JSON), con estilo APA por defecto (`refs/apa.csl`). Esto asegura consistencia entre texto, evidencias y bibliografía en ambas salidas (HTML y PDF).

El despliegue web se puede realizar mediante [**GitHub Pages**](https://www.youtube.com/watch?v=8IdBAysf-U4) desde `docs/`. 

## Flujo de trabajo sugerido

1. Organizar datos brutos en `input/data-orig/` y documentarlos brevemente.
2. Procesar la informacion dentro de `procesamiento/` usando scripts o notebooks; guardar productos limpios en `input/data-proc/` o `output/` segun corresponda.
3. Referenciar tablas y graficos listos desde los capitulos usando rutas relativas (por ejemplo `output/tables/tabla-01.html`).
4. Usar `freeze: auto` (configurado) para mantener resultados reproducibles y evitar re-ejecuciones innecesarias.
5. Antes de liberar una version final, ejecutar `quarto render --to html pdf` para garantizar paridad entre formatos.

## Elementos a considerar en la evaluación

- Preregistro de hipótesis
- Registro del proyecto en OSF
- Elaboración de pre-print
- Generación de GitHub Pages para la publicación de resultados
