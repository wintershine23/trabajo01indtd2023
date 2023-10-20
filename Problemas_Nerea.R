#######################################################################
# PROBLEMA 1
#######################################################################
# Se pide resolver la siguiente tabla de decisión.
# Estados de la naturaleza
# Alternativas
# e1 e2
# a1 3.000 -1.000
# a2 2.000 2.000
# Se debe resolver con cada uno de los métodos o funciones individuales de Incertidumbre
# por separado (tanto en situación favorable como desfavorable)


#------------------------------------------------------------------------
# Realizamos la lectura del material complementario
#------------------------------------------------------------------------

source("teoriadecision_funciones_incertidumbre.R")

#------------------------------------------------------------------------
# Creamos la tabla.
#------------------------------------------------------------------------

#Realizamos la creación de la tabla, ayudados de las funciones en el archivo anterior.

tablaProblema1N<-crea.tablaX(c(3,-1,
                               2,2),2,2)

# Tenemos que como los datos estan en miles de euros lo pondremos en unidades simples. Siendo 3 realmente 3000.

# Notar que tenemos dos estados de la naturaleza y dos posibles alternativas.

#------------------------------------------------------------------------
# Procedamos a la resolución
#------------------------------------------------------------------------

# Vamos a proceder a resolverlo mediante cada uno de los métodos por separado.

# Tenemos que como no tenemos ninguna información, no sabemos si un criterio es más o menos probable que otro. Por lo que estamos en decisión bajo incertidumbre. Además, tenemos que ningún criterio tiene un peso mayor que otro. Por lo que respecto al criterio de huriwtzc alfa=0.5.
# Supondremos además que tenemos un único decisor.

# Supondremos primero la decisión suponiendo que son beneficios. Y a continuación, lo haremos para cada criterio suponiendo perdidas. Es decir, en el primer caso querremos maximizar beneficios y en el segundo caso minimizar perdidas.

##------------------------------
# 1) Criterio wald o pesimista
#------------------------------

criterio.Wald(tablaProblema1N,favorable = T)

# Tenemos que según este criterio la mejor alternativa es d2 con valor óptio 2

# Ahora suponiendo costes
criterio.Wald(tablaProblema1N,favorable = F)

# Nuevamente por el criterio de wald es preferible la segunda alternativa, nombrada como d2.

#-----------------------
# 2) Criterio óptimista
#-----------------------

criterio.Optimista(tablaProblema1N,favorable = T)

# Por el criterio optimista vemos que es preferible la primera alternativa(d1) con un valor óptimo de 3.

criterio.Optimista(tablaProblema1N,favorable = F)

# Nuevamente la alternativa preferida por este método es d1.

#-----------------------
# 3) Criterio Hurwicz
#-----------------------

criterio.Hurwicz(tablaProblema1N,alfa = 0.5,favorable = T)

# Por el criterio Hurwicz vemos que es preferible la primera alternativa(d2) con un valor óptimo de 2.

# Dibujaremos el criterio de hurwicz

dibuja.criterio.Hurwicz(tablaProblema1N,favorable = T)

# Esto nos da el gráfico en el que nos muestra con una linea punteada cual es la mejor alternativa en función de los valores de alfa. Para ver donde cambia cada alternativa (entre que valores de alfa) vamso a usar la siguiente función:

dibuja.criterio.Hurwicz_Intervalos(tablaProblema1N,favorable = T)

# Vemos que para valores de alfa entre 0 y 0.75 es preferible la segunda alternativa(d2), pero para valores entre 0.75 y 1 es preferible la primera(d1)


criterio.Hurwicz(tablaProblema1N,alfa = 0.5,favorable = F)

# En este caso, es preferible la opción d1. 

# Vemos que de momento este es el único criterio que ha variado de respuesta entre si estamos con beneficios o con costes


# Dibujaremos el criterio de hurwicz

dibuja.criterio.Hurwicz(tablaProblema1N,favorable = F)

# Esto nos da el gráfico en el que nos muestra con una linea punteada cual es la mejor alternativa en función de los valores de alfa. Para ver donde cambia cada alternativa (entre que valores de alfa) vamso a usar la siguiente función:

dibuja.criterio.Hurwicz_Intervalos(tablaProblema1N,favorable = F)

# Vemos que para valores de alfa entre 0 y 0.25 es preferible la segunda alternativa(d2), pero para valores entre 0.25 y 1 es preferible la primera(d1)

#-----------------------
# 4) Criterio Savage
#-----------------------

criterio.Savage(tablaProblema1N,favorable = T)

# Vemos que es preferible la alternativo d2 según el criterio de Savage. Con valor óptimo 1.

criterio.Savage(tablaProblema1N,favorable = F)
# Vemos que el criterio de Savage en costes prefiere la opción 1.

#-----------------------
# 5) Criterio Laplace
#-----------------------

criterio.Laplace(tablaProblema1N,favorable = T)
# Vemos que es preferible la alternativo d2 según el criterio de Laplace. Con valor óptimo 2.

criterio.Laplace(tablaProblema1N,favorable = F)
# Vemos que el criterio de Laplace en costes prefiere la opción 1 con valor óptimo 1.


#-------------------------
# 6) Criterio Punto Ideal
#-------------------------

criterio.PuntoIdeal(tablaProblema1N,favorable = T)
# Vemos que es preferible la alternativo d2 según el criterio de el Punto Ideal. Con valor óptimo 1.

criterio.PuntoIdeal(tablaProblema1N,favorable = F)
# Vemos que el criterio del Punto Ideal en costes prefiere la opción 1 con valor óptimo 1.




#######################################################################
# PROBLEMA 2
#######################################################################
# Pepe es un estudiante que estudia un grado superior en su pueblo natal.
#Se plantea si cambiarse a un instituto de Sevilla, donde hay mayor cantidad de empresas, en las cuales espera obtener trabajo. Mudarse a Sevilla, le reportaría un coste de 3.000 euros.
#Sin embargo, si obtiene trabajo (suponiendo que la duración mínima fuera un año, con sueldo de 2000 euros), obtendría un benefcio de 24.000 euros.
# Por el contrario, si no se cambia de instituto, no tendría ningún gasto.

# Las dos alternativas posibles para Pepe son:
# a1= Realizar un cambio de instituto
# a2= Quedarse en el instituto en el que esta

# Los dos estados de la naturaleza posibles son:
# a1= Ser contratado
# a2= No ser contratado

#------------------------------------------------------------------------
# Creamos la tabla.
#------------------------------------------------------------------------

# Tenemos nuevamente dos alternativas y dos estados de la naturaleza.
# Tenemos también una serie de costes y beneficios para cada alternativa. Por lo que supondremos la tabla de decisión como la resultante de Beneficios-costes para cada alternativa respecto de cada estado de la naturaleza.

# Igual que en el ejericicio anterior, al venir en miles de euros. Las unidades de la tabla serán miles de euros.

tablaProblema2N<-crea.tablaX(c(21,-3,
                               -24,0),2,2)
#------------------------------------------------------------------------
# Procedamos a la resolución
#------------------------------------------------------------------------

# Usaremos la función que engloba todos los criterios para la resolución del problema.

# Tenemos que como no tenemos ninguna información, no sabemos si un criterio es más o menos probable que otro. Por lo que estamos en decisión bajo incertidumbre. Además, tenemos que ningún criterio tiene un peso mayor que otro. Por lo que respecto al criterio de huriwtzc alfa=0.5.
#También, podemos observar que tenemos un único decisor y que no muestra tendencia por nada, es decir, no se le ve pesimista u optimista.

# Claramente, Pepe quiere realizar la opción que mayor beneficio le reporte

criterio.Todos(tablaProblema2N,0.5,favorable = T)

# Vemos que todos los criterios indican que la mejor decisión es la primera alternativa, es decir, cambiarse de instituto.

# Vemos que según el criterio de Savage(del coste de oportunidad) el cambiarse de instituto es la mejor opción.