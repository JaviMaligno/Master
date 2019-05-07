# A continuacion generamos grafos alaetorios y ponemos a prueba los metodos
# y comparamos resultados.

from holland import *
from integer import *
from matching import *
import networkx as nx
import numpy as np
import time
import pandas as pd

# Establecemos una semilla predeterminada para asegurar la reproducibilidad
# de los resultados

# La siguiente funcion nos devuelve el tiempo e iteraciones medias de los 
# distintos metodos que hemos programado para funciones y grafos aleatorios
# con un numero de nodos N y utilizando I casos.

def analisis(N, I, par1 = 0, par2 = 0, lamb = 0):
    iter0, iter1, iter2 = 0, 0, 0
    t0, t1, t2 = 0, 0, 0
    bcut = 0
    aristas = 0
    for i in range(I):
        #Construimos un grafo aleatorio
        H = nx.gnp_random_graph(N,0.5)
        E = len(H.edges())
        
        # Construimos funciones de coste aleatorias
        c1 = np.random.rand(E)
        c2 = np.random.rand(E)
        
        # Optimizamos utilizando Groetschel-Holland
        modelo0, simplex0, tiempo0 = optimiza(H,c1,c2,lamb)
        
        # Actualizamos los parametros
        iter0 = iter0 + simplex0
        t0 = t0 + tiempo0
        aristas = aristas + E
        # Si hemos activado la Programacio Entera
        if (par1 == 1):
            # Optimizamos usando Programacion Entera

            modelo1, simplex1, bcut1, tiempo1 = integer(H,c1,c2,lamb)

            # Actualizamos los parametros
            iter1 = iter1 + simplex1
            t1 = t1 + tiempo1
            bcut = bcut + bcut1
        
        if (par2 == 1):
            # Utilizamos el simplex normal
            modelo2, simplex2, tiempo2 = matching(H,c1,c2,lamb)
            
            # Actualizamos los parametros
            iter2 = iter2 + simplex2
            t2 = t2 + tiempo2
        
    return([N, I, aristas/I, iter0/I,  iter1/I, iter2/I,  
            t0/I, t1/I, t2/I, bcut/I])
      
