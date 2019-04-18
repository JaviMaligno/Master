# A continuacion vamos a programar en Python 3.7 el metodo de Padberg-Rao 
# modificado para, dado un grafo G y un vector x, obtener una desigualdad
# del poliedro del matching -en caso de que exista- que sea violada por x.

import networkx as nx
import numpy as np

# En primer lugar tenemos que definir una rutina que, dado un grafo G y un
# vector de costes, nos calcule el arbol de Gomory
A = nx.gnp_random_graph(10,0.5)
N = len(A.edges())
c = np.random.rand(N)


A.edges()
A
G = nx.Graph()
G.add_node(A)
def gomorytree(A,c):
    Vt = 


# Vamos a definir una funcion cuyas entradas sean una grafo G y un vector x

def padbergraomod(G,x):
    # En primer lugar, consideramos el grafo G con los capacidad x, lo cual
    # sera necesario para aplicar los metodos de cortes minimos.
    H = nx.Graph()
    for i, e in enumerate(G.edges()):
        H.add_edge(e[0],e[1],capacity = x[i])
        
    # Comenzamos el bucle sobre los conjuntos de T
    Vt = list(H.nodes())
    
    return()
