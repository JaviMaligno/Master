# A continuacion vamos a programar en Python 3.7 el metodo de Padberg-Rao 
# modificado para, dado un grafo G y un vector x, obtener una desigualdad
# del poliedro del matching -en caso de que exista- que sea violada por x.

import networkx as nx
import numpy as np

# En primer lugar tenemos que definir una rutina que, dado un grafo G y un
# vector de costes, nos calcule el arbol de Gomory
A = nx.gnp_random_graph(10,0.5)
N = len(A.edges())
x = np.random.rand(N)

def padbergraomod(G,x):
    # Para aplicar el algoritmo, consideramos el vector de pesos
    c = np.array([min(a,1-a) for a in x])
    
    # Construimos el nuevo grafo H sobre G con los pesos c 
    H = nx.Graph()
    for i, e in enumerate(G.edges()):
        H.add_edge(e[0],e[1],capacity = c[i])
        
    # A continuacion, consideramos el arbol de Gomory-Hu para el grafo H
    GomHu = nx.gomory_hu_tree(H)

    # Delta(W) es el cutset asociado a cada arista del arbol y, naturalmente,
    # W es el conjunto de nodos a los que pertenecen esas aristas. Enterate
    # y programalo de una vez hijo de puta.
    
    return()
