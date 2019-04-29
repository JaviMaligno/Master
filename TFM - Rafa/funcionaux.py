# En este codigo pasamos a escribir funciones auxilares para el resto del
# trabajo.

import networkx as nx
import numpy as np
import operator
import random 
import itertools


# Construimos una funcion que comprueba si un vector es de coordenadas enteras
def entero(s):
    a = map(lambda x: x == int(x),s)
    return(all(a))
    
# Dado un grafo G y un subcojunto de aristas F con capacidad, construimos
# una funcion que nos de la suma de las capacidades de F.
def totalcap(G,F,capacity = 'weight'):
    cap = [G[e[0]][e[1]][capacity] for e in F]
    val = sum(cap)
    return(val)

# Definimos la funcion delta, que dado un conjunto U de vertices, nos devuelve
# las aristas que tienen un unico extremo en U.
def delta(G,U):
    ejes = set()
    for l, nbrs in ((n, G[n]) for n in U):
        ejes.update((l, y) if l<y else (y,l) for y in nbrs if y not in U)
    return(ejes)
    
# Por propósitos teóricos, aunque no la utilicemos en nuestro algoritmo, vamos
# a definir una funcion que comprueba si una solucion x esta en el poliedro
# del matching de una grafo G mediante la comprobacion del numero exponencial
# de las desigualdades sobre los conjuntos impares.
def comprueba(G,x):
    H = nx.Graph()
    for i, e in enumerate(G.edges()):
        H.add_edge(e[0],e[1],weight = x[i])
    for v in H.nodes():
        val = sum([H[e[0]][e[1]]['weight'] for e in H.edges(v)])
        if (val > 1):
            return("No está dentro por los vértices") 
    conj = set()
    for i in range(3,len(G),2):
        conj.update(set(itertools.combinations(set(G.nodes()), i)))
    for s in conj:
        card = (len(s)-1)/2
        M = H.subgraph(s)
        val = totalcap(M,M.edges())
        if (val > card):
            return("No está dentro del poliedro por los odds")
    return("Está dentro")