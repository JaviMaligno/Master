# A continuacion vamos a programar en Python 3.7 el metodo de Padberg-Rao 
# modificado para, dado un grafo G y un vector x, obtener una desigualdad
# del poliedro del matching -en caso de que exista- que sea violada por x.

import networkx as nx
import numpy as np

x = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 1.0]

def padraomod(G,x):
    # Para aplicar el algoritmo, consideramos el vector de pesos
    c = np.array([min(a,1-a) for a in x])
    
    # Construimos el nuevo grafo H sobre G con capacidad c, atributo con el
    # que calculamos el arbol de Gomory-Hu, y con peso x.
    H = nx.Graph()
    for i, e in enumerate(G.edges()):
        H.add_edge(e[0],e[1],capacity = c[i], weight = x[i])
    
    # Definimos una funcion que nos de la suma de los pesos de un conjunto
    # de aristas
    def coste(G,S):
        s = 0
        for e in S:
            s = s + G[e[0]][e[1]]['weight']
        return(s)
        
    # A continuacion, consideramos el arbol de Gomory-Hu para el grafo H
    GomHu = nx.gomory_hu_tree(H, capacity = 'capacity')
    
    # Consideramos ahora el conjunto de corte asociado a cada arista del arbol
    # y para cada uno de ellos buscamos una desigualdad deseada.
    for e in GomHu.edges():
        # Asignamos una copia de GomHu a la cual eliminamos la arista y 
        # calculamos las componentes conexas
        Te = nx.Graph(GomHu)
        Te.remove_edge(*e)
        U,V = list(nx.connected_components(Te))
        
        # Calculamos las arista del conjunto de cortes
        cutset = set()
        for l, nbrs in ((n, H[n]) for n in U):
            cutset.update((l, y) for y in nbrs if y in V)
        cutset = set(H.edges(U))    
        # Nodos del cutset
        
        # Tenemos que encontrar ahora el conjunto F con las propiedades 
        # deseadas. 
        Fe = set([e for e in cutset if 1- H[e[0]][e[1]]['weight'] < 
              H[e[0]][e[1]]['weight']])
        
        # Si el conjunto anterior verifica la propiedad de que la suma de los
        # cardinales sea impar, es el optimo. En otro caso obtenemos el optimo
        # mediante el siguiente metodo
        if ((len(cutset) + len(Fe)) % 2 == 0):
            def term(e):
                s = max(H[e[0]][e[1]]['weight'],1-H[e[0]][e[1]]['weight'])
                t = min(H[e[0]][e[1]]['weight'],1-H[e[0]][e[1]]['weight'])
                return(s-t)
            Faux = sorted([(term(e),e) for e in cutset])
            fp = set()
            fp.add(Faux[0][1])
            Fe = Fe.symmetric_difference(fp)
        
        # Finalmente, comprobamos si se viola la desigualdad y, en dicho caso,
        # hemos acabado y devolvemos.
        des = coste(H,cutset.difference(Fe)) + len(Fe) - coste(H,Fe)
        if (des<1):
            return((cutset,Fe))
            
        
    # Notemos que dado que este algoritmo es correcto, si x es una solcion
    # fraccional necesariamente ha de encontrarse algun plano de corte.
    return("La has liado")
