# Comenzamos el codigo para resolver el problema del matching mediante 
# resolucion directa del problema de Programacion Lineal.
# Utilizamos Python 3.7 como interfaz para el software Gurobi 8.1.1.

# El siguiente codigo genera una solucion y valor objetivo asociado
# para cada elemento de un mallado de valores de lambda, de manera que
# podemos observar como varian ambos en funcion de dicho parametro.

# A continuacion pasamos a programar el algoritmo de Groetschel-Holland 
# para resolver el problema biobjetivo del emparejamiento con funciones
# de coste c1, c2 y parametro lambda.

from gurobipy import *
import numpy as np
import operator
import networkx as nx
import random 
import itertools
from padbergmod import *

# DATOS DE ENTRADA
##############################################################################

# Establecemos una semilla predeterminada para asegurar la reproducibilidad
# de los resultados
#np.random.seed(1234)
#random.seed(1234)

# Construimos un grafo aleatorio
mat = nx.gnp_random_graph(10,0.5)

# Numerando los vertices a partir de 0, obtenemos la matriz de adyacencia
# del grafo como un array 
#A = nx.convert_matrix.to_numpy_matrix(mat)
A = np.array([[0., 1., 1., 1., 0., 0., 0., 0., 1., 1.],
        [1., 0., 1., 0., 1., 0., 0., 1., 1., 0.],
        [1., 1., 0., 0., 0., 0., 1., 1., 0., 0.],
        [1., 0., 0., 0., 1., 1., 0., 1., 0., 1.],
        [0., 1., 0., 1., 0., 1., 0., 1., 0., 1.],
        [0., 0., 0., 1., 1., 0., 0., 0., 0., 0.],
        [0., 0., 1., 0., 0., 0., 0., 1., 0., 0.],
        [0., 1., 1., 1., 1., 0., 1., 0., 1., 1.],
        [1., 1., 0., 0., 0., 0., 0., 1., 0., 1.],
        [1., 0., 0., 1., 1., 0., 0., 1., 1., 0.]])
#A = np.loadtxt(open("file_name.csv", "rb"), delimiter=",", skiprows=0)
# Numero de vertices
N = len(A)

# Sea M el numero de puntos en el que queremos dividir el intervalo [0,1],
# es decir, los M+1 un puntos que forman la malla de valores de lambda para
# los cuales vamos a resolver el problema.
M = 5
lambdas = np.array(range(0,M+1,1))/M

# FUNCIONES 
##############################################################################

# Construimos algunas funciones que nos resultaran utiles.

# Comencemos creando el conjunto de indices que vamos a utilizar
# para las variables
def indices(a):
    s = []
    for i in range(N):
        for j in range(i+1,N):
            if (a[i,j] == 1):
                s.append((i,j))
    return(s)

# Calculamos una vez el conjunto de indices, que no es mas que las aristas
# del grafo ordendas de forma lexicografica y sin repeticiciones.
IND = indices(A)

# Construimos las funciones de coste. En este caso las generamos de forma
# aleatoria.
c1 = np.random.rand(len(IND))
c2 = np.random.rand(len(IND))
    
# Definimos la funcion delta, que nos da los ejes adyacentes a un vertice dado.
def delta(v,A):
    s = [x for x in indices(A) if x[0] == v or x[1] == v]
    return(s)
    
# Construimos una funcion que comprueba si un vector es de coordenadas enteras
def entero(s):
    a = map(lambda x: x == int(x),s)
    return(all(a))

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
    for i in range(3,N,2):
        conj.update(set(itertools.combinations(set(G.nodes()), i)))
    for s in conj:
        card = (len(s)-1)/2
        M = H.subgraph(s)
        aris = [M[e[0]][e[1]]['weight'] for e in M.edges()]
        val = sum(aris)
        if (val > card):
            return("No está dentro del poliedro por los odds")
    return("Está dentro")
    
    


# OPTIMIZACION
###############################################################################
    
# Pasamos a definir la funcion optimiza, que resuelve el problema del
# emparejamiento parametro con parametros
# lamb: Valor de lambda
# c1,c2: Funciones de coste
# indices: Conjunto de aristas que definen el problema

def optimiza(lamb, c1 = c1, c2 = c2, indices = IND):
    
    # Paso 1: En primer lugar deberiamos seleccionar un conjunto de aristas
    # relativamente pequeño para iniciar el algoritmo, pero dado que ya de por
    # si cuesta lidiar con grafos relativamente grandes, podemos permitirnos
    # utilizar la cantidad total de aristas, ya que estas crecen a lo sumo de 
    # forma cuadratica con respecto los vertices. El problema principal es el
    # de las restricciones, que sabemos con certeza que crecen de manera
    # exponencial respecto a los vertices. 
    
    # Al utilizar todos las aristas, si la solucion optima de algun subproblema
    # es de coordenadas enteras, entonces es optima global y, por tanto, no es
    # necesario añadir rutinas para acutalizar el conjunto de aristas.
    
    # Paso 2: En este caso, Holland y Groetschel nos indican que utilicemos un 
    # algoritmo voraz para encontrar una solucion inicial, pero para eso
    # Gurobi tiene implementado un presolve, asi que pasamos a crear el modelo
    m = Model("biobjetivo");

    # Creamos las variables. El tipo puede ser 
    # 'C' para continuas, 'B' para binarias, 'I' para enteras,
    # 'S' para semicontinuas, or 'N' for semienteras.
    # El parametro lb nos da una cota inferior a las variables.
    x = m.addVars(IND, lb = 0, vtype='C', name = "x");
    
    # Definimos algunas funciones para ayudarnos a escribir las restricciones
    # asi como la funcion objetivo de manera comoda y general.
    def suma(S):
        s = 0
        for (a,b) in S:
            if a<b:
                s = s + x[a,b]
            else:
                s = s + x[b,a]
        return(s)
        
    def objind(c):
        s = 0
        for i in range(len(IND)):
            (a,b) = IND[i]
            s = s + c[i]*x[a,b] 
        return(s)
    
    # De esta forma, generamos las dos funciones objetivo. Notemos que 
    # escribimos directamente los costes sobre las variables aristas
    # del modelo, pues si una arista no esta en el grafo, no tiene sentido
    # generar una variable para ella.       
    cos1 = objind(c1)
    cos2 = objind(c2)
    
    # Definimos la funcion objetivo y queremos maximizar/minimizar.
    m.ModelSense = GRB.MINIMIZE
    
    # Añadimos la funcion objetivo
    m.setObjective((1-lamb)*cos1+lamb*cos2); 
    
    # Generamos el primer bloque de restricciones.
    for i in range(N):
        m.addConstr(suma(delta(i,A)) == 1, name = "delta" + str(i));
    
    # Dejamos comentado los comandos que añaden el número exponencial de
    # restricciones del poliedro del matching. 
    
    # Paso 4: Resolvemos el prolbema.
    m.optimize()
    
    # Paso 5: Comprobamos si la solucion es entera y, en caso de no serlo,
    # utilizamos las distintas heuristicas y metodos para encontrar
    # planos de corte.
    
    # Obtenemos el vector de soluciones
    sols = [m.getVars()[i].X for i in range(len(IND))]
    entero(sols)
    while (not (entero(sols))):        
#        # Paso 7.1, 7.2: Vamos a programar las heuristica para detectar 
#        # planos de corte.
#        
#        # Fijamos un parametro epsilon de tolerancia para la segunda heuristica.
#        # Tal y como Groetschel y Holland en su trabajo, tomamos 0.3. 
#        epsilon = 0.3 
#        
#        # Obtenemos las aristas con variables asociadas en
#        aristasN = [IND[i] for i in range(len(sols)) if sols[i]>0]
#        aristasE = [IND[i] for i in range(len(sols)) if sols[i]>epsilon]
#            
#        # Construimos sendos grafos.
#        GN = nx.Graph()
#        GN.add_edges_from(aristasN)
#        
#        GE = nx.Graph()
#        GE.add_edges_from(aristasE)
#        
#        # Buscamos las componentes conexas que tengan cardinalidad impar
#        conN = [a for a in list(nx.connected_components(GN)) if len(a) %2 == 1]
#        
#        # Tenemos que tener en cuenta que los planos de corte que proporciona
#        # la segunda heuristica pueden no ser utiles, ya que nuestra solucion
#        # no tiene por que violarlos necesariamente y seria un esfuerzo 
#        # innecesario volver a reoptimizar para nada, por lo que solo nos 
#        # quedamos con dichas componentes. Definimos previamente una funcion
#        # que nos haga dicha comprobacion
#        def comprueba(nodos):
#            aristaux = list(GE.edges(nodos))
#            aristord = [(a,b) if a<b else (b,a) for (a,b) in aristaux]
#            indiaux = [IND.index(a) for a in aristord]
#            card = (len(nodos)-1)/2
#            suma = sum([sols[i] for i in indiaux])
#            return(suma <= card)
#        
#        # Generamos las componentes adecuadas
#        conE = [a for a in list(nx.connected_components(GE)) if len(a) %2 == 1 
#                and comprueba(a)]
#        
#        # Si las lista con es no vacia, hemos encontrado planos de cortes, los
#        # añadimos a nuestro modelo y volvemos a comprobar. 
#        if (conN != []):
#            for a in conN:
#                S = list(GN.edges(a))
#                card = len(a)-1
#                m.addConstr(suma(S) <= card/2) 
#            m.optimize()
#            print("Hemos usado 1")
#            
#        elif (conE != []):
#            for a in conE:
#                S = list(GE.edges(a))
#                card = len(a)-1
#                m.addConstr(suma(S) <= card/2) 
#            m.optimize()
#            print("Hemos usado 2")
#        
#        # Paso 7.3: En el caso de que las heuristicas no hayan funcionado, 
#        # construimos un plano de corte mediante el procedimiento de Padberg
#        # y Rao.
#        else:        
        W,F = padraomod(nx.Graph(A),sols)
        m.addConstr(suma(W.difference(F))-suma(F) >= 1-len(F))
        m.optimize()
        print("Hemos usado Padberg-Rao")
        sols = [m.getVars()[i].X for i in range(len(IND))]
        print(entero(sols))
    print(entero(sols))
    return(m)

modelo = optimiza(0)
sols = [modelo.getVars()[i].X for i in range(len(IND))]