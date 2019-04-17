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
import itertools 
import operator
import networkx as nx
import random 
0,1,2,3,4,5,16,20,23,24,

# DATOS DE ENTRADA
##############################################################################

# Establecemos una semilla predeterminada para asegurar la reproducibilidad
# de los resultados
np.random.seed(1234)
random.seed(1234)

# Construimos un grafo aleatorio
mat = nx.gnp_random_graph(200,0.3)

# Numerando los vértices a partir de 0, obtenemos la matriz de adyacencia
# del grafo como un array 
#A = nx.convert_matrix.to_numpy_matrix(mat)
A = np.loadtxt(open("file_name.csv", "rb"), delimiter=",", skiprows=0)
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

# A continuacion, definimos la funcion gamma, que dado un conjunto
# de vertices, debe devolvernos las aristas que tienen extremos dentro
# de dicho conjunto.
def gamma(S,A):
    m = []
    s = []
    for v in S:
        s = s + delta(v,A)
    seti = set(s)
    for (a,b) in seti:
        if (a in S and b in S):
            m.append((a,b))
    return(m)
    
# Definimos la operacion diferencia de conjuntos
def diff(conj1, conj2):
        conj2 = set(conj2)
        return [item for item in conj1 if item not in conj2]
    
# Definimos una funcion combinatoria para obtener los conjuntos impares
# de vertices.
def subconj(N,m):
    S = list(range(N))
    return list(itertools.combinations(S, m))

# Calculamos los conjuntos impares que dan lugar a restricciones
#IMPARES = []
#for i in range(3,N,2):
#    IMPARES = IMPARES + subconj(N,i)

# Construimos una funcion que comprueba si un vector es de coordenadas enteras
def entero(s):
    a = map(lambda x: x == int(x),s)
    return(all(a))

# OPTIMIZACION
###############################################################################
    
# Pasamos a definir la funcion optimiza, que resuelve el problema del
# emparejamiento parametro con parametros
# lamb: Valor de lambda
# c1,c2: Funciones de coste
# indices: Conjunto de aristas que definen el problema

def optimiza(lamb, c1 = c1, c2 = c2, indices = IND):
    
    # Paso 1: En primer lugar deberíamos seleccionar un conjunto de aristas
    # relativamente pequeño para iniciar el algoritmo, pero dado que ya de por
    # si cuesta lidiar con grafos relativamente grandes, podemos permitirnos
    # utilizar la cantidad total de aristas, ya que estas crecen a lo sumo de 
    # forma cuadratica con respecto los vertices. El problema principal es el
    # de las restricciones, que sabemos con certeza que crecen de manera
    # exponencial respecto a los vertices.
    
    # Paso 2: En este caso, Holland y Groetschel nos indican que utilicemos un 
    # algoritmo voraz para encontrar una solucion inicial, pero para eso
    # Gurobi tiene implementado un presolve, así que pasamos a crear el modelo
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
    # del modelo, pues si una arista no está en el grafo, no tiene sentido
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
        
    # Paso 4: Resolvemos el prolbema.
    m.optimize()
    
    # Paso 5: Comprobamos si la solucion es entera y, en caso de no serlo,
    # utilizamos las distintas heuristicas y metodos para encontrar
    # planos de corte.
    
    # Obtenemos el vector de soluciones
    sols = [m.getVars()[i].X for i in range(len(IND))]
    num = 0
    while (not (entero(sols)) and num < 5):        
        num = num +1
        
        # Paso 7.1: Vamos a programar la primera heuristica para detectar 
        # planos de corte.
        
        # Obtenemos las aristas con coste no nulo.
        aristas = [IND[i] for i in range(len(sols)) if sols[i]>0]

        # Construimos el grafo en el que solo estan las aristas con solcion 
        # no nula
        G = nx.Graph()
        G.add_edges_from(aristas)
        
        # Buscamos las componentes conexas que tengan cardinalidad impar
        con = [x for x in list(nx.connected_components(G)) if len(x) % 2 == 1 ]
        print(con)
        # Si las lista con es no vacia, hemos encontrado planos de cortes, los
        # añadimos a nuestro modelo y volvemos a comprobar. En otro caso, 
        # utilizamos la segunda heuristica.
        if (con != []):
            for a in con:
                S = list(G.edges(a))
                card = len(a)-1
                m.addConstr(suma(S) <= card/2)
            m.optimize()
        sols = [m.getVars()[i].X for i in range(len(IND))]
        
        
        # Paso 7.2: En el caso de que no hayamos encontrado dicho plano de
        # corte usando la heurisitca anterior, utilizamos esta segunda:

            
        
            

    
#    # Imponemos las condiciones sobre los conjuntos impares.
#    for a in IMPARES:

#    print(m.getVars())
#    # En el caso que decidamos utilizar una base inicial ejecutamos
#    if (vbas != []):
#        m.update()
#        for i in range(len(vbas)):
#            m.getVars()[i].setAttr("VBasis",vbas[i])
#        for i in range(len(cbas)):
#            m.getConstrs()[i].setAttr("CBasis",cbas[i])
#    
#    # Resolvemos el modelo
#    m.optimize()
    print(entero(sols))
    return(m)

modelo = optimiza(0)
#    