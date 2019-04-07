# Comenzamos el codigo para resolver el problema del matching mediante 
# resolucion directa del problema de Programacion Lineal.
# Utilizamos Python 3.6 como interfaz para el software Gurobi.

# El siguiente codigo genera una solucion y valor objetivo asociado
# para cada elemento de un mallado de valores de lambda, de manera que
# podemos observar como varian ambos en funcion de dicho parametro.

from gurobipy import *
import numpy as np
import itertools 
import operator

# Numeramos los vertices comenzando en 0.

# Matriz de adyacencias del grafo.
A = np.array([[0,1,0,0,1,0],[1,0,1,0,1,0],[0,1,0,1,0,1],
             [0,0,1,0,1,1],[1,1,0,1,0,0],[0,0,1,1,0,0]])

# Numero de vertices
N = len(A)

# Construyamos algunas funciones que nos resultaran utiles
# para describir generalmente el problema

# Comencemos creando el conjunto de indices que vamos a utilizar
# para las variables
def indices(n,a):
    s = []
    for i in range(n):
        for j in range(i+1,n):
            if (a[i,j] == 1):
                s.append((i,j))
    return(s)

# Calculamos una vez el conjunto de indices
IND = indices(N,A)

# Definimos la funcion delta, que nos da los vertices
# ejes adyacentes a un vertice dado.
def delta(v):
    s = []
    for (a,b) in IND:
        if (a==v or b==v):
            s.append((a,b))
    return(s)

# Calculamos una vez el conjunto delta
deltaV = [delta(v) for v in range(N)]

# A continuacion, definimos la funcion gamma, que dado un conjunto
# de vertices, debe devolvernos las aristas que tienen extremos dentro
# de dicho conjunto.
def gamma(S):
    m = []
    s = []
    for v in S:
        s = s + delta(v)
    seti = set(s)
    for (a,b) in seti:
        if (a in S and b in S):
            m.append((a,b))
    return(m)

def subconj(N,m):
    S = list(range(N))
    return list(itertools.combinations(S, m))

# Calculamos los conjuntos impares que dan lugar a restricciones
IMPARES = []
for i in range(3,N,2):
    IMPARES = IMPARES + subconj(N,i)
    
M = 10
lambdas = np.array(range(0,M+1,1))/M

def lambdafun(lam):
    # Creamos un modelo
    m = Model("escenarios");
    
    # Creamos las variables. El tipo puede ser 
    # 'C' para continuas, 'B' para binarias, 'I' para enteras,
    # 'S' para semicontinuas, or 'N' for semienteras.
    # El parametro lb nos da una cota inferior a las variables.
    x = m.addVars(IND, lb = 0, vtype='C', name = "x");
    
    # Damos la siguiente función para ayudarnos a escribir las restricciones
    def suma(S):
        s = 0
        for (a,b) in S:
            s = s + x[a,b]
        return(s)
    # También diseñamos una para escribir las funciones objetivo
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
    c1 = [20,0,0,20,20,20,20,0]
    c2 = [0,20,20,20,20,0,0,20]       
    
    cos1 = objind(c1)
    cos2 = objind(c2)
    
    # Definimos la funcion objetivo y queremos maximizar/minimizar.
    m.ModelSense = GRB.MINIMIZE
    
    # Añadimos la funcion objetivo
    m.setObjective((1-lam)*cos1+lam*cos2); 
    
    # Pasamos a generar las restricciones.
    # Imponemos que cada vertice tenga una pareja.
    for i in range(N):
        m.addConstr(suma(delta(i)) == 1);
    
    # Imponemos las condiciones sobre los conjuntos impares.
    for a in IMPARES:
        S = gamma(a)
        card = len(a)-1
        m.addConstr(suma(S) <= card/2)
    m.optimize();
    
    # Preparamos la salida por pantalla
    s=[]
    for i in range(len(x)):
        q = m.getVars()
        v = q[i]
        s.append(v.x)
      
    return([m.objVal,s])


m = []
for i in lambdas:
    m.append(lambdafun(i))    
m