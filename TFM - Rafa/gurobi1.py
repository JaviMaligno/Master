# Comenzamos el código para resolver el problema del matching mediante 
# resolución directa del problema de Programación Lineal.
# Utilizamos Python 3.6 como interfaz para el software Gurobi.

from gurobipy import *
import numpy as np
import itertools 
import operator

# Numeramos los vértices comenzando en 0.

# Matriz de adyacencias del grafo.
A = np.array([[0,1,1,1],[1,0,1,0],[1,1,0,0],[1,0,0,0]])

# Número de vértices
N = len(A)

# Construyamos algunas funciones que nos resultarán útiles
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

# Definimos la función delta, que nos da los vertices
# ejes adyacentes a un vertice dado.
def delta(v):
    s = []
    for (a,b) in IND:
        if (a==v or b==v):
            s.append((a,b))
    return(s)

# Calculamos una vez el conjunto delta
deltaV = [delta(v) for v in range(N)]

# A continuación, definimos la funcion gamma, que dado un conjunto
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
    
try:
    # Creamos un modelo
    m = Model("escenarios");
    
    # Vector de costes
    c = [1,1,1/4,1/4]
    
    # Creamos las variables. El tipo puede ser 
    # 'C' para continuas, 'B' para binarias, 'I' para enteras,
    # 'S' para semicontinuas, or 'N' for semienteras.
    # El parámetro lb nos da una cota inferior a las variables.
    x = m.addVars(IND, lb = 0, vtype='C', name = "x");
    
    
    def suma(S):
        s = 0
        for (a,b) in S:
            s = s + x[a,b]
        return(s)
    # Definimos la función objetivo y queremos maximizar/minimizar.
    m.setObjective(x[0,1], GRB.MINIMIZE);
    
    # Pasamos a generar las restricciones.
    # Imponemos que cada vértice tenga una pareja.
    for i in range(N):
        m.addConstr(suma(delta(i)) == 1);
    
    # Imponemos las condiciones sobre los conjuntos impares.
    for a in IMPARES:
        S = gamma(a)
        card = len(a)-1
        m.addConstr(suma(S) <= card/2)
    m.optimize();
        
    for v in m.getVars():
        print('%s %g' % (v.varName, v.x))

    print('Obj: %g' % m.objVal)

except GurobiError as e:
    print('Error code ' + str(e.errno) + ": " + str(e))

except AttributeError:
    print('Encountered an attribute error')