
# Comenzamos el código para resolver los problemas de programación entera.
# Utilizamos Python 3.6 como interfaz para el software Gurobi.

from gurobipy import *
import numpy as np
import itertools
import operator

# Leemos todas las muestras.
datos = [np.loadtxt("muestreo/datos" + str(i) + ".txt", skiprows=1) for i in range(1,41)]

# Aunque nuestro tamaño muestra varía, vamos a resolver siempre 20 problemas.
# Es decir, cada elemento de datos es una lista con 20*(15*i) listas de 10 elementos.

# Con la siguiente función resolvemos un único problema asociado a una muestra
# W1,...,Wn.
def problema(w):
    
    # Dimensión
    n = 17
    N = len(w)
    # Creamos un modelo
    m = Model("escenarios");
    
    # Vector de costes
    c = [0.55, 0.57, 0.46, 0.02,0.25, 0.34, 0.89, 0.49 ,0.50, 0.91,
      0.52, 0.4, 0.6, 0.2,0.225, 0.344, 0.9]
    
    # Parámetro de rendimiento
    v = 19
    
    # Probabilidad
    alfa = 0.05
    beta = 1 - alfa

    # Creamos las variables. El tipo puede ser 
    # 'C' para continuas, 'B' para binarias, 'I' para enteras,
    # 'S' para semicontinuas, or 'N' for semienteras.
    x = m.addVars(range(n), vtype='B', name="x");
    z = m.addVars(range(N), vtype='B', name="z")
    
    # Definimos un producto escalar 
    def productx(v):
        k = 0
        for i in range(len(v)):
            k = k + v[i]*x[i]
        return(k)    
                
    # Definimos la función objetivo y queremos maximizar/minimizar.
    m.setObjective(productx(c), GRB.MINIMIZE);
                
    # Añadimos las restricciones
    def productz(v):
        k = 0
        for i in range(len(v)):
            k = k + v[i]*z[i]
        return(k)   
        
    unos = np.repeat(1,N)
    m.addConstr(productz(unos) >= N*beta, "c0");
    
    for i in range(N):
        m.addConstr(productx(w[i])-v >= -100*(1-z[i]), "cad");
                
                
    m.optimize();
         
    s=[]
    for i in range(17):
        q = m.getVars()
        v = q[i]
        s.append(v.x)
      
#  print(v.varName, v.x)
        
    return([m.objVal,s])  

# Hemos exportado todas las 20 muestras, para cada tamaño muestral, todas 
# juntas, por lo que es necesario dividirlas en paquetes de tamaño adecuado.
def divide(v,n):
    return([v[x:x+n] for x in range(0, len(v), n)])
        
# Finalmente, utilizamos este método para calcular la media y la mediana
# truncadas con 3 decimales.
def resuelve(w):
    N = len(w)//20
    a = divide(w,N)
    s = [problema(a[i])[0] for i in range(0,20)]
    mean = np.mean(s)
    med = np.median(s)
    k = np.round(mean,3)
    l = np.round(med,3)
    return([k,l])

# Utilizamos este comando para ver la solución más repetida en cada caso
# y tomarla como solución asociada al tamaño muestral.

def modaux(L):
  SL = sorted((x, i) for i, x in enumerate(L))
  # print 'SL:', SL
  groups = itertools.groupby(SL, key=operator.itemgetter(0))
  def _auxfun(g):
    item, iterable = g
    count = 0
    min_index = len(L)
    for _, where in iterable:
      count += 1
      min_index = min(min_index, where)
    return count, -min_index
  return max(groups, key=_auxfun)[0]

def moda(w):
    N = len(w)//20
    a = divide(w,N)
    s = [problema(a[i])[1] for i in range(0,20)]
    h = modaux(s)
    return(h)
    

punt = [moda(w) for w in datos]

