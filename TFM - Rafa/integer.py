# En este codigo resolvemos con Gurobi el problema biobjetivo del
# emparejamiento con funciones de coste c1, c2 y parametro lambda utlizando
# la formulacion del problema como problema de Progrmacion Entera.

from gurobipy import *
from funcionaux import *
import time


# Pasamos a definir la funcion entero, que resuelve el problema del
# emparejamiento parametro con parametros
# G: Un grafo.
# lamb: Valor de lambda.
# c1,c2: Funciones de coste

def integer(G, c1, c2, lamb = 0):
    
    start = time.time()
    
    m = Model("biobjetivo");
    
    # Algunos parametros del grafo
    N = len(G)
    E = len(G.edges())
    
    # Creamos las variables. El tipo puede ser 
    # 'C' para continuas, 'B' para binarias, 'I' para enteras,
    # 'S' para semicontinuas, or 'N' for semienteras.
    # El parametro lb nos da una cota inferior a las variables.
    x = m.addVars(list(G.edges()), vtype='B', name = "x");
    
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
        for i,e in enumerate(G.edges()):
            s = s + c[i]*x[e[0],e[1]] 
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
    
    # Generamos las restricciones del problema entero
    for v in G:
        m.addConstr(suma(delta(G,[v])) == 1, name = "delta" + str(v));
    
    # Resolvemos el prolbema.
    m.optimize()
    
    # Guardamos el numero de iteraciones y de exploraciones de branch and cut
    itera = m.IterCount
    bac = m.NodeCount
    
    end = time.time()
    return(m, itera, bac, end-start)
