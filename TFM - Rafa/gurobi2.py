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
from padbergmod import *
from funcionaux import *

# DATOS DE ENTRADA
#######################################################

# Establecemos una semilla predeterminada para asegurar la reproducibilidad
# de los resultados
#np.random.seed(1234)
#random.seed(1234)

# Construimos un grafo aleatorio
H = nx.gnp_random_graph(20,0.5)

# Mediante el siguiente comando podemos obtener la matriz de adyacencias
# del grafo anterior
A = nx.convert_matrix.to_numpy_matrix(H)

# Ademas, podemos tener una representacion grafica del mismo
nx.draw_circular(H)

# Numero de aristas y vértices
N = len(H)
E = len(H.edges())

# Construimos las funciones de coste. En este caso las generamos de forma
# aleatoria.
c1 = np.random.rand(E)
c2 = np.random.rand(E)


# OPTIMIZACION
########################################################
    
# Pasamos a definir la funcion optimiza, que resuelve el problema del
# emparejamiento parametro con parametros
# G: Un grafo.
# lamb: Valor de lambda.
# c1,c2: Funciones de coste

def optimiza(G, lamb = 0, c1 = c1, c2 = c2):
    
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
    x = m.addVars(list(G.edges()), lb = 0, vtype='C', name = "x");
    
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
    
    # Generamos el primer bloque de restricciones.
    for v in H:
        m.addConstr(suma(delta(G,[v])) == 1, name = "delta" + str(v));
    
    # Dejamos comentado los comandos que añaden el número exponencial de
    # restricciones del poliedro del matching. 
    
    # Paso 4: Resolvemos el prolbema.
    m.optimize()
    
    # Paso 5: Comprobamos si la solucion es entera y, en caso de no serlo,
    # utilizamos las distintas heuristicas y metodos para encontrar
    # planos de corte.
    
    # Obtenemos el vector de soluciones
    sols = [m.getVars()[i].X for i in range(E)]
    entero(sols)
    while (not (entero(sols))):     
        # Construimos un grafo auxiliar con capacidades sols.
        M = nx.Graph()
        for i, e in enumerate(G.edges()):
            M.add_edge(e[0],e[1], weight = sols[i])
        
        # Paso 7.1, 7.2: Vamos a programar las heuristica para detectar 
        # planos de corte.
        
        # Fijamos un parametro epsilon de tolerancia para la segunda heuristica.
        # Tal y como Groetschel y Holland en su trabajo, tomamos 0.3. 
        epsilon = 0.3 
        
        # Obtenemos las aristas con variables asociadas en
        aristasN = [e for e in M.edges() if totalcap(M,[e])>0]
        aristasE = [e for e in M.edges() if totalcap(M,[e])>epsilon]
            
        # Construimos nuevos grafos con estas aristas.
        GN = nx.Graph()
        GN.add_edges_from(aristasN)
        
        GE = nx.Graph()
        GE.add_edges_from(aristasE)
        
        # Buscamos las componentes conexas que tengan cardinalidad impar
        conN = [a for a in list(nx.connected_components(GN)) if len(a) %2 == 1]
        
        # Tenemos que tener en cuenta que los planos de corte que proporciona
        # la segunda heuristica pueden no ser utiles, ya que nuestra solucion
        # no tiene por que violarlos necesariamente y seria un esfuerzo 
        # innecesario volver a reoptimizar para nada, por lo que solo nos 
        # quedamos con dichas componentes. Definimos previamente una funcion
        # que nos haga dicha comprobacion
        def comprueba(nodos):
            E = GE.subgraph(nodos).edges()
            suma = totalcap(H,E)
            card = (len(nodos)-1)/2
            return(suma <= card)
        
        # Generamos las componentes adecuadas
        conE = [a for a in list(nx.connected_components(GE)) if len(a) %2 == 1 
                and comprueba(a)]
        
        # Si las lista con es no vacia, hemos encontrado planos de cortes, los
        # añadimos a nuestro modelo y volvemos a comprobar. 
        if (conN != []):
            for a in conN:
                S = list(GN.edges(a))
                card = len(a)-1
                m.addConstr(suma(S) <= card/2) 
            m.optimize()
            print("Hemos usado H1")
            
        elif (conE != []):
            for a in conE:
                S = list(GE.edges(a))
                card = len(a)-1
                m.addConstr(suma(S) <= card/2) 
            m.optimize()
            print("Hemos usado H2")
        
        # Paso 7.3: En el caso de que las heuristicas no hayan funcionado, 
        # construimos un plano de corte mediante el procedimiento de Padberg
        # y Rao modificado.
        else:        
            W,F = padraomod(nx.Graph(A),sols)
            m.addConstr(suma(W.difference(F))-suma(F) >= 1-len(F))
            m.optimize()
            print("Hemos usado Padberg-Rao")
        sols = [m.getVars()[i].X for i in range(E)]
    return(m)

modelo = optimiza(H,0)
sols = [1-modelo.getVars()[i].X for i in range(E)]
nx.draw_circular(H,edge_color = sols)