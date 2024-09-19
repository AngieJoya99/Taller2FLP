#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|Función Auxiliar
juntarListas: List x List -> List
usage: (juntarListas L1 L2) = Lista resultante de juntar los elementos de L1 con los elementos de L2

Gramática: <lista> := () | (<valor-de-scheme> <lista>)
|#

(define juntarListas
  (lambda (L1 L2)
    (cond
      [(null? L1) L2]
      [else (cons (car L1) (juntarListas (cdr L1) L2))]
    )
  )
)

(define vertice
  (lambda (v)
    (list 'vertices v)
  )
)

(define arista
  (lambda (a)
    (cond
      [(null? a) (list 'aristas '())]
      [else (list 'aristas a)]
    )
  )
)

(define graph
  (lambda (v a)
    (list 'graph  v a)
  )
)

(define graph->vertices
  (lambda (g)
    (cadr g)
  )
)

(define graph->edges
  (lambda (g)
    (caddr g)
  )
)


(define vertices->nodelist
  (lambda (v)
    (cadr v)
  )
)

(define edges->pairs
  (lambda (a)
    (cond
      [(null? a) '()]
      [else (cadr a)]
    )
  )
)

(define-datatype graph-type graph-type?
  (graph-exp (v vertices-type?)(e edges-type?)) 
)

(define-datatype vertices-type vertices-type?
  (vertices-exp (ve(list-of symbol?)))
)

(define-datatype edges-type edges-type?
  (edges-exp (ed(list-of edges?)))
)

(define-datatype edges edges?
  (edge-exp (left-edge symbol?) (right-edge symbol?))
)


#|--------------------------------------------------------
Punto 2.3.1 Add-edge
Gramática:
<grafo-dirigido> ::= ('graph <vertice> <arista>)
<vertice> ::= ('vertices (<symbol>)+)
<arista> ::= ('aristas (<symbol> <symbol>)*)

add-edge: grafo-dirigido x List -> grafo-dirigido
usage: (add-edge g a) = Agrega la arista a al grafo dirigido g
si esta no se encuentra ya dentro del grafo

Casos de prueba:
(add-edge (graph (vertice '(a b c)) (arista '((a b) (b c) (c a)))) '(a b))
(add-edge (graph (vertice '(a b c)) (arista '((a b) (b c) (c a)))) '(a c))
(add-edge (graph (vertice '(a b c)) (arista '((a b) (b c) (c a)))) '(b a))
(add-edge (graph (vertice '(a b c)) (arista '((a b) (b c) (c a)))) '())
|#

(define add-edge
  (lambda (g a)
    (letrec
      (
        [aristas (edges->pairs (graph->edges g))]
        #|arista-existe: grafo-dirigido-> Boolean
        usage: (arista-existe g) = retorna #t si el a es una arista de g y #f si no|#
        [arista-existe
          (lambda (g)
            (cond
              [(null? g) #f]
              [(equal? (car g) a) #t]
              [else (arista-existe (cdr g))]
            )                
          )
        ]
      )
      (cond
        [(null? a) g]
        [(equal? #t (arista-existe aristas)) g]
        [else (graph (graph->vertices g) (arista(juntarListas aristas (list a)))) ]
      )
    )
  )
)


#|--------------------------------------------------------
Punto 2.3.2 Vecinos-salientes

Usage: Recibe un grafo dirigido (basada en datatypes) y un vertice o nodo ar y da como
respuesta una lista de nodos que se pueden alcanzar directamente desde el nodo dado

Gramática:
<graph-type>   ::= ('graph <vertices-type> <edges-type>)
<vertice-type> ::= ('vertices (<symbol>)+)
<edges-type>   ::= ('edges-exp <edges>)
<edges>        ::= ('edge_exp <symbol> <symbol>)
               ::= ('edge_exp <symbol><symbol>)<edge-exp>

Casos de prueba:
(Vecinos-salientes (graph-exp
    (vertices-exp (list 'a 'b 'c 'd))
    (edges-exp(list
      (edge-exp 'a 'b) 
      (edge-exp 'c 'd)
      (edge-exp 'c 'b)
      (edge-exp 'a 'c)
    ))
  ) 'b
)

(Vecinos-salientes (graph-exp
    (vertices-exp (list 'x 'y 'z 'w))
    (edges-exp (list
      (edge-exp 'x 'y)
      (edge-exp 'y 'z)
      (edge-exp 'z 'w)
      (edge-exp 'y 'x)
    ))
  ) 'y
)

(Vecinos-salientes (graph-exp
    (vertices-exp (list 'p 'q 'r 's))
    (edges-exp (list
      (edge-exp 'p 'q)
      (edge-exp 'q 'r)
      (edge-exp 'p 's)
      (edge-exp 's 'p)
      (edge-exp 'p 'r)
    ))
  ) 'p
)
|#

(define Vecinos-salientes
  (lambda (exp ar)
    (cases graph-type exp
      (graph-exp (v e) (aux-vs-edges e ar))
    )
  )
)

#|
  Usage: procesa una lista de aristas (edges-exp) y devuelve una lista de vértices que están 
  conectados al vértice especificado (ar).

  Casos de prueba:
  (aux-vs-edges (edges-exp(list (edge-exp 'a 'b)(edge-exp 'c 'd)(edge-exp 'c 'b)(edge-exp 'a 'c))) 'b)
  (aux-vs-edges (edges-exp (list (edge-exp 'x 'y)(edge-exp 'y 'z)(edge-exp 'z 'w)(edge-exp 'y 'x))) 'y)
  (aux-vs-edges (edges-exp (list (edge-exp 'p 'q)(edge-exp 'q 'r)(edge-exp 'p 's)(edge-exp 's 'p)(edge-exp 'p 'r))) 'p)
|#
(define aux-vs-edges
  (lambda (exp ar)
    (cases edges-type exp
      (edges-exp (ed) (if (null? ed)
                          empty
                          (append (aux-vs-edge(car ed) ar)(aux-vs-edges (edges-exp(cdr ed)) ar))
                      )
      )
    )
  )
)

#|
  Usage: Procesa una arista individual (edge-exp) y verifica si el vértice de origen (left-edge) es 
  igual al vértice especificado (ar). Si es así, devuelve el vértice de destino (right-edge) como una 
  lista, de lo contrario devuelve empty.

  Casos de prueba:
  (aux-vs-edge (edge-exp 'a 'b) 'a)
  (aux-vs-edge (edge-exp 'x 'y) 'y)
|#

(define aux-vs-edge
  (lambda (exp ar)
    (cases edges exp
      (edge-exp (left-edge right-edge)
                (if (equal? left-edge ar)
                    (list right-edge)
                    empty
                )
      )
    )
  )
)

#|--------------------------------------------------------
Punto 2.3.3 Vecinos-entrantes

Usage: Recibe un grafo dirigido (basada en datatypes) y un vertice o nodo ar y da como
respuesta una lista de nodos desde los cuales se puede llegar al nodo dado.

Gramática:
<graph-type>   ::= ('graph <vertices-type> <edges-type>)
<vertice-type> ::= ('vertices (<symbol>)+)
<edges-type>   ::= ('edges-exp <edges>)
<edges>        ::= ('edge_exp <symbol> <symbol>)
               ::= ('edge_exp <symbol><symbol>)<edge-exp>

Casos de prueba:
(Vecinos-entrantes (graph-exp
    (vertices-exp (list 'a 'b 'c 'd))
    (edges-exp(list
      (edge-exp 'a 'b) 
      (edge-exp 'c 'd)
      (edge-exp 'c 'b)
      (edge-exp 'a 'c)
    ))
  ) 'a
)

(Vecinos-entrantes (graph-exp
    (vertices-exp (list 'x 'y 'z 'w))
    (edges-exp (list
      (edge-exp 'x 'y)
      (edge-exp 'y 'z)
      (edge-exp 'z 'x)
      (edge-exp 'w 'x)
    ))
  ) 'x
)

(Vecinos-entrantes (graph-exp
    (vertices-exp (list 'p 'q 'r 's))
    (edges-exp (list
      (edge-exp 'p 'q)
      (edge-exp 'q 'r)
      (edge-exp 'r 's)
      (edge-exp 's 'r)
      (edge-exp 'p 'r)
    ))
  ) 'r
)
|#
 
(define Vecinos-entrantes
  (lambda (exp ar)
    (cases graph-type exp
      (graph-exp (v e) (aux-ve-edges e ar))
    )
  )
)

#|
  Usage: Recorre una lista de aristas (edges-exp) y devuelve una lista de los vértices 
  que tienen una conexión hacia el vértice ar. Es decir, busca todas las aristas donde ar es el 
  vértice de destino y devuelve los vértices de origen de dichas aristas.

  Casos de prueba:
  (aux-ve-edges (edges-exp(list (edge-exp 'a 'b)(edge-exp 'c 'd)(edge-exp 'c 'b)(edge-exp 'a 'c))) 'a)
  (aux-ve-edges (edges-exp (list (edge-exp 'x 'y)(edge-exp 'y 'z)(edge-exp 'z 'x)(edge-exp 'y 'x))) 'x)
  (aux-ve-edges (edges-exp (list (edge-exp 'p 'q)(edge-exp 'q 'r)(edge-exp 'r 's)(edge-exp 's 'r)(edge-exp 'p 'r))) 'r)
|#
(define aux-ve-edges
  (lambda (exp ar)
    (cases edges-type exp
      (edges-exp (ed) (if (null? ed)
                          empty
                          (append (aux-ve-edge(car ed) ar)(aux-ve-edges (edges-exp(cdr ed)) ar))
                      )
      )
    )
  )
)

#|
  Usage: Analiza una arista individual (edge-exp) y verifica si el vértice de destino (right-edge) es 
  igual al vértice especificado (ar). Si lo es, devuelve el vértice de origen (left-edge) como una 
  lista, de lo contrario devuelve empty.

  Casos de prueba:
  (aux-ve-edge (edge-exp 'a 'b) 'b)
  (aux-ve-edge (edge-exp 'x 'y) 'x)
|#
(define aux-ve-edge
  (lambda (exp ar)
    (cases edges exp
      (edge-exp (left-edge right-edge)
                (if (equal? right-edge ar)
                    (list left-edge)
                    empty
                )
      )
    )
  )
)