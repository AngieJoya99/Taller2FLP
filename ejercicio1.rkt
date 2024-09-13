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

#|-----------------------------------------------------------
Punto 2.1.1: Gramática en listas
Gramática:
<grafo-dirigido> ::= ('graph <vertice> <arista>)
<vertice> ::= ('vertices (<valor-de-scheme>)+)
<arista> ::= ('aristas (<valor-de-scheme> <valor-de-scheme>)*)

vertice: List -> vertice
usage: (vertice v) = Crea una lista con la representación de los vertices v:
la cabeza es la palabra 'vertices y la cola es la lista v

Casos de prueba:
(vertice '(a b c))
(vertice '(z x @ w))
(vertice '(1 6 a 8))
(vertice '(y))
|#

(define vertice
  (lambda (v)
    (list 'vertices v)
  )
)

#|arista: List -> arista
usage: (arista a) = Crea una lista con la representación de las aristas a:
la cabeza es la palabra 'aristas y la cola es la lista a

Casos de prueba:
(arista '((a b) (b c) (c a)))
(arista '((@ x) (x @) (c a) (w z) (w x) (@ z)))
(arista '((8 1) (a 6) (6 a) (8 6)))
(arista '())|#

(define arista
  (lambda (a)
    (cond
      [(null? a) (list 'aristas '())]
      [else (list 'aristas a)]
    )
  )
)

#|graph: vertice x arista -> List
usage: (graph v a) = Crea una lista que representa el grafo dirigido de vértices v y aristas a 

Casos de prueba:
(graph (vertice '(a b c)) (arista '((a b) (b c) (c a))))
(graph (vertice '(z x @ w)) (arista '((@ x) (x @) (c a) (w z) (w x) (@ z))))
(graph (vertice '(1 6 a 8)) (arista '((8 1) (a 6) (6 a) (8 6))))
(graph (vertice '(y)) (arista '()))|#

(define graph
  (lambda (v a)
    (list 'graph  v a)
  )
)

#|graph->vertices: grafo-dirigido -> vertice
usage: (graph->vertices g) = Crea una lista con lo la palabra 'vertice
y la lista de vértices del grafo g

Casos de prueba:
(graph->vertices (graph (vertice '(a b c)) (arista '((a b) (b c) (c a)))))
(graph->vertices (graph (vertice '(z x @ w)) (arista '((@ x) (x @) (c a) (w z) (w x) (@ z)))))
(graph->vertices (graph (vertice '(1 6 a 8)) (arista '((8 1) (a 6) (6 a) (8 6)))))
(graph->vertices (graph (vertice '(y)) (arista '())))|#

(define graph->vertices
  (lambda (g)
    (cadr g)
  )
)

#|graph->edges: grafo-dirigido -> arista
usage: (graph->edges g) = Crea una lista con la palabra 'arista
y la lista de aristas del grafo g

Casos de prueba:
(graph->edges (graph (vertice '(a b c)) (arista '((a b) (b c) (c a)))))
(graph->edges (graph (vertice '(z x @ w)) (arista '((@ x) (x @) (c a) (w z) (w x) (@ z)))))
(graph->edges (graph (vertice '(1 6 a 8)) (arista '((8 1) (a 6) (6 a) (8 6)))))
(graph->edges (graph (vertice '(y)) (arista '())))|#


(define graph->edges
  (lambda (g)
    (caddr g)
  )
)

#|vertices->nodelist: vertice -> List
usage: (vertices->nodelist g) = Crea una lista con los vértices del grafo g

Casos de prueba:
(vertices->nodelist (vertice '(a b c)))
(vertices->nodelist (vertice '(z x @ w)))
(vertices->nodelist (vertice '(1 6 a 8)))
(vertices->nodelist (vertice '(y)))|#

(define vertices->nodelist
  (lambda (v)
    (cadr v)
  )
)

#|edges->pairs: arista -> List
usage: (edges->pairs g) = Crea una lista con las aristas del grafo g

Casos de prueba:
(edges->pairs (arista '((a b) (b c) (c a))))
(edges->pairs (arista '((@ x) (x @) (c a) (w z) (w x) (@ z))))
(edges->pairs (arista '((8 1) (a 6) (6 a) (8 6))))
(edges->pairs (arista '()))|#

(define edges->pairs
  (lambda (a)
    (cond
      [(null? a) '()]
      [else (cadr a)]
    )
  )
)

#|--------------------------------------------------------
Punto 2.1.2: Gramática en datatypes

Gramática:
<grafo-dirigido> ::= ('graph <vertice> <arista>)
<vertice> ::= ('vertices (<valor-de-scheme>)+)
<arista> ::= ('aristas (<valor-de-scheme> <valor-de-scheme>)*)
|#

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
#|Punto 2.2.1 Parse|#

(define PARSEBNF
  (lambda (exp)
    (cond
      [(equal? (car exp) 'graph) (graph-exp (PARSEBNF (cadr exp))(PARSEBNF (caddr exp)))]
      [(equal? (car exp) 'vertices) (vertices-exp (cadr exp))]
      [(equal? (car exp) 'edges) (edges-exp (parse-edges (cadr exp)))]
    )
  )
)

(define parse-edges
  (lambda (exp)
    (cond
      [(null? exp) empty]
      [(pair? (car exp))
       (cons (edge-exp (caar exp)(cadr(car exp)))(parse-edges(cdr exp)))
      ]
    )
  )
)

#|--------------------------------------------------------
Punto 2.2.2 Unparse|#

(define UNPARSEBNF
  (lambda (exp)
    (cases graph-type exp
      (graph-exp (v e)
                 (letrec 
                    (
                     (ver (cases vertices-type v
                        (vertices-exp (ve) (list 'vertices ve))))
                     (edgess (unparse-edge e))
                    )
                   (list 'graph ver (list 'edges edgess))
                  )
      )
    )
  )
)

(define unparse-edges
  (lambda (exp)
    (cases edges exp
      (edge-exp (left-edge right-edge) (list left-edge right-edge))
    )
  )
)

(define unparse-edge
  (lambda (exp)
    (cases edges-type exp
      (edges-exp (ed) (if (null? ed)
                          empty
                          (cons (unparse-edges(car ed))(unparse-edge (edges-exp(cdr ed))))
                      )
      )
    )
  )
)

#|--------------------------------------------------------
Punto 2.3.1 Add-edge
Gramática:
<grafo-dirigido> ::= ('graph <vertice> <arista>)
<vertice> ::= ('vertices (<valor-de-scheme>)+)
<arista> ::= ('aristas (<valor-de-scheme> <valor-de-scheme>)*)

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
Punto 2.3.2 Vecinos-salientes|#

(define vecinos-salientes
  (lambda (g ar)
    (letrec
          (
            (unparse (UNPARSEBNF g))
            (aristas (cadr(graph->edges unparse)))
            (aux
              (lambda (L1 AR)
                (cond
                  [(null? L1) empty]
                  [(pair? (car L1))
                          (if (eqv? (caar L1) AR)
                              (cons (cadr(car L1))(aux (cdr L1) AR))
                              (aux (cdr L1) AR))
                  ]
                  [else empty]
                )
              )
            )
          )
          (aux aristas ar)
    )
  )
)

#|--------------------------------------------------------
Punto 2.3.3 Vecinos-entrantes|#
 
(define vecinos-entrantes
  (lambda (g ar)
    (letrec
          (
            (unparse (UNPARSEBNF g))
            (aristas (cadr(graph->edges unparse)))
            (aux
              (lambda (L1 AR)
                (cond
                  [(null? L1) empty]
                  [(pair? (car L1))
                          (if (eqv? (cadr(car L1)) AR)
                              (cons (caar L1)(aux (cdr L1) AR))
                              (aux (cdr L1) AR))
                  ]
                  [else empty]
                 )
               )
             )
          )
          (aux aristas ar)
    )
  )
)