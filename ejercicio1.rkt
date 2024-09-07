#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|
-----------------------------------------------------------
Punto 2.1.1: Gramática en listas
Gramática:
<grafo-dirigido> ::= () | ('graph <vertice> <arista>)
<vertice> ::= () | ('vertices <valor-de-scheme>+)
<arista> ::= () | ('aristas (<valor-de-scheme> <valor-de-scheme>)+)

vertice: List -> List
usage: (vertice v) = Crea una lista con la representación de los vertices v:
la cabeza es la palabra 'vertices y la cola es la lista v

Casos de prueba
(vertice '(a b c))
(vertice '(z x @ w))
(vertice '(1 6 a 8))
(vertice '())
|#

(define vertice
  (lambda (v)
    (cond
      [(null? v) '()]
      [else (list 'vertices v)]
    )
  )
)

#|arista List -> List
usage: (arista a) = Crea una lista con la representación de las aristas a:
la cabeza es la palabra 'aristas y la cola es la lista a

Casos de prueba
(arista '((a b) (b c) (c a)))
(arista '((@ x) (x @) (c a) (w z) (w x) (@ z)))
(arista '((8 1) (a 6) (6 a) (8 6)))
(arista '())|#

(define arista
  (lambda (a)
    (cond
      [(null? a) '()]
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
(graph (vertice '()) (arista '()))|#

(define graph
  (lambda (v a)
    (cond
      [(and (null? v) (null? a)) '()]
      [else (list 'graph  v a)]
    )
  )
)

#|Punto 2.1.2: Gramática en datatypes|#

#|Punto 2.2.1 Parse|#

(define PARSEBNF
  (lambda (g)
    ('falta)
  )

)

#|Punto 2.2.2 Unparse|#

#|Punto 2.3.1 Add-edge|#

(define add-edge
  (lambda (g a)
    ('falta)
  )
)

#|Punto 2.3.2 Vecinos-salientes|#

#|Punto 2.3.3 Vecinos-entrantes|#
 