#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|Punto 2.1.1: Gramática en listas
grafo-dirigido: List x List -> List
usage: (grafo-dirigido v a) = Crea una lista que representa el grafo dirigido
de vértices v y aristas a 

Gramática:
<grafo-dirigido> ::= '() | '(graph (vertices <vertice>) (aristas <arista>))
<vertice> ::= '() | '(<valor-de-scheme> <vertice>)
<arista> ::= '() | '((<valor-de-scheme> <valor-de-scheme>) <arista>)

Casos de prueba:
(grafo-dirigido '(a b c) '((a b) (b c) (c a)))
(grafo-dirigido '(z x @ w) '((@ x) (x @) (c a) (w z) (w x) (@ z)))
(grafo-dirigido '(1 6 3 8) '((8 1) (3 6) (6 3) (8 3)))
(grafo-dirigido '() '())
|#

(define grafo-dirigido
  (lambda (v a)
    (cond
      [(and (null? v) (null? a)) '()]
      [else (list 'graph (list 'vertices v) (list 'aristas a))]
    )
  )
)

#|Punto 2.1.2: Gramática en datatypes|#

#|Punto 2.2.1 Parse|#

#|Punto 2.2.2 Unparse|#

#|Punto 2.3.1 Add-edge|#

#|Punto 2.3.2 Vecinos-salientes|#

#|Punto 2.3.3 Vecinos-entrantes|#
 