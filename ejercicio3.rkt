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

#|-----------------------------------------------------|#
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

(define Vecinos-salientes
  (lambda (exp ar)
    (cases graph-type exp
      (graph-exp (v e) (aux-vs-edges e ar))
    )
  )
)

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
Punto 2.3.3 Vecinos-entrantes|#
 
(define Vecinos-entrantes
  (lambda (exp ar)
    (cases graph-type exp
      (graph-exp (v e) (aux-ve-edges e ar))
    )
  )
)

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