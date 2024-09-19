#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

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
#|Punto 2.2.1 Parse

usage: Dada una lista con la representacion concreta de un grafo dirigido, construye el arbol de 
sintaxis abstracta basado en datatypes. 

Gramática parámetro:
<graph>       ::= ('graph <vertices> <edges>)
<vertices>    ::= ('vertices (<symbol>)+)
<edges>       ::= ('edges <edge>)
<edge>        ::= (<symbol> <symbol>)
              ::= (<symbol> <symbol>)<edge>

Gramática respuesta:
<graph-type>   ::= ('graph <vertices-type> <edges-type>)
<vertice-type> ::= ('vertices (<symbol>)+)
<edges-type>   ::= ('edges-exp <edges>)
<edges>        ::= ('edge_exp <symbol> <symbol>)
               ::= ('edge_exp <symbol><symbol>)<edge-exp>

Casos de prueba:
(PARSEBNF '(graph (vertices (a b c d)) (edges ((a b) (c d) (c b) (a c)))))
(PARSEBNF '(graph (vertices (x y z w)) (edges ((x y) (y z) (z w) (w x)))))
(PARSEBNF '(graph (vertices (p q r s)) (edges ((p q) (q r) (r s) (s p) (p r)))))
|#

(define PARSEBNF
  (lambda (exp)
    (cond
      [(equal? (car exp) 'graph) (graph-exp (PARSEBNF (cadr exp))(PARSEBNF (caddr exp)))]
      [(equal? (car exp) 'vertices) (vertices-exp (cadr exp))]
      [(equal? (car exp) 'edges) (edges-exp (parse-edges (cadr exp)))]
    )
  )
)


#|
  Usage: Procesa una lista de aristas y las convierte en una estructura de datos que sigue el formato edge-exp

  Casos de prueba:
  (parse-edges '((a b) (c d) (c b) (a c)))
  (parse-edges '((x y) (y z) (z w) (w x)))
  (parse-edges '((p q) (q r) (r s) (s p) (p r)))
|#
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
Punto 2.2.2 Unparse

usage: Dado un arbol de sintaxis abstracta de un grafo dirigido, 
entrega la representacion concreta basada en listas.

Gramática parámetro:
<graph-type>   ::= ('graph <vertices-type> <edges-type>)
<vertice-type> ::= ('vertices (<symbol>)+)
<edges-type>   ::= ('edges-exp <edges>)
<edges>        ::= ('edge_exp <symbol> <symbol>)
               ::= ('edge_exp <symbol><symbol>)<edge-exp>

Gramática respuesta:
<graph>       ::= ('graph <vertices> <edges>)
<vertices>    ::= ('vertices (<symbol>)+)
<edges>       ::= ('edges <edge>)
<edge>        ::= (<symbol> <symbol>)
              ::= (<symbol> <symbol>)<edge>

Casos de prueba:
(UNPARSEBNF (graph-exp
    (vertices-exp (list 'a 'b 'c 'd))
    (edges-exp(list
      (edge-exp 'a 'b) 
      (edge-exp 'c 'd)
      (edge-exp 'c 'b)
      (edge-exp 'a 'c)
    ))
  )
)

(UNPARSEBNF (graph-exp
    (vertices-exp (list 'x 'y 'z 'w))
    (edges-exp (list
      (edge-exp 'x 'y)
      (edge-exp 'y 'z)
      (edge-exp 'z 'w)
      (edge-exp 'w 'x)
    ))
  )
)

(UNPARSEBNF (graph-exp
    (vertices-exp (list 'p 'q 'r 's))
    (edges-exp (list
      (edge-exp 'p 'q)
      (edge-exp 'q 'r)
      (edge-exp 'r 's)
      (edge-exp 's 'p)
      (edge-exp 'p 'r)
    ))
  )
)

|#

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

#|
  Usage: Toma una arista de tipo edge-exp y la convierte nuevamente en una lista de dos elementos, que 
  representa los dos vértices de la arista.

  Casos de prueba:
  (unparse-edges (edge-exp 'a 'b))
  (unparse-edges (edge-exp 'x 'y))
  (unparse-edges (edge-exp 'p 'q))
|#
(define unparse-edges
  (lambda (exp)
    (cases edges exp
      (edge-exp (left-edge right-edge) (list left-edge right-edge))
    )
  )
)

#|
  Usage: Toma una lista de aristas (edges-exp) y la convierte en una lista de listas, donde cada lista 
  interna representa una arista como un par de vértices.

  Casos de prueba:
  (unparse-edge (edges-exp(list (edge-exp 'a 'b)(edge-exp 'c 'd)(edge-exp 'c 'b)(edge-exp 'a 'c))))
  (unparse-edge (edges-exp (list (edge-exp 'x 'y)(edge-exp 'y 'z)(edge-exp 'z 'w)(edge-exp 'w 'x))))
  (unparse-edge (edges-exp (list (edge-exp 'p 'q)(edge-exp 'q 'r)(edge-exp 'r 's)(edge-exp 's 'p)(edge-exp 'p 'r))))
|#
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
