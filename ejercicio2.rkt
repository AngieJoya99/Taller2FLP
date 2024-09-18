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
