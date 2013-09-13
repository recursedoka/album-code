(define make-readtable
  (fn (converters parsers)
    (list 'readtable converters parsers)))

(define readtable?
  (fn (readtable)
    (equal? (car readtable) 'readtable)))

(define readtable-converters
  (fn (readtable)
    (and (readtable? readtable) (cadr readtable))))

(define readtable-parsers
  (fn (readtable)
    (and (readtable? readtable) (caddr readtable))))
