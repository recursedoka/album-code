(define-syntax fn (syntax-rules () ((fn args . body) (lambda args . body))))
(define-syntax defn (syntax-rules () ((defn name args . body) (define name (lambda args . body)))))
(define-syntax def (syntax-rules () ((def name . stuff) (define name . stuff))))

(def (++ n) (+ 1 n))
(def (-- n) (- n 1))

(defn bind (fn . binds)
  (fn args (apply fn (append binds args))))

(defn find-first (string character index okay-checker)
  "Finds the first character in the string that passes `okay-checker`.
    `okay-checker` is a predicate that takes a string and an index in it."
  (def rec
    (fn (i)
        (if (equal? i #f)
            #f
            (if (okay-checker string i)
                i
                (rec (string-index string character i))))))
  (rec (string-index string character index)))

(defn distribute (li)
  (define le (length li))
  (defn rec (list1 list2 n)
    (if (>= (++ n) le)
        (list (reverse list1) (reverse list2))
        (rec
          (cons (list-ref li n) list1)
          (cons (list-ref li (++ n) list2))
          (+ n 2))))
  (rec '() '() 0))

(defn list-index (li item)
  (defn rec (i newli)
    (if (equal? (car newli) item)
        i
        (rec (++ i) (cdr newli))))
  (rec 0 li))

'(album/helpers bind find-first make-prefix-checker distribute list-index)
