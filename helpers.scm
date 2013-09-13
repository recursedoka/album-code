(define-syntax fn (syntax-rules () ((fn args . body) (lambda args . body))))
(define-syntax def (syntax-rules () ((def name . stuff) (lambda name . stuff))))

(def (++ n) (+ 1 n))
(def (-- n) (- n 1))

(def bind
  (fn (fn . binds)
    (fn args (apply fn (list-append binds args)))))

(def find-first
  (fn (string character okay-checker)
    "Finds the first character in the string that passes `okay-checker`.
     `okay-checker` is a predicate that takes a string and an index in it."
    (def rec
      (fn (i)
        (if (equal? i #f)
          #f
          (if (okay-checker string i)
            i
            (rec (string-index string character i))))))
    (rec (string-index string character 0))))

(def make-prefix-checker
  (fn (prefix-char)
    "Returns a function that checks if `-- i` in `string` equals `prefix-char`."
    (fn (string i)
      (def prefix (string-ref string (-- i)))
      (not (equal? prefix prefix-char)))))

(def distribute
  (fn (li)
    (define le (length li))
    (define rec
      (fn (list1 list2 n)
        (if (>= (++ n) le)
          (list (reverse list1) (reverse list2))
          (rec
            (cons (list-ref li n) list1)
            (cons (list-ref li (++ n) list2))
            (+ n 2)))))
    (rec '() '() 0)))

(def list-index
  (fn (li item)
    (def rec
      (fn (i newli)
        (if (equal? (car newli) item)
          i
          (rec (++ i) (cdr newli)))))
    (rec 0 li)))

'(bind find-first make-prefix-checker distribute list-index)
