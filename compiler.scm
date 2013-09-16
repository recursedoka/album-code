"Parses a syntax, by default s-expr, into lists and atoms."

(load "helpers.scm")
(load "readtable.scm")

;;;;;;;;;;;;;;;;;;;;

(defn find-form-end (string index)
  (defn rec (nesting index)
    (if (> nesting 0)
        (let ((next (find-first string (char-set #\( #\)) index
                                (fn (string i)
                                    (not (equal? (string-ref string (-- i)) #\\))))))
          (if next
              (let ((paren (string-ref string next)))
                (if (equal? paren #\()
                    (rec (++ nesting) (++ next))
                    (rec (-- nesting) (++ next))))
              #f))
        index))
  (rec 1 (++ index)))

(defn find-atom-end (string index)
  (def i (find-first string #\space index
                     (fn (string i)
                         (not (equal? (string-ref string (-- i)) #\\)))))
  (if (not (equal? i #f))
      (++ i)
      #f))

(defn convert-prefixes (prefixes string)
  "Converts prefixes to a form with the operator and what follows the prefix.

  The `prefixes` list contains any number of prefix characters, each followed
  by their corresponding operation. Each prefix-operation pair specifies a
  single conversion to take place. Each conversion looks for the prefix
  character, and takes whatever directly follows it, either a list or an
  atom, and converts it to a form with the first item being the operation and
  the second item being whatever followed the prefix character."  
  (def a (distribute prefixes))
  (def chars (car a))
  (def pre-set (list->char-set chars))
  (def index (find-first string pre-set 0
                         (fn (string i)
                             (not (equal? (string-ref string (-- i)) #\\)))))
  (if index
      (let ((before (substring string 0 index)))
        (defn rec (i)
          (def next (string-ref i))
          (def op (list-ref (cdr a) (list-index chars (string-ref index))))
          (cond
            ((equal? next #\()
             (let ((end (find-form-end string index)))
               (cons (convert-prefixes prefixes (substring index end)) end)))
            ((char-set-contains? pre-set next)
             (let ((n (rec (++ i))))
               (cons (string-append "(" op " " (car n) ")") (cdr n))))
            (else
             (let (end (find-atom-end string index))
               (cons (substring index end) end)))))
        (def ret (rec index))
        (string-append before (car form) (convert-prefixes prefixes (substring (cdr form)))))
      string))

(defn find-string-delimiter (string delimiter index)
  (defn i (find-first string delimiter index
                      (fn (string i)
                          (not (equal? (string-ref string (-- i)) #\\)))))
  (if (not (equal? i #f))
      i
      #f))

; Returns a pair with the character and the index of the last character
(defn convert-escape (string index)
  )

(defn convert-characters (escape-character string)
  (defn rec (i)
    (def char (string-ref string char))   
    (if (equal? char escape-character)
        (let ((ret (convert-escape string i))
	      (string-append "#\\" (car ret " " (rec (+ i (cdr ret))))))
        (string-append "#\\" (string char) " " (rec (++ i)))))
  (string-append "'(" (rec 0) ")"))

(defn convert-string (delimiter escape-character string)
  "Converts instances of strings with escapes into lists of characters.

  Strings are delimited with the specified delimiter, and have an escape
  character to allow inserting special characters, like carriage returns or
  the delimiter itself. The table demonstrates some conversions if the escape
  character is `~`.

  Escape  |Result
  --------|--------
  ~newline|\\newline
  ~tab    |\\tab
  ~08     |\\08
  ~00F2   |\\00F2
  ~a      |\\a
  ~~      |\\~

  Note that a is neither a single digit unicode or a special character like
  a bell signal. Any escaped single character is treated as the character
  itself, thereby removing ambiguities."
  (def start (find-string-delimiter string delimiter 0))
  (def end (find-string-delimiter string delimiter start))
  (def string (convert-characters escape-character (substring (++ start) end)))
  (string-append (substring 0 start) string (substring (++ end))))

(defn find-comment-start (string comment-character index)
  (defn i (find-first string comment-character index
                      (fn (string i)
                          (not (equal? (string-ref string (-- i)) #\\)))))
  (if (not (equal? i #f))
      i
      #f))

(defn find-comment-end (string index)
  (defn i (string-index string #\newline index))
  (if (not (equal? i #f))
      (++ i)
      #f))

(defn remove-comments (comment-character string)
  "Removes anything from the comment character to the end of the line"
  (def start (find-comment-start string comment-character 0))
  (if (not (equal? #f start))
      (let ((end (or (find-comment-end string start) (length string))))
         (string-append (substring 0 start) (remove-comments (substring end)))
      string)))

(defn parse-list (reader)
  "Parses lists in s-expr syntax")

(defn parse-number (reader)
  "Parses rational, complex, integer and real numbers")

(defn parse-character (reader)
  "Parses characters prefixed with a backslash")

(defn parse-symbol (reader)
  "Parses symbols")

(def *readtable*
  (make-readtable
    (list (bind convert-string #\" #\\)
          (bind remove-comments #\;)
          (bind convert-prefixes '(#\' quote)))
    (list parse-list
          parse-number
          parse-character
          parse-symbol)))

(defn parse (string . li)
  "Parses a string using the rules in the given readtable."
  (defn convert (string converters)
    (convert (cdr converters) ((car converters) string)))
  (defn p (string parsers)
    )
  (if (and (> (length li) 0) (readtable? (car li)))
      (p (convert string (readtable-converters (car li))) (readtable-parsers (car li)))
      (p (convert string (readtable-converters *readtable*)) (readtable-parsers *readtable*))))

'(parse parse-list parse-number parse-character parse-symbol make-readtable
  readtable? convert-string convert-prefix remove-comments)
