"Parses a syntax, by default s-expr, into lists and atoms."

(load "helpers.scm")
(load "readtable.scm")

;;;;;;;;;;;;;;;;;;;;

(def convert-prefixes
  (fn (prefixes string)
    "Converts prefixes to a form with the operator and what follows the prefix.

     The `prefixes` list contains any number of prefix characters, each followed
     by their corresponding operation. Each prefix-operation pair specifies a
     single conversion to take place. Each conversion looks for the prefix
     character, and takes whatever directly follows it, either a list or an
     atom, and converts it to a form with the first item being the operation and
     the second item being whatever followed the prefix character."
     (def a (distribute prefixes))
     (def chars (car a))
     (def ops (cadr a))
     (def le (length chars))
     (def handle-list
       (fn (str index)
         )) ; find the matching paren
     (def handle-atom
       (fn (str index)
         )) ; find the next space
     (def handle-prefix
       (fn (str index)
         (def pchar (string-ref str index))
         (if (equal? (member pchar chars) #f)
           (if (equal? (list-ref str index) #\()
             (handle-list str index)
             (handle-atom str index))
           (letrec ((opind (list-index chars pchar))
                    (op (list-ref ops opind))
                    (next (handle-prefix str (++ index))))
             (list (string-append "(" op " " (car next) ")") (cadr next))))))
     (def convert-prefix
       (fn (prefix-char str)
         (def precheck (make-prefix-checker #\\))
         (def rec
           (fn (str)
             (def i (find-first str prefix-char precheck))
             (if (equal? i #f)
               str
               (rec
                 (let ((res (handle-prefix str i)))
                 (substring-set! ; THIS IS WRONG
                   (string-copy str)
                   i (cadr res)
                   (car res)))))))
         (rec str)))
     (def rec
       (fn (i str)
         (if (< i le)
           (rec (++ i) (convert-prefix (list-ref chars i) str))
           str)))
     (rec 0 string)))

(def convert-string
  (fn (delimiter escape-character string)
    "Converts instances of strings with escapes into lists of characters.

     Strings are delimited with the specified delimiter, and have an escape
     character to allow inserting special characters, like carriage returns or
     the delimiter itself. The table demonstrates some conversions if the escape
     character is `~`.

     Escape  |Result
     --------|--------
     ~n      |\newline
     ~t      |\tab
     ~c      |\cr
     ~~      |\~"))

(def remove-comments
  (fn (comment-character string)
    "Removes anything from the comment character to the end of the line"))

(def parse-list
  (fn (reader)
    "Parses lists in s-expr syntax"))

(def parse-number
  (fn (reader)
    "Parses rational, complex, integer and real numbers"))

(def parse-character
  (fn (reader)
    "Parses characters prefixed with a backslash"))

(def parse-symbol
  (fn (reader)
    "Parses symbols"))

(def *readtable*
  (make-readtable
    (list (bind convert-string #\" #\~)
          (bind remove-comments #\;)
          (bind convert-prefixes '(#\' quote)))
    (list parse-list
          parse-number
          parse-character
          parse-symbol)))

(def parse
  (fn (string . li)
    "Parses a string using the rules in the given readtable."
    (def (p string readtable)
      )
    (if (> (length li) 0)
        (p string (car li))
        (p string *readtable*))))

'(parse parse-list parse-number parse-character parse-symbol make-readtable
  readtable? convert-string convert-prefix remove-comments)
