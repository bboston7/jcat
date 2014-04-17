#!/usr/bin/racket
#lang racket/base

(require racket/string)

#|
Returns #t if str starts with token, else #f
|#
(define (string-starts-with? str token)
  (regexp-match? (regexp (string-append "^" (regexp-quote token))) str))

#|
Modifies the line as required for concatenating java files
|#
(define (process-line line)
  (string-replace line "public class" "class"))

#|
Cat a java file onto a list of lines and imports

Returns two values, the updated list of lines, and the updated list of imports
|#
(define (cat-file file lines imports)
  (define line (read-line file))
  (if (eof-object? line)
    (values lines imports)
    (let-values ([(lines imports) (cat-file file lines imports)])
      (if (or (string-starts-with? line "import")
              (string-starts-with? line "package"))
        (values lines (cons line imports))
        (values (cons (process-line line) lines) imports)))))

#|
Takes a list of lines and a list of imports and displays them to stdout
|#
(define (output-java lines imports)
  (for ([i imports]) (displayln i))
  (for ([i lines]) (displayln i)))

#|
Takes a list of java files and returns two values, a list of lines and a list
of imports
|#
(define (files->java-lists files)
  (define (fn files lines imports)
    (if (null? files)
      (values lines imports)
      (let-values ([(lines imports) (cat-file (car files) lines imports)])
        (fn (cdr files) lines imports))))
  (fn files null null))

#|
Ensure there is only one package declaration in the final output

FIXME: If the first file has a package line, those that do not are not caught
|#
(define (handle-package imports)
  (define package
    (cond [(null? imports) #f]
          [(string-starts-with? (car imports) "package") (car imports)]
          [else #f]))

  (define (fn lst acc)
    (cond [(null? lst) acc]
          [(not (string-starts-with? (car lst) "package"))
           (fn (cdr lst) (cons (car lst) acc))]
          [(equal? (car lst) package) (fn (cdr lst) acc)]
          [else (error 'handle-package
                       "found incompatable packages ~a and ~a"
                       (if package
                         (string-replace (substring package 8) ";" "")
                         "default")
                       (string-replace (substring (car lst) 8) ";" ""))]))

  (if package (cons package (fn (cdr imports) null)) (fn imports null)))


(module* main #f
  (define files
    (for/list ([i (current-command-line-arguments)]) (open-input-file i)))

  (define-values (lines imports) (files->java-lists files))

  (let ([imports (handle-package imports)])
    (output-java lines imports)))
