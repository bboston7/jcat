#!/usr/bin/racket
#lang racket/base

(require racket/list
         racket/port
         racket/string)

#|
Returns #t if str starts with token, else #f
|#
(define (string-starts-with? str token)
  (regexp-match? (regexp (string-append "^" (regexp-quote token))) str))

#|
Modifies the line as required for concatenating java files
|#
(define (process-line line static)
  (if static
    (string-replace line "class " "static class ")
    (string-replace line "public class" "class")))

#|
Cat a java file onto a list of lines and imports

Returns two values, the updated list of lines, and the updated list of imports
|#
(define (cat-file file lines imports static)
  (define line (cond [(null? file) null]
                     [(list? file) (begin0 (car file) (set! file (cdr file)))]
                     [else (read-line file)]))
  (if (or (null? line) (eof-object? line))
    (values lines imports)
    (let-values ([(lines imports) (cat-file file lines imports static)])
      (if (or (string-starts-with? line "import")
              (string-starts-with? line "package"))
        (values lines (cons line imports))
        (values
          (cons (if (eq? static 'no-process) line (process-line line static))
                lines)
          imports)))))

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
(define (files->java-lists files static)
  (define (fn files lines imports)
    (if (null? files)
      (values lines imports)
      (let-values ([(lines imports) (cat-file (car files) lines imports static)])
        (fn (cdr files) lines imports))))
  (if static
    (let*-values ([(top bottom)
                   (splitf-at-right
                     (port->lines (car files))
                     (lambda (x) (not (string-starts-with? x "public class"))))]
                  [(bot-lines bot-imports) (cat-file bottom null null 'no-process)]
                  [(mid-lines mid-imports) (fn (cdr files) bot-lines bot-imports)])
      (cat-file top mid-lines mid-imports 'no-process))
    (fn files null null)))

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
  (require racket/cmdline)

  (define static-inner (make-parameter #f))
  (define filenames
    (command-line
      #:program "jcat"
      #:once-each
      [("-s" "--static")
       "Cat classes to be static member classes of the first class"
       (static-inner #t)]
      #:args filenames
      filenames))

  (define files
    (for/list ([i filenames]) (open-input-file i)))

  (define-values (lines imports) (files->java-lists files (static-inner)))

  (let ([imports (handle-package imports)])
    (output-java lines imports)))
