;; encrypt
(define (encrypt msg public-key)
  (let ((integer (message->integer msg)))
    (modulo (expt integer (public-exponent public-key))
	    (modulos public-key))))
;; decrypt
(define (decrypt cypher private-key)
  (let ((msg (modulo (expt cypher (private-exponent private-key))
		     (modulos private-key))))
    (integer->message msg)))
;;; data representation of public and private key

;; constructors

;; make-public-key: number number -> pair
(define (make-public-key prime1 prime2)
  (let ((modulos (* prime1 prime2))
	(public-exponent (find-public-exponent prime1 prime2)))
    (cons modulos public-exponent)))

;; make-private-key: number number -> pair
(define (make-private-key prime1 prime2)
  (let ((modulos (* prime1 prime2))
	(private-key (find-private-exponent prime1 prime2)))
    (cons modulos private-key)))

;; selectors

;; modulos: pair -> number
(define (modulos key)
  (car key))
;; public-exponent: pair -> number
(define (public-exponent public-key)
  (cdr public-key))
;; private-key: pair -> number
(define (private-exponent private-key)
  (cdr private-key))
;;----

;;; helpers

;; find-public-exponent: number number -> number
;; given two primes this computes e, the public exponent
;; given: (find-public-exponent 2 7)
;; expect: 5
(define (find-public-exponent prime1 prime2)
  (define (totient p q) (* (- p 1) (- q 1)))
  (define (iter p1 p2 counter)
    (let ((phi (totient p1 p2)))
      (if (and (< 1 counter)
	       (< counter phi)
	       (= (gcd counter phi) 1))
	  counter
	  (iter p1 p2 (+ counter 1)))))
  (iter prime1 prime2 1))

;; find-private-exponent: number number -> number
;; computes d, the private exponent given two primes.
(define (find-private-exponent prime1 prime2)
  (define (totient p q) (* (- p 1) (- q 1)))
  (define (iter p1 p2 counter)
    (let ((public-exponent (find-public-exponent prime1 prime2)))
      (let ((phi (totient prime1 prime2)))
	(if (= (modulo (* counter public-exponent) phi) 1)
	    counter
	    (iter p1 p2 (+ counter 1))))))
  (iter prime1 prime2 1))

;; message->integer: string -> number
(define (message->integer msg)
  (let ((lst1 (string->ascii msg)))
    (let ((cypher (join-numbers lst1 1000)))
      cypher)))

;; integer->message
(define (integer->message int)
  (list->string (lstascii->lstchar (split-number int 1000))))

;; join-numbers: list number -> number
(define (join-numbers digits radix)
  (reduce 
   + 0
   (map 
    (lambda (place digit)
      (* digit (expt radix place)))
    (iota (length digits)) digits)))

;; split-number: number -> list
(define (split-number n radix)
  (if (< n 1) '()
      (cons (remainder n radix)
	    (split-number (quotient n radix) radix))))

;; string->listof-char: string -> list
(define (string->ascii str)
  (let ((lst (string->list str)))
    (if (eq? (string-length str) 0)
	'()
	(cons (char->ascii (car lst))
	      (string->ascii (string-tail str 1))))))

;; anscii-list->char-list: list -> list
(define (lstascii->lstchar n)
  (cond ((null? n) '())
	(else
	  (cons (ascii->char (car n))
		(lstascii->lstchar (cdr n))))))
;; example
(define key1 (make-public-key 41 43))
(define key2 (make-private-key 41 43))

(encrypt "h" key1)
;;value: 48

(decrypt (encrypt "h" key1) key2)
;;value: "h"
