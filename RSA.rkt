;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname RSA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/match)

;generating primes
(define (makelst i n)
  (cond
    [(= i n) empty]
    [else (cons i (makelst (+ i 1) n))]))

(define (listprimes n)
  (editlist (makelst 2 n) n))

(define (editlist lst n)
  (cond
    [(> (* (first lst) (first lst)) n) lst]
    [else (cons (first lst) (editlist (removelist (first lst) (rest lst)) n))]))
  
(define (removelist i lst)
  (cond
    [(empty? lst) empty]
    [(= (remainder (first lst) i) 0) (removelist i (rest lst)) ]
    [else (cons (first lst) (removelist i (rest lst)))]))

;generating unique primes p and q
(define (unique new n)
  (cond
    [(equal? new n) (unique (list-ref plist (random (length plist))) n)]
    [else new]))
(define plist (listprimes 100000))
(define p (list-ref plist (random (length plist))))
(define q (unique (list-ref plist (random (length plist))) p))

;more variables
(define n (* p q))
(define thetan (*(- p 1)(- q 1)))
(define (gcd1 thetan e)
  (cond
    [(= 1 (gcd thetan e)) e]
    [(> thetan 4294967087) (gcd1 thetan (random 4294967087))] ;because random can only hold up to that, and thetan can be larger
    [else (gcd1 thetan (random 4294967087))]))
(define E 
  (cond
    [(> thetan 4294967087) (gcd1 thetan (random 4294967087))] ;because random can only hold up to that, and thetan can be larger
    [else (gcd1 thetan (random 4294967087))]))

;eea
(define (eea lst)
  (match lst
    [(list (list x y r q) (list X Y R Q)) ;two rows previous
     (cond
       [(= 0 R) (list x y r q)] ;the answer row
       [else 
        (let* ([quot (quotient r R)] ;new q
               [newx (- x (* quot X))] 
               [newy (- y (* quot Y))]
               [newr (- r (* quot R))])
          (eea (list (list X Y R Q)(list newx newy newr quot))))])]))

;mod
(define (mod k m)
  (cond
    [(> k m) (- k (*(quotient k m) m))]
    [(< k 0) (- k (*(sub1 (quotient k m)) m))]
    [else k]))

;d is found with ed = 1 (mod thetan)      aka solve     e(d) + thetan(k) = 1   using EEA       then take d mod thetan
(define d (mod 
           (first (eea (list (list 1 0 E 0) (list 0 1 thetan 0)))) 
           thetan))

(define (modexp b e m)
  (cond 
    [(= e 1) (mod b m)]
    [(odd? e) (mod (* (modexp (mod (sqr b) m) (quotient e 2) m) b) m)]
    [(even? e) (modexp (mod (sqr b) m) (quotient e 2) m)]))

(define (encrypt M e n)
  (modexp M e n))

(define (decrypt C d n)
  (modexp C d n))

;tools to make a string into something encryptable****************************
(define (log10 n) (/ (log n) (log 10)))
(define (maxsize n)  (inexact->exact (floor (/ (ceiling (log10 n)) 3)))) ;max num of letters we can chunk

;string -> listof string
(define (str->strlst str size)
  (cond
    [(<= (string-length str) size) (cons str empty)]
    [else (cons (substring str 0 size) (str->strlst (substring str size) size))]))


(define (str->int str n)
  (cond
    [(= (string-length str) 1) (* n (char->integer (string-ref str 0)))]
    [else (+ (* n (char->integer (string-ref str 0))) (str->int (substring str 1) (* n 1000)))])) 


(define (int->charlst i)
  (cond
    [(< i 1000) (cons (integer->char i) empty)]
    [else 
     (let ([n (floor (/ i 1000))])
       (cons (integer->char (- i (* n 1000))) (int->charlst n)))]))                
(define (int->str int) (list->string (int->charlst int)))


(define (str->ilst str size)
  (foldr
   (lambda (fst rec)
     (cons (str->int fst 1) rec))
   empty
   (str->strlst str size)))

(define (ilst->str lst)
  (foldr
   (lambda (fst rec)
     (string-append (int->str fst) rec))
   ""
   lst))

;actual encryption time***********************************************
(define (encryptstr str e n)
  (foldr
   (lambda (fst rec)
     (cons (encrypt fst e n) rec))
   empty
   (str->ilst str (maxsize n))))

(define (decryptlst lst d n)
  (foldr
   (lambda (fst rec)
     (string-append (int->str (decrypt fst d n)) rec))
   ""
   lst))
  
                        