
#lang slideshow

(provide (all-defined-out))
#(require "foo.rkt")
(define s "hello")
(define x 3) ; val x=3
(define cube1
  (lambda (x)
    (* x x x)))
(define (cube2 x)
  (* x x x))
(define (pow1 x y)
  (if (= y 0)
      1
      (* x(pow1 x(- y 1)))))
 
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))
(define three-to-the (pow2 3))


; sum all the numbers in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

;阶乘
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
;
(define (sum1 xs)
	(if (null? xs)
		0
		(if (number? (car xs))
			(+ (car xs) (sum1 (cdr xs)))
			(+ (sum1 (car xs)) (sum1 (cdr xs))))))


;#lang slideshow
(circle 10)
(rectangle 10 20)
(hc-append (circle 10) (rectangle 10 20))
(define c( circle 10))
(define r( rectangle 10 20))
(hc-append 20 c r c)
(define (square n)
; A semi-colon starts a line comment.
; The expression below is the function body.
(filled-rectangle n n))
(square 10)
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four (circle 10))
(define (checker p1 p2)
  ( let ([p12(hc-append p1 p2)]
         [p21(hc-append p2 p1)])
     (vc-append p12 p21)))
(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))
circle
(define (series mk)
   (hc-append 4 (mk 5) (mk 10) (mk 20)))
(series circle)
(series square)
(series (lambda (size) (checkerboard (square size))))
(define series1
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))

;显示彩色图案

(define (rgb-series mk)
  (vc-append
    (series (lambda (sz) (colorize (mk sz) "red")))
    (series (lambda (sz) (colorize (mk sz) "green")))
    (series (lambda (sz) (colorize (mk sz) "blue")))))
(rgb-series circle)
(rgb-series square)
;一样的
(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))
(series (rgb-maker circle))
(series (rgb-maker square))
;8  list
(list "red" "green" "blue")
(list (circle 10) (square 10))
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 5))
(apply vc-append (rainbow (square 5)))
;9 modules
(require pict/flash)
(filled-flash 40 30)
(provide rainbow square)
(rainbow (square 5))
;Macros
(require slideshow/code)
(code (circle 10))
(code 1000)
(define-syntax pict+code
  (syntax-rules ( )
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))

(pict+code (circle 10))
(hc-append 10 (circle 10) (code (circle 10)))
;objects
(require racket/class
         racket/gui/base)
(define f (new frame% [label "My Art"]
               [width 300]
               [height 300]
               [alignment '(center center)]))
(send f show #t)
(define (add-drawing p)
   (let ([drawer (make-pict-drawer p)])
     (new canvas% [parent f]
                  [style '(border)]
                  [paint-callback (lambda (self dc)
                                    (drawer dc 0 0))])))
(add-drawing (pict+code (circle 10)))
(add-drawing (colorize (filled-flash 50 30) "yellow"))
;Where to go From Here























