#lang web-server/insta
;https://docs.racket-lang.org/continue/index.html
;Getting Started 打开默认网页
(define (start request)
  (response/xexpr
   '(html
     (head(title "my blog"))
     (body (h1 "under construction")))))

;the application
;Basic Blog
(struct post (title body) )
title : string? 
body : string?
blog:(listof post?)
(define BLOG (list (post "First Post!"
                         "Hey ,this is my first post!")))

(define xexpr/c
  (flat-rec-contract
   xexpr
   (or/c string?
         (cons/c symbol? (listof xexpr))
         (cons/c symbol?
                 (cons/c (listof (list/c symbol string?))
                         (listof xexpr))))))
; render-greeting: string -> response
; Consumes a name, and produces a dynamic response.
(define (render-greeting a-name)
  (response/xexpr
   `(html (head (title "Welcome"))
          (body (p ,(string-append "Hello " a-name))))))


render-post : (post "first post!" "This is a first post.")












