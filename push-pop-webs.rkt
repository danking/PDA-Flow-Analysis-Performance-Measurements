#lang racket

(provide (struct-out web)
         empty-web
         webset-add-connection/uid
         get-web/push get-web/pop
         web-union
         webset-from-relation)


(struct web (pushes pops) #:transparent)

(define empty-web (web (set) (set)))

(define (webset-from-relation R)
  (for/fold
      ((w (set)))
      ((pair R))
    (webset-add-connection/uid w (first pair) (second pair))))

(define (webset-add-connection/uid webs push pop)
  (let ((push-web (get-web/push webs push))
        (pop-web (get-web/pop webs pop)))
    (let ((new-webset (set-remove (set-remove webs push-web) pop-web)))
      (set-add new-webset (web-union push-web pop-web)))))

(define (get-web/push webs push)
  (or (for/first ((web (in-set webs))
                  #:when (set-member? (web-pushes web) push))
        web)
      (web (set push) (set))))

(define (get-web/pop webs pop)
  (or (for/first ((web (in-set webs))
                  #:when (set-member? (web-pops web) pop))
        web)
      (web (set) (set pop))))

(define (web-union web1 web2)
  (web (set-union (web-pushes web1) (web-pushes web2))
       (set-union (web-pops web1) (web-pops web2))))
