(use reser clojurian-syntax matchable)

(define (app r)
  (match-route
   r
   (('GET) (response body: "root"))
   (('POST "echo") (response body: (alist-ref 'body r)))
   (else (response body: (conc "no matching uri for "
                               (request-route r))))))

(define app*
  (->  (lambda (r) (app r))
       (wrap-trailing-newline)
       (wrap-errors)
       (wrap-cors-headers "*")))

(define server-thread
  (thread-start!
   (lambda () (reser-start (lambda (r) (app* r))))))

(thread-join! server-thread)
;; or try: (use nrepl) (nrepl 1234)
