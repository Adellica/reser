(use srfi-1 spiffy intarweb uri-common ports data-structures)

;; construct a response object
(define (response #!key body status code reason headers)
  `((body    . ,body)
    (status  . ,status)
    (code    . ,code)
    (reason  . ,reason)
    (headers . ,headers)))

;; call handler with an exception handler, and log error to request
;; response instead of stderr.
(define ((wrap-errors handler) r)
  (handle-exceptions
   exn
   (response body: (conc (get-condition-property exn 'exn 'message) ": "
                         (get-condition-property exn 'exn 'arguments) "\n"
                         (with-output-to-string (lambda () (pp (condition->list exn)))))
             status: 'bad-request)
   (handler r)))

;; append \n at end of server response. makes it terminal friendly
(define ((wrap-trailing-newline handler) r)
  (let ((resp (handler r)))
    (alist-update 'body (string-append (or (alist-ref 'body resp) "") "\n") resp)))


;; access domain-name string, use "*" to give everybody access
;; ((wrap-cors-headers "*" (lambda _ (response headers: (headers `((abd "klm")))))) '())
;; ((wrap-cors-headers "*" (lambda _ (response body: "hi"))) '())
(define ((wrap-cors-headers access handler) r)
  (let ((response (handler r)))
    (alist-update 'headers
                  (alist-update 'Access-Control-Allow-Origin
                                (list access)
                                (or (alist-ref 'headers response) '()))
                  response)))

;; slurp entire request payload into string
(define (request-string!)
  ;; TODO: what to do is we have more than 16MB? we can't just ignore it all.
  (read-string (min (* 16 1024 1024) ;; <- max 16MB
                    (or (header-value 'content-length (request-headers (current-request))) 0))
               (request-port (current-request))))

(define (reser-handler handler)

  (define request `((body    . ,(request-string!))
                    (uri     . ,(request-uri (current-request)))
                    (headers . ,(request-headers (current-request)))
                    (method  . ,(request-method (current-request)))))

  (define resp (handler request))

  (send-response body:    (or (alist-ref 'body resp) "")
                 status:  (alist-ref 'status resp)
                 code:    (alist-ref 'code resp)
                 reason:  (alist-ref 'reason resp)
                 headers: (or (alist-ref 'headers resp) '())))

(define (reser-start handler)
  (vhost-map `((".*" . ,(lambda (c) (reser-handler handler)))))
  (start-server))


;; ==================== routes ====================

(define (request-route request)
  (cons (alist-ref 'method request)
        ;; remove trailing slashes:
        ;;   curl localhost:8080  => '(/)
        ;;   curl localhost:8080/ => '(/ "")
        (cdr (remove (lambda (x) (equal? x ""))
                     (uri-path (alist-ref 'uri request))))))

(define-syntax match-route
  (syntax-rules ()
    ((_ request specs ...)
     (match (request-route request)
       specs ...))))

