(use spiffy intarweb persistent-hash-map uri-common ports data-structures)

;; construct a response object
(define (response #!key body status code reason headers)
  (persistent-map 'body    body
                  'status  status
                  'code    code
                  'reason  reason
                  'headers headers))

;; call handler with an exception handler, and log error to request
;; response instead of stderr.
(define (wrap-errors handler)
  (lambda (r)
    (handle-exceptions exn
      (response body: (conc ((condition-property-accessor 'exn 'message) exn)
                            ": " ((condition-property-accessor 'exn 'arguments) exn) "\n"
                            (with-output-to-string (lambda () (pp (condition->list exn)))))
                status: 'bad-request)
      (handler r))))

(define ((wrap-log handler) r)
  (print "incoming " (uri->string (request-uri (current-request))))
  (handler r))

;; append \n at end of server response. makes it terminal friendly
(define ((trailing-newline handler) r)
  (map-update-in (handler r) '(body) (lambda (body) (string-append body "\n"))))

;; slurp entire request payload into string
(define (request-string!)
  (read-string (min (* 16 1024 1024) ;; <- max 16MB
                    (or (header-value 'content-length (request-headers (current-request))) 0))
               (request-port (current-request))))

(define (reser-handler handler)

  (define request (persistent-map 'body (request-string!)
                                  'uri (request-uri (current-request))
                                  'headers (request-headers (current-request))
                                  'method (request-method (current-request))))
  (define map (handler request))

  (send-response body:    (or (map-ref map 'body) "")
                 status:  (map-ref map 'status)
                 code:    (map-ref map 'code)
                 reason:  (map-ref map 'reason)
                 headers: (or (map-ref map 'headers) '())))

(define (reser-start handler)
  (vhost-map `((".*" . ,(lambda (c) (reser-handler handler)))))
  (start-server))
