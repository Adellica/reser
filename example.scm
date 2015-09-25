(use reser matchable persistent-hash-map nrepl)

(define app
  (wrap-errors ;; this prints error messages in HTTP response
   (wrap-log ;; this prints every incoming request to stderr I think
    (lambda (r)
      (match-route r
                   (('GET)
                    (response body: "usage: POST some data to /echo\n"))
                   (('POST "echo")
                    (response body: (conc (alist-ref 'body r) " right back atcha\n")))
                   (('GET path)
                    (response body: (conc "your toplevel path is " path "\n")))
                   ;; responses should be completely customizable. here's a 404:
                   (else (response body: "url not found!\n" status: 'not-found)))))))

;; start HTTP server in our thread
;; to change the port, do: (use spiffy) (server-port 8090)
(define reser-thread (thread-start! (lambda () (reser-start (lambda (r) (app r))))))

;; you can redefine app from the outside like this:
(nrepl 8081)
