#lang racket/base

(require racket/date
         net/http-client
         net/base64
         net/url
         netrc
         json)

(define *librato-api-host* "metrics-api.librato.com")

(struct librato-client (conn username password))

(define (make-librato-client #:conn [conn #f]
                             #:username [username ""]
                             #:password [password ""])
  (define new-conn (if (http-conn? conn) conn (http-conn-open *librato-api-host* #:ssl? #t)))
  (define entry (if (or (string=? username "") (string=? password ""))
                    (netrc-find-entry *librato-api-host*)
                    #f))
  (librato-client
   new-conn
   (if (and (string=? username "") entry) (netrc-entry-login entry) username)
   (if (and (string=? password "") entry) (netrc-entry-password entry) username)))

(define (send-request client path
                      #:method [method 'get]
                      #:data [data #""]
                      #:headers [headers '()])
  (define u:p (string-append (librato-client-username client)
                             ":"
                             (librato-client-password client)))
  (define authorization (bytes-append #"Authorization: Basic "
                                      (base64-encode (string->bytes/utf-8 u:p) #"")))
  (define out-headers (cons authorization headers))

  (http-conn-sendrecv! (librato-client-conn client)
                       path
                       #:method method
                       #:version "HTTP/1.1"
                       #:headers out-headers
                       #:data data))

(define (gauge name value
               #:timestamp [timestamp (date->seconds (current-date))]
               #:source [source #f]
               #:count [count #f]
               #:sum: [sum #f]
               #:max [max #f]
               #:min [min #f]
               #:sum-squares [sum-squares #f])
  (define data (make-hasheq (list (cons 'name name)
                                  (cons 'value value)
                                  (cons 'measure_time timestamp))))
  (when source (hash-set! data 'source source))
  (when count (hash-set! data 'count count))
  (when sum (hash-set! data 'sum sum))
  (when max (hash-set! data 'max max))
  (when min (hash-set! data 'min min))
  (when sum-squares (hash-set! data 'sum_squares sum-squares))
  (list 'gauge data))

(define (counter name value
                 #:timestamp [timestamp (date->seconds (current-date))]
                 #:source [source #f])
  (define data (make-hasheq (list (cons 'name name)
                                  (cons 'value value)
                                  (cons 'measure_time timestamp))))
  (when source (hash-set! data 'source source))
  (list 'counter data))

(define (librato-post-measurement client measurement
                                  #:source [source ""]
                                  #:timestamp [timestamp #f])
  (librato-post-measurements client (list measurement) #:source source #:timestamp timestamp))

(define (librato-post-measurements client measurements
                                   #:source [source #f]
                                   #:timestamp [timestamp #f])
  (define path "/v1/metrics")
  (define headers '("Content-Type: application/json"))
  (define counters (for/list ([m measurements] #:when (eq? (car m) 'counter)) (cadr m)))

  (define gauges (for/list ([m measurements] #:when (eq? (car m) 'gauge)) (cadr m)))
  (define data (make-hasheq))
  (when source (hash-set! data 'source source))
  (when timestamp (hash-set! data 'measure_time timestamp))
  (when (not (null? gauges)) (hash-set! data 'gauges gauges))
  (when (not (null? counters)) (hash-set! data 'counters counters))

  (send-request client
                path
                #:method 'POST
                #:headers headers
                #:data (jsexpr->string data)))
