#lang racket

(require (planet williams/simulation/simulation-with-graphics))

(define services 1)




;;; generate costemer 
(define n-customers 0)
(define n-superwash 0)
(define n-dogwash 0)
(define end-simulation-time (* 24 60))
(define mean-arrival-time 26.0)


(define service #f)
(define wait-times #f)


(define (generator)
  (let loop ((i 0))
    (unless (> (current-simulation-time) end-simulation-time)
      (define prossess (random-integer 2))
   
      
      

      
     (cond ((= prossess 0)
         (schedule #:now (customer i ' superwash)))
        (else
         (schedule #:now (customer i ' dogwash))))
      (wait (random-exponential mean-arrival-time))
      (loop (+ i 1)))))



(define (transaction-time transaction)
  (case transaction
    ((superwash) 5)
    ((dogwash) 7)
   
    (else
     (error 'transaction-time
            "Unknown transaction ~a" transaction))))

(define-process (customer id transaction)
  (define arrival-time (current-simulation-time))
  (printf "~a: Customer ~a arrives and requested for ~a.~n"
          (current-simulation-time) id transaction)

;          (case transaction
;;            ((superwash) "requested for superwash")
;            ((dogwash) "requested for dogwash")
;           
;            (else
;             (error 'customer
;                    "Unknown transaction ~a" transaction))))
  (case transaction
    ((superwash)
     (set! n-superwash (+ n-superwash 1)))
    ((dogwash) 
    (set! n-dogwash (+ n-dogwash 1)))
    (else
     (error 'customer
            "Unknown transaction ~a" transaction)))     
  (define service-id (modulo id services))
  (define servise-station (vector-ref service service-id))
  (define wait-time (vector-ref wait-times service-id))
  (resource-request servise-station )
  (printf "~a: Customer ~a got service ~a.~n"
          (current-simulation-time) id service-id)
  (set-variable-value! wait-time (- (current-simulation-time) arrival-time))
  (work (transaction-time transaction))
  (resource-relinquish servise-station)
  (printf "~a: Customer ~a departs.~n"
          (current-simulation-time) id))

(define (run-simulation)
  (with-new-simulation-environment
    (set! service (build-vector
                   services
                   (lambda (i)
                     (make-resource))))
    (set! wait-times (build-vector
                      services
                      (lambda (i)
                        (make-variable))))
    (for ((i (in-range services)))
      (tally (variable-statistics (vector-ref wait-times i))))
    (schedule #:at 0.0 (generator))
    (start-simulation)
    (printf "Simulation ends at time ~a.~n"
            (current-simulation-time))
    (printf "Maximum queue lengths:~n")
    (for ((i (in-range services)))
      (printf "  carwish-station ~a = ~a~n"
              i (variable-maximum (resource-queue-variable-n (vector-ref service i)))))
    (printf "Average customer wait times:~n")
    (for ((i (in-range services)))
      (printf "  carwish-station  ~a = ~a~n"
              i (variable-mean (vector-ref wait-times i))))
   (define revenue (+ (* n-superwash 11) (* n-dogwash 13)))
    (printf "revenue for each day=$~a~n" revenue)
(define monthly-income (* revenue 30))
    (printf "monthly income=$~a~n" monthly-income)
; define the cost which include water, electricity and washing liquid
    (define monthly-electricity-cost1 825)
    (printf "monthly electricity charge for superwash=$~a~n" monthly-electricity-cost1)
    (define monthly-washingliquid-cost1 366)
    (printf "monthly washing liquid charge for superwash=$~a~n" monthly-washingliquid-cost1)
     (define monthly-water-cost1 366)
    (printf "monthly water charge for superwash=$~a~n" monthly-water-cost1)
    (define monthly-investment-superwash (+  monthly-electricity-cost1 monthly-washingliquid-cost1 monthly-water-cost1))
    (printf "monthly investment for superwash=$~a~n" monthly-investment-superwash)
     (define monthly-electricity-cost2 975)
    (printf "monthly electricity charge for dogwash=$~a~n" monthly-electricity-cost2)
    (define monthly-washingliquid-cost2 433)
     (printf "monthly washing liquid charge for dogwash=$~a~n" monthly-washingliquid-cost2)
     (define monthly-water-cost2 433)
      (printf "monthly water charge for dogwash=$~a~n" monthly-water-cost2)
    (define monthly-investment-dogwash (+  monthly-electricity-cost2 monthly-washingliquid-cost2 monthly-water-cost2))
     (printf "monthly investment for dogwash=$~a~n" monthly-investment-dogwash)
          (define total-investment (+ monthly-investment-dogwash monthly-investment-superwash))
    (printf " total monthly investment on car wash station=$~a~n" total-investment)
;(define monthly-investment-on-washstation (+ (* n-superwash 3.99) (* n-dogwash 4.50)))
   
; define monthly profit
(define monthly-profit (- monthly-income total-investment))
    (printf "monthly profit=$~a~n" monthly-profit)


(define investment 100000)
    (printf "initial investment=$~a~n" investment)
(define return-on-investment (floor(/ investment monthly-profit)))
    (printf "Owners investment will be met in ~a~n months" return-on-investment)
    ))

(run-simulation)
