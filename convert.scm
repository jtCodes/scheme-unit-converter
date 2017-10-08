;; Helper functions for compat?
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; read-file produces a list whose elements are the expressions in the file.
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the database.
(define source (with-input-from-file "units.dat" read-file))

;; Helper function for appending non-list item.
(define (to-list item)    
  (cons item '()))

;;; MAIN FUNCTION ;;;
;;
;; ==>(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
;; (0.010230654761904762 (mi 1) (hr -1))
;; ==>(convert '(1 (l 1)) '(m 3))
;; (0.001 (m 3))
(define (convert from-list to-list)
  (let ((q-num (car from-list))
        (f-normal (normalize from-list))
        (t-normal (normalize to-list)))
    (cond ((or (null? f-normal) (null? t-normal))
           "NOTHING TO CONVERT")
          (else
           (cond ((compat? f-normal t-normal)
                  (let ((from-q (car f-normal))
                        (to-q (car t-normal)))
                    (cons (* q-num (exact->inexact (/ from-q to-q)))
                          to-list)))
                 (else "NOT COMPAT"))))))

;;
;; Takes two normalized list and see if they both have the same units
;;==>(compat? '(0.00016630952380952381 (m 1) (sec -1))
;;            '(0.44704 (m 1) (sec -1)))
;;#t
;;
(define (compat? from-units to-units)
  (let ((fu-len (length (cdr from-units)))
        (tu-len (length (cdr to-units)))
        (inter-len (length (intersection-set (cdr from-units)
                                             (cdr to-units)))))
    (cond ((and (= fu-len tu-len)
                (= inter-len fu-len)
                (not (equal? from-units to-units)))
           #t)
          (else #f))))

;;
;; Take a unit-list and turn it into the most basic form
;; ==>(normalize '((furlong 1)(fortnight -1)))
;; (0.00016630952380952381 (m 1) (sec -1))
;;
(define (normalize l)
  (let ((rat (make-rat l)))
    (cond ((null? (car rat)) '())
          (else
           (let ((watered-down
                  (cons (collect-relevant (car rat))
                        (collect-relevant (cdr rat)))))
             (cond ((equal? 1 (car watered-down))
                    '())
                   (else
                    (let ((normal (group-like watered-down)))
                      normal))))))))

;;
;; Used to turn user input into a rational representation. car
;; is the numerator and cdr is the denominator
;;
;; ==>(make-rat '(27.5 (furlong 1)(fortnight -1)))
;; ((1 furlong (201.168 (m 1))) -1 fortnight (1209600 (sec 1)))
;; ==>(car (make-rat '(27.5 (furlong 1)(fortnight -1))))
;; (1 furlong (201.168 (m 1)))
;;
(define (make-rat l)
  (let ((n (check-each-for numer l)) (d (check-each-for denom l)))
    (cond ((and (not (null? n)) (not (null? d))) 
           (cond ((and (not (number? (car n))) (number? (car d)))
                  (let ((fix-n (cons 1 n)))   ;add number to stay consistent
                    (cons fix-n d)))          ;in the form (number (unit-list))
                 ((and (not (number? (car d))) (number? (car n)))
                  (let ((fix-d (cons 1 d)))
                    (cons n fix-d)))
                 ((and (not (number? (car d))) (not (number? (car n))))
                  (let ((fix-n (cons 1 n)) (fix-d (cons 1 d)))
                    (cons fix-n fix-d)))
                 (else (cons n d))))
          ((and (null? n) (not (null? d))) ;no numerator
                 (cons 1 d))
          (else (cons n '(1 '(nothing))))))) ;no denominator

;;
;; Takes either denom or numer as proc and either the from unit-list
;; or to unit-list as l. Feeds proc one element at a time from l
;; and try to build a list from it.
;;
(define (check-each-for proc l)
  (cond ((null? l) '())
        ((symbol? (car l)) (proc l))
        (else (append (proc l) (check-each-for proc (cdr l))))))

;;
;; Gets input from function check-each-for and check if the input is a unit
;; with a negative exponent. Return the input as base-unit form if condition
;; met, else return empty list.
;;
;; ==>(denom '((fortnight -1)))
;; (-1 fortnight (1209600 (sec 1)))
;;
(define (denom item)
  (cond ((list? (car item))
         (cond ((negative? (cadar item))  ;(car (cdr (car '((joule 2)(hr 1)))))
                (cond ((assoc (caar item) source)
                       (cons (cadar item) (assoc (caar item) source)))
                      (else               ;for cases such as '((m 1)(sec 1))
                       (to-list (cons (caar item) (to-list (cadar item)))))))
               (else '())))
        ((symbol? (car item))             ;for input such as '(sec -1)
         (cond ((negative? (cadr item))
                (cond ((assoc (car item) source)
                       (cons (cadr item) (assoc (car item) source)))
                      (else
                        (cons 1 (cons (car item) ;(1 sec (1 (sec 1)))
                                     (to-list
                                      (cons 1 (to-list
                                      (cons (car item) (cdr item))))))))))
               (else '())))
        (else '())))
;;
;; Same as denom except for positive exponents
;;==>(check-each-for numer '((m 1)(sec 1)))
;;(1 m (1 (m 1)) 1 sec (1 (sec 1)))
;;
(define (numer item)
  (cond ((list? (car item))
         (cond ((positive? (cadar item))  ;(car (cdr (car '((joule 2)(hr 1)))))
                (cond ((assoc (caar item) source)
                       (cons (cadar item) (assoc (caar item) source)))
                      (else               ;for cases such as '((m 1)(sec 1))
                       (cons 1 (cons (caar item) ;'(1 m (1 (m 1))
                                     (to-list
                                      (cons 1 (to-list
                                      (cons (caar item)
                                            (to-list (cadar item)))))))))))
               (else '())))
        ((symbol? (car item))             ;for input such as '(sec 1)
         (cond ((positive? (cadr item))
                (cond ((assoc (car item) source)
                       (cons (cadr item) (assoc (car item) source)))
                      (else
                        (cons 1 (cons (car item) ;(1 sec (1 (sec 1)))
                                     (to-list
                                      (cons 1 (to-list
                                      (cons (car item) (cdr item))))))))))
               (else '())))
        (else '())))

;;           
;; Collect only relevent number and units from either the numer or denom.
;; Units and numbers will be have their exponent applied to them. 
;;
;; ==>test
;; ((1 furlong (201.168 (m 1))) -1 fortnight (1209600 (sec 1)))
;; ==>(cons (collect (car test)) (collect (cdr test)))
;; ((201.168 (m 1)) 1209600 (sec -1))
;;
(define (collect-relevant l)
  (define (iter things num pow units)
    (let ((result (cons num units)))
      (cond ((null? things) result)
            ((and (number? (car things)) (symbol? (cadr things)))
             (let ((next (cdddr things)) (current-pow (car things))
                                         (current-num (caaddr things))
                                         (current-ul (cdaddr things)))
               (iter next
                     (* num (expt current-num (abs current-pow)))
                     1 
                     (append units (apply-pow current-pow current-ul))))))))
  (cond ((equal? 1 l) 1)   ;nothing in numer
        ((or (null? l) (list? (cadr l))) '())
         (else
          (iter l 1 1 '()))))

;;
;; Helper function to collect-relevant.
;; Takes exponent and a unit-list as input and apply the exponent to
;; all units in the list.
;;
;; ==>(apply-pow 2 '((kg 2) (m 4) (sec -4)))
;;((kg 4) (m 8) (sec -8))
;;
(define (apply-pow pow l)
  (cond ((= pow 1) l)
        ((null? l) '())
        ((= pow 0)
         (let ((unit (append (to-list(caar l))     ;(car (car '((kg 1)))) = kg
                                  (to-list 1))))
                (cons unit (apply-pow pow (cdr l)))))
        (else (let ((unit (append (to-list(caar l))           
                                  (to-list (* pow (cadar l))))))
                (cons unit (apply-pow pow (cdr l)))))))

;;
;; Takes result returned by function collect and combine like terms.
;; car will be number and cdr will be the unit-list.
;; Convenient form to decide compatibility. 
;;
;; ==>(group-like '((3600 (kg 2) (m 4) (sec -4) (sec 1))
;;                  2 (kg -3) (m -3) (sec 6)))
;;(1800.0 (kg -1) (m 1) (sec 3))
;;
(define (group-like l)
  (cond ((or (null? (car l)) (null? (cdr l)) (equal? 1 l))
         (let ((number (caar l))
               (pre-comb-unit-list (cdar l)))
           (cons number
                 (for-each-unique-unit combine-units pre-comb-unit-list))))
        (else
         (let ((number (exact->inexact (/ (caar l) (cadr l))))
               (pre-comb-unit-list (append (cdar l) (cddr l))))
           (cons number
                 (for-each-unique-unit combine-units pre-comb-unit-list))))))

;;
;; Helper function to group-like. This function, with the help of remove-units,
;; ensures that combine-units always have (car l) as a unique base unit that
;; needs to be check for duplicates.
;; Essentially the followings are what is being fed to combine-units:
;; '((kg 2) (m 4) (sec -4) (sec 1) (kg -3) (m -3) (sec 6))
;; '((m 4) (sec -4) (sec 1) (m -3) (sec 6))
;; '((sec -4) (sec 1) (sec 6))
;; '()
(define (for-each-unique-unit proc l)
  (cond ((null? l) '())
        (else (cons (proc l)
                    (for-each-unique-unit proc (remove-unit (caar l) l))))))

;; Takes a list from for-each-unit function and add all duplicate unit's
;; exponent to the first item of the list. for-each-unique-unit ensures
;; all base units only appear at (car l) once.
(define (combine-units l)
  (define (iter things unique-unit)
    (cond ((or (null? things) (not (pair? things)))
           (cond ((= (cadr unique-unit) 0)
                  '())
                 (else unique-unit)))
          (else
           (let ((current-base-unit (caar things))    ;delcare variables
                 (unique-base-unit (car unique-unit))
                 (current-exp (cadar things))
                 (unique-exp (cadr unique-unit)))
             (cond ((equal? current-base-unit         ;base-unit match
                            unique-base-unit)
                    (cond ((and
                            (negative? current-exp)   ;when both neg exponents
                            (negative? unique-exp))
                           (iter (cdr things)         ;update unique-unit
                                 (append (to-list unique-base-unit)
                                         (to-list
                                          (to-neg (+ (abs current-exp)
                                                     (abs unique-exp)))))))
                          (else                  ;not both negative exponents
                           (iter (cdr things)
                                 (append (to-list unique-base-unit)
                                         (to-list
                                          (+ current-exp unique-exp)))))))
                   (else                        ;check for next unit to combine
                    (iter (cdr things) unique-unit)))))))
    (iter (cdr l) (car l)))

;; Helper functions for when combining two negative exponents
(define (to-neg pos)
    (* -1 pos))

;;
;; Takes a base-unit and unit-list as input and remove all units matching
;; base-unit input.
;;
;; ==>(remove-unit 'kg '((kg 2) (mi 3)(kg 4)))
;; ((mi 3))
;;
(define (remove-unit target l)
  (cond ((null? l) '())
        ((not (equal? target (caar l)))                             
         (append (to-list (car l)) (remove-unit target (cdr l))))
        (else (remove-unit target (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
;;; TESTs
;(define cup '(1 (cup 1)))
;(define tsp '(tsp 1))
;(define fur '(27.5 (furlong 1)(fortnight -1)))
;(define mi '((mi 1)(hr -1)))

;(define t (cons (check-each-for numer fur) (check-each-for denom fur)))
;(define imp (cons (check-each-for numer '(5 (joule 2)(N -3)(hr 1)))
;                 (check-each-for denom '(5 (joule 2)(N -3)(hr 1)))))
;(define t1 (cons (collect-relevant (car imp))
;                 (collect-relevant (cdr imp))))
;(define n '(5 (joule 2)(N -3)(hr 1)))

;(define ul '((kg 1) (m 2) (sec -2)))
;(define ull '(35 (m 1)(sec -1)))
;(define t2 (cons (check-each-for numer '(5 (joule 2)(N -3)(hr 1)(min 1)))
;                   (check-each-for denom '(5 (joule 2)(N -3)(hr 1)(min 1)))))
;(define t3 (cons (collect-relevant (car t2))
;                 (collect-relevant (cdr t2))))
;(define t8  '(35 (m 1)(sec 1)) ) 
;(define tf (make-rat fur))
;(define f (cons (collect-relevant (car tf))
;                (collect-relevant (cdr tf))))

;(define t10 (make-rat '(35 (m 1)(sec 1))))

;(convert '(19 (knot 1)) '((mi 1) (hr -1)))
;(convert '(1 (l 1)) '((cup 1)))
;(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
