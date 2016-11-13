(ns derivatives.core
  (:gen-class))

;;Hi i'm mark :P

(defn third [list]
  (second (next list)))

;; True if the form is a variable (symbol).
(defn variable? [form]
  (symbol? form))

;; True if the two forms are the same variable.
(defn same-variable? [form1 form2]
  (and (variable? form1) (variable? form2) (= form1 form2)))

;; True if the form represents a sum.
(defn sum? [form]
  (and (list? form) (= '+ (first form))))

;; True if the form represents a diff.
(defn diff? [form]
  (and (list? form) (= '- (first form))))

;; Returns true if the form is a power function
(defn power? [form] 
  (and (list? form)(= '** (first form))))

;; Returns true if the form is a quotient
(defn quot? [form] 
  (and (list? form)(= '/ (first form))))

;; Returns true if the form is a Logarithm
(defn ln? [form]
  (and (list? form)(= 'ln (first form))))

;; Returns true if the form is sin
(defn sin? [form] 
  (and (list? form)(= 'sin (first form))))

;; Returns true if the form is cos
(defn cos? [form]
  (and (list? form)(= 'cos (first form))))

;; Returns true if the form is tan
(defn tan? [form]
  (and (list? form)(= 'tan (first form))))

;; Returns the u of ln (u)
(defn log-of [ln]
  (second ln))

;; Returns the angle of a trig function
(defn angle [form]
   (second form))

;;Make a sin with angle a
(defn make-sin [a]
  (list 'sin a))

;;Make a cos with angle a
(defn make-cos [a]
  (list 'cos a))

;;Make a sec with angle a
(defn make-sec [a]
  (list 'sec a))

;; Constructs a sum of a and b.
(defn make-sum [a b]
  (cond
    (= a 0) b
    (= b 0) a
    (and (number? a) (number? b)) (+ a b)
    :else(list '+ a b)))

;;Constructs a difference of a and b.
;;(defn make-diff [a b]
;;  (list '- a b))

(defn make-diff [a b]
  (cond 
    (= a 0) (list '- b)
    (= b 0) a
    (and (number? a) (number? b)) (- a b)
    :else (list '- a b)))

;;Constructs a power of base a and power b
;;(defn make-power [a b] 
;;  (list '** a b))

(defn make-power [a b]
  (cond
   (= b 0) 1
   (= b 1) a
   (and (number? a) (number? b)) (Math/pow a b)
   :else (list '** a b)))

(defn make-ln [a] ;;this never gets used
  (cond
    (number? a)(Math/log a)
    :else (list 'ln a)))
  

;;Constructs a quotient between a and b.
;;(defn make-quot [a b]
;;  (list '/ a b))

(defn make-quot [a b]
  (cond
    (= b 1) a
    (= a 0) 0
    (and (number? a) (number? b)) (/ a b)
    :else (list '/ a b)))


;; Selects the addend (first value) of a sum.
(defn addend [sum]
  (second sum))

;; Selects the augend (second value) of a sum.
(defn augend [sum]
  (third sum))

;; Selects the base (first value) of a power.
(defn base [power]
  (second power))

;; Selects the augend (second value) of a power.
(defn exponent [power]
  (third power))

;; True if the form represents a product.
(defn prod? [form]
  (and (list? form) (= '* (first form))))

;; Constructs a product of a and b.
;;(defn make-prod [a b]
;;  (list '* a b))

(defn make-prod [a b]
  (cond 
    (= a 0) 0
    (= b 0) 0
    (= a 1) b
    (= b 1) a
    (and (number? a) (number? b)) (* a b)
    :else (list '* a b)))
 

;; Selects the multiplier (first value) of a product.
(defn multiplier [prod]
  (second prod))

;; Selects the multiplicand (second value) of a product.
(defn multiplicand [prod]
  (third prod))

;; Selects the dividend (first value) of a quotient.
(defn dividend [quot]
  (second quot))

;; Selects the divisor (second value) of a quotient.
(defn divisor [quot]
  (third quot))



;; Returns the derivative of a function expressed in Clojure notation, where variables are quoted.
;; The second parameter is the variable which the derivative is calculated with respect to.
(defn derivative [form var]
  (cond ; The derivative of a constant is 0
        (number? form) 0 
        ; The derivative of a variable is 0 if the variable is not the one being derived; or 1, if it is.
        (variable? form) (if (same-variable? form var) 1 0)
        ; Sum rule
        (sum? form) (make-sum (derivative (addend form) var)
                              (derivative (augend form) var))
        ; Diffrence rule
        (diff? form) (make-diff (derivative (addend form) var)
                                (derivative (augend form) var))
        ; Product rule    
        (prod? form) (make-sum (make-prod (multiplier form)
                                      (derivative (multiplicand form) var))
                               (make-prod (derivative (multiplier form) var)
                                      (multiplicand form)))
        ; Power rule
        (power? form) (make-prod (make-prod (exponent form)
                                        (make-power (base form) (- (exponent form) 1)))
                                 (derivative (base form) var))
        ; Quotient rule
        (quot? form) (make-quot (make-diff (make-prod (divisor form) (derivative (dividend form) var))
                                      (make-prod (dividend form) (derivative (divisor form) var)))
                                (make-power (divisor form) 2))
        
        ; Natural logarithm
        (ln? form) (make-quot (derivative (log-of form) var) 
                              (log-of form))
        
        ; Sin Rule
        (sin? form) (make-prod 
                      (make-cos (angle form)) 
                      (derivative (angle form) var))
        ; Cos Rule
        (cos? form) (make-prod -1
                      (make-prod 
                        (make-sin (angle form)) 
                        (derivative (angle form) var))) 
        ; Tan Rule
        (tan? form) (make-prod 
                      (make-power (make-sec (angle form)) 2) 
                      (derivative (angle form) var))))  
 
                                          
(defn -main []
 (println (derivative '(- (* 5 x) y) 'x)) ;;working
 (println (derivative '(** (ln (+ (** x 2) 1)) 5) 'x))  ;;fixed
 (println (derivative '(/ (* 5 (sin x)) (cos x)) 'x))) ;;fixed
  
 



 
 

 
