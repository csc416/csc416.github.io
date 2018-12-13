; Author: Dor Rondel
; Course: CSc 416
;
; Problem: Programming Challenge: Knapsack Problem Genetic Algorithm

; TASK 1 - Essence of the individual

(setf *limit* 10)
(setf *knapsack-capacity* 20)

; randomly chooses either 0 or 1
(defmethod binary () 
    (nth (random 2) '(0 1))
)

; Generates an binary list of size len
(defmethod binary-list ((len integer)) 
    (cond 
        ((<= len 0)
            (princ "Length must have a positive polarity")
            nil
        )
        ((= len 1)
            (list (binary))
        )
        ((> len 1) 
            (cons 
                (binary) 
                (binary-list (- len 1))
            )
        )
    )
)

; Generates binary list of length limit
(defmethod binary-string ()
    (binary-list *limit*)
)

; generates prices 
(defmethod generate-prices ()
    (setf final '())
    (dotimes (i *limit*)
        (push (random 100) final)
    )
    final
)

; generates weights
(defmethod generate-weights ()
    (setf final '())
    (dotimes (i *limit*)
        (push (random 10) final)
    )
    final
)

; Generates multi-dimensional list of dotted pairs
(defmethod generate-items ()
    (setf prices (generate-prices))
    (setf weights (generate-weights))
    (pairlis prices weights)
)

(setf *items-list* (generate-items))

; TASK 2 - Mutation

; returns opposite of whats in bad at given index
(defmethod others ((int integer)) 
    (if (eq int '0) 
        1
        0
    )
)

; Returns random element from list
(defmethod pick ((lst list))
    (nth (random (length lst)) lst)
)

; Replces 2 arg for 3 arg from list in first arg
(defmethod change ((lst list) (new integer) (pos integer) &aux tmp)
    (cond
        ((< pos 0 ) 
            (princ "Must enter natural integer index")
            nil
        )
        ((= pos 0)
            (cons new (rest lst))
        )
        ((> pos 0)
            (cons (first lst) (change (rest lst) new (- pos 1)))
        )
    )
)

; supplied method, applies mutation on single base of string
(defmethod mutation ((binary-str list) &aux p q)
    (setf p (random (length binary-str)))
    (setf q (others (nth p binary-str)))
    (change binary-str q p)
)

; TASK 3 - Crossover

; Makes a list of first pos elements of list m
(defmethod first-n ((m list) (pos integer))
   (cond
        ((< pos 0 ) 
            (princ "Must enter natural integer index")
            nil
        )
        ((= pos 0)
            '()
        )
        ((> pos 0)
            (cons (first m) (first-n (rest m) (- pos 1)))
        )
    ) 
)

; Makes list of all elements occuring after pos index of m
(defmethod rest-n ((m list) (pos integer))
   (cond
        ((< pos 0 ) 
            (princ "Must enter natural integer index")
            nil
        )
        ((= pos 0)
            m
        )
        ((> pos 0)
            (rest-n (rest m) (- pos 1))
        )
    ) 
)

; Given method, applies crossover to binary string
(defmethod crossover ((m list) (f list))
    (setf pos (+ 1 (random (length m))))
    (append (first-n m pos) (rest-n f pos))
)

; TASK 4 - Demo for Mutation and Crossover

; Supplied method, demonstrates mutation behind the scence action
(defmethod mutation-demo (&aux s m)
	(setf s (binary-string))
	(dotimes (i 10)
		(format t "s = ~A~%" s)
		(setf m (mutation s))
		(format t "m = ~A~%~%" m)
	)
)

; Supplied method, demonstrates on crossover behind the scence action
(defmethod crossover-demo (&aux m f x)
    (setf m (binary-string))
    (setf f (binary-string))
    (dotimes (i 10)
        (format t "m = ~A~%" m)
        (setf x (crossover m f))
        (format t "x  = ~A~%" x)
        (format t "f = ~A~%~%" f)
    )
)

; TASK 5 - The Fitness Metric

; calculates total price of items in bag
(defmethod fitness-binary ((lst list))
    (setf total 0)
    (dotimes (i *limit*)
        (when (eq (nth i lst) '1)
            (setf total (+ total (car (nth i *items-list*))))
        )
    )
    total 
)

; Provided method to demonstrate different fitness methods
(defmethod fitness-demo (&aux binary-str fitness)
    (setf binary-str (binary-string))
    (format t "binary-str = ~A~%" binary-str)
    (format t "Directly applying the fitness metrics ...~%")
    (format t "fitness-binary = ~A~%" (fitness-binary binary-str))
)



; TASK 6 - The Individual Class

; class definition for individual
(defclass individual ()
	(
		(binary-string :accessor individual-binary-string :initarg :binary-string)
		(fitness :accessor individual-fitness :initarg :fitness)
		(number :accessor individual-number :initarg :number)
	)
)

; instantiates a random individual type
(defmethod random-individual (&aux binary) 
	(setf binary-str (binary-string))
	(make-instance 'individual
		:binary-string binary-str
		:fitness (funcall *fitness* binary-str)
		:number 0 
	)	
)

; customized instantiation of individual type
(defmethod new-individual ((nr number) (notes list))
	(make-instance 'individual
		:binary-string notes
		:fitness (funcall *fitness* notes)
		:number nr
	)	
)


(defmethod display ((i individual))
	(display-nnl i) (terpri)
)

; prints <num> <dna> <fitness>
(defmethod display-nnl ((i individual))
	(prin1 (individual-number i))
	(princ (filler (individual-number i)))
	(prin1 (individual-binary-string i))
	(princ "  ")
	(prin1 (individual-fitness i))
	(princ (filler (individual-fitness i)))
)

; whitespace for demo
(defmethod filler ((n number))
	(cond
		((< n 10) "     ")
		((< n 100) "    ")
		((< n 1000) "   ")
		((< n 10000) "  ")
		((< n 100000) " ")
	)
)

; calculates fitness of binary individual
(defmethod fitness-binary ((i individual))
	(fitness-binary (individual-binary-string i))
)

; demo for fitnesses of different individual types
(defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
	(setf *fitness* #'fitness-binary)
	(setf i0 (random-individual))
	(display i0)
	(setf one (binary-string))
	(setf i1 (new-individual 1 one))
	(display i1)
	(setf two (binary-string))
	(setf i2 (new-individual 2 two))
	(display i2)
	(setf three (binary-string))
	(setf i3 (new-individual 3 three))
	(display i3)
	(format t "Fitness of i0 = ~A~%" (funcall *fitness* i0))
	(format t "Fitness of i1 = ~A~%" (funcall *fitness* i1))
	(format t "Fitness of i2 = ~A~%" (funcall *fitness* i2))
	(format t "Fitness of i3 = ~A~%" (funcall *fitness* i3))
	nil
)

; TASK 7 - The Population Class

(defconstant *population-size* 50)
(defconstant *selection-size* 10)
(setf *select-demo* nil)
(setf *fitness* #'fitness-binary)

; class to model population of individual types
(defclass population ()
    (
        (individuals :accessor population-individuals :initarg :individuals)
        (generation :accessor population-generation :initform 0)
    )
)

; amount of individuals in the population
(defmethod size ((p population))
    (length (population-individuals p))
)

; prints string representation of a population
(defmethod display ((p population))
    (format t "~%~%Generation ~A population ...~%~%" (population-generation p))
    (dolist (indiv (population-individuals p))
        (display indiv)
    )
    (terpri)
)

; instantiates a population of individual types
(defmethod initial-population (&aux individuals)
    (setf individuals '())
    (dotimes (i *population-size*)
        (push (new-individual (+ i 1) (binary-string)) individuals)
    )
    (make-instance 'population :individuals (reverse individuals))
)

; calculates average fitness of population
(defmethod average ((p population) &aux (sum 0))
    ;(float (/ (apply #'+ (mapcar #'individual-fitness (population-individuals p)) (size p))))
    ;*** - APPLY: argument list given to + is dotted (terminated by 50)
    (dotimes (i (size p))
        (setf sum (+ sum (individual-fitness (nth i (population-individuals p)))))
    )
    (float (/ sum (size p)))
)

; checks individual bag weight to see if it's within the permissible capacity
(defmethod within-capacity ((indiv individual))
    (setf total-weight 0)
    (dotimes (i *limit*)
        (when (eq (nth i (individual-binary-string indiv)) '1)
            (setf total-weight (+ total-weight (cdr (nth i *items-list*))))
        )
    )
    (if (<= total-weight *knapsack-capacity*)
        t
        nil
    )
)

; returns most fit individual in a population
(defmethod most-fit-individual ((l list) &aux max-value max-individual)
	(setf max-value 0)
	(setf max-individual nil)
	(dolist (i l)
		(cond
			(
                (and 
                    (> (funcall *fitness* i) max-value) 
                    (within-capacity i)
                )

                (setf max-value (funcall *fitness* i))
                (setf max-individual i)
			)
		)	
	)
	max-individual
)

; randomly select individuals subject to selection
(defmethod select-individuals ((p population) &aux individuals candidates rn)
	(setf individuals (population-individuals p))
	(setf candidates '())
	(dotimes (i *selection-size*)
		(setf rn (random (size p)))
		(push (nth rn individuals) candidates)
	)
	candidates
)

; returns most fit individual from candidates for selection
(defmethod select-individual ((p population) &aux candidates)
    (setf candidates (select-individuals p))
    (setf mfi (most-fit-individual candidates))
    (if *select-demo* (select-demo-helper candidates mfi))
    mfi
)

; detailed printing of population string representation
(defmethod select-demo-helper ((l list) (i individual))
    (princ "the sample of individuals ...")
    (terpri)
    (mapcar #'display l)
    (terpri)
    (princ "the most fit of the sample ...")
    (terpri)
    (display i)
    (terpri)
)

; supplied method, demonstatrates population related code
(defmethod population-demo (&aux p)
    (setf p (initial-population))
    (display p)
    (format t "Average fitness = ~A~%~%" (average p))
    (setf *select-demo* t)
    (format t "Sampling ...~%~%")
    (select-individual p)
    (terpri)
    (format t "Sampling ...~%~%")
    (select-individual p)
    (terpri)
    (format t "Sampling ...~%~%")
    (select-individual p)
    (terpri)
)

; TASK 8 - Incorporating Mutation

; instantiates mutated individual type
(defmethod mutate ((i individual) &aux mutation)
    (setf mutation (mutation (individual-binary-string i)))
    (make-instance 'individual
        :number (individual-number i)
        :binary-string mutation
        :fitness (funcall *fitness* mutation)
    )
)

(defconstant *pc-m* 50)

; toin coss for mutation
(defmethod maybe-mutate ((i individual))
    (if (<= (+ 1 (random 100)) *pc-m*)
        (mutate i)
        i
    )
)

; demo mutation on an individual 
(defmethod mutate-demo ()
    (setf i (random-individual))
    (display i)
    (dotimes (x 20)
        (setf i (mutate i))
        (display i)
    )
)

; Show that not every generation gets mutated
(defmethod maybe-mutate-demo ()
    (setf i (random-individual))
    (display i)
    (dotimes (x 20)
        (setf n (maybe-mutate i))
        (display-nnl n)
        (if (not (equal n i ))(princ " *"))
        (terpri)
        (setf i n)
    )
)


; TASK 9 - Copy!

(setf *copy-demo* nil)
(defconstant *pc-c* 40)

; copies one individual from one generation to the next
(defmethod perform-copies ((cp population) (np population))
    (dotimes (i (nr-copies))
        (perform-one-copy cp np)
    )
)

; determines number of individuals passed down per generation
(defmethod nr-copies ()
    (* (/ *pc-c* 100) *population-size*)
)

; copies a possibly mutated individual from one generation to the next
(defmethod perform-one-copy ((cp population) (np population) &aux x m mm new-i)
    (setf m (select-individual cp))
    (if *copy-demo* (format t "Selected individual = ~%"))
    (if *copy-demo* (display m))
    (setf mm (maybe-mutate m))
    (if *copy-demo* (format t "Possibly mutated individual = ~%"))
    (if *copy-demo* (display mm))
    (setf (individual-number mm) (+ 1 (size np)))
    (if *copy-demo* (format t "Renumbered individual = ~%"))
    (if *copy-demo* (display mm))
    (setf new-i (new-individual (+ 1 (size np)) (individual-binary-string mm)))
    (setf
    (population-individuals np)
    (append (population-individuals np) (list new-i))
    )
    nil
)

; generates new empty incremented population generation
(defmethod empty-population ((cp population) &aux np)
    (setf np (make-instance 'population))
    (setf (population-individuals np) ())
    (setf (population-generation np) (+ 1 (population-generation cp)))
    np
)

; Demos copying of individuals from one generation to the next
(defmethod perform-copies-demo (&aux cp np)
    (setf cp (initial-population))
    (setf np (empty-population cp))
    (terpri) (display np) (terpri) (terpri)
    (setf *select-demo* t)
    (setf *copy-demo* t)
    (dotimes (i 10)
        (perform-one-copy cp np)
        (terpri) (display np) (terpri) (terpri)
    )
    (setf *select-demo* nil)
    (setf *copy-demo* nil)
    nil
)

; TASK 10 - Crossover!

(setf *crossover-demo* nil)
(defconstant *pc-x* 60)

; performs one crossover from current population to new population
(defmethod perform-crossovers ((cp population) (np population))
    (dotimes (i (nr-crossovers))
        (perform-one-crossover cp np)
    )
)

; determines number of crossovers to perform
(defmethod nr-crossovers ()
    (* (/ *pc-x* 100) *population-size*)
)

; makes a new individual through crossover of two selected
; individuals from current population, possibly mutates it
; and adds it to the population of the next generation
(defmethod perform-one-crossover ((cp population) (np population))
    (let (x m mm mother father new-i) 
        (setf mother (select-individual cp))
        (setf father (select-individual cp))
        (if *crossover-demo* (format t "Selected mother = ~%"))
        (if *crossover-demo* (display mother))
        (if *crossover-demo* (format t "Selected father = ~%"))
        (if *crossover-demo* (display father))
        (setf m (crossover mother father))
        (if *crossover-demo* (format t "the crossover = ~&"))
        (if *crossover-demo* (display m))
        (setf mm (maybe-mutate m))
        (if *crossover-demo* (format t "the possibly mutated individual = ~&"))
        (if *crossover-demo* (display mm))
        (setf (individual-number mm) (+ 1 (size np)))
        (if *crossover-demo* (format t "the renumbered individual = ~&"))
        (if *crossover-demo* (display mm))
        (setf new-i (new-individual (+ 1 (size np)) (individual-binary-string mm)))
        (setf 
            (population-individuals np)
            (append (population-individuals np) (list new-i))
        )
    )
    nil
)

; performs crossover on two individuals's dna lists
(defmethod crossover ((mother individual) (father individual) &aux mi fi x i)
    (setf mi (individual-binary-string mother))
    (setf fi (individual-binary-string father))
    (setf x (crossover mi fi))
    (setf i (new-individual 0 x))
    i
)

; displays crossover principles and code functionality
(defmethod perform-crossovers-demo (&aux cp np) 
    (setf cp (initial-population))
    (setf np (empty-population cp))
    (terpri) (display np) (terpri) (terpri)
    (setf *select-demo* t)
    (setf *crossover-demo* t)
    (dotimes (i 10)
        (perform-one-crossover cp np)
        (terpri) (display np) (terpri) (terpri)
    )
    (setf *select-demo* nil)
    (setf *crossover-demo* nil)
    nil
)

; TASK 11 - The GA

(defconstant *nr-generations* 20)

; returns new generation based off of old one
(defmethod next-generation ((cp population) &aux np)
    (setf np (empty-population cp))
    (perform-copies cp np)
    (perform-crossovers cp np)
    np
)

; prints representation of *items-list*
(defmethod print-items ()
    (print "Items List:")
    (setf count 0)
    (dolist (item *items-list*)
        (format t "~% Id: ~a        Price: ~a         Weight: ~a" count (car item) (cdr item))
        (setf count (+ count 1))
    )
)

; mimic the GA
(defmethod ga (&aux p)
    (format t "Bag Capacity: ~a lb ~%" *knapsack-capacity*)
    (print-items)
    (setf *fitness* #'fitness-binary)
    (setf p (initial-population))
    (terpri)
    (summarize p)

    (dotimes (i *nr-generations*)
        (setf p (next-generation p))
        (check-average p)
    )

    (summarize p)

    (format t "~% Most fit individual from final generation ~%~a" 
        (individual-binary-string (select-individual p))
    )
)

; Method to provide information on "Progress"
(defmethod summarize ((p population))
    (display p)
    (check-average p)
    (terpri)
)

(defmethod check-average ((p population))
    (format t "average fitness of population ~A = ~A~%" (population-generation p) (average p))
)