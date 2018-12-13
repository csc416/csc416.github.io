; Author: Dor Rondel
; Course: CSc-416

; Problem: RBG GA

; TASK 1 - RBG-string

; letters in rbg string
(setf *limit* 25)

; Randomally returns either r, b, or g
(defmethod rbg () 
    (nth (random 3) '(R B G))
)

; Generates an RBG list of size len
(defmethod rbg-list ((len integer)) 
    (cond 
        ((<= len 0)
            (princ "Length must have a positive polarity")
            nil
        )
        ((= len 1)
            (list (rbg))
        )
        ((> len 1) 
            (cons 
                (rbg) 
                (rbg-list (- len 1))
            )
        )
    )
)

; Generates rbg list of length limit
(defmethod rbg-string ()
    (rbg-list *limit*)
)

; TASK 2 - Mutation

; Removes symbol from list
(defmethod others ((lst list) (sym symbol)) 
    (remove sym lst)
)

; Returns random element from list
(defmethod pick ((lst list))
    (nth (random (length lst)) lst)
)

; Replces 2 arg for 3 arg from list in first arg
(defmethod change ((lst list)  (new symbol) (pos integer) &aux tmp)
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
(defmethod mutation ((rbg-str list) &aux p q)
    (setf p (random (length rbg-str)))
    (setf q (others '(r b g) (nth p rbg-str)))
    (change rbg-str (pick q) p)
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

; Given method, applies crossover to rbg string
(defmethod crossover ((m list) (f list))
    (setf pos (+ 1 (random (length m))))
    (append (first-n m pos) (rest-n f pos))
)

; TASK 4 - Demo for Mutation and Crossover

; Supplied method, demonstrates mutation behind the scence action
(defmethod mutation-demo (&aux s m)
	(setf s (rbg-string))
	(dotimes (i 10)
		(format t "s = ~A~%" s)
		(setf m (mutation s))
		(format t "m = ~A~%~%" m)
	)
)

; Supplied method, demonstrates on crossover behind the scence action
(defmethod crossover-demo (&aux m f x)
    (setf m (rbg-string))
    (setf f (rbg-string))
    (dotimes (i 10)
        (format t "m = ~A~%" m)
        (setf x (crossover m f))
        (format t "x  = ~A~%" x)
        (format t "f = ~A~%~%" f)
    )
)

; TASK 5 - The Fitness Metric

(defmethod fitness-r ((lst list))
    (count 'R lst)
)

(defmethod fitness-g ((lst list))
    (count 'G lst)
)

(defmethod fitness-b ((lst list))
    (count 'B lst)
)

; Provided method to demonstrate different fitness methods
(defmethod fitness-demo (&aux rbg-str fitness)
    (setf rbg-str (rbg-string))
    (format t "rbg-str = ~A~%" rbg-str)
    (format t "Directly applying the fitness metrics ...~%")
    (format t "fitness-r = ~A~%" (fitness-r rbg-str))
    (format t "fitness-b = ~A~%" (fitness-b rbg-str))
    (format t "fitness-g = ~A~%" (fitness-g rbg-str))
    (format t "Indirectly applying the fitness metrics ...~%")
    (setf fitness #'fitness-r)
    (format t "fitness-r = ~A~%" (funcall fitness rbg-str))
    (setf fitness #'fitness-b)
    (format t "fitness-b = ~A~%" (funcall fitness rbg-str))
    (setf fitness #'fitness-g)
    (format t "fitness-g = ~A~%" (funcall fitness rbg-str))
)
 
; TASK 6 - The Individual Class

; class definition for individual
(defclass individual ()
	(
		(rbg-string :accessor individual-rbg-string :initarg :rbg-string)
		(fitness :accessor individual-fitness :initarg :fitness)
		(number :accessor individual-number :initarg :number)
	)
)

; instantiates a random individual type
(defmethod random-individual (&aux rbg) 
	(setf rbg (rbg-string))
	(make-instance 'individual
		:rbg-string rbg
		:fitness (funcall *fitness* rbg)
		:number 0 
	)	
)

; customized instantiation of individual type
(defmethod new-individual ((nr number) (notes list))
	(make-instance 'individual
		:rbg-string notes
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
	(prin1 (individual-rbg-string i))
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

; fitness-b on rbg-string accessor indivual
(defmethod fitness-b ((i individual))
	(fitness-b (individual-rbg-string i))
)

(defmethod fitness-r ((i individual))
	(fitness-r (individual-rbg-string i))
)	

(defmethod fitness-g ((i individual))
	(fitness-g (individual-rbg-string i))
)

; demo for fitnesses of different individual types
(defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
	(setf *fitness* #'fitness-r)
	(setf i0 (random-individual))
	(display i0)
	(setf one (rbg-string))
	(setf i1 (new-individual 1 one))
	(display i1)
	(setf two (rbg-string))
	(setf i2 (new-individual 2 two))
	(display i2)
	(setf three (rbg-string))
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
        (push (new-individual (+ i 1) (rbg-string)) individuals)
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

; returns most fit individual in a population
(defmethod most-fit-individual ((l list) &aux max-value max-individual)
	(setf max-value 0)
	(setf max-individual nil)
	(dolist (i l)
		(cond
			(
                (> (funcall *fitness* i) max-value)
				
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
    (setf mutation (mutation (individual-rbg-string i)))
    (make-instance 'individual
        :number (individual-number i)
        :rbg-string mutation
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
    (setf new-i (new-individual (+ 1 (size np)) (individual-rbg-string mm)))
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
        (setf new-i (new-individual (+ 1 (size np)) (individual-rbg-string mm)))
        (setf 
            (population-individuals np)
            (append (population-individuals np) (list new-i))
        )
    )
    nil
)

; performs crossover on two individuals's dna lists
(defmethod crossover ((mother individual) (father individual) &aux mi fi x i)
    (setf mi (individual-rbg-string mother))
    (setf fi (individual-rbg-string father))
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

; Demo
(defmethod ga-text-demo (&aux p)
    (format t "THE WORLD IS RED~%~%")
    (setf *fitness* #'fitness-r)
    (setf p (initial-population))
    (terpri)
    (summarize p)
    (dotimes (i *nr-generations*)
        (setf p (next-generation p))
        (check-average p)
    )
    (terpri)
    (summarize p)
    (format t "THE WORLD IS BLUE~%~%")
    (setf *fitness* #'fitness-b)
    (dotimes (i *nr-generations*)
        (setf p (next-generation p))
        (check-average p)
    )
    (terpri)
    (summarize p)
    (format t "THE WORLD IS GREEN~%~%")
    (setf *fitness* #'fitness-g)
    (dotimes (i *nr-generations*)
        (setf p (next-generation p))
        (check-average p)
    )
    (terpri)
    (summarize p)
)

; mimic the GA
(defmethod ga (&aux p)
    (format t "THE WORLD IS RED~%~%")
    (setf *fitness* #'fitness-r)
    (setf p (initial-population))
    (terpri)
    (summarize p)
    (dotimes (i *nr-generations*)
        (setf p (next-generation p))
        (check-average p)
    )
    (summarize p)
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
