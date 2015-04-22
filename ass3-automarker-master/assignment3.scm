;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Programming Assignment --- Fixing The World    ;;
;; 25/3/15                                                   ;;
;; Muhummad Patel --- PTLMUH006                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)
;; ;TESTS
;; ; (print (= 1 (index '(1 2 3 4 5) 0)) "\n")
;; ; (print (= 4 (index '(1 2 3 4 5) 3)) "\n")
;; ; (print (not (= 1 (index '(1 2 3 4 5) 2))) "\n")
;; ; (print (not (= 0 (index '(1 2 3 4 5) 0))) "\n")

;; ;checks if an item is in a list
;; ;You might want to do a more efficient version of this.
;;
(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)
;; ;TESTS
;; ; (print (in 1 '(1 2 3)) "\n")
;; ; (print (in 2 '(1 2 3)) "\n")
;; ; (print (not (in 4 '(1 2 3))) "\n")
;; ; (print (in '(1 2) '((1 2) (3 4) 5)) "\n")

;; ;helper function for finding the length of a list
(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)
;; ;TESTS
;; ; (print (= 4 (length '(1 2 3 4))) "\n")
;; ; (print (= 1 (length '(1))) "\n")
;; ; (print (= 2 (length '((1 2) (3 4)))) "\n")
;; ; (print (not (= 4 (length '(1 2 3 4 5)))) "\n")
;; ;-----------------------------------------------------------


;---------------------SOLVED STATES & INITIAL STATE------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(  ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)

;initial state of a cube (to make testing easier)
(define initialState '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes
(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0) ;x-axis
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1) ;y-axis
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2) ;z-axis
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 3]
                    [(= orientation 6) 1]
                )
            )
        ]
    )
)
;; ;TESTS
;; ; (print (= 2 (recalculateOrientation 1 0)) "\n")
;; ; (print (= 5 (recalculateOrientation 5 0)) "\n")
;; ; (print (= 1 (recalculateOrientation 1 1)) "\n")
;; ; (print (= 6 (recalculateOrientation 2 1)) "\n")
;; ; (print (= 5 (recalculateOrientation 1 2)) "\n")
;; ; (print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (rotateX ispositive state)
	(if (eq? ispositive #t)
        ;positive rotation required
        (let (  
                ;keep track of the 4 octants being moved
                (octant0 (list-ref (list-ref state 0) 0))
                (orient0 (list-ref (list-ref state 0) 1))
                (octant2 (list-ref (list-ref state 2) 0))
                (orient2 (list-ref (list-ref state 2) 1))
                (octant4 (list-ref (list-ref state 4) 0))
                (orient4 (list-ref (list-ref state 4) 1))
                (octant6 (list-ref (list-ref state 6) 0))
                (orient6 (list-ref (list-ref state 6) 1))
             )
             
            ;build up new state list after rotation
            (list
                (list
                    (list octant4 (recalculateOrientation orient4 0))
                    (list-ref state 1)
                    (list octant0 (recalculateOrientation orient0 0))
                    (list-ref state 3)
                    (list octant6 (recalculateOrientation orient6 0))
                    (list-ref state 5)
                    (list octant2 (recalculateOrientation orient2 0))
                    (list-ref state 7)
                )
                
                (list "x")
            )
        )
        
        ;negative rotation required
	    (list 
	        ;negaative rotation is the same as 3 positive rotations
	        (car (rotateX #t (car (rotateX #t (car (rotateX #t state))))))
	        (list "X")
        )
	)
)
;; ;TESTS
;; ; (print (equal? (rotateX #t initialState) '(((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) ("x"))) "\n")
;; ; (print (equal? (rotateX #f initialState) '(((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3)) ("X"))) "\n")

;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
   	(if (eq? ispositive #t)
        ;positive rotation required
        (let (  
                ;keep track of the 4 octants being moved
                (octant4 (list-ref (list-ref state 4) 0))
                (orient4 (list-ref (list-ref state 4) 1))
                (octant5 (list-ref (list-ref state 5) 0))
                (orient5 (list-ref (list-ref state 5) 1))
                (octant6 (list-ref (list-ref state 6) 0))
                (orient6 (list-ref (list-ref state 6) 1))
                (octant7 (list-ref (list-ref state 7) 0))
                (orient7 (list-ref (list-ref state 7) 1))
             )
             
            ;build up new state list after rotation
            (list
                (list
                    (list-ref state 0)
                    (list-ref state 1)
                    (list-ref state 2)
                    (list-ref state 3)
                    (list octant5 (recalculateOrientation orient5 1))
                    (list octant7 (recalculateOrientation orient7 1))
                    (list octant4 (recalculateOrientation orient4 1))
                    (list octant6 (recalculateOrientation orient6 1))
                )
                
                (list "y")
            )
        )
        
        ;negative rotation required
        (list
            ;negative rotation is the same thing as 3 positive rotations
	        (car (rotateY #t (car (rotateY #t (car (rotateY #t state))))))
	        (list "Y")
        )
	)
)
;; ;TESTS
;; ; (print (equal? (rotateY #t initialState) '(((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3)) ("y"))) "\n")
;; ; (print (equal? (rotateY #f initialState) '(((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3)) ("Y"))) "\n")

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)
	(if (eq? ispositive #t)
        ;positive rotationrequired
        (let (  
                ;keep track of the 4 octants being moved
                (octant0 (list-ref (list-ref state 0) 0))
                (orient0 (list-ref (list-ref state 0) 1))
                (octant1 (list-ref (list-ref state 1) 0))
                (orient1 (list-ref (list-ref state 1) 1))
                (octant4 (list-ref (list-ref state 4) 0))
                (orient4 (list-ref (list-ref state 4) 1))
                (octant5 (list-ref (list-ref state 5) 0))
                (orient5 (list-ref (list-ref state 5) 1))
             )
             
            ;build up new state list after rotation
            (list
                (list
                    (list octant1 (recalculateOrientation orient1 2))
                    (list octant5 (recalculateOrientation orient5 2))
                    (list-ref state 2)
                    (list-ref state 3)
                    (list octant0 (recalculateOrientation orient0 2))
                    (list octant4 (recalculateOrientation orient4 2))
                    (list-ref state 6)
                    (list-ref state 7)
                )
                
                (list "z")
            )
        )
        
        ;negative rotation required
        (list
	        (car (rotateZ #t (car (rotateZ #t (car (rotateZ #t state))))))
	        (list "Z")
	    )
	)
)
;; ;TESTS
;; ; (print (equal? (rotateZ #t initialState) '(((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3)) ("z"))) "\n")
;; ; (print (equal? (rotateZ #t initialState) '(((5 5) (1 6) (3 1) (4 1) (6 5) (2 6) (7 3) (8 3)) ("Z"))) "\n")

;; ;helper for rotate function
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (car (rotateX #t state))]
        [(char=? char #\X) (car (rotateX #f state))]
        [(char=? char #\y) (car (rotateY #t state))]
        [(char=? char #\Y) (car (rotateY #f state))]
        [(char=? char #\z) (car (rotateZ #t state))]
        [(char=? char #\Z) (car (rotateZ #f state))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)
;; ;TESTS
;; ; (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
;; ; (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")
;; ;------------------------------------------------------------

;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
(define (generateSuccessorStates state prevMoves) 
    (list
        (list
            ;generate list of states from every move from current state
            (rotate "x" state)
            (rotate "X" state)
            (rotate "y" state)
            (rotate "Y" state)
            (rotate "z" state)
            (rotate "Z" state)
        )
        
        (list
            ;include the move we just did in the history section of the list
            (append prevMoves '("x"))
            (append prevMoves '("X"))
            (append prevMoves '("y"))
            (append prevMoves '("Y"))
            (append prevMoves '("z"))
            (append prevMoves '("Z"))
        )
    )
)
;; ;TESTS
;; ; (print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;         (list
;; ;             (list
;; ;                 (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;             )
;; ;             '(("x") ("X") ("y") ("Y") ("z") ("Z"))
;; ;         )
;; ;     )
;; ; "\n")


;-----------------------------QUESTION 2.1--------------------------

;finds all the states at a specific depth
(define (genStates n state moves)
    (if (eq? n 0)
    
        ;base case, just return a list of the next possible moves
        (list (list state) (list moves))
        
        ;recursive step. call getStates on all child states
        (let (
                (childStates (generateSuccessorStates state moves))
             )
            
            ;all this down here is just to make sure that the returned list is
            ;formatted the right way.
            (let (
                    (state1 (list-ref (list-ref childStates 0) 0))
                    (moves1 (list-ref (list-ref childStates 1) 0))
                    (state2 (list-ref (list-ref childStates 0) 1))
                    (moves2 (list-ref (list-ref childStates 1) 1))
                    (state3 (list-ref (list-ref childStates 0) 2))
                    (moves3 (list-ref (list-ref childStates 1) 2))
                    (state4 (list-ref (list-ref childStates 0) 3))
                    (moves4 (list-ref (list-ref childStates 1) 3))
                    (state5 (list-ref (list-ref childStates 0) 4))
                    (moves5 (list-ref (list-ref childStates 1) 4))
                    (state6 (list-ref (list-ref childStates 0) 5))
                    (moves6 (list-ref (list-ref childStates 1) 5))
                 )
                 
                 (let (
                        (res1 (genStates (- n 1) state1 moves1))
                        (res2 (genStates (- n 1) state2 moves2))
                        (res3 (genStates (- n 1) state3 moves3))
                        (res4 (genStates (- n 1) state4 moves4))
                        (res5 (genStates (- n 1) state5 moves5))
                        (res6 (genStates (- n 1) state6 moves6))
                      )
                      
                      (list
                        (append
                            (car res1)
                            (car res2)
                            (car res3)
                            (car res4)
                            (car res5)
                            (car res6)
                        )
                        
                        (append
                            (cadr res1)
                            (cadr res2)
                            (cadr res3)
                            (cadr res4)
                            (cadr res5)
                            (cadr res6)
                        )
                      )
                 )
            )
        )
    )
)
;; ;TESTS
;; ; (print (equal? (genStates 0 initialState '()) '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))) "\n")
;; ; (print (equal? (genStates 2 initialState '()) '((((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((5 4) (2 1) (1 2) (4 1) (6 3) (8 3) (7 5) (3 6)) ((5 4) (2 1) (1 2) (4 1) (3 5) (7 6) (8 3) (6 3)) ((2 5) (6 6) (1 2) (4 1) (5 4) (7 4) (3 2) (8 3)) ((7 4) (5 4) (1 2) (4 1) (6 5) (2 6) (3 2) (8 3)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3)) ((3 4) (2 1) (7 2) (4 1) (6 3) (8 3) (1 5) (5 6)) ((3 4) (2 1) (7 2) (4 1) (5 5) (1 6) (8 3) (6 3)) ((2 5) (6 6) (7 2) (4 1) (3 4) (1 4) (5 2) (8 3)) ((1 4) (3 4) (7 2) (4 1) (6 5) (2 6) (5 2) (8 3)) ((6 4) (2 1) (1 2) (4 1) (5 4) (8 3) (3 2) (7 3)) ((3 4) (2 1) (5 2) (4 1) (1 4) (8 3) (6 2) (7 3)) ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((2 5) (8 6) (3 1) (4 1) (1 5) (6 6) (5 3) (7 3)) ((6 5) (1 6) (3 1) (4 1) (8 5) (2 6) (5 3) (7 3)) ((7 4) (2 1) (1 2) (4 1) (8 4) (5 3) (3 2) (6 3)) ((3 4) (2 1) (8 2) (4 1) (1 4) (5 3) (7 2) (6 3)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3)) ((2 5) (5 6) (3 1) (4 1) (1 5) (7 6) (8 3) (6 3)) ((7 5) (1 6) (3 1) (4 1) (5 5) (2 6) (8 3) (6 3)) ((1 5) (6 6) (2 5) (4 1) (7 4) (5 6) (3 2) (8 3)) ((3 4) (6 6) (7 2) (4 1) (2 5) (5 6) (1 5) (8 3)) ((2 5) (6 6) (3 1) (4 1) (5 4) (8 3) (1 2) (7 3)) ((2 5) (6 6) (3 1) (4 1) (7 3) (1 4) (8 3) (5 2)) ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((6 5) (1 6) (5 5) (4 1) (7 4) (2 6) (3 2) (8 3)) ((3 4) (1 6) (7 2) (4 1) (5 5) (2 6) (6 5) (8 3)) ((5 5) (1 6) (3 1) (4 1) (2 4) (8 3) (6 2) (7 3)) ((5 5) (1 6) (3 1) (4 1) (7 3) (6 4) (8 3) (2 2)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)))(("x" "x") ("x" "X") ("x" "y") ("x" "Y") ("x" "z") ("x" "Z") ("X" "x") ("X" "X") ("X" "y") ("X" "Y") ("X" "z") ("X" "Z") ("y" "x") ("y" "X") ("y" "y") ("y" "Y") ("y" "z") ("y" "Z") ("Y" "x") ("Y" "X") ("Y" "y") ("Y" "Y") ("Y" "z") ("Y" "Z") ("z" "x") ("z" "X") ("z" "y") ("z" "Y") ("z" "z") ("z" "Z") ("Z" "x") ("Z" "X") ("Z" "y") ("Z" "Y") ("Z" "z") ("Z" "Z")))) "\n")
;----------------------------------------------------------


;---------------------------QUESTION 3.1-----------------------

;Helper function. Returns the index of the solution in states or -1 if there is none
(define (solvedStateIndex states solved)
    (if (null? states)
        ; if the states list passed in is null, return -1 right away (no solution in the null list)
        -1
        
        ;state list not null, so look for a solution
        (if (not (eq? (member (car states) solved) #f))
            ;first item of states list is solution, so return 0
            0
            
            ;recursive step
            (let (
                    (result (solvedStateIndex (cdr states) solved))
                 )
                 
                ;check if solution was found
                (if (= result -1)
                    ;solution was not found in the rest of the list, so keep sending -1 up the stack
                    -1
                    
                    ;solution was found, so add 1 to the index each time as it unwinds the stack
                    (+ 1 result)
                )
            )
        )
    )
)
;; ;TESTS
;; ; (solvedStateIndex 'a '(a b c d e f g)) == 0
;; ; (solvedStateIndex 'd '(a b c d e f g)) == 3
;; ; (solvedStateIndex '(a) '((a) b c d e f g)) == 0
;; ; (solvedStateIndex '(d) '(a b c (d) e f g)) == 3
;; ; (solvedStateIndex '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (9 3)) ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (9 3) (6 3)) ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (9 3) (5 3)) ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (9 3) (7 3)) ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (9 5) (4 6)) ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (9 5) (2 6)) ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (9 5) (1 6)) ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (9 5) (3 6)) ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (9 5) (7 6)) ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6)) ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (9 5) (6 6)) ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (9 5) (8 6))) solvedStates) == 9
;; ; (solvedStateIndex '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (9 3)) solvedStates) == -1

;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves in reasonable time.
;___________NOTE: ***This function is TAIL RECURSIVE***___________
; ___proof: trace output___
; (trace solveCube)
; (solveCube solvedStates (rotate "xyzXXyZ" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | > (solveCube '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((3 1) (1 1) (4 1) (2 1) (7 3)...
; | ("z" "Y" "x" "x" "Z" "Y" "X")
; ("z" "Y" "x" "x" "Z" "Y" "X") 
(define (solveCube solved initial n)
    (let (
            (statesAtDepth (genStates n initial '()))
         )
         
         (let
            (
                (solutionIndex (solvedStateIndex (list-ref statesAtDepth 0) solvedStates))
                (movesList (list-ref statesAtDepth 1))
            )
            
            ;check if any of the states at this depth are solutions
            (if (not (eq? solutionIndex -1))
                ;we have found a valid solution at solutionIndex
                (list-ref movesList solutionIndex)
                
                ;no Solution at this depth, so check next step
                (solveCube solved initial (+ n 1))
            )
         )
    )
)
;; ;TESTS
;; ; (print (equal? '("Z" "Y" "X") (solveCube solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;; ; (print (equal? '("X") (solveCube solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;---------------------------------------------------------------------
