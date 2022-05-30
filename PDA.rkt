#lang racket
;PDA

(define-struct State (stack cur_state input))
(define (finalstate? s0 F)
  (cond
    ((null? F) #f)
    ((and (equal? (State-cur_state s0) (car F)) (zero? (string-length (State-input s0)))) #t)
    (else (finalstate? s0 (cdr F)))))

(define (PDAfinalstate? q0 F)
  (cond
    ((null? q0) #f)
    ((finalstate? (car q0) F) #t)
    (else (PDAfinalstate? (cdr q0) F))
    )
  )
(define (push stack element)
  (cons element stack))
  
(define (pop stack)
  (cond ((null? stack)
         null)
        (else
         (cdr stack))))

(define (peek stack)
  (cond ((null? stack)
         null)
        (else (car stack))))
(define (nextStates stack input end next)
  (cond
    ((null? end) next)
    (else (nextStates (make-State stack (car end) input) input (cdr end) (append (cons (make-State stack (car end) input) '()) next)))))

(define (findTransition delta s0 trans_states)
  
  (cond
    ((null? delta) trans_states)
    
    (else
     
    (define stateStack (State-stack s0))
    (define stateState (State-cur_state s0))
    (define stateInp (State-input s0))
    (define trans (car delta))
    (define start_state (car trans))
    (define trans_input (cadr trans))
    (define stackPop (caddr trans))
    (define stackPush (cadddr trans))
    (define next_state (car (cdr (cdr (cdr (cdr trans))))))
     (cond
        ((equal? stateState start_state)
         (cond
        ((and (equal? trans_input "") (equal? stackPop "") (equal? stackPush ""))
        
         (define newState (nextStates stateStack stateInp next_state null))
         (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState)))))

        ((and (not (equal? trans_input ""))  (not (equal? stateInp "")) (equal? stackPop "") (equal? stackPush "") (equal? trans_input (substring (State-input s0) 0 1)))
         
         (define newState (nextStates stateStack (substring stateInp 1) next_state null))
                (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState))))
         )

        ((and (equal? trans_input "") (equal? stackPop "") (not (equal? stackPush "")))
         
         (define newState (nextStates (push stateStack stackPush) stateInp next_state null))
         (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState)))))

        ((and (equal? trans_input "") (not (equal? stackPop "")) (equal? stackPush "") (equal? (peek (State-stack s0)) stackPop))
 
         (define newState (nextStates (pop stateStack) stateInp next_state null))
                (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState)))))

        ((and (not (equal? trans_input "")) (not (equal? stateInp "")) (equal? stackPop "") (not (equal? stackPush "")) (equal? trans_input (substring (State-input s0) 0 1)))
                
                (define newState (nextStates (push stateStack stackPush) (substring stateInp 1) next_state null))
                (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState)))))

        ((and (not (equal? trans_input "")) (not (equal? stateInp ""))(not (equal? stackPop "")) (equal? stackPush "")(equal? trans_input (substring (State-input s0) 0 1)) (equal? (peek (State-stack s0)) stackPop))
         
         (define newState (nextStates (pop stateStack) (substring stateInp 1) next_state null))
                (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState))))
         )

        ((and (equal? trans_input "") (not (equal? stackPop "")) (not (equal? stackPush ""))(equal? (peek (State-stack s0)) stackPop))
        
         (define newState (nextStates (push (pop stateStack) stackPush) stateInp next_state null))
                (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState)))))

        ((and (not (equal? trans_input "")) (not (equal? stateInp "")) (not (equal? stackPop "")) (not (equal? stackPush ""))(equal? trans_input (substring (State-input s0) 0 1)) (equal? (peek (State-stack s0)) stackPop))
         
         (define newState (nextStates (push (pop stateStack) stackPush) (substring stateInp 1) next_state null))
                (cond ((null? trans_states) (findTransition (cdr delta) s0 newState))(else (findTransition (cdr delta) s0 (append trans_states newState))))
         )
        (else
         
         (findTransition (cdr delta) s0 trans_states))))
     (else
      
      (findTransition (cdr delta) s0 trans_states))))))


(define (PDAfindTransition delta s0)
  (cond
    ((null? s0) null)
    (else
     (append (findTransition delta (car s0) '()) (PDAfindTransition delta (cdr s0))))
    ))

(define (sameStates? s0 s1)
  (cond
    ((null? s1) #t)
    (else
     (and (in? (car s1) s0) (sameStates? s0 (cdr s1))))
    )
  )
(define (in? s s0)
  (cond
    ((null? s0) #f)
    (else (or (state-equal? (car s0) s) (in? s (cdr s0))))
    )
  )
(define (state-equal? s0 s1)
  (and (equal? (State-stack s1) (State-stack s0)) (equal? (State-input s1) (State-input s0)) (equal? (State-cur_state s0) (State-cur_state s1)))
 )
(define (allInputempty? s0)
  (cond
    ((null? s0) #f)
    (else (and (equal? "" (State-input (car s0) )) (allInputempty? (cdr s0))))))
                       
(define (PDA input Sigma Gamma S s0 Delta F)
  (define (PDAHelper s0 Delta F)
    (cond
      ((PDAfinalstate? s0 F) #t)
      (else
       (define current_states (PDAfindTransition Delta s0))
       (cond
         ((sameStates? s0 current_states) #f)
         ((allInputempty? current_states) #f)
         (else
          (PDAHelper current_states Delta F))))
      )
    )
       
       
  (define initial_state (make-State '() s0 input))
  (PDAHelper (cons initial_state null) Delta F))





