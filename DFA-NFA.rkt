#lang racket

;DFA
(define (DFA input Sigma S s0 delta F)
  (define (invalidState? state S)
    (cond ((null? S) #t)
          (else
           (cond ((equal? state (car S)) #f)
                 (else (invalidState? state (cdr S)))))))

  (define (nextState current transition transitions S)
    (cond ((or (null? transitions) (invalidState? current S)) null)
          (else
           (cond ((and (equal? current (caar transitions)) (equal? transition (cadar transitions)))
                  (caddar transitions))
                 (else (nextState current transition (cdr transitions) S))))))

  (define (iterate inputList current transitions S)
    (cond ((null? (cdr inputList)) (nextState current (string (car inputList)) transitions S))
          (else (iterate (cdr inputList) (nextState current (string (car inputList)) transitions S) transitions S))))

  (define (finalState? state F)
    (cond ((null? F) #f)
          (else
           (cond ((equal? state (car F)) #t)
                 (else (finalState? state (cdr F)))))))

  (define (letterInAlphabet? Sigma letter)
    (cond ((null? Sigma) #f)
          (else
           (cond ((equal? (car Sigma) letter) #t)
                 (else (letterInAlphabet? (cdr Sigma) letter))))))

  (define (inputInAlphabet? inputList Sigma)
    (cond ((null? inputList) #t)
          (else
           (cond ((letterInAlphabet? Sigma (string (car inputList))) (inputInAlphabet? (cdr inputList) Sigma))
                 (else #f)))))

  (cond ((equal? input "") (finalState? s0 F))
  (else (define inputList (string->list input))
        (define result (iterate inputList s0 delta S))
  (and (not (null? result)) (finalState? result F) (inputInAlphabet? inputList Sigma)))))


;NFA
(define (NFA input Sigma Q q0 Delta F)
  (define (nextState current transition transitions results)
  (cond ((null? transitions) results)
        (else
               (cond ((and (equal? current (caar transitions)) (equal? transition (cadar transitions)))
                      (nextState current transition (cdr transitions) (append-element results (caddar transitions)))
                     )
                     (else (nextState current transition (cdr transitions) results))))))
(define (validState? state Q)
  (cond ((null? Q) #f)
        (else
         (cond ((equal? state (car Q)) #t)
               (else (validState? state (cdr Q)))))))
(define (append-element ls y)
  (cond((null? ls) (cons y null))
    (else
     (cond
       ((null? (cdr ls)) (cons (car ls) (cons y null)))
       (else (cons (car ls) (append-element (cdr ls) y)))))))

(define (multipleNextStates currentStates transition transitions newStates)
  (cond ((null? currentStates) '())
        (else
         (cond ((null? (cdr currentStates)) (append newStates (nextState (car currentStates) transition transitions '())))
               (else (multipleNextStates (cdr currentStates) transition transitions (append newStates (nextState (car currentStates) transition transitions '()))))))))

(define (iterate currentStates directions transitions Q)
  (cond ((null? currentStates) '())
        (else
         (cond ((null? (cdr directions)) (multipleNextStates (removeBadStates currentStates Q '()) (string (car directions)) transitions '()))
               (else (iterate (removeBadStates (multipleNextStates currentStates (string (car directions)) transitions '()) Q '()) (cdr directions) transitions Q))))))




(define (hasFinalState? finishedStates F)
  (cond ((null? (cdr finishedStates)) (finalState? (car finishedStates) F))
        (else
         (cond ((finalState? (car finishedStates) F) #t)
               (else (hasFinalState? (cdr finishedStates) F))))))

(define (finalState? state F)
    (cond ((null? F) #f)
          (else
           (cond ((equal? state (car F)) #t)
                 (else (finalState? state (cdr F)))))))

  (define (letterInAlphabet? Sigma letter)
    (cond ((null? Sigma) #f)
          (else
           (cond ((equal? (car Sigma) letter) #t)
                 (else (letterInAlphabet? (cdr Sigma) letter))))))

  (define (inputInAlphabet? inputList Sigma)
    (cond ((null? inputList) #t)
          (else
           (cond ((letterInAlphabet? Sigma (string (car inputList))) (inputInAlphabet? (cdr inputList) Sigma))
                 (else #f)))))

(define (removeBadStates currentStates Q fixedStates)
  (cond ((null? currentStates) '())
        (else
         (cond ((null? (cdr currentStates))
                (cond ((validState? (car currentStates) Q) (append-element fixedStates (car currentStates)))
                      (else fixedStates)))
               (else
                (cond ((validState? (car currentStates) Q) (removeBadStates (cdr currentStates) Q (append-element fixedStates (car currentStates))))
                      (else (removeBadStates (cdr currentStates) Q fixedStates))))))))
  (cond ((equal? input "") finalState? q0)
  (else (define inputList (string->list input))
        (define endStates (iterate (removeBadStates '(q0) Q '()) inputList Delta Q) )
  (and (not (null? endStates)) (hasFinalState? endStates F) (inputInAlphabet? inputList Sigma)))))


;TEST CASES
(display "\nTEST CASES\n\n")


;Test Cases: DFA
(display "DFA\n")
(DFA "0000" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1)) ;#f
(DFA "0001" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1)) ;#t
(DFA "" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1)) ;#f
(DFA "2" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1)) ;#f
(DFA "10111001101" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1)) ;#t


;Test Cases: NFA
(display "\nNFA\n")
(NFA "aaab" '("a" "b") '(t1 t2 t3 t4 NA) 't1 
    '(
         (t1 "b" (t1)) (t1 "a" (t1 t2)) (t2 "b" (t3)) (t2 "a" (NA)) (t3 "b" (NA))
         (t3 "a" (t4)) (t4 "a" (t4)) (t4 "b" (t4)) (NA "a" (NA)) (NA "b" (NA))
     ) 
    '(t4)) ;#f



