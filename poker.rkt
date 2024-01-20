#lang racket
(require rackunit)

;=======================
; constants

(define deck (foldr append '() 
            (map (lambda (s) 
                (map (lambda (r) (list->string (list r s)))
                    (string->list "23456789TJQKA"))) 
                (string->list "CDHS"))))



;=======================
; functions


(define (deal n_players n_cards)
    ; N N -> [ListOf [ListOf String]]
    ; Creates a series of hands dealt from a single deck
    (local ( 
        (define shuffled (shuffle deck))
        (define (deal-a-round cards p)
            (cond
                [(empty? cards) #f]
                [(= p 0) '()]
                [else (cons (take cards n_cards) 
                            (deal-a-round (drop cards n_cards) (sub1 p)))])))
        ; - IN -
    (deal-a-round shuffled n_players)))


(define (card-rank card)
    """ String -> N
    Returns the equivalent numerical value of a card"""
    (local (
        (define ranks (string->list "__23456789TJQKA"))
        (define my-rank (first (string->list card)))
        (define (iterator remranks n)
        """ [ListOf 1String] -> N
        iterates through the rank list looking for a match and returning the value"""
            (cond
                [(empty? remranks) #f]
                [(equal? my-rank (first remranks)) n]
                [else (iterator (rest remranks) (add1 n))])))
    (iterator ranks 0)))



;======================
; tests

(check-equal? (card-rank "QH") 12 "not equal")
(check-equal? (card-rank "AS") 14 "not equal")


;======================
; action!

(deal 3 5)