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


(define (kind n ranks)
    """ N [ListOf N] -> N
    determines if there's an n-of-a-kind match and if so returns the rank"""
    (local (
        (define (count-ranks this-rank)
            """ N -> [Maybe N]
            Counts up the number of a given rank in a hand"""
            (cond
                [(empty? this-rank) #f]
                [else (if (= (count (lambda (r) (= (first this-rank) r)) ranks) n)
                            (first this-rank)
                            (count-ranks (rest this-rank)))])))
        ; - IN -
        (count-ranks ranks)))



;======================
; tests

(check-equal? (card-rank "QH") 12 "not equal")
(check-equal? (card-rank "AS") 14 "not equal")
(check-equal? (kind 2 (list 5 14 5 13 4)) 5)
(check-equal? (kind 3 (list 5 14 5 13 5)) 5)
(check-equal? (kind 2 (list 5 14 5 13 5)) #f)
(check-equal? (kind 3 (list 5 14 5 13 4)) #f)
(check-equal? (kind 2 (list 5 14 5 14 5)) 14)


;======================
; action!

(deal 3 5)