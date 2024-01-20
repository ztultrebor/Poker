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
    """ N N -> [ListOf [ListOf String]]
    Creates a series of hands dealt from a single deck"""
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


(define (hand-rank hand)
    """ [ListOf String] -> [ListOf N]
    takes a hand of cards and returns an ordered list of card ranks"""
    (local (
        (define ranks (sort (map (lambda (c) (card-rank c)) hand) >)))
    ; - IN -
        (cond
            [(equal? ranks '(14 5 4 3 2)) '(5 4 3 2 1)]
            [else ranks])))
    

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
                [else (if (= n (count (lambda (r) (= (first this-rank) r)) ranks))
                            (first this-rank)
                            (count-ranks (rest this-rank)))])))
        ; - IN -
        (count-ranks ranks)))


(define (hand-value hand)
    """ [ListOf String -> [Tuple N]
    determines the relative value of a poker hand. 
    Only winning matters, so not a quantitative measure"""
    (local (
            (define suits (map (lambda (c) (second (string->list c))) hand))
            (define ranks (hand-rank hand))
            (define pairs 
                (sort (set->list (list->set (list (kind 2 ranks) (kind 2 (sort ranks <))))) >))
            (define three-kind (kind 3 ranks))
            (define four-kind (kind 4 ranks))
            (define straight (andmap (lambda (r1 r2) (= (- r1 r2) 1))
                                    (reverse (rest (reverse ranks))) (rest ranks)))
            (define flush (= (length (set->list (list->set suits))) 1)))
            ; - IN -
            (cond
                [(and flush straight) (cons 8 ranks)]
                [(true? four-kind) (cons 7 (cons four-kind ranks))]
                [(and (true? three-kind) (true? (first pairs))) (cons 6 (cons three-kind pairs))]
                [flush (cons 5 ranks)]
                [straight (cons 4 ranks)]
                [(true? three-kind) (cons 3 (cons three-kind ranks))]
                [(and (true? (first pairs)) (= (length pairs) 2) (cons 2 (append pairs ranks)))]
                [(true? (first pairs)) (cons 1 (append pairs ranks))]
                [else (cons 0 ranks)])))


(define (true? x)
    """ [Maybe X] -> Boolean
    Like false?, but the opposite"""
    (not (false? x)))



(define (list>? lst1 lst2)
    """[ListOf N] [ListOf N] -> Boolean
    Returns #true if lst1 >= lst2"""
    (cond
        [(empty? lst1) #f]
        [(> (first lst1) (first lst2)) #t]
        [(< (first lst1) (first lst2)) #f]
        [else (list>? (rest lst1) (rest lst2))]))


(define (best-hand hands)
    """ [ListOf [ListOf String]] -> [ListOf String]
    returns the winning hand from a round of poker"""
    (local (
        (define ordered (sort hands #:key hand-value list>?)))
        ; - IN -
        ordered
        #;
        (append (first ordered)
            (if (list>? (second ordered) (first ordered)) (list (second ordered)) '()))))



;======================
; tests

(check-equal? (card-rank "QH") 12 "not equal")
(check-equal? (card-rank "AS") 14 "not equal")
(check-equal? (kind 2 (list 5 14 5 13 4)) 5)
(check-equal? (kind 3 (list 5 14 5 13 5)) 5)
(check-equal? (kind 2 (list 5 14 5 13 5)) #f)
(check-equal? (kind 3 (list 5 14 5 13 4)) #f)
(check-equal? (kind 2 (list 5 14 5 14 5)) 14)
(check-equal? (hand-rank '("KD" "9S" "TD" "TC" "9H")) '(13 10 10 9 9))
(check-equal? (hand-rank '("5H" "2D" "AD" "4S" "3C")) '(5 4 3 2 1))
(check-equal? (hand-value '("KD" "9S" "TD" "TC" "9H")) '(2 10 9 13 10 10 9 9))
(check-equal? (hand-value '("9D" "9S" "TD" "TC" "9H")) '(6 9 10))
(check-equal? (hand-value '("9D" "8S" "7D" "6C" "5H")) '(4 9 8 7 6 5))
(check-equal? (hand-value '("KD" "9D" "2D" "TD" "9D")) '(5 13 10 9 9 2))
(check-equal? (hand-value '("KD" "9S" "TD" "9C" "9H")) '(3 9 13 10 9 9 9))
(check-equal? (hand-value '("9D" "9S" "TD" "9C" "9H")) '(7 9 10 9 9 9 9))
(check-equal? (hand-value '("9D" "8D" "7D" "6D" "5D")) '(8 9 8 7 6 5))
(check-equal? (hand-value '("5H" "2D" "AD" "4S" "3C")) '(4 5 4 3 2 1))
(check-equal? (hand-value '("AS" "6C" "AC" "5H" "3C")) '(1 14 14 14 6 5 3))
(check-equal? (hand-value '("4S" "5D" "AD" "4H" "AH")) '(2 14 4 14 14 5 4 4))

;======================
; action!

(define round (deal 10 5))

(best-hand round)