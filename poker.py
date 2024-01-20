#lang python
import random


#========================
# constants

deck = ["".join((r,s)) for r in "23456789TJQKA" for s in "CDHS"]



#=========================
# functions

def deal(n_players, n_cards=5):
    """N N -> [ListOf [ListOf String]]
    Creates a series of hands dealt from a single deck"""
    random.shuffle(deck)
    return [[deck[n_cards*j+i]
             for i in range(n_cards)]
             for j in range(n_players)]


def card_rank(card):
    """String -> N
    Returns the equivalent numerical value of a card"""
    return "__23456789TJQKA".index(card[0])



#===========================
# tests

def tests():
    assert (card_rank("QH") == 12)
    assert (card_rank("AS") == 14)
    return "tests pass!"
    
print(tests())

#===========================
# action!

hand = deal(3)
print(hand)