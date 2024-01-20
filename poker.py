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


def hand_rank(hand):
    """ [ListOf String] -> [ListOf N]
    takes a hand of cards and returns an ordered list of card ranks"""
    ranks =  sorted([card_rank(c[0]) for c in hand], reverse=True)
    if ranks == [14, 5, 4, 3, 2]:
        return [5, 4, 3, 2, 1]
    else:
        return ranks


def card_rank(card):
    """ String -> N
    Returns the equivalent numerical value of a card"""
    return "__23456789TJQKA".index(card[0])


def kind(n, ranks):
    """ N [ListOf N] -> N
    determines if there's an n-of-a-kind match and if so returns the rank"""
    for r in ranks:
        if ranks.count(r) == n: return r
    return None


def hand_value(hand):
    """ [ListOf String -> [Tuple N]
    determines the relative value of a poker hand. 
    Only winning matters, so not a quantitative measure"""
    suits = [c[1] for c in hand]
    ranks = hand_rank(hand)
    pair = kind(2, ranks)
    pair2 = kind(2, sorted(ranks))
    three_kind = kind(3, ranks)
    four_kind = kind(4, ranks)
    straight = all(map((lambda r1,r2: r1-r2==1), ranks[:-1], ranks[1:]))
    flush = len(set(suits))==1
    return  ((8, *ranks) if (flush and straight) else
            ((7, four_kind, *ranks) if four_kind else
            ((6, three_kind, pair) if (three_kind and pair) else
            ((5, *ranks) if flush else
            ((4, *ranks) if straight else
            ((3, three_kind, *ranks) if three_kind else
            ((2, pair, pair2, *ranks) if (pair and pair2) else
            ((1, pair, *ranks) if pair else 
            (0, *ranks)))))))))



#===========================
# tests

def tests():
    assert (card_rank("QH") == 12)
    assert (card_rank("AS") == 14)
    assert (kind(2, [5, 14, 5, 13, 4]) == 5)
    assert (kind(3, [5, 14, 5, 13, 5]) == 5)
    assert (kind(2, [5, 14, 5, 13, 5]) == None)
    assert (kind(3, [5, 14, 5, 13, 4]) == None)
    assert (kind(2, [5, 14, 5, 14, 5]) == 14)
    assert (hand_rank(['KD', '9S', 'TD', 'TC', '9H']) == [13, 10, 10, 9, 9])
    assert (hand_rank(['5H', '2D', 'AD', '4S', '3C']) == [5, 4, 3, 2, 1])
    assert (hand_value(['KD', '9S', 'TD', 'TC', '9H']) == (2, 10, 9, 13, 10, 10, 9, 9))
    assert (hand_value(['9D', '9S', 'TD', 'TC', '9H']) == (6, 9, 10))
    assert (hand_value(['9D', '8S', '7D', '6C', '5H']) == (4, 9, 8, 7, 6, 5))
    assert (hand_value(['KD', '9D', '2D', 'TD', '9D']) == (5, 13, 10, 9, 9, 2))
    assert (hand_value(['KD', '9S', 'TD', '9C', '9H']) == (3, 9, 13, 10, 9, 9, 9))  
    assert (hand_value(['9D', '9S', 'TD', '9C', '9H']) == (7, 9, 10, 9, 9, 9, 9))  
    assert (hand_value(['9D', '8D', '7D', '6D', '5D']) == (8, 9, 8, 7, 6, 5))
    assert (hand_value(['5H', '2D', 'AD', '4S', '3C']) == (4, 5, 4, 3, 2, 1))
    return "tests pass!"
print(tests())



#===========================
# action!

hand = deal(3)
print(hand)