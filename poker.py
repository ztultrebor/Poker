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
    pairs = sorted(list(set([kind(2, ranks), kind(2, sorted(ranks))])), reverse=True)
    three_kind = kind(3, ranks)
    four_kind = kind(4, ranks)
    straight = all(map((lambda r1,r2: r1-r2==1), ranks[:-1], ranks[1:]))
    flush = len(set(suits))==1
    return  ((8, *ranks) if (flush and straight) else
            ((7, four_kind, *ranks) if four_kind else
            ((6, three_kind, *pairs) if (three_kind and pairs[0]) else
            ((5, *ranks) if flush else
            ((4, *ranks) if straight else
            ((3, three_kind, *ranks) if three_kind else
            ((2, *pairs, *ranks) if (pairs[0] and len(pairs)==2) else
            ((1, *pairs, *ranks) if pairs[0] else 
            (0, *ranks)))))))))


def best_hand(hands):
    """ [ListOf [ListOf String]] -> [ListOf String]
    returns the winning hand from a round of poker"""
    ordered = sorted(hands, key=hand_value, reverse=True)
    for i in range(len(ordered)-1):
        if hand_value(ordered[i]) > hand_value(ordered[i+1]):
            return ordered[:i+1]
    return ordered



#===========================
# tests

def tests():
    hc = ["AS", "6C", "4C", "5H", "3C"]
    hc2 = ["6C", "4C", "5H", "3C", "AS"]
    hc3 = ["4C", "5H", "3C", "AS", "6C"]
    p = ["AS", "6C", "AC", "5H", "3C"]
    p2 = ["KD", "9S", "TD", "TC", "9H"]
    k3 = ["KD", "9S", "TD", "9C", "9H"]
    s = ["9D", "8S", "7D", "6C", "5H"]
    als = ["5H", "2D", "AD", "4S", "3C"]
    f = ["KD", "9D", "2D", "TD", "9D"]
    fh = ["9D", "9S", "TD", "TC", "9H"]
    k4 = ["9D", "9S", "TD", "9C", "9H"]
    sf = ["9D", "8D", "7D", "6D", "5D"]
    assert (card_rank("QH") == 12)
    assert (card_rank("AS") == 14)
    assert (kind(2, [5, 14, 5, 13, 4]) == 5)
    assert (kind(3, [5, 14, 5, 13, 5]) == 5)
    assert (kind(2, [5, 14, 5, 13, 5]) == None)
    assert (kind(3, [5, 14, 5, 13, 4]) == None)
    assert (kind(2, [5, 14, 5, 14, 5]) == 14)
    assert (hand_rank(p2) == [13, 10, 10, 9, 9])
    assert (hand_rank(als) == [5, 4, 3, 2, 1])
    assert (hand_value(sf) == (8, 9, 8, 7, 6, 5))
    assert (hand_value(k4) == (7, 9, 10, 9, 9, 9, 9))
    assert (hand_value(fh) == (6, 9, 10))
    assert (hand_value(f) == (5, 13, 10, 9, 9, 2))
    assert (hand_value(s) == (4, 9, 8, 7, 6, 5))
    assert (hand_value(als) == (4, 5, 4, 3, 2, 1))
    assert (hand_value(k3) == (3, 9, 13, 10, 9, 9, 9))  
    assert hand_value(p2) == (2, 10, 9, 13, 10, 10, 9, 9)
    assert hand_value(p) == (1, 14, 14, 14, 6, 5, 3)
    assert hand_value(hc) == (0, 14, 6, 5, 4, 3)
    assert best_hand([f, hc, fh, p2]) == [fh]
    assert best_hand([hc, hc2, hc3]) == [hc, hc2, hc3]
    return "tests pass!"
print(tests())



#===========================
# action!

round = deal(10)
print(round)
print(best_hand(round))