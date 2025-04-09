-- Suit, the suit of a card: hearts, spades, diamonds, and clubs.
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

-- Rank, the rank of a card: numeric and its value (2-10), jack, queen, king, and ace.
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Eq, Show)

-- Card, the card itself which has a rank and a suit.
data Card = Card Rank Suit
  deriving (Eq, Show)

-- Hand, the hand of a player which is a list of cards, a type synonym will suffice.
type Hand = [Card]

-- Player, either bank or guest.
data Player = Bank | Guest
  deriving (Eq, Show)

-- Define a function faceCards :: Hand -> Integer that returns the number of face cards in the hand.
faceCards :: Hand -> Integer
faceCards (Card r _ : xs) = if (isInt r) then 0 + faceCards xs else 1 + faceCards xs
faceCards [] = 0

isInt :: Rank -> Bool
isInt (Numeric _) = True
isInt _ = False

-- Define a function value :: Hand -> Integer that calculates the total value of a hand but if the value exceeds 21, turn the hand's aces into 1s instead of 11s.
value :: Hand -> Integer
value hand = cardsValue (notAceValue hand) (aceNumber hand)

notAceValue :: Hand -> Integer
notAceValue (Card (Numeric n) _ : xs) = n + notAceValue xs
notAceValue (Card Ace _ : xs) = notAceValue xs
notAceValue (Card r _ : xs) = 10 + notAceValue xs
notAceValue [] = 0

aceNumber :: Hand -> Integer
aceNumber (Card Ace _ : xs) = 1 + aceNumber xs
aceNumber (Card r _ : xs) = 0 + aceNumber xs
aceNumber [] = 0

cardsValue :: Integer -> Integer -> Integer
cardsValue notAceV aceN = if (aceN > 0) then if (notAceV + 11 + aceN - 1 > 21) then notAceV + aceN else notAceV + 11 + aceN - 1 else notAceV

-- Define a function isBlackjack :: Hand -> Bool that determines whether the hand forms a blackjack. A blackjack is hand with 2 cards that has the value of 21.
isBlackjack :: Hand -> Bool
isBlackjack hand = if (value hand == 21 && length hand == 2) then True else False

-- Define a function gameOver :: Hand -> Bool that checks if the given hand loses (value greater than 21).
gameOver :: Hand -> Bool
gameOver hand = if (value hand > 21) then True else False

-- Define a function winner :: Hand -> Hand -> Player given the guest hand and the bank hand returns the player who won. Tie goes to the bank.
winner :: Hand -> Hand -> Player
winner guest bank = if (value guest > value bank && not (gameOver guest)) then Guest else Bank

-- Define an operator (<+) :: Hand -> Hand -> Hand that places the first hand on top of the other and returns the resulting hand.
(<+) :: Hand -> Hand -> Hand
(<+) guest bank = guest ++ bank

-- Define a function handSuit :: Suit -> Hand that given a suit, returns a hand with all 13 cards of that suit.
handSuit :: Suit -> Hand
handSuit suit = [Card (Numeric n) suit | n <- [2..10]] ++ [Card r suit | r <- [Jack, Queen, King, Ace]]

-- Define a function belongsTo :: Card -> Hand -> Bool that given a card and a hand checks whether the card is in this hand.
belongsTo :: Card -> Hand -> Bool
belongsTo card hand = if (elem card hand) then True else False

-- Define a value fullDeck :: Hand that consists of the 52 card complete deck.
fullDeck :: Hand
fullDeck = [Card (Numeric n) suit | n <- [2..10], suit <- [Hearts, Spades, Diamonds, Clubs]] ++ [Card r suit | r <- [Jack, Queen, King, Ace], suit <- [Hearts, Spades, Diamonds, Clubs]]

-- Define a function draw :: Hand -> Hand -> (Hand, Hand) that given a deck and a hand, draws a card from the deck and returns the remaining deck and the new hand. Throw an error if the deck is empty.
draw :: Hand -> Hand -> (Hand, Hand)
draw [] hand = error "Deck is empty"
draw (d:ds) hand = (ds, hand ++ [d])

-- Define a function playBank :: Hand -> Hand -> Hand that given the deck and the current bank's hand plays a move for the bank. The bank's logic is to draw if the current score is less than 16.
playBank :: Hand -> Hand -> Hand
playBank deck bank = if (value bank < 16) then snd (draw deck bank) else bank

-- test 