# Erlang Shenzen IO solitaire solver

A solver for the solitaire mini game in the game "Shenzen IO"

Shenzen IO has a stack-based, card sorting mini-game they simply call "Solitaire". The story is
that the owner's daughter has developed the game.

The game is loosley based on solitaire but has major differences:
- All cards are dealt out onto the gameboard at the start
  - no cycling deck of cards
- Three "swap" spaces for storing single cards
- Three colours instead of four suits in two colours:
  - Red, green, black instead of black aces, black clubs, red diamonds
  - alternating colours can be stacked in numerical order
and red hearts
- Dragons
  - Three sets of four cards that can be removed from the board when they're all showing
  - each set of dragon cards that is removed takes up one of the swap spaces
- Useless "poppy" card that rounds out the total number of cards

## Objective
1. Sort the numbered cards in ascending order by colour
1. "Slay" all dragons into the three swap spaces
1. Put the poppy in the poppy slot

## Gameboard
1. Three swap spaces that will accept any one card or a "slain" dragon
1. One "poppy" space for the useless poppy
1. Three "finish" spaces for stacks of cards in order
1. Eight starting "stack" spaces where the cards are dealt out

## Definitions
1. "Substack": a stack is one or more cards stacked on top of each other with alternating colours and ascending numbers with no gaps: e.g. 4 green on top of 5 black on top of 6 red. A stack has no gaps in numbers, no dragons and no poppy.

## Rules
1. A single card can be moved to one of the three swap spaces if it is empty
1. A single card can be moved from one of the three swap spaces to an empty stack space
1. A single card can be moved from one of the three swap spaces to a valid target stack space
1. Substacks can be moved in the stack spaces onto a blank stack space or onto a valid target stack
1. A valid target stack has a number one higher than the lowest card on the substack and a different colour than the lowest card on the substack
1. Nothing can be stacked on top of a dragon or a poppy.
1. Dragons cannot stack on top of anything
1. Poppies cannot stack on top of anything (but this doesn't matter because the poppy just automatically moves to the poppy space)
1. "1" cards can be moved into the empty "finish" spaces
1. Ascending cards can be moved onto the top of the finished space of the same suit if the card is one higher (i.e. the next card) than the top of the finished stack

## Solution Strategy
I've started with a brute force, depth-first tree search of moves. The moves are tested in order:
1. poppy to the poppy space
1. four matching, face up dragon cards of the same colour to a swap space
1. single card from a swap space to a finished space (e.g. black 2 onto a black 1 in the black finished space)
1. single card from the "top" of a stack to a finished space
1. substack to stack (e.g. black 3, red 2 from any stack onto a red 4 or a green 4)
1. single card from a swap space onto the "top" of a stack
1. single card from the "top" of a stack to a free space

## Solution Details
A state data structure is created to represent the game board with stacks, swap (free) spaces and finished spaces. All valid moves are applied to the starting state to generate a list of new states. Each new state has a history of previous moves. The first resulting state (based on the first move) is then used to generate new states by trying all the moves in order. Cycles are prevented by storing a set of previously seen stacks: we never go back to a previously seen stack.

## Results
It either doesn't work, or doesn't work fast enough. I left it running for one hundred million moves and it still wasn't solved. Either I'm not testing for the solution properly, I'm not tracking cycles properly, the search space is larger than I expect, or ... something else.

## Improvements
I could add advanced moves:
1. Swap two cards that are out of order: e.g. if there is a 7 red, 5 red, 6 green, I could use the swap spaces to hold the 6 green, then 5 red, then put the 6 green back on the 7 red and then the 5 red on the 6 green
1. Take advantage of the three different colours to exchange substacks to free up target stacks: if we have a card that could fit in a stack but the stack is already occupied _and_ if we have somewhere else we could move the cards that are occupying the necessary spot, we can move a substack to free up the space: for example:
Stack 1:
red 6
green 5
Stack 2: 
black 6
swap space:
black 5
We can't simply move "b5" (black 5) from swap to b6, but we _can_ move g5 to b6 and then move b5 to r6. 

