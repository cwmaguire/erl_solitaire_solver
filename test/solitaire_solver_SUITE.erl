-module(solitaire_solver_SUITE).

-export([all/0]).
%% I know I could do export_all, but I like this. Sue me. :)
-export([test_is_valid_stack_move/1]).
-export([test_get_orig_target_stack/1]).
-export([test_remove_stack/1]).
-export([test_substacks/1]).
-export([test_has_alternating_suits/1]).

-define(SS, solitaire_solver).

all() ->
    [test_is_valid_stack_move,
     test_get_orig_target_stack,
     test_remove_stack,
     test_has_alternating_suits,
     test_substacks].

test_is_valid_stack_move(_Config) ->
    MoveSingleCardToEmptyFreeCell =
        {[single_card], '->', {free, 1}},
    State1 = #{moves => [MoveSingleCardToEmptyFreeCell],
               {free, 1} => empty},
    true = ?SS:is_valid_stack_move(State1),

    MoveSingleCardToOccupiedFreeCell =
        {[single_card], '->', {free, 1}},
    State2 = #{moves => [MoveSingleCardToOccupiedFreeCell],
               {free, 1} => a_card},
    false = ?SS:is_valid_stack_move(State2),

    MoveMultipleCardsToFreeCell =
        {[card1, card2], '->', {free, 1}},
    State3 = #{moves => [MoveMultipleCardsToFreeCell],
               {free, 1} => empty},
    false = ?SS:is_valid_stack_move(State3),

    Move1ToEmptyFinishedCell = {[{1, red}], '->', {finished, red}},
    State4 = #{moves => [Move1ToEmptyFinishedCell],
               {finished, red} => []},
    true = ?SS:is_valid_stack_move(State4),

    MoveNot1ToEmptyFinishedCell = {[{2, red}], '->', {finished, red}},
    State5 = #{moves => [MoveNot1ToEmptyFinishedCell],
               {finished, red} => []},
    false = ?SS:is_valid_stack_move(State5),

    MoveNextTo1FinishedCell = {[{2, red}], '->', {finished, red}},
    State6 = #{moves => [MoveNextTo1FinishedCell],
               {finished, red} => [{1, red}]},
    true = ?SS:is_valid_stack_move(State6),

    MoveOutOfOrderToFinishedCell = {[{4, red}], '->', {finished, red}},
    State7 = #{moves => [MoveOutOfOrderToFinishedCell],
               {finished, red} => [{2, red}, {1, red}]},
    false = ?SS:is_valid_stack_move(State7),

    MoveWrongSuitToEmptyFinished = {[{1, red}], '->', {finished, green}},
    State8 = #{moves => [MoveWrongSuitToEmptyFinished],
               {finished, red} => []},
    false = ?SS:is_valid_stack_move(State8),

    MoveWrongSuitNextToFinished = {[{2, red}], '->', {finished, green}},
    State9 = #{moves => [MoveWrongSuitNextToFinished],
               {finished, red} => [{1, green}]},
    false = ?SS:is_valid_stack_move(State9),

    MoveDragonToFinished = {[{dragon, 1}], '->', {finished, green}},
    State10 = #{moves => [MoveDragonToFinished],
               {finished, red} => []},
    false = ?SS:is_valid_stack_move(State10),

    RepeatedStack = [{dragon, 1}, {1, red}, {9, green}],
    DummyMove = {[{1, red}], '->', [{2, green}]},
    State11 = #{moves => [DummyMove],
                stacks => [[{1, red}, {9, black}],
                           RepeatedStack],
                previous_stacks => [[{dragon, 1}, {1, red}, {9, green}]]},
    false = ?SS:is_valid_stack_move(State11),

    MoveStackWithDragon = {[{1, green}, {dragon, 1}], '->', [{3, green}]},
    State12 = #{moves => [MoveStackWithDragon],
                stacks => [[{1, green}, {dragon, 1}, {7, green}]],
                previous_stacks => []},
    false = ?SS:is_valid_stack_move(State12),

    MoveOutOfOrderStack = {[{5, green}, {3, red}], '->', [{6, black}]},
    State13 = #{moves => [MoveOutOfOrderStack],
                stacks => [[{5, green}, {3, red}, {8, red}],
                           [{1, black}]],
                previous_stacks => []},
    false = ?SS:is_valid_stack_move(State13),

    MoveNonAlternatingSuitStack = {[{2, green}, {3, green}], '->', [{4, black}]},
    State14 = #{moves => [MoveNonAlternatingSuitStack],
                stacks => [[{5, green}, {3, red}, {8, red}],
                           [{2, green}, {3, green}, {4, black}]],
                previous_stacks => []},
    false = ?SS:is_valid_stack_move(State14),

    MoveStackToDragon = {[{2, green}, {3, green}], '->', [{dragon, 2}]},
    State15 = #{moves => [MoveStackToDragon],
                stacks => [[{5, green}, {3, red}, {8, red}],
                           [{2, green}, {3, green}, {dragon, 2}]],
                previous_stacks => []},
    false = ?SS:is_valid_stack_move(State15),

    MoveStackNonSequentially = {[{2, black}, {3, green}], '->', [{5, red}]},
    State16 = #{moves => [MoveStackNonSequentially],
                stacks => [[{5, green}, {3, red}, {8, red}],
                           [{2, black}, {3, green}, {5, red}]],
                previous_stacks => []},
    false = ?SS:is_valid_stack_move(State16),

    MoveStackOntoMatchingSuit = {[{2, black}, {3, green}], '->', [{4, green}]},
    State17 = #{moves => [MoveStackOntoMatchingSuit],
                stacks => [[{5, green}, {3, red}, {8, red}],
                           [{2, black}, {3, green}, {4, green}]],
                previous_stacks => []},
    false = ?SS:is_valid_stack_move(State17).

test_get_orig_target_stack(_Config) ->
    Stacks = [[1, 2, 3, 4, 5, 6],
              [a, b, c],
              [8, 9, 10],
              [2, 3, 4],
              [1, 2],
              [3, 1, 2]],
    [1, 2, 3, 4, 5, 6] = ?SS:get_orig_target_stack([1, 2, 3], Stacks).

test_remove_stack(_Config) ->
    [] = ?SS:remove_stack([], []),
    [1] = ?SS:remove_stack([], [1]),
    [1, 2] = ?SS:remove_stack([], [1, 2]),
    [2] = ?SS:remove_stack([1], [1, 2]),
    [3, 4] = ?SS:remove_stack([1, 2], [1, 2, 3, 4]).

test_has_alternating_suits(_Config) ->
    true = ?SS:has_alternating_suits([]),
    true = ?SS:has_alternating_suits([{1, black}]),
    true = ?SS:has_alternating_suits([{1, black}, {2, red}]),
    true = ?SS:has_alternating_suits([{1, black}, {2, green}]),
    false = ?SS:has_alternating_suits([{1, black}, {2, black}]),
    true = ?SS:has_alternating_suits([{1, black}, {3, green}, {9, red}]),
    true = ?SS:has_alternating_suits([{1, black}, {3, red}, {9, black}]),
    false = ?SS:has_alternating_suits([{1, black}, {3, red}, {9, red}]).

test_substacks(_Config) ->
    Stack1 = [],
    SubStacks1 = [],
    SubStacks1 = ?SS:sub_stacks(Stack1),

    Stack2 = [1],
    SubStacks2 = [[1]],
    SubStacks2 = ?SS:sub_stacks(Stack2),

    Stack3 = [1, 2],
    SubStacks3 = [[1], [1, 2]],
    SubStacks3 = ?SS:sub_stacks(Stack3).
