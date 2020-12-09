-module(solitaire_solver_SUITE).

-export([all/0]).
%% I know I could do export_all, but I like this. Sue me. :)
-export([test_solve/1]).
-export([test_is_solved/1]).
-export([test_poppy_move/1]).
-export([test_multi_substacks/1]).
-export([test_stack_to_stack_moves/1]).
-export([test_remove_substack/1]).
-export([test_cards_to_free_moves/1]).
-export([test_slay_dragon_moves/1]).
-export([test_visible_dragons/1]).
-export([test_maybe_slay_dragon/1]).
-export([test_has_empty_free_cell/1]).
-export([test_has_matching_free_cell_dragon/1]).
-export([test_dragon_free_cells/1]).
-export([test_add_dragon/1]).
-export([test_slay_dragon/1]).
-export([test_remove_dragon/1]).
-export([test_is_valid_stack_move/1]).
-export([test_get_orig_target_stack/1]).
-export([test_remove_stack/1]).
-export([test_substacks/1]).
-export([test_has_alternating_suits/1]).

-define(SS, solitaire_solver).
all() ->
    [test_is_solved].

% all() ->
%     [test_solve,
%      test_poppy_move,
%      test_multi_substacks,
%      test_stack_to_stack_moves,
%      test_remove_substack,
%      test_cards_to_free_moves,
%      test_slay_dragon_moves,
%      test_visible_dragons,
%      test_maybe_slay_dragon,
%      test_has_empty_free_cell,
%      test_has_matching_free_cell_dragon,
%      test_dragon_free_cells,
%      test_add_dragon,
%      test_slay_dragon,
%      test_remove_dragon,
%      test_is_valid_stack_move,
%      test_get_orig_target_stack,
%      test_remove_stack,
%      test_has_alternating_suits,
%      test_substacks].

% Saving this hear because I don't want to look it up again
%dbg:tracer(),
%dbg:p(all, call),
%dbg:tpl(solitaire_solver, is_valid_stack_move, [{'_', [], [{return_trace}]}]),
%dbg:tpl(solitaire_solver, stack_to_stack_moves_, [{'_', [], [{return_trace}]}]),

test_solve_mini(_Config) ->
    Cards = [],
    MoveLists = ?SS:solve(Cards),
    ct:pal("~p: MoveLists~n\t~p~n", [?MODULE, MoveLists]).

test_solve(_Config) ->
    Cards =
        [[{4, black},
          {dragon, 1}, % 1 = black
          {dragon, 2}, % 2 = red
          {5, green},
          {2, red}],
         [{dragon, 1},
          {dragon, 2},
          {9, black},
          {1, green},
          {dragon, 3}], % 3 = green
         [{8, green},
          {1, black},
          {dragon, 1},
          {dragon, 3},
          {6, black}],
         [{4, green},
          {7, red},
          {2, green},
          {3, black},
          {4, red}],
         [{dragon, 3},
          {1, red},
          {8, red},
          {3, red},
          {dragon, 2}],
         [{2, black},
          {3, green},
          {6, red},
          {dragon, 3},
          {dragon, 1}],
         [{7, green},
          {dragon, 2},
          {9, red},
          {7, black},
          {6, green}],
         [{5, red},
          {9, green},
          {poppy},
          {5, black},
          {8, black}]],
    MoveLists = ?SS:solve(Cards),
    ct:pal("~p: MoveLists~n\t~p~n", [?MODULE, MoveLists]).

test_is_solved(_Config) ->
    ok.

test_poppy_move(_Config) ->
    NewStacks =
        [[{9, red}],
         [{3, black}, {2, black}]],
    Stacks =
        [[{poppy}, {9, red}],
         [{3, black}, {2, black}]],
    PrevStacks =
        [[{9, red}],
         [{poppy}, {9, red}],
         [{3, black}, {2, black}]],
    Move = {[{poppy}], '->', {poppy}},
    State = #{stacks => Stacks,
              previous_stacks => Stacks,
              moves => []},

    #{stacks := NewStacks,
      moves := [Move],
      previous_stacks := PrevStacks} =
        ?SS:poppy_move(State).

test_multi_substacks(_Config) ->
    Stacks = [[a, b, c],
              [d, e, f],
              [g, h, i]],
	SubStacks =
	    [{[{[a],[b,c]},{[a,b],[c]},{[a,b,c],[]}],
          [[d,e,f],[g,h,i]],
          #{stacks => [[a,b,c],[d,e,f],[g,h,i]]}},
         {[{[d],[e,f]},{[d,e],[f]},{[d,e,f],[]}],
          [[a,b,c],[g,h,i]],
          #{stacks => [[a,b,c],[d,e,f],[g,h,i]]}},
         {[{[g],[h,i]},{[g,h],[i]},{[g,h,i],[]}],
          [[a,b,c],[d,e,f]],
          #{stacks => [[a,b,c],[d,e,f],[g,h,i]]}}],

    State = #{stacks => Stacks},
    X = ?SS:multi_substacks(State),
    ct:pal("~p: X~n\t~p~n", [?MODULE, X]),
    SubStacks = ?SS:multi_substacks(State).

%% TODO test filtering out backtracking moves
test_stack_to_stack_moves(_Config) ->
    Stacks = [[{1, red}, {2, green}, {9, black}],
              [{3, black}, {4, red}, {5, green}],
              [{6, black}, {7, red}, {8, green}]],
    State =
        #{stacks => Stacks,
          moves => [],
          previous_stacks => Stacks},


	[#{moves := [{[{1,red},{2,green}],'->',{3,black}}],
           previous_stacks :=
               [[{9,black}],
                [{1,red},{2,green},{3,black},{4,red},{5,green}],
                [{1,red},{2,green},{9,black}],
                [{3,black},{4,red},{5,green}],
                [{6,black},{7,red},{8,green}]],
           stacks :=
               [[{9,black}],
                [{1,red},{2,green},{3,black},{4,red},{5,green}],
                [{6,black},{7,red},{8,green}]]},
         #{moves := [{[{3,black},{4,red},{5,green}],'->',{6,black}}],
           previous_stacks :=
               [[{3,black},{4,red},{5,green},{6,black},{7,red},{8,green}],
                [{1,red},{2,green},{9,black}],
                [{3,black},{4,red},{5,green}],
                [{6,black},{7,red},{8,green}]],
           stacks :=
               [[],
                [{3,black},{4,red},{5,green},{6,black},{7,red},{8,green}],
                [{1,red},{2,green},{9,black}]]}]
        = ?SS:stack_to_stack_moves(State).


test_remove_substack(_Config) ->
    Stacks = [[{1, red}, {2, green}],
              [{3, black}, {4, red}, {5, green}],
              [{6, black}, {7, red}, {8, green}]],
    [{5, green}] = ?SS:remove_substack([{3, black}, {4, red}], Stacks),
    [{4, red}, {5, green}] = ?SS:remove_substack([{3, black}], Stacks),
    [] = ?SS:remove_substack([{1, red}, {2, green}], Stacks).

test_cards_to_free_moves(_config) ->
    State =
        #{{free, 1} => empty,
          {free, 2} => {1, red},
          {free, 3} => empty,
          stacks => [[{2, black}],
                     [{3, green}, {4, red}]],
          moves => []},

	[#{moves := [{{2, black}, '->', free}],
       stacks := [[], [{3, green}, {4, red}]],
       {free, 1} := {2, black},
       {free, 2} := {1, red},
       {free, 3} := empty},
     #{moves := [{{3, green}, '->', free}],
       stacks := [[{4, red}], [{2, black}]],
       {free, 1} := {3, green},
       {free, 2} := {1, red},
       {free, 3} := empty}] = lists:sort(?SS:cards_to_free_moves(State)).

test_slay_dragon_moves(_Config) ->
    StateWithNoSlayableDragons =
        #{stacks => []},
    [] = ?SS:slay_dragon_moves(StateWithNoSlayableDragons),

    StateWithOneSlayableDragons1 =
        #{{free, 1} => {dragon, 1},
          stacks => [[{dragon, 1}],
                     [{dragon, 1}],
                     [{dragon, 1}]],
          moves => []},
    [#{{free, 1} := {slayed_dragon, 1},
       stacks := [[], [], []],
       moves := [{slay, 1}]}] =
        ?SS:slay_dragon_moves(StateWithOneSlayableDragons1),

    StateWithOneSlayableDragons2 =
        #{{free, 1} => {dragon, 1},
          {free, 2} => {dragon, 1},
          stacks => [[{dragon, 1}],
                     [{dragon, 1}],
                     [{2, red}]],
          moves => []},
    [State = #{{free, 1} := _,
               {free, 2} := __,
               stacks := Stacks,
               moves := [{slay, 1}]}] =
        ?SS:slay_dragon_moves(StateWithOneSlayableDragons2),

    [empty, {slayed_dragon, 1}] =
        lists:sort([V || {{free, _}, V} <- maps:to_list(State)]),
    [[], [], [{2, red}]] = lists:sort(Stacks).

test_visible_dragons(_Config) ->
    State1 =
        #{{free, 1} => {dragon, 1},
          {free, 2} => {dragon, 2},
          {free, 3} => empty,
          stacks => [[{1, red}, {2, green}, {dragon, 1}],
                     [{3, black}, {4, red}, {dragon, 1}],
                     [{dragon, 3}, {dragon, 1}],
                     _EmptyStack = []]},

    [] = ?SS:visible_dragons(State1),

    State2 =
        #{{free, 1} => {dragon, 1},
          {free, 2} => {dragon, 1},
          {free, 3} => empty,
          stacks => [[{dragon, 1}, {1, red}, {2, green}],
                     [{dragon, 1}, {3, black}, {4, red}],
                     [{dragon, 2}],
                     _EmptyStack = []]},

    [1] = ?SS:visible_dragons(State2),

    State3 =
        #{{free, 1} => {dragon, 1},
          {free, 2} => {dragon, 1},
          {free, 3} => {dragon, 2},
          stacks => [[{dragon, 1}, {1, red}, {2, green}],
                     [{dragon, 1}, {3, black}, {4, red}],
                     [{dragon, 2}, {5, gree}, {6, black}],
                     [{dragon, 2}],
                     [{dragon, 2}],
                     _EmptyStack = []]},

    [1, 2] = lists:sort(?SS:visible_dragons(State3)).


test_maybe_slay_dragon(_Config) ->
    StateWithZeroAvailFreeCells
       = #{stacks => [[{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{green, 9}]],
           {free, 1} => {dragon, 3},
           {free, 2} => {1, red},
           {free, 3} => {dragon, 2},
           moves => []},
    [] = ?SS:maybe_slay_dragon(1, StateWithZeroAvailFreeCells),

    StateWithEmptyFreeCell
       = #{stacks => [[{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{green, 9}]],
           {free, 1} => empty,
           {free, 2} => {1, red},
           {free, 3} => {dragon, 2},
           moves => []},
    #{{free, 1} := {slayed_dragon, 1},
      {free, 2} := {1, red},
      {free, 3} := {dragon, 2},
      stacks := Stacks1,
      moves := [{slay, 1}]} =
        ?SS:maybe_slay_dragon(1, StateWithEmptyFreeCell),

    [[], [], [], [], [{dragon, 2}], [{green, 9}]] = lists:sort(Stacks1),

    StateWithFreeCellMatchingDragon
       = #{stacks => [[{1, red}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{9, green}]],
           {free, 1} => {dragon, 1},
           {free, 2} => {1, red},
           {free, 3} => {dragon, 2},
           moves => []},
    #{{free, 1} := {slayed_dragon, 1},
      {free, 2} := {1, red},
      {free, 3} := {dragon, 2},
      stacks := Stacks2,
      moves := [{slay, 1}]} =
        ?SS:maybe_slay_dragon(1, StateWithFreeCellMatchingDragon),

    [[], [], [], [{1, red}], [{9, green}], [{dragon, 2}]] =
        lists:sort(Stacks2).

test_has_empty_free_cell(_Config) ->
    false = ?SS:has_empty_free_cell(#{{free, 1} => {1, red}}),
    false = ?SS:has_empty_free_cell(#{{free, 1} => {1, red},
                                      {free, 2} => {2, black}}),
    true = ?SS:has_empty_free_cell(#{{free, 1} => empty}),
    true = ?SS:has_empty_free_cell(#{{free, 1} => {1, red},
                                     {free, 2} => empty}).

test_has_matching_free_cell_dragon(_Config) ->
    false = ?SS:has_matching_free_cell_dragon(1, #{}),
    true = ?SS:has_matching_free_cell_dragon(1, #{{free, 1} => {dragon, 1}}).

test_dragon_free_cells(_Config) ->
    State = #{{free, 1} => empty,
              {free, 2} => {dragon, 1},
              {free, 3} => {dragon, 2},
              finished => {3, green},
              stacks => [[{1, red}], [{dragon, 3}, {2, black}]],
              moves => [{[{1, green}], '->', [{2, black}]}]},

    [{{free, 3}, {dragon, 2}}] = ?SS:dragon_free_cells(2, State).

test_add_dragon(_Config) ->
    Counts = #{1 => 0, 2 => 1, 3 => 2, foo => bar},
    #{1 := 1} = ?SS:add_dragon({dragon, 1}, Counts),
    #{2 := 2} = ?SS:add_dragon({dragon, 2}, Counts),
    #{3 := 3} = ?SS:add_dragon({dragon, 3}, Counts),
    #{4 := 1} = ?SS:add_dragon({dragon, 4}, Counts),
    #{} = ?SS:add_dragon({1, red}, Counts).

test_slay_dragon(_Config) ->
    StateWithDragonInZeroFreeCells
       = #{stacks => [[{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{green, 9}]],
           {free, 1} => empty,
           {free, 2} => {1, red},
           {free, 3} => {dragon, 2},
           moves => []},
    #{{free, 1} := {slayed_dragon, 1},
      {free, 2} := {1, red},
      {free, 3} := {dragon, 2},
      stacks := Stacks1,
      moves := [{slay, 1}]} =
        ?SS:slay_dragon(1, StateWithDragonInZeroFreeCells),

    [[], [], [], [], [{dragon, 2}], [{green, 9}]] =
        lists:sort(Stacks1),

    StateWithDragonInOneFreeCell
       = #{stacks => [[{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{green, 9}]],
           {free, 1} => {dragon, 1},
           {free, 2} => {1, red},
           {free, 3} => {dragon, 2},
           moves => []},
    #{{free, 1} := {slayed_dragon, 1},
      {free, 2} := {1, red},
      {free, 3} := {dragon, 2},
      stacks := Stacks2,
      moves := [{slay, 1}]} =
        ?SS:slay_dragon(1, StateWithDragonInOneFreeCell),

    [[], [], [], [{dragon, 2}], [{green, 9}]] =
        lists:sort(Stacks2),

    StateWithDragonInTwoFreeCells
       = #{stacks => [[{black, 7}],
                      [{dragon, 1}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{green, 9}]],
           {free, 1} => {dragon, 1},
           {free, 2} => {dragon, 1},
           {free, 3} => {dragon, 2},
           moves => []},
    #{{free, 1} := Free1Value,
      {free, 2} := Free2Value,
      {free, 3} := {dragon, 2},
      stacks := Stacks3,
      moves := [{slay, 1}]} =
        ?SS:slay_dragon(1, StateWithDragonInTwoFreeCells),

    [[], [], [{black, 7}], [{dragon, 2}], [{green, 9}]] =
        lists:sort(Stacks3),

    [empty, {slayed_dragon, 1}] = lists:sort([Free1Value, Free2Value]),

    StateWithDragonInThreeFreeCells
       = #{stacks => [[{black, 7}],
                      [{red, 5}],
                      [{dragon, 1}],
                      [{dragon, 2}],
                      [{green, 9}]],
           {free, 1} => {dragon, 1},
           {free, 2} => {dragon, 1},
           {free, 3} => {dragon, 1},
           moves => []},
    #{{free, 1} := Free1Value,
      {free, 2} := Free2Value,
      {free, 3} := Free3Value,
      stacks := Stacks4,
      moves := [{slay, 1}]} =
        ?SS:slay_dragon(1, StateWithDragonInThreeFreeCells),

    [[], [{black, 7}], [{dragon, 2}], [{green, 9}], [{red, 5}]] =
        lists:sort(Stacks4),

    [empty, empty, {slayed_dragon, 1}] =
        lists:sort([Free1Value, Free2Value, Free3Value]).

test_remove_dragon(_Config) ->
    Stack1 = [{dragon, 1}, {1, red}, {2, green}],
    Stack2 = [{3, black}, {1, red}, {2, green}],
    Stack3 = [{9, red}, {1, red}, {5, black}],

    Stacks = [Stack1, Stack2, Stack3],

    State = #{stacks => Stacks,
              moves => [{slay, 1}]},

    #{stacks := NewStacks} = ?SS:remove_dragon(Stack1, State),
    true = lists:member(tl(Stack1), NewStacks),
    false = lists:member(Stack1, NewStacks),
    [] = [D || D = {dragon, _} <- lists:flatten(NewStacks)],
    true = lists:member(Stack2, NewStacks),
    true = lists:member(Stack3, NewStacks),
    3 == length(NewStacks).

test_is_valid_stack_move(_Config) ->
    MoveSingleCardToEmptyFreeCell =
        {[single_card], '->', {free, 1}},
    State1 = #{moves => [MoveSingleCardToEmptyFreeCell],
               {free, 1} => empty},
    true = ?SS:is_valid_stack_move({x, x, State1}),

    MoveSingleCardToOccupiedFreeCell =
        {[single_card], '->', {free, 1}},
    State2 = #{moves => [MoveSingleCardToOccupiedFreeCell],
               {free, 1} => a_card},
    false = ?SS:is_valid_stack_move({x, x, State2}),

    MoveMultipleCardsToFreeCell =
        {[card1, card2], '->', {free, 1}},
    State3 = #{moves => [MoveMultipleCardsToFreeCell],
               {free, 1} => empty},
    false = ?SS:is_valid_stack_move({x, x, State3}),

    Move1ToEmptyFinishedCell = {[{1, red}], '->', {finished, red}},
    State4 = #{moves => [Move1ToEmptyFinishedCell],
               {finished, red} => []},
    true = ?SS:is_valid_stack_move({x, x, State4}),

    MoveNot1ToEmptyFinishedCell = {[{2, red}], '->', {finished, red}},
    State5 = #{moves => [MoveNot1ToEmptyFinishedCell],
               {finished, red} => []},
    false = ?SS:is_valid_stack_move({x, x, State5}),

    MoveNextTo1FinishedCell = {[{2, red}], '->', {finished, red}},
    State6 = #{moves => [MoveNextTo1FinishedCell],
               {finished, red} => [{1, red}]},
    true = ?SS:is_valid_stack_move({x, x, State6}),

    MoveOutOfOrderToFinishedCell = {[{4, red}], '->', {finished, red}},
    State7 = #{moves => [MoveOutOfOrderToFinishedCell],
               {finished, red} => [{2, red}, {1, red}]},
    false = ?SS:is_valid_stack_move({x, x, State7}),

    MoveWrongSuitToEmptyFinished = {[{1, red}], '->', {finished, green}},
    State8 = #{moves => [MoveWrongSuitToEmptyFinished],
               {finished, red} => []},
    false = ?SS:is_valid_stack_move({x, x, State8}),

    MoveWrongSuitNextToFinished = {[{2, red}], '->', {finished, green}},
    State9 = #{moves => [MoveWrongSuitNextToFinished],
               {finished, red} => [{1, green}]},
    false = ?SS:is_valid_stack_move({x, x, State9}),

    MoveDragonToFinished = {[{dragon, 1}], '->', {finished, green}},
    State10 = #{moves => [MoveDragonToFinished],
               {finished, red} => []},
    false = ?SS:is_valid_stack_move({x, x, State10}),

    NewBacktrackingSourceStack =
        [{1, red}, {3, black}, {9, green}],
    NewTargetStack11 =
        [{dragon, 1}, {4, red}, {9, black}],
    NewStacks11 =
        [NewTargetStack11, NewBacktrackingSourceStack],
    PrevStacks11 = [NewBacktrackingSourceStack],
    Move11 = {[{1, red}], '->', {3, black}},
    State11 = #{moves => [Move11],
                stacks => NewStacks11,
                previous_stacks => NewStacks11 ++ PrevStacks11},
    false = ?SS:is_valid_stack_move({PrevStacks11,
                                     NewBacktrackingSourceStack,
                                     State11}),

    NewSourceStack12 =
        [{dragon, 1}, {4, red}, {9, black}],
    NewBacktrackingTargetStack =
        [{1, red}, {3, black}, {9, green}],
    NewStacks12 =
        [NewSourceStack12, NewBacktrackingTargetStack],
    PrevStacks12 = [NewBacktrackingTargetStack],
    Move12 = {[{1, red}], '->', {3, black}},
    State12 = #{moves => [Move12],
                stacks => NewStacks12,
                previous_stacks => NewStacks12 ++ PrevStacks12},
    false = ?SS:is_valid_stack_move({PrevStacks12,
                                     NewBacktrackingSourceStack,
                                     State12}),

    NewSourceStack13 =
        [{dragon, 2}],
    NewStacks13 =
        [[{1, green}, {dragon, 1}, {3, green}],
         [{dragon, 2}]],
    NewSourceStack13 =
        [{dragon, 2}],
    PrevStacks13 =
        [[{3, green}],
         [{1, green}, {dragon, 1}, {dragon, 2}]],
    MoveStackWithDragon =
        {[{1, green}, {dragon, 1}], '->', [{3, green}]},
    State13 = #{moves => [MoveStackWithDragon],
                stacks => NewStacks13,
                previous_stacks => PrevStacks13},
    false = ?SS:is_valid_stack_move({PrevStacks13,
                                     NewSourceStack13,
                                     State13}),

    NewSourceStack14 = [{1, black}],
    NewStacks14 =
        [[{5, green}, {3, red}, {6, black}],
         NewSourceStack14],
    PrevStacks14 =
        [[{6, black}],
         [{5, green}, {3, red}, {1, black}]],
    MoveOutOfOrderStack =
        {[{5, green}, {3, red}], '->', [{6, black}]},
    State14 = #{moves => [MoveOutOfOrderStack],
                stacks => NewStacks14,
                previous_stacks => NewStacks14 ++ PrevStacks14},
    false = ?SS:is_valid_stack_move({PrevStacks14,
                                     NewSourceStack14,
                                     State14}),

    NewSourceStack15 = [{2, green}, {3, green}, {4, black}],
    NewStacks15 =
        [[{5, green}, {3, red}, {8, red}],
         NewSourceStack15],
    PrevStacks15 =
        [[{2, green}, {3, green}, {5, green}, {3, red}, {8, red}],
         [{4, black}]],
    MoveNonAlternatingSuitStack =
        {[{2, green}, {3, green}], '->', [{4, black}]},
    State15 = #{moves => [MoveNonAlternatingSuitStack],
                stacks => NewStacks15,
                previous_stacks => NewStacks15 ++ PrevStacks15},
    false = ?SS:is_valid_stack_move({PrevStacks15,
                                     NewSourceStack15,
                                     State15}),

    NewSourceStack16 =
        [{2, green}, {3, black}, {dragon, 2}],
    NewStacks16 =
        [[{5, green}, {3, red}, {8, red}],
         NewSourceStack16],
    PrevStacks16 =
        [[{2, green}, {3, black}, {5, green}, {3, red}, {8, red}],
         [{dragon, 2}]],
    MoveStackToDragon =
        {[{2, green}, {3, black}], '->', [{dragon, 2}]},
    State16 = #{moves => [MoveStackToDragon],
                stacks => NewStacks16,
                previous_stacks => NewStacks16 ++ PrevStacks16},
    false = ?SS:is_valid_stack_move({PrevStacks16,
                                     NewSourceStack16,
                                     State16}),

    NewSourceStack17 =
        [{2, black}, {3, green}, {5, red}],
    NewStacks17 =
        [[{5, green}, {3, red}, {8, red}],
         NewSourceStack17],
    PrevStacks17 =
        [[{2, black}, {3, green}, {5, green}, {3, red}, {8, red}],
         [{5, red}]],
    MoveStackNonSequentially =
        {[{2, black}, {3, green}], '->', [{5, red}]},
    State17 = #{moves => [MoveStackNonSequentially],
                stacks => NewStacks17,
                previous_stacks => NewStacks17 ++ PrevStacks17},
    false = ?SS:is_valid_stack_move({PrevStacks17,
                                     NewSourceStack17,
                                     State17}),

    NewSourceStack18 =
        [{2, black}, {3, green}, {4, green}],
    NewStacks18 =
        [[{5, green}, {3, red}, {8, red}],
         NewSourceStack18],
    PrevStacks18 =
        [[{2, black}, {3, green}, {5, green}, {3, red}, {8, red}],
         [{4, green}]],
    MoveStackOntoMatchingSuit =
        {[{2, black}, {3, green}], '->', [{4, green}]},
    State18 = #{moves => [MoveStackOntoMatchingSuit],
                stacks => NewStacks18,
                previous_stacks => NewStacks18 ++ PrevStacks18},
    false = ?SS:is_valid_stack_move({PrevStacks18,
                                     NewSourceStack18,
                                     State18}).

test_get_orig_target_stack(_Config) ->
    Stacks = [[1, 2, 3, 4, 5, 6],
              [a, b, c],
              [8, 9, 10],
              [2, 3, 4],
              [1, 2],
              [3, 1, 2]],
    MovedStack = [1, 2, 3],
    [4, 5, 6] = ?SS:get_orig_target_stack(MovedStack, Stacks).

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
    SubStacks2 = [{[1], []}],
    SubStacks2 = ?SS:sub_stacks(Stack2),

    Stack3 = [1, 2],
    SubStacks3 = [{[1], [2]}, {[1, 2], []}],
    SubStacks3 = ?SS:sub_stacks(Stack3).
