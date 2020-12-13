-module(solitaire_solver).

-export([solve/1]).
-export([solve_abbrev/1]).
-export([draw_moves/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

% TODO check for duplicate cards!

solve_abbrev(AbbrevCards) ->
    FullCards = [full_card(C) || C <- AbbrevCards],
    maybe_solve(are_cards_valid(FullCards)).

full_card(poppy) -> {poppy};
full_card(dr) -> {dragon, red};
full_card(db) -> {dragon, black};
full_card(dg) -> {dragon, green};
full_card(NumSuit) ->
    [NumChar, SuitChar] = atom_to_list(NumSuit),
    {list_to_integer([NumChar]), suit(SuitChar)}.

suit($r) -> red;
suit($b) -> black;
suit($g) -> green.

solve(Cards) ->
    maybe_solve(are_cards_valid(Cards)).

maybe_solve({errors, Errors}) ->
    io:format("Errors: ~p~n", [Errors]);
maybe_solve(Cards) ->
    Stacks = stacks(Cards),
    Finished = [[] || _ <- lists:seq(1, 4)],
    State =
        #{{free, 1} => empty,
          {free, 2} => empty,
          {free, 3} => empty,
          {finish, red} => [empty],
          {finish, black} => [empty],
          {finish, green} => [empty],
          stacks => Stacks,
          previous_stacks => gb_sets:from_list(Stacks),
          finished => Finished,
          moves => []},
    solve_([State], 5000000).

solve_(States, 0) ->
    NumBestGuesses = 2,
    NumStates = length(States),
    io:format("Ran out of rounds, "
              "fetching ~p best guesses"
              " of ~p remaining states:~n",
              [NumBestGuesses, NumStates]),
    BestStates = lists:sublist(lists:sort(fun num_moves_sort/2, States), 20),
    draw_states(BestStates),

    {best_guesses, reversed_moves_sorted_by_length(States, NumBestGuesses)};
solve_(_NoMoreMoves = [], _Rounds) ->
    io:format("Ran out of states");
solve_([State | Rest] = States, Rounds) ->
    case lists:filter(fun is_solved/1, States) of
        [#{moves := Moves} | _] ->
            lists:reverse(Moves);
        _ ->
            NewStates = move(State),
            %[ct:pal("Moves: ~p~n", [M]) || #{moves := [M | _]} <- NewStates],
            %[ct:pal("Stacks: ~p~n", [S]) || #{stacks := S} <- NewStates],

            % NonFreeMoveStates =
            %     [S || S = #{moves := [{_, '->', Target} | _]} <- NewStates, Target /= free],
            % [begin
            %     AbbrevMoves = lists:map(fun abbrev_move/1, Moves),
            %     AbbrevPrevStacks = lists:map(fun abbrev_stack/1, gb_sets:to_list(PrevStacks)),
            %     io:format("Moves: ~p~n"
            %               "Previous stacks: ~p~n",
            %               [AbbrevMoves, AbbrevPrevStacks]),
            %     draw_moves(NewState)
            %  end || #{moves := Moves,
            %           previous_stacks := PrevStacks} = NewState <- NonFreeMoveStates],


            case Rounds rem 50000 of
                0 ->
                    %io:format("______________~n"),
                    %io:format("______________~n");
                    ok;
                _ ->
                    ok
            end,

            case Rounds rem 50000 of
                0 ->
                    NumStates = length(States),
                    %Gauge = [$| || _ <- lists:seq(1, NumStates div 2)],
                    Gauge = [$| || _ <- lists:seq(1, NumStates)],
                    io:format("~p (~p)~n", [Gauge, NumStates]),
                    ok;
                _ ->
                    ok
            end,

            %LongMoves = lists:filter(fun(#{moves := Moves}) -> length(Moves) > 30 end, NewStates),
            %StateParts = [maps:with([moves], S) || S <- NewStates],
            %case LongMoves of
            %    [] ->
            %        ok;
            %    LongMoves_ ->
            %        StateParts = [maps:with([moves], S) || S <- LongMoves],
            %        ct:pal("~p: Rounds: ~p~nNewStates~n\t~p~n", [?MODULE, Rounds, StateParts])
            %end,
            %ct:pal("~p: NewMoves~n\t~p~n", [?MODULE, NewMoves]),

            solve_(NewStates ++ Rest, Rounds - 1)
    end.

num_moves_sort(#{moves := Moves1}, #{moves := Moves2}) ->
    length(Moves2) < length(Moves1).

reversed_moves_sorted_by_length(States, Num) ->
    %Moves = [lists:reverse(M) || #{moves := M} <- States],
    %lists:sublist(lists:sort(fun sort_by_length/2, Moves), Num).
    Moves = [M || #{moves := M} <- States],
    lists:sublist(lists:sort(fun sort_by_length/2, Moves), Num).

sort_by_length(List1, List2) ->
    length(List1) > length(List2).

stacks([]) ->
    [];
stacks([[_ | _] = Stack | Rest]) ->
    [Stack | stacks(Rest)];
stacks([C1, C2, C3, C4, C5 | Rest]) ->
  [[C1, C2, C3, C4, C5] | stacks(Rest)].

is_solved(#{stacks := Stacks} = _State) ->
    % If all the stacks are empty it implies that we've slain all the dragons
    % FreeCells = [FC || FC = {{free, N}, _} <- maps:to_list(State),
    % AreAllDragonsSlayed = lists:all(fun is_slayed_dragon/1, FreeCells),
    _AreAllStacksEmpty = lists:all(fun is_stack_empty/1, Stacks).
    %AreAllDragonsSlayed and AreAllStacksEmpty.


is_stack_empty([]) ->
    true;
is_stack_empty([_ | _]) ->
    false.

% is_slayed_dragon({slayed_dragon, _}) ->
%     true;
% is_slayed_dragon(_) ->
%     false.

% TODO automatically just delete a visible poppy
move(State) ->
    StackToStack = stack_to_stack_moves(State),
    NewStates =
        [poppy_move(State),
         slay_dragon_moves(State),
         free_to_finish_moves(State),
         cards_to_finish_moves(State),
         StackToStack,
         free_to_stack_moves(State),
         cards_to_free_moves(State)],
    FlattenedNewStates = lists:flatten(NewStates),

    % case FlattenedNewStates of
    %     [] ->
    %         StacksAndMoves = maps:without([previous_stacks], State),
    %         io:format("Ran out of moves:~n~p~n", [StacksAndMoves]);
    %     _ ->
    %         ok
    % end,


    % StateParts = [maps:with([moves], S) || S <- FlattenedNewStates],
    % ct:pal("~p: FlattenedNewStates~n\t~p~n", [?MODULE, StateParts]),

    %StateParts = [maps:with([moves], S) || S <- FlattenedNewStates],

    % StateParts2 = [maps:with([moves], S) || S <- StackToStack],
    % ct:pal("~p: StackToStack~n\t~p~n", [?MODULE, StateParts2]),

    FlattenedNewStates.


stack_to_stack_moves(State) when is_map(State) ->
    MultiSubStacks = multi_substacks(State),
    %io:format("MultiSubStacks~p~n", [MultiSubStacks]),
    lists:flatten([stack_to_stack_moves(MS) || MS <- MultiSubStacks]);

stack_to_stack_moves({SubStacks, OtherStacks, State} = _Substack) ->
    SubStackStates = [{SubStack, OtherStacks, State} || SubStack <- SubStacks],
    lists:map(fun stack_to_stack_moves_/1, SubStackStates).

%% TODO I could return the original stack too, which would make things easier later:
%%      I wouldn't have to find the original stack to remove the substack
%%      from it.
multi_substacks(#{stacks := Stacks} = State) ->
    SubStackFun =
        fun(Stack) ->
                OtherStacks = lists:delete(Stack, Stacks),
                {sub_stacks(Stack), OtherStacks, State}
        end,
    lists:flatten(lists:map(SubStackFun, Stacks)).

stack_to_stack_moves_({{SubStack, RestOfStack}, OtherStacks, State}) ->
    #{previous_stacks := PrevStacks} = State,
    %ct:pal("SubStack: ~p~nRestOfStack: ~p~nOtherStacks: ~p~nState: ~p~n",
           %[SubStack, RestOfStack, OtherStacks, State]),
    NewStates =
        [stack_to_stack_move(SubStack,
                             RestOfStack,
                             OtherStack,
                             _RestOfOther =
                                 lists:delete(OtherStack, OtherStacks),
                             State)
         || OtherStack <- OtherStacks],
    FilterableStates =
        [{PrevStacks, NewSourceStack, State_} || {NewSourceStack, State_} <- NewStates],
    %ct:pal("~p: FilterableStates~n\t~p~n", [?MODULE, FilterableStates]),
    ValidMovesPlusPrevStacks =
        lists:filter(fun is_valid_stack_move/1, FilterableStates),

    %Filterable = [State || {_, _, State} <- FilterableStates],
    %Filtered = lists:subtract(Filterable, ValidMovesPlusPrevStacks),
    %FilteredParts = [maps:with([moves], S) || S <- Filtered],
    %ct:pal("~p: FilteredParts~n\t~p~n", [?MODULE, FilteredParts]),

    %FilterableStateParts = [maps:with([moves], S) || S <- FilterableStates],
    %ct:pal("~p: FilterableStateParts~n\t~p~n", [?MODULE, FilterableStateParts]),

    %Valid = [State || {_, _, State} <- ValidMovesPlusPrevStacks],
    %ValidParts = [maps:with([moves], S) || S <- Valid],
    %ct:pal("~p: RemainingParts~n\t~p~n", [?MODULE, ValidParts]),

    [ValidState || {_, _, ValidState} <- ValidMovesPlusPrevStacks].

stack_to_stack_move(SubStack,
                    NewSourceStack,
                    OtherStack,
                    RestOfStacks,
                    #{moves := Moves,
                      previous_stacks := PrevStacks} = State) ->
    NewTargetStack = SubStack ++ OtherStack,
    NewStacks = [NewSourceStack, NewTargetStack | RestOfStacks],
    PrevStacks1 =
        case {NewSourceStack, NewTargetStack} of
            {[], []} ->
                PrevStacks;
            {SourceStack, []} ->
                gb_sets:add(SourceStack, PrevStacks);
            {[], TargetStack} ->
                gb_sets:add(TargetStack, PrevStacks);
            {SourceStack, TargetStack} ->
                gb_sets:add(SourceStack,
                            gb_sets:add(TargetStack,
                                        PrevStacks))
        end,
    Move =
        case OtherStack of
            [] ->
                {SubStack, '->', _MoveToEmptyStack = []};
            [TopOfTargetStack | _] ->
                {SubStack, '->', TopOfTargetStack}
        end,
    {NewSourceStack,
     State#{moves => [Move | Moves],
            previous_stacks => PrevStacks1,
            stacks => NewStacks}}.

cards_to_free_moves(#{stacks := Stacks} = State) ->
    case [FC || FC = {{free, _}, empty} <- maps:to_list(State)] of
        [FreeCell | _] ->
            OrderedNumberStacks = lists:sort(number_stacks(Stacks)),
            DragonStacks = dragon_stacks(Stacks),
            OrderedStacks = OrderedNumberStacks ++ DragonStacks,
            [card_to_free_move(FreeCell, Stack, S) || S <- OrderedStacks];
        [] ->
            []
    end.

number_stacks(Stacks) ->
    [Stack || Stack = [{I, S} | _] <- Stacks, is_integer(I)].

dragon_stacks(Stacks) ->
    [Stack || Stack = [{dragon, _} | _] <- Stacks].

card_to_free_move({{free, N}, empty},
                   [Card | RestOfStack] = Stack,
                   #{stacks := Stacks,
                     moves := Moves,
                     previous_stacks := PrevStacks} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    NewStacks = [RestOfStack | OtherStacks],
    NewPrevStacks =
        case RestOfStack of
            [] ->
                PrevStacks;
            NotEmpty ->
                gb_sets:add(NotEmpty, PrevStacks)
        end,
    State#{stacks => NewStacks,
           {free, N} => Card,
           moves => [{Card, '->', free} | Moves],
           previous_stacks => NewPrevStacks};
card_to_free_move(_FreeCell, _Stack, _State) ->
    _InvalidMove = [].

free_to_stack_moves(State) ->
    FullFreeCells =
        [FullFreeCell || [FullFreeCell = {free, _}, Card] <- maps:to_list(State), Card /= empty],
    [free_to_stack_moves_(FullFreeCell, State) || FullFreeCell <- FullFreeCells].

free_to_stack_moves_(FreeCell, #{stacks := Stacks} = State) ->
    [free_to_stack_move(FreeCell, Stack, State) || Stack <- Stacks].

free_to_stack_move({{free, Suit}, Card},
                   Stack,
                   #{stacks := Stacks,
                     moves := Moves,
                     previous_stacks := PrevStacks} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    NewStack = [Card | Stack],
    State#{stacks => [NewStack | OtherStacks],
           {free, Suit} => empty,
           moves => [{Card, '->', hd(Stack)} | Moves],
           previous_stacks => gb_sets:add(NewStack, PrevStacks)}.

free_to_finish_moves(State) ->
    FullFreeCells = [Free || Free = {{free, _}, {_, _}} <- maps:to_list(State)],
    FinishedCells = [Finish || Finish = {{finish, _}, _} <- maps:to_list(State)],
    Combos = [{FFC, FC, State} || FFC <- FullFreeCells, FC <- FinishedCells],
    lists:filtermap(fun free_to_finish_move/1, Combos).

free_to_finish_move({{{free, N}, {1, Suit} = Card},
                     {{finish, Suit}, [empty] = Cards},
                     State}) ->
    {true, free_n_to_finish(N, Card, Suit, Cards, State)};
free_to_finish_move({{{free, N}, {NumPlus1, Suit} = Card},
                     {{finish, Suit}, [{Num, _} | _] = Cards},
                     State}) when NumPlus1 == (Num + 1) ->
    {true, free_n_to_finish(N, Card, Suit, Cards, State)};
free_to_finish_move(_) ->
    false.

free_n_to_finish(FreeN, Card, Suit, FinishedCards, State = #{moves := Moves}) ->
     State#{moves => [{Card, '->', finish} | Moves],
            {free, FreeN} => empty,
            {finish, Suit} => [Card | FinishedCards]}.


cards_to_finish_moves(#{stacks := Stacks} = State) ->
    FinishCells = [FC || FC = {{finish, _}, _Cards} <- maps:to_list(State)],
    [card_to_finish_move(FinishCell, Stack, State) || FinishCell <- FinishCells,
                                                      Stack <- Stacks].

card_to_finish_move({{finish, Suit}, [FinCard | _ ] = FinishStack},
                   [{CardNum, Suit} = TopStackCard | RestOfStack] = Stack,
                   #{stacks := Stacks,
                     moves := Moves,
                     previous_stacks := PrevStacks} = State) ->
    case {FinCard, CardNum} of
        {empty, NotOne} when NotOne /= 1 ->
            _InvalidMove = [];
        {{NumFin, _}, NumCard} when NumCard /= (NumFin + 1) ->
            _InvalidMove = [];
        _ ->
            OtherStacks = lists:delete(Stack, Stacks),
            NewStacks = [RestOfStack | OtherStacks],
            NewPrevStacks =
                case RestOfStack of
                    [] ->
                        _DontTrackEmptyLists = PrevStacks;
                    NotEmpty ->
                        gb_sets:add(NotEmpty, PrevStacks)
                end,
            NewFinishStack =
                case FinCard of
                    empty ->
                        [TopStackCard];
                    _ ->
                        [TopStackCard | FinishStack]
                end,
            State#{stacks => NewStacks,
                   {finish, Suit} => NewFinishStack,
                   moves => [{TopStackCard, '->', finish} | Moves],
                   previous_stacks => NewPrevStacks}
    end;
card_to_finish_move(_FreeCell, _Stack, _State) ->
    _InvalidMove = [].

slay_dragon_moves(State) ->
    VisibleDragons = visible_dragons(State),
    [maybe_slay_dragon(VisibleDragon, State) || VisibleDragon <- VisibleDragons].

visible_dragons(#{stacks := Stacks} = State) ->
    FreeCards = [Card || {{free, _}, Card} <- maps:to_list(State), Card /= empty],
    TopCards = [TopCard || Stack = [TopCard | _] <- Stacks, Stack /= []],
    DragonCounts =
        lists:foldl(fun add_dragon/2, #{}, FreeCards ++ TopCards),
    [N || {N, Count} <- maps:to_list(DragonCounts), Count == 4].

maybe_slay_dragon(DragonSuit, State) ->
    HasAvailFreeCell = has_empty_free_cell(State) orelse
                       has_matching_free_cell_dragon(DragonSuit, State),
    case HasAvailFreeCell of
        true ->
            slay_dragon(DragonSuit, State);
        false ->
            _NoFreeCellAvailable = []
    end.

has_empty_free_cell(State) ->
    lists:any(fun is_empty_free_cell/1, maps:to_list(State)).

is_empty_free_cell({{free, _}, empty}) ->
    true;
is_empty_free_cell(_) ->
    false.

has_matching_free_cell_dragon(DragonSuit, State) ->
    0 < length(dragon_free_cells(DragonSuit, State)).

dragon_free_cells(DragonSuit, State) ->
    [FC || FC = {{free, _N}, {dragon, DragonSuit_}} <- maps:to_list(State),
                                                      DragonSuit == DragonSuit_].

add_dragon({dragon, N}, Counts) ->
    CurrentCount = maps:get(N, Counts, 0),
    Counts#{N => CurrentCount + 1};
add_dragon(_NotDragon, Counts) ->
    Counts.

%% TODO add previous stacks
%% ... maybe. I'm not sure if you could go back to a previous
%%     stack by moving a dragon: you couldn't move it _onto_ the
%%     stack, so you can't get back to a previous stack
%%     Oh, but, the new stack with a dragon removed could be
%%     seen twice since it won't have been recorded when it came
%%     into existence
slay_dragon(DragonSuit,
            #{stacks := Stacks,
              moves := Moves,
              previous_stacks := PrevStacks} = State) ->
    {{free, DragonFreeCellNumber}, _} =
        case dragon_free_cells(DragonSuit, State) of
            [] ->
                hd(lists:filter(fun is_empty_free_cell/1, maps:to_list(State)));
            [{{free, _N}, _} = DragonFreeCell | _] ->
                DragonFreeCell
        end,
    OtherDragonFreeCells =
        [FC || FC = {{free, FN},
                     {dragon, DS}} <- maps:to_list(State),
                                     DragonFreeCellNumber /= FN,
                                     DragonSuit == DS],
    StacksWithDragons =
        [S || S = [{dragon, DS} | _] <- Stacks, DragonSuit == DS],
    NewStacks =
        [Rest || [{dragon, DS} | Rest] <- Stacks, DragonSuit == DS, Rest /= []],
    State2 = lists:foldl(fun remove_dragon/2, State, OtherDragonFreeCells ++ StacksWithDragons),
    State2#{{free, DragonFreeCellNumber} => {slayed_dragon, DragonSuit},
            moves => [{slay, DragonSuit} | Moves],
            previous_stacks =>
                gb_sets:union(PrevStacks,
                              gb_sets:from_list(NewStacks))}.

remove_dragon([{dragon, _DragonSuit} | StackTail] = Stack,
              #{stacks := Stacks} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    State#{stacks => [StackTail | OtherStacks]};
remove_dragon({{free, N}, _}, State) ->
    State#{{free, N} => empty}.

poppy_move(#{stacks := Stacks,
             moves := Moves,
             previous_stacks := PrevStacks} = State) ->
    case [S || [{poppy} | _] = S <- Stacks] of
        [[{poppy} | NewStack] = PoppyStack] ->
            Move = {[{poppy}], '->', {poppy}},
            NewPrevStacks =
                case NewStack of
                    [] ->
                        PrevStacks;
                    NotEmpty ->
                        gb_sets:add(NotEmpty, PrevStacks)
                end,
            NewStacks = [NewStack | lists:delete(PoppyStack, Stacks)],
            State#{stacks => NewStacks,
                   moves => [Move | Moves],
                   previous_stacks => NewPrevStacks};
        [] ->
            []
    end.

is_valid_stack_move({_, _, #{moves := [{[_SingleCard], '->', {free, N}}  | _]} = State}) ->
    FreeN = maps:get({free, N}, State),
    _CanMoveSingleCardToFreeCellN = (FreeN == empty);
is_valid_stack_move({_, _, #{moves := [{_MultipleCards, '->', {free, _}}  | _]}}) ->
    _CanMoveStackToFreeCell = false;
is_valid_stack_move({_, _, #{moves := [{[{1, Suit} =_SingleCard],
                                        '->',
                                        {finished, Suit} = Target}  | _]} = State}) ->
    _CanMove1ToFinishedCell =
        _IsEmptyFinishedCell = [] == maps:get(Target, State);
is_valid_stack_move({_, _, #{moves := [{[{SourceNumber, Suit}] = _SingleCard,
                                        '->',
                                        {finished, Suit} = Target} | _]} = State}) ->
    case maps:get(Target, State) of
        [{TargetNumber, _Suit} | _] ->
            _CardIsNextInSuit =
                SourceNumber == TargetNumber + 1;
        _ ->
            false
    end;
is_valid_stack_move({_, _, #{moves := [{[{_, Suit} = _SingleCard],
                                        '->',
                                        {finished, NotSuit}}  | _]} = _State})
        when Suit /= NotSuit ->
    _CanMixSuitsInFinishedPiles = false;
is_valid_stack_move({_, _, #{moves := [{[{dragon, _}] = _SingleCard,
                                        '->',
                                        {finished, _}}  | _]} = _State}) ->
    _CanPutDragonInFinishedPile = false;
is_valid_stack_move({_, _, #{moves := [{[{1, _} | _], '->', {Num, _}}  | _]}})
  when is_integer(Num) ->
    _CanMove1ToStack = false;
is_valid_stack_move({_, _, #{moves := [{[{1, _} | _], '->', free}  | _]}}) ->
    _CanMove1ToFree = false;
is_valid_stack_move(StackMove) ->
    is_valid_stack_to_stack_move(StackMove).

is_valid_stack_to_stack_move({_PrevStacks, _, _State} = StackMove) ->
    Functions =
        [{"not in order", fun is_moved_stack_in_order/1},
         {"not alternating", fun has_alternating_suits/1},
         {"dragon in stack", 'not', fun has_dragon_in_stack/1},
         {"target top dragon", 'not', fun is_target_top_dragon/1},
         {"top not sequential", fun is_target_top_sequential/1},
         {"top same suit", fun is_target_top_different_suit/1},
         {"target top poppy", 'not', fun is_target_top_poppy/1},
         {"poppy in stack", 'not', fun has_poppy_in_stack/1},
         {"backtrack", 'not', fun is_backtracking/1}],

    %#{moves := [Move | _] = Moves} = State,
    %AbbrevMoves = lists:map(fun abbrev_move/1, Moves),
    %AbbrevPrevStacks = lists:map(fun abbrev_stack/1, gb_sets:to_list(PrevStacks)),
    %io:format("Running validators against move: ~p~n"
    %          "All moves: ~p~n"
    %          "Previous stacks: ~p~n",
    %          [Move, AbbrevMoves, AbbrevPrevStacks]),

    IsValid = lists:foldl(fun maybe_run_validator/2, StackMove, Functions),
    case IsValid of
        false ->
            %draw_moves(State),
            false;
        _StackMove ->
            true
    end.

maybe_run_validator({_, _ValidatorFun}, _IsStillValid = false) ->
    false;
maybe_run_validator({_, _, _ValidatorFun}, _IsStillValid = false) ->
    false;
maybe_run_validator({_Name, 'not', ValidatorFun}, StackMove) ->
    case ValidatorFun(StackMove) of
        false ->
            StackMove;
        true ->
            %io:format("IsValid failed at not ~p~n", [Name]),
            false
    end;
maybe_run_validator({_Name, ValidatorFun}, StackMove) ->
    case ValidatorFun(StackMove) of
        true ->
            StackMove;
        false ->
            %io:format("IsValid failed at ~p~n", [Name]),
            false
    end.

has_dragon_in_stack({_, _, State}) ->
    has_card_in_stack(dragon, State).

has_poppy_in_stack({_, _, State}) ->
    has_card_in_stack(poppy, State).

has_card_in_stack(CardType, State) ->
    #{moves := [{MovedStack, '->', _} | _]} = State,
    [] /= [Card || Card <- MovedStack, CardType == element(1, Card)].

is_backtracking({PrevStacks,
                 _NewSourceStack,
                 #{moves := [{MovedStack, '->', _Target} | _] = _Moves,
                   stacks := Stacks} = _State}) ->
    OrigTargetStack = get_orig_target_stack(MovedStack, Stacks),
    UpdatedTargetStack = MovedStack ++ OrigTargetStack,
    %ModifiedStacks = [NewSourceStack, UpdatedTargetStack],
    %IsBacktracking = are_previous_stacks(ModifiedStacks, PrevStacks),
    IsBacktracking = gb_sets:is_member(UpdatedTargetStack, PrevStacks),
    case IsBacktracking of
        true ->
            % AbbrevMoves = lists:map(fun abbrev_move/1, Moves),
            % AbbrevPrevStacks = lists:map(fun abbrev_stack/1, gb_sets:to_list(PrevStacks)),
            % io:format("***** Backtracking *****~n"
            %           "New Source: ~p~n"
            %           "New Target: ~p~n"
            %           "Moves: ~p~n"
            %           "Previous stacks: ~p~n",
            %           [abbrev_stack(NewSourceStack),
            %            abbrev_stack(UpdatedTargetStack),
            %            AbbrevMoves,
            %            AbbrevPrevStacks]),
            % draw_moves(State),
            true;
        false ->
            false
    end.

is_moved_stack_in_order({_, _, State}) ->
    #{moves := [{MovedStack, '->', _} | _]} = State,
    Numbers = [N || {N, _} <- MovedStack, is_integer(N)],
    Numbers == lists:sort(Numbers).

has_alternating_suits({_, _, #{moves := [{MovedStack, _, _} | _]}}) ->
    has_alternating_suits_(MovedStack).

has_alternating_suits_([_]) ->
    true;
has_alternating_suits_([{_, Suit}, {_, Suit} | _]) ->
    false;
has_alternating_suits_([_ | Rest]) ->
    has_alternating_suits_(Rest).

is_target_top_dragon({_, _, State}) ->
    #{moves := [{MovedStack, _, _} | _],
      stacks := Stacks} = State,
    case get_orig_target_stack(MovedStack, Stacks) of
        [{dragon, _} | _] ->
            true;
        _ ->
            false
    end.

is_target_top_poppy({_, _, State}) ->
    #{moves := [{MovedStack, _, _} | _],
      stacks := Stacks} = State,
    case get_orig_target_stack(MovedStack, Stacks) of
        [{poppy} | _] ->
            true;
        _ ->
            false
    end.

is_target_top_sequential({_, _, State}) ->
    #{moves := [{MovedStack, _, _} | _],
      stacks := Stacks} = State,
    TargetStack = get_orig_target_stack(MovedStack, Stacks),
    case {lists:last(MovedStack), TargetStack} of
        {_, []} ->
            true;
        {{Num, _}, [{NumPlusOne, _} | _]} when (Num + 1) == NumPlusOne ->
            true;
        _ ->
            false
    end.

is_target_top_different_suit({_, _, State}) ->
    #{moves := [{MovedStack, _, _} | _],
      stacks := Stacks} = State,
    TargetStack = get_orig_target_stack(MovedStack, Stacks),
    case {lists:last(MovedStack), TargetStack} of
        {_, []} ->
            true;
        {{_, Suit1}, [{_, Suit2} | _]} when Suit1 /= Suit2 ->
            true;
        _ ->
            false
    end.

get_orig_target_stack([X | _] = Addition, [[X | _] = Stack | _Rest]) ->
    lists:subtract(Stack, Addition);
get_orig_target_stack([_ | _] = Addition, [_ | Rest]) ->
    get_orig_target_stack(Addition, Rest).

are_previous_stacks(NewStacks, PrevStacks) ->
    IsPrevStackFun =
        fun(Stack) ->
            gb_sets:is_member(Stack, PrevStacks)
        end,
    lists:any(IsPrevStackFun, NewStacks).

sub_stacks([]) ->
    [];
sub_stacks(Stack) ->
    InitSubStacks = [{Stack, []}],
    SubStacks = sub_stacks(Stack, InitSubStacks, _RestOfStack = []),
    lists:filter(fun is_valid/1, SubStacks).

sub_stacks([_], SubStacks, _) ->
    SubStacks;
sub_stacks(Stack, SubStacks, Rest) ->
    Last = lists:last(Stack),
    AllButLast = lists:droplast(Stack),
    RestOfStack = [Last | Rest],
    sub_stacks(AllButLast, [{AllButLast, RestOfStack} | SubStacks], RestOfStack).

is_valid({[{dragon, _} | _], _}) ->
    false;
is_valid([{poppy} | _]) ->
    false;
is_valid({SubStack, _}) ->
    NumCards = length(SubStack),
    NumNumbers = length([N || {N, _} <- SubStack, is_integer(N)]),
    case NumCards == NumNumbers of
        true ->
            is_in_order(SubStack);
        false ->
            false
    end.

is_in_order(SubStack) ->
    Numbers = [N || {N, _} <- SubStack, is_integer(N)],
    case lists:sort(Numbers) == Numbers of
        true ->
            has_subsequent_numbers(SubStack);
        false ->
            false
    end.

has_subsequent_numbers(SubStack) ->
    has_subsequent_numbers(SubStack, SubStack).

has_subsequent_numbers([_], SubStack) ->
    has_alternating_colours(SubStack);
has_subsequent_numbers([{X, _} | [{Y, _} | _] = Rest], SubStack)
        when (X + 1) == Y ->
    has_subsequent_numbers(Rest, SubStack);
has_subsequent_numbers(_, _) ->
    false.

has_alternating_colours([_]) ->
    true;
has_alternating_colours([{_, C}, {_, C} | _]) ->
    false;
has_alternating_colours([_ | Rest]) ->
    has_alternating_colours(Rest).

are_cards_valid(Cards) ->
    are_cards_valid(Cards, 40).

are_cards_valid(Cards, Count) ->
    MaybeErrors = [are_unique(Cards),
                   correct_number_of_cards(Cards, Count),
                   correct_number_of_dragons(Cards)],
    case lists:flatten(MaybeErrors) of
        [] ->
            Cards;
        Errors ->
            {errors, Errors}
    end.

correct_number_of_cards(Cards, Count) ->
    case length(Cards) of
        X when X < Count ->
            Error = io_lib:format("Only ~p cards, need ~p.", [X, Count]),
            [list_to_binary(Error)];
        X when X > Count ->
            Error = io_lib:format("~p cards is too many, should have ~p.", [X, Count]),
            [list_to_binary(Error)];
        _ ->
            []
    end.

correct_number_of_dragons(Cards) ->
    Suits = [red, green, black],
    Errors = [correct_number_of_dragons(Cards, Suit) || Suit <- Suits],
    lists:flatten(Errors).

correct_number_of_dragons(Cards, Suit) ->
    SuitDragons =
    [Dragon || Dragon = {dragon, Suit_} <- Cards, Suit == Suit_],
    case length(SuitDragons) of
        4 ->
            [];
        X ->
            Error = io_lib:format("Counted ~p ~p dragons, expected 4", [X, Suit]),
            [list_to_binary(Error)]
    end.

are_unique(Cards) ->
    {_, NonDragonCards} = lists:partition(fun is_dragon/1, Cards),
    Sorted = lists:sort(NonDragonCards),
    SortedUnique = lists:usort(NonDragonCards),
    case Sorted == SortedUnique of
        true ->
            [];
        false ->
            Dupes = lists:subtract(Sorted, SortedUnique),
            Error = io_lib:format("Dupe cards: ~p", [Dupes]),
            [list_to_binary(Error)]
    end.

is_dragon({dragon, _}) ->
    true;
is_dragon(_) ->
    false.

draw_states(States) ->
    [begin
        AbbrevMoves = lists:map(fun abbrev_move/1, Moves),
        AbbrevPrevStacks = lists:map(fun abbrev_stack/1, gb_sets:to_list(PrevStacks)),
        io:format("Moves: ~p~n"
                  "Previous stacks: ~p~n",
                  [AbbrevMoves, AbbrevPrevStacks]),
        draw_moves(State)
     end || #{moves := Moves,
              previous_stacks := PrevStacks} = State <- States].

draw_moves(State) ->
    StackLines = stack_lines(State),
    FreeLine = free_line(State),
    FinishedLine = finished_line(State),
    Merged = merge_lines(StackLines, [FreeLine, FinishedLine], []),
    [io:format("~p~n", [iolist_to_binary(Line)]) || Line <- Merged].

merge_lines([], [], Merged) ->
    lists:reverse(Merged);
merge_lines([], [Line2 | Rest2], Merged) ->
    EmptyLine = <<"                                ">>,
    merge_lines([], Rest2, [[EmptyLine, Line2] | Merged]);
merge_lines([Line1 | Rest1], [], Merged) ->
    EmptyLine = <<"                        ">>,
    merge_lines(Rest1, [], [[Line1, EmptyLine] | Merged]);
merge_lines([Line1 | Rest1], [Line2 | Rest2], Merged) ->
    merge_lines(Rest1, Rest2, [[Line1, Line2] | Merged]).

free_line(State) ->
    FreeCells = lists:sort([F || F = {{free, _}, _} <- maps:to_list(State)]),
    [free_cell(FC) || FC <- FreeCells].

free_cell({{free, Num}, empty}) ->
    [<<" ">>, i2b(Num), <<":[  ] ">>];
free_cell({{free, Num}, {I, Suit}}) when is_integer(I), is_atom(Suit) ->
    [<<" ">>, i2b(Num), <<":[">>, i2b(I), a2b(Suit), <<"] ">>];
free_cell({{free, Num}, {Dragon, Suit}}) when is_atom(Dragon), is_atom(Suit) ->
    [<<" ">>, i2b(Num), <<":[">>, a2b(Dragon), a2b(Suit), <<"] ">>].

finished_line(State) ->
    FinishedCells =
        [fin_head(FC) || FC = {{finish, _}, _} <- maps:to_list(State)],
    lists:sort(FinishedCells).

fin_head({{finish, Suit}, [empty]}) ->
    [<<" ">>, a2b(Suit), <<":[  ] ">>];
fin_head({{finish, Suit}, [{Num, _Suit} | _]}) ->
    [<<" ">>, a2b(Suit), <<":[">>, i2b(Num), a2b(Suit), <<"] ">>].

stack_lines(#{stacks := Stacks}) ->
    OrdStacks = lists:sort(fun sort_stacks_by_last/2, Stacks),
    stack_lines(OrdStacks, _Lines = []).

sort_stacks_by_last(Stack1, Stack2) ->
    sort_stacks(lists:reverse(Stack1), lists:reverse(Stack2)).

sort_stacks([], _) ->
    _EmptyStackOnRight = false;
sort_stacks(_, []) ->
    _EmptyStackOnRight = true;
sort_stacks([{X, _} | _], [{Y, _} | _]) when is_integer(X), is_integer(Y) ->
    _NaturalOrderOfInts = X < Y;
sort_stacks([{X, _} | _], _) when is_integer(X) ->
    _IsIntBeforeNonInt = true;
sort_stacks(_, [{Y, _} | _]) when is_integer(Y) ->
    _IsIntBeforeNonInt = false;
sort_stacks([X | _], [Y | _]) ->
    _NaturalOrderOfNonInts = X < Y;
sort_stacks(_, _) ->
    true.

stack_lines(Stacks, Lines) ->
    case length(lists:flatten(Stacks)) == 0 of
        false ->
            {Lasts, Inits} = lists:unzip([chop(Stack) || Stack <- Stacks]),
            BinLasts = iolist_to_binary([Lasts, <<" | ">>]),
            stack_lines(Inits, [BinLasts | Lines]);
        true ->
            lists:reverse(Lines)
    end.

chop([]) ->
    {<<"    ">>, []};
chop(List) ->
    Last = lists:last(List),
    Init = lists:droplast(List),
    BinLast = to_bin(Last),
    {BinLast, Init}.

to_bin({I, A}) when is_integer(I) and is_atom(A) ->
    [i2b(I), a2b(A), <<"  ">>];
to_bin({A1, A2}) when is_atom(A1) and is_atom(A2) ->
    [a2b(A1), a2b(A2), <<"  ">>];
to_bin({poppy}) ->
    <<"pp  ">>.

a2b(A) ->
    [X | _] = atom_to_list(A),
    list_to_binary([X]).

i2b(I) ->
    integer_to_binary(I).

abbrev_move({Num, Suit} = _Card) when is_integer(Num), is_atom(Suit) ->
    abbrev_card({Num, Suit});
abbrev_move({slay, Suit}) when is_atom(Suit) ->
    AtomStr = hd(atom_to_list(Suit)),
    list_to_atom(lists:flatten(["slay_", AtomStr]));
abbrev_move({SingleCard = {_, _}, '->', TargetCard}) ->
    {abbrev_card(SingleCard), '->', abbrev_card(TargetCard)};
abbrev_move({Cards, '->', TargetCard}) when is_list(Cards) ->
    {lists:map(fun abbrev_card/1, Cards),
     '->',
     abbrev_card(TargetCard)}.

abbrev_card({Num, Suit}) when is_integer(Num), is_atom(Suit) ->
    NumStr = integer_to_list(Num),
    AtomStr = hd(atom_to_list(Suit)),
    list_to_atom(lists:flatten([NumStr, AtomStr]));
abbrev_card({dragon, Suit}) when is_atom(Suit) ->
    AtomStr = hd(atom_to_list(Suit)),
    list_to_atom(lists:flatten(["d", AtomStr]));
abbrev_card({poppy}) ->
    pp;
abbrev_card(Atom) when is_atom(Atom) ->
    Atom;
abbrev_card(_EmptyStack = []) ->
    [].

abbrev_stack(Cards) ->
    lists:map(fun abbrev_card/1, Cards).
