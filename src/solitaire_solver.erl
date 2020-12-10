-module(solitaire_solver).

-export([solve/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

% TODO check for duplicate cards!

solve_abbrev(AbbrevCards) ->
    FullCards = [full_card(C) || C <- AbbrevCards],
    solve(FullCards).

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
          previous_stacks => Stacks,
          finished => Finished,
          moves => []},
    solve_([State], 30000).

solve_(States, 0) ->
    NumBestGuesses = 2,
    io:format("Ran out of rounds, fetching ~p best guesses:~n", [NumBestGuesses]),
    {best_guesses, reversed_moves_sorted_by_length(States, NumBestGuesses)};
solve_([State | Rest] = States, Rounds) ->
    case lists:filter(fun is_solved/1, States) of
        [#{moves := Moves} | _] ->
            lists:reverse(Moves);
        _ ->
            NewStates = move(State),

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
         cards_to_finish_moves(State),
         StackToStack,
         free_to_stack_moves(State),
         cards_to_free_moves(State)],
    FlattenedNewStates = lists:flatten(NewStates),

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
        case NewSourceStack of
            [] ->
                [NewTargetStack | PrevStacks];
            _ ->
                [NewSourceStack, NewTargetStack | PrevStacks]
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

remove_substack([X | _] = SubStack, [[X | _] = Stack | _Stacks]) ->
    lists:subtract(Stack, SubStack);
remove_substack(SubStack, [_ | Stacks]) ->
    remove_substack(SubStack, Stacks).

cards_to_free_moves(#{stacks := Stacks} = State) ->
    case [FC || FC = {{free, _}, empty} <- maps:to_list(State)] of
        [EmptyFreeCell | _] ->
            [card_to_free_move(EmptyFreeCell, Stack, State) || Stack <- Stacks];
        [] ->
            []
    end.

card_to_free_move({{free, N}, empty},
                   [Card | RestOfStack] = Stack,
                   #{stacks := Stacks,
                     moves := Moves,
                     previous_stacks := PrevStacks} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    NewStacks = [RestOfStack | OtherStacks],
    State#{stacks => NewStacks,
           {free, N} => Card,
           moves => [{Card, '->', free} | Moves],
           previous_stacks => [RestOfStack | PrevStacks]};
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
           previous_stacks => [NewStack | PrevStacks]}.

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
                   previous_stacks => [RestOfStack | PrevStacks]}
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

maybe_slay_dragon(DragonNum, State) ->
    HasAvailFreeCell = has_empty_free_cell(State) orelse
                       has_matching_free_cell_dragon(DragonNum, State),
    case HasAvailFreeCell of
        true ->
            slay_dragon(DragonNum, State);
        false ->
            _NoFreeCellAvailable = []
    end.

has_empty_free_cell(State) ->
    lists:any(fun is_empty_free_cell/1, maps:to_list(State)).

is_empty_free_cell({{free, _}, empty}) ->
    true;
is_empty_free_cell(_) ->
    false.

has_matching_free_cell_dragon(DragonNum, State) ->
    0 < length(dragon_free_cells(DragonNum, State)).

dragon_free_cells(DragonNum, State) ->
    [FC || FC = {{free, _N}, {dragon, DragonNum_}} <- maps:to_list(State),
                                                      DragonNum == DragonNum_].

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
slay_dragon(DragonNum, #{stacks := Stacks,
                         moves := Moves,
                         previous_stacks := PrevStacks} = State) ->
    {{free, DragonFreeCellNumber}, _} =
        case dragon_free_cells(DragonNum, State) of
            [] ->
                hd(lists:filter(fun is_empty_free_cell/1, maps:to_list(State)));
            [{{free, _N}, _} = DragonFreeCell | _] ->
                DragonFreeCell
        end,
    OtherDragonFreeCells =
        [FC || FC = {{free, FN},
                     {dragon, DN}} <- maps:to_list(State),
                                     DragonFreeCellNumber /= FN,
                                     DragonNum == DN],
    StacksWithDragons =
        [S || S = [{dragon, DN} | _] <- Stacks, DragonNum == DN],
    NewStacks =
        [Rest || [{dragon, DN} | Rest] <- Stacks, DragonNum == DN],
    State2 = lists:foldl(fun remove_dragon/2, State, OtherDragonFreeCells ++ StacksWithDragons),
    State2#{{free, DragonFreeCellNumber} => {slayed_dragon, DragonNum},
            moves => [{slay, DragonNum} | Moves],
            previous_stacks => PrevStacks ++ NewStacks}.

remove_dragon([{dragon, _DragonNum} | StackTail] = Stack,
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
            NewPrevStacks = [NewStack | PrevStacks],
            NewStacks = [NewStack | lists:delete(PoppyStack, Stacks)],
            State#{stacks => NewStacks,
                   moves => [Move | Moves],
                   previous_stacks => NewPrevStacks};
        [] ->
            []
    end.

is_valid_stack_move({_, _, #{moves := [{[_SingleCard], '->', {free, N}}  | _]} = State}) ->
    FreeN = maps:get({free, N}, State),
    _CanMoveSingleCardToFreeCellN =
        FreeN == empty;
is_valid_stack_move({_, _, #{moves := [{_MultipleCards, '->', {free, _}}  | _]}}) ->
    _CanMoveStackToFreeCell =
        false;
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
    _CanMixSuitsInFinishedPiles =
        false;
is_valid_stack_move({_, _, #{moves := [{[{dragon, _}] = _SingleCard,
                                        '->',
                                        {finished, _}}  | _]} = _State}) ->
    _CanPutDragonInFinishedPile =
        false;
is_valid_stack_move({_, _, #{moves := [{MovedStack, '->', _} | _]}} = StackMove) ->
    NumPoppies = length([P || {poppy} = P <- MovedStack]),
    NumDragons = length([D || {dragon, _} = D <- MovedStack]),
    case {NumPoppies, NumDragons} of
        {0, 0} ->
            is_valid_stack_move_(StackMove);
        _ ->
            _CanMoveStacksContainingDragonsOrPoppies =
                false
    end.

is_valid_stack_move_({PrevStacks,
                      NewSourceStack,
                      #{moves := [{MovedStack, '->', Target} | _],
                        stacks := Stacks} = _State}) ->
    OrigTargetStack = get_orig_target_stack(MovedStack, Stacks),
    UpdatedTargetStack = MovedStack ++ OrigTargetStack,
    ModifiedStacks = [NewSourceStack, UpdatedTargetStack],
    IsBackTrackingFun = fun(Stack) -> lists:member(Stack, ModifiedStacks) end,
    IsBacktracking =
        case Target of
            {finish, _} ->
                _IsBacktrackingToFinishACard = false;
            _ ->
                lists:any(IsBackTrackingFun, PrevStacks)
        end,
    Numbers = [N || {N, _Suit} <- MovedStack],
    IsInOrder = (Numbers == lists:sort(Numbers)),
    HasAlternatingSuits = has_alternating_suits(MovedStack),
    {SourceNumber, SourceSuit} = _BottomMoved = lists:last(MovedStack),
    AreStacksCompatible =
        case OrigTargetStack of
            [] ->
                _CanMoveStackIntoEmptyStackSpace =
                    true;
            [{poppy} = _OldTop | _] ->
                _CanMoveStackOntoPoppy =
                    false;
            [{dragon, _} = _OldTop | _] ->
                _CanMoveStackOntoDragon =
                    false;
            [{TargetNumber, TargetSuit} = _OldTop | _] ->
                AreSequential = (SourceNumber + 1 == TargetNumber),
                AreDifferentSuits = (SourceSuit /= TargetSuit),
                AreSequential andalso AreDifferentSuits
        end,

    not IsBacktracking and
        IsInOrder and
        HasAlternatingSuits and
        AreStacksCompatible.

get_orig_target_stack([X | _] = Addition, [[X | _] = Stack | _Rest]) ->
    lists:subtract(Stack, Addition);
get_orig_target_stack([_ | _] = Addition, [_ | Rest]) ->
    get_orig_target_stack(Addition, Rest).

remove_stack([], Remaining) ->
    Remaining;
remove_stack([_ | Rest1], [_ | Rest2]) ->
    remove_stack(Rest1, Rest2).

has_alternating_suits([]) ->
    true;
has_alternating_suits([_]) ->
    true;
has_alternating_suits([{_, Suit}, {_, Suit} | _]) ->
    false;
has_alternating_suits([_ | Rest]) ->
    has_alternating_suits(Rest).

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
    AllButLast = lists:delete(Last, Stack),
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
