-module(solitaire_solver).

-export([solve/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

solve(Cards) ->
  Stacks = stacks(Cards),
  Finished = [[] || _ <- lists:seq(1, 4)],
  State = #{free => [],
            stacks => Stacks,
            previous_stacks => Stacks,
            finished => Finished},
  solve_([State]).

solve_([State | Rest] = States) ->
    case lists:filter(fun is_solved/1, States) of
        [{solved, _Moves} | _] = Solutions ->
            Solutions;
        _ ->
            solve_(Rest ++ move(State))
    end.

stacks([]) ->
    [];
stacks([[C1, C2, C3, C4, C5] | Rest]) ->
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

move(State) ->
    NewStates =
        [stack_to_stack_moves(State),
         cards_to_free_moves(State),
         free_to_stack_moves(State),
         cards_to_finish_moves(State),
         slay_dragon_moves(State),
         rose_moves(State)],
    lists:flatten(NewStates).


stack_to_stack_moves(State) when is_map(State) ->
    SubstackMoves = lists:flatten(substack_move_list_lists(State)),
    lists:flatten([stack_to_stack_moves(List) || List <- SubstackMoves]);

stack_to_stack_moves({SubStacks, OtherStacks, State}) ->
    SubStackStates = [{SubStack, OtherStacks, State} || SubStack <- SubStacks],
    lists:map(fun stack_to_stack_moves_/1, SubStackStates).

stack_to_stack_moves_({SubStack, OtherStacks, State}) ->
    #{previous_stacks := PrevStacks} = State,
    NewStates =
        [stack_to_stack_move(SubStack,
                             OtherStack,
                             _RestOfOther =
                                 lists:delete(OtherStack, OtherStacks),
                             State)
         || OtherStack <- OtherStacks],
    FilterableStates =
        [{PrevStacks, NewSourceStack, State} || {NewSourceStack, State} <- NewStates],
    ValidMovesPlusPrevStacks =
        lists:filter(fun is_valid_stack_move/1, FilterableStates),
    [ValidState || {_, _, ValidState} <- ValidMovesPlusPrevStacks].


%% TODO I could return the original stack too, which would make things easier later:
%%      I wouldn't have to find the original stack to remove the substack
%%      from it. I could also pass in the old stack minus the sub stack
substack_move_list_lists(#{stacks := Stacks} = State) ->
    SubStackFun =
        fun(Stack) ->
                OtherStacks = lists:delete(Stack, Stacks),
                {sub_stacks(Stack), OtherStacks, State}
        end,
    lists:map(SubStackFun, Stacks).

stack_to_stack_move(SubStack,
                    OtherStack,
                    RestOfStacks,
                    #{stacks := Stacks,
                      moves := Moves,
                      previous_stacks := PrevStacks} = State) ->
    NewSourceStack = remove_substack(SubStack, Stacks),
    NewTargetStack = SubStack ++ OtherStack,
    NewStacks = [NewSourceStack, NewTargetStack | RestOfStacks],
    PrevStacks1 =
        case NewSourceStack of
            [] ->
                [NewTargetStack | PrevStacks];
            _ ->
                [NewSourceStack, NewTargetStack | PrevStacks]
        end,
    Move = {SubStack, '->', hd(OtherStack)},
    {NewSourceStack,
     State#{moves => [Move | Moves],
            previous_stacks => PrevStacks1,
            stacks => NewStacks}}.

remove_substack([X | _] = SubStack, [[X | _] = Stack | _Stacks]) ->
    lists:subtract(Stack, SubStack);
remove_substack(SubStack, [_ | Stacks]) ->
    remove_substack(SubStack, Stacks).

cards_to_free_moves(#{stacks := Stacks} = State) ->
    [EmptyFreeCell | _] = [FC || FC = {{free, _}, empty} <- maps:to_list(State)],
    [card_to_free_move(EmptyFreeCell, Stack, State) || Stack <- Stacks].

card_to_free_move({{free, N}, empty},
                   [Card | RestOfStack] = Stack,
                   #{stacks := Stacks,
                     moves := Moves} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    NewStacks = [RestOfStack | OtherStacks],
    State#{stacks => NewStacks,
           {free, N} => Card,
           moves => [{Card, '->', free} | Moves]};
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
                     moves := Moves} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    NewStack = [Card | Stack],
    State#{stacks => [NewStack | OtherStacks],
           {free, Suit} => empty,
           moves => [{Card, '->', hd(Stack)} | Moves]}.

cards_to_finish_moves(#{stacks := Stacks} = State) ->
    FinishCells = [FC || FC = {{finish, _}, _Cards} <- maps:to_list(State)],
    [card_to_finish_move(FinishCell, Stack, State) || FinishCell <- FinishCells,
                                                    Stack <- Stacks].

card_to_finish_move({{finish, Suit},
                     [{FinNum, Suit} | _ ] = FinishStack},
                   [{CardNum, Suit} = TopStackCard | RestOfStack] = Stack,
                   #{stacks := Stacks,
                     moves := Moves} = State)
        when FinNum == empty, CardNum == 1;
             CardNum == FinNum + 1 ->
    OtherStacks = lists:delete(Stack, Stacks),
    NewStacks = [RestOfStack | OtherStacks],
    NewFinishStack = [TopStackCard | FinishStack],
    State#{stacks => NewStacks,
           {finish, Suit} => NewFinishStack,
           moves => [{TopStackCard, '->', finish} | Moves]};
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
                         moves := Moves} = State) ->
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
    State2 = lists:foldl(fun remove_dragon/2, State, OtherDragonFreeCells ++ StacksWithDragons),
    State2#{{free, DragonFreeCellNumber} => {slayed_dragon, DragonNum},
            moves => [{slay, DragonNum} | Moves]}.

remove_dragon([{dragon, _DragonNum} | StackTail] = Stack,
              #{stacks := Stacks} = State) ->
    OtherStacks = lists:delete(Stack, Stacks),
    State#{stacks => [StackTail | OtherStacks]};
remove_dragon({{free, N}, _}, State) ->
    State#{{free, N} => empty}.

rose_moves(#{stacks := Stacks,
             moves := Moves,
             previous_stacks := PrevStacks} = State) ->
    IsRoseVisible =
        lists:any(fun is_rose_visible/1, Stacks),
    case IsRoseVisible of
        true ->
            [maybe_rose_move_to_empty(State),
             maybe_rose_move_to_free(State)];
        false ->
            []
    end.

maybe_rose_move_to_empty(#{stacks := Stacks} = State) ->
    HasEmptyStack = (0 < length([[] || [] <- Stacks])),
    case HasEmptyStack of
        true ->
            rose_move_to_empty(State);
        false ->
            []
    end.

rose_move_to_empty(#{stacks := Stacks,
                     moves := Moves,
                     previous_stacks := PrevStacks} = State) ->
    HasEmptyStack = (0 < length([[] || [] <- Stacks])),
                [_ | NewStack] =
                    RoseStack =
                        [S || [{rose} | _] = S <- Stacks],
                Move = {[{rose}], '->', []},
                NewPrevStacks = [NewStack | PrevStacks],
                NewStacks = [[{rose}],
                             NewStack |
                                 lists:delete(RoseStack,
                                              lists:delete([],
                                                           Stacks))],
                #{stacks => NewStacks,
                  moves => [Move | Moves],
                  previous_stacks => NewPrevStacks}
            false ->
                []
        end,

maybe_rose_move_to_free(#{stacks := Stacks,
                          moves := Moves,
                          previous_stacks := PrevStacks} = State) ->
    EmptyFreeCells = [N || {{free, N}, empty} <- maps:to_list(State)],
    case EmptyFreeCells of
        [{{free, N}, empty} | _] ->
            rose_move_to_empty(State, N);
        false ->
            []
    end.

rose_move_to_free(#{stacks := Stacks,
                    moves := Moves,
                    previous_stacks := PrevStacks} = State,
                  FreeNum) ->
        [_ | NewStack] =
            RoseStack =
                [S || [{rose} | _] = S <- Stacks],
        Move = {[{rose}], '->', {free, FreeNum}},
        NewPrevStacks = [NewStack | PrevStacks],
        NewStacks = [NewStack | lists:delete(RoseStack, Stacks)],
        #{{free, FreeNum} => {rose},
          stacks => NewStacks,
          moves => [Move | Moves],
          previous_stacks => NewPrevStacks}

is_rose_visible([]) ->
  false;
is_rose_visible([{rose} | _]) ->
  true;

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
is_valid_stack_move({PrevStacks,
                     NewSourceStack,
                     #{moves := [{MovedStack, '->', _} | _],
                       stacks := Stacks} = _State}) ->
    OrigTargetStack = get_orig_target_stack(MovedStack, Stacks),
    UpdatedTargetStack = MovedStack ++ OrigTargetStack,
    ModifiedStacks = [NewSourceStack, UpdatedTargetStack],
    IsBackTrackingFun = fun(Stack) -> lists:member(Stack, ModifiedStacks) end,
    IsBacktracking = lists:any(IsBackTrackingFun, PrevStacks),

    MaybeNumbers = [MaybeNumber || {MaybeNumber, _Suit} <- MovedStack],
    IsOnlyNumbers = lists:all(fun is_integer/1, MaybeNumbers),
    IsInOrder = (MaybeNumbers == lists:sort(MaybeNumbers)),
    HasAlternatingSuits = has_alternating_suits(MovedStack),
    {SourceNumber, SourceSuit} = _BottomMoved = lists:last(MovedStack),
    AreStacksCompatible = IsOnlyNumbers andalso
        case OrigTargetStack of
            [] ->
                _CanMoveStackIntoEmptyStackSpace =
                    true;
            [{dragon, _} = _OldTop | _] ->
                _CanMoveStackOntoDragon =
                    false;
            [{TargetNumber, TargetSuit} = _OldTop | _] ->
                AreSequential = is_integer(SourceNumber) andalso
                                (SourceNumber + 1 == TargetNumber),
                AreDifferentSuits = (SourceSuit /= TargetSuit),
                AreSequential andalso AreDifferentSuits
        end,

    not IsBacktracking and
    IsOnlyNumbers and
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

sub_stacks(Stack) ->
    sub_stacks(Stack, []).

sub_stacks([], Stacks) ->
    Stacks;
sub_stacks(Stack, Stacks) ->
    AllButLast = lists:delete(lists:last(Stack), Stack),
    sub_stacks(AllButLast, [Stack | Stacks]).
