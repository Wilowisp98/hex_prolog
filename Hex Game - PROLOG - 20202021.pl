:-dynamic(isVisited/1).
:-dynamic(isPath/1).
:-dynamic(getAllElemNeighbours/2).
:-discontiguous(moves/3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Starting Player vs Player board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startPvPBoard([
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o]
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Starting Player(W) vs AI Board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startPvBAIBoard([
    [o,o,o,o,o,o,o,o],
    [o,b,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o]
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Starting Player(B) vs AI Board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startPvWAIBoard([
    [o,o,o,o,o,o,o,o],
    [o,w,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o]
]).


%%%%%%%%%%%%%%%%%%%
% Starts the game %
%%%%%%%%%%%%%%%%%%%
play :-
    retractall(isPath(_)),
    nl,
    write('=========='), nl,
    write('=     Hex    ='), nl,
    write('=========='), nl,
    gamemode.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Makes the Player able to choose if he wants  %
% to play against another Player or against AI %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gamemode :-
    repeat,
    nl, 
    write('===================='),nl,
    write('=  Choose game mode  ='), nl,
    write('===================='), nl, nl,
    write('If you want to play against another player type:   1'), nl, nl,
    write('If you want to play against an AI type:            2'), nl,
    read(Mode), nl,
    mode(Mode).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Makes the Player1 able to choose the color he wants %
% to play with when Player vs Player mode is choosen  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode(1):-
    repeat,
    nl, write('Color for Player1? (w or b and w goes first)'), nl,
    read(Player), nl,
    (Player == w; Player == b),
    startPvPBoard(Board),
    showBoard(Board), nl, nl,
    playPvP([w, Board], Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Makes the Player able to choose the color he wants %
% to play with when Player vs AI mode is choosen     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode(2):-
    repeat,
    nl, write('Color for human player ? (w or b)'), nl,
    read(Player), nl,
    (Player == w; Player == b),
    playerVSAI(Player).


playerVSAI(w) :-
    startPvBAIBoard(Board),
    showBoard(Board), nl, nl,
    play([w, Board], w).


playerVSAI(b) :-
    startPvWAIBoard(Board),
    showBoard(Board), nl, nl,
    play([b, Board], b).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recursive predicate that keeps the game running when %
%          Player vs Player mode is choosen            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
playPvP([Player, Board], Player) :- !,
    write('Next move line?'), nl,
    read(NL), nl,
    write('Next move column?'), nl,
    read(NC), nl,
    Pos = (NL, NC),
    (
        humanMove([Player, Board], Pos, [NextPlayer, NextBoard]), !,
        (
            retractall(isVisited(_)),
            isGameOver(NextBoard, Winner) ->
            (
                showBoard(NextBoard),
                Player = Winner, !,                                 
                nl, write('End of game : '), write(Player), write(' win !'), nl, nl
                ;                           
                nl, write('End of game : '), write(Player), write(' loss !'), nl, nl
            )
            ;
            (
                showBoard(NextBoard),
                playPvP([NextPlayer, NextBoard], Player)
            )
        )
        ;
        write('-> Bad Move !'), nl,
        playPvP([Player, Board], Player)
    ).
playPvP([Player, Board], _) :-
    write('Next move line?'), nl,
    read(NL), nl,
    write('Next move column?'), nl,
    read(NC), nl,
    Pos = (NL, NC),
    (
        humanMove([Player, Board], Pos, [NextPlayer, NextBoard]), !,
        (
            retractall(isVisited(_)),
            isGameOver(NextBoard, Winner) ->
            (
                showBoard(NextBoard),
                Player = Winner, !,                                 
                nl, write('End of game : '), write(Player), write(' win !'), nl, nl
                ;                           
                nl, write('End of game : '), write(Player), write(' loss !'), nl, nl
            )
            ;
            (
                showBoard(NextBoard),
                playPvP([NextPlayer, NextBoard], Player)
            )
        )
        ;
        write('-> Bad Move !'), nl,
        playPvP([Player, Board], Player)
    ).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recursive predicate that keeps the game running when %
%            Player vs AI mode is choosen              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play([Player, Board], Player) :- !,
    write('Next move line?'), nl,
    read(NL), nl,
    write('Next move column?'), nl,
    read(NC), nl,
    Pos = (NL, NC),
    (
        nextPlayer(Player, NextPlayer),
        humanMove([Player, Board], Pos, [NextPlayer, NextBoard]), !,
        retractall(isVisited(_)),
        (

            isGameOver(NextBoard, Winner) ->
            (
                showBoard(NextBoard),
                Player = Winner, !,                                 
                nl, write('End of game : '), write(Player), write(' win !'), nl, nl
                ;                           
                nl, write('End of game : '), write(Player), write(' loss !'), nl, nl
            )
            ;
            (showBoard(NextBoard), play([NextPlayer, NextBoard], Player))
        )
        ;
        write('-> Bad Move !'), nl,
        play([Player, Board], Player)
    ).
play([Player, Board], HumanPlayer) :-
    write('Computer play :  '), nl,
    minimax(Player,Board, 2, Move, Value),
    write('AI Move :  '), write(Move), nl,
    write('Move Value :  '), write(Value), nl,
    setMoveOnBoard(Board, Move, Player, BestSuccBoard),
    nextPlayer(Player, NextPlayer),
    retractall(isVisited(_)),
    (
        isGameOver(BestSuccBoard, Winner) ->
        (
            showBoard(BestSuccBoard),
            Player = Winner, !,                                 
            nl, write('End of game : '), write(Player), write(' win !'), nl, nl
            ;                           
            nl, write('End of game : '), write(Player), write(' loss !'), nl, nl
        )
        ;
        (showBoard(BestSuccBoard), play([NextPlayer, BestSuccBoard], HumanPlayer))
    ).

%%%%%%%%%%%%%
% UTILITIES %
%%%%%%%%%%%%%


%%%%%%%%%%%%%%%
% Next player %
%%%%%%%%%%%%%%%
nextPlayer(w, b).
nextPlayer(b, w).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate that prints the board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
showBoard(Board) :-
    write("WHITE\t  BLACK"), nl,
    writeUpperHalfBoard(Board, 0),
    writeBottomHalfBoard(Board, 1),
    write("BLACK\t  WHITE"), nl, nl.


writeUpperHalfBoard(Board, LineIdx) :-
    boardElem(Board, LineIdx, 0, Elem), !,
    writeSpacing(LineIdx),
    (
        isPath((LineIdx, 0))
        ->
            (string_upper(Elem, ElemUppercase), write(ElemUppercase))
        ;
            write(Elem)
    ),
    N1 is LineIdx - 1, 
    writeLineForward(Board, N1, 1), nl,
    NextLineIdx is LineIdx + 1,
    writeUpperHalfBoard(Board, NextLineIdx).
writeUpperHalfBoard(_, _).


writeBottomHalfBoard(Board, ColIdx) :-
    boardElem(Board, 7, ColIdx, Elem), !,
    Spacing is 7 - ColIdx,
    writeSpacing(Spacing),
    (
        isPath((7, ColIdx))
        ->
            (string_upper(Elem, ElemUppercase), write(ElemUppercase))
        ;
            write(Elem)
    ),
    N1 is ColIdx + 1, 
    writeLineForward(Board, 6, N1), nl,
    NextColIdx is ColIdx + 1,
    writeBottomHalfBoard(Board, NextColIdx).
writeBottomHalfBoard(_, _).


writeLineForward(Board, LineIdx, ColIdx) :-
    boardElem(Board, LineIdx, ColIdx, Elem), !,
    write(' '),
    (
        isPath((LineIdx, ColIdx))
        ->
            (string_upper(Elem, ElemUppercase), write(ElemUppercase))
        ;
            write(Elem)
    ),
    NextLineIdx is LineIdx - 1, 
    NextColIdx is ColIdx + 1,
    writeLineForward(Board, NextLineIdx, NextColIdx).
writeLineForward(_, _, _).


writeSpacing(LineIdx) :-
    TotalSpaces is 7 - LineIdx,
    writeSpacing_1(0, TotalSpaces).


writeSpacing_1(TotalSpaces, TotalSpaces) :- 
    !.
writeSpacing_1(NumSpaces, TotalSpaces) :-
    write(' '),
    NextNumSpaces is NumSpaces + 1,
    writeSpacing_1(NextNumSpaces, TotalSpaces).

        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate that checks if the game is over or not, that is, %
%   if there is a path from a starting tile to an end tile   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isGameOver(Board, w) :-
    isWhiteWinner(Board), !.
isGameOver(Board, b) :-
    isBlackWinner(Board).


isWhiteWinner(Board) :-
    checkPosWhite(Board, 0).


isBlackWinner(Board) :-
    checkPosBlack(Board, 0).


checkPosWhite(_, 8) :- !, fail.
checkPosWhite(Board, LineIdx) :-
    boardElem(Board, LineIdx, 0, w),
    Pos = (LineIdx, 0),
    findPath(Board, Pos, [], Path),
    optimizePath(Path, PathOptimized),
    assertPath(PathOptimized).
checkPosWhite(Board, LineIdx) :-
    NextLineIdx is LineIdx + 1,
    checkPosWhite(Board, NextLineIdx).


checkPosBlack(_, 8) :- !, fail.
checkPosBlack(Board, ColIdx) :-
    boardElem(Board, 0, ColIdx, b),
    Pos = (0, ColIdx),
    findPath(Board, Pos, [], Path),
    optimizePath(Path, PathOptimized),
    assertPath(PathOptimized).
checkPosBlack(Board, ColIdx) :-
    NextColIdx is ColIdx + 1,
    checkPosBlack(Board, NextColIdx).


findPath(Board, Pos, Acc, [Pos|Acc]) :-
    boardElem(Board, Pos, w),
    Pos = (_, 7), 
    !.
findPath(Board, Pos, Acc, [Pos|Acc]) :- 
    boardElem(Board, Pos, b),
    Pos = (7, _), 
    !.
findPath(Board, Pos, Acc, Path) :-
    assertz(isVisited(Pos)),
    getElemNeighbours(Board, Pos, Neighbours),
    findPathFromNeighours(Board, [Pos|Acc], Path, Neighbours).


findPathFromNeighours(Board, Acc, Path, [N|_]) :-
    findPath(Board, N, Acc, Path), 
    !.
findPathFromNeighours(Board, Acc, Path, [_|Tail]) :-
    findPathFromNeighours(Board, Acc, Path, Tail).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets the neighbours of an element %
%  that were not visited/are free   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                                      
getElemNeighbours(Board, ElemPos, Neighbours) :-       
    boardElem(Board, ElemPos, Elem),                   
    setof(                                             
        (L,C),
        Board^ElemPos^Elem^(
            getNeighbour(ElemPos, (L,C)),
            boardElem(Board, L, C, Elem),
            not(isVisited((L,C)))
        ),
        Neighbours
    ).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets all the neighbours of a specific Tile %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getAllElemNeighbours(ElemPos, Neighbours) :-
    findall(
        (L,C), 
        (getNeighbour(ElemPos, (L,C)), between(0,7, L), between(0,7,C)), 
        Neighbours
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets the neighbours that are not occupied %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getFreeElemNeighbours(Board, ElemPos, Neighbours) :-
    findall(
        (L,C), 
        (getNeighbour(ElemPos, (L,C)), boardElem(Board, (L,C), Elem), Elem == o)
        ,
        Neighbours
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets rid of redudant tiles in the path %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
optimizePath(Path, OptimizedPath) :-
    optimizePath_1(Path, [], OptimizedPath).

optimizePath_1([H1,_,H3|T], Acc, OptimizedPath) :-
    getNeighbour(H1, H3), !,
    optimizePath_1([H3|T], [H1|Acc], OptimizedPath).

optimizePath_1([H1,H2,H3|T], Acc, OptimizedPath) :-
    !, 
    optimizePath_1([H2,H3|T], [H1|[H2|Acc]], OptimizedPath).

optimizePath_1(RemainingPath, Acc, OptimizedPath) :-
    append(RemainingPath, Acc, OptimizedPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Asserts the founded path into the database %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assertPath([H|T]) :-
    assertz(isPath(H)),
    assertPath(T).

assertPath([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checks if the chosen tile is empty and if so sets it to the corresponding player %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
humanMove([X1, Board], Pos, [X2, NextBoard]) :-
    boardElem(Board, Pos, o), 
    nextPlayer(X1, X2),
    setMoveOnBoard(Board, Pos, X1, NextBoard).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sets the "NewTile" on the specific (Line,Column) and makes a new board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
setMoveOnBoard(Board, (Line,Col), NewTile, UpdatedBoard) :-
    nth0(Line,Board,Old),
    auxUpdate(Col,Old,NewTile,Upd),
    auxUpdate(Line,Board,Upd,UpdatedBoard).


auxUpdate(N,I,V,O) :-
    nth0(N,I,_,T),
    nth0(N,O,V,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Neighours of a tile seen as in a graph %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getNeighbour((L,C), (L,C1)) :-
    C1 is C - 1.
getNeighbour((L,C), (L,C1)) :-
    C1 is C + 1.
getNeighbour((L,C), (L1,C)) :-
    L1 is L - 1.
getNeighbour((L,C), (L1,C)) :-
    L1 is L + 1.
getNeighbour((L,C), (L1,C1)) :-
    L1 is L - 1, C1 is C + 1.
getNeighbour((L,C), (L1,C1)) :-
    L1 is L + 1, C1 is C - 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets/Checks a tile based on his position %
%      on the board (Line, Column)         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boardElem(Board, Pos, Elem) :-
    (LineIdx, ColIdx) = Pos,
    boardElem(Board, LineIdx, ColIdx, Elem).
boardElem(Board, LineIdx, ColIdx, Elem) :-
    LineIdx >= 0, LineIdx < 8,
    ColIdx >= 0, ColIdx < 8,
    nth0(LineIdx, Board, Line),
    nth0(ColIdx, Line, Elem).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Makes a new board where the tiles are 0, 1 or 1000 depending on the Color %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updateSingularResistance(Board,Color,BoardWithRes):-
    resFinder(Board,Color,[],FullBoardWithRes),
    part(FullBoardWithRes, 8, BoardWithRes).


resFinder([H|T],Color,Acc,BoardWithRes) :-
    helpRes(H,Color,Acc,LineRes),
    resFinder(T,Color,Acc,Bordie),
    append(LineRes, Bordie, BoardWithRes),
    !.
resFinder(_,_,BoardWithRes,BoardWithRes).

        
helpRes([],_,LineRes,LineRes):-!.
helpRes([H|T],Color,Acc,LineRes) :-
    H == Color,
    append(Acc,[0],Acc1),
    helpRes(T,Color,Acc1,LineRes);
    H == o,
    append(Acc,[1],Acc1),
    helpRes(T,Color,Acc1,LineRes);
    append(Acc,[1000],Acc1),
    helpRes(T,Color,Acc1,LineRes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Turns a list into a list of lists with N elements each %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
part([], _, []) :-
    !.
    part(List, N, [DL|DLTail]) :-
    length(DL, N),
    append(DL, LTail, List),
    part(LTail, N, DLTail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets the neighbours of a tile with their  %
%           paired resistance               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neighList(SingularResBoard, Color, (LineIdx, ColIdx), NeighWithRes):-
    getAllElemNeighbours((LineIdx, ColIdx), Neighbours),
    getNeighList(SingularResBoard, Color, (LineIdx, ColIdx), Neighbours, [], NeighWithRes),
    !.


getNeighList(_,_,_, [], NeighWithRes, NeighWithRes).
getNeighList(SingularResBoard, b, (LineIdx, ColIdx), [(NLineIdx,NColIdx)|T], Acc, NeighWithRes):-
    resistance(SingularResBoard, (LineIdx, ColIdx), (NLineIdx, NColIdx), ResValue),
    Depth is 7 - NLineIdx,
    LineDepth is ColIdx - NColIdx,
    getNeighList(SingularResBoard, b, (LineIdx, ColIdx), T, [(ResValue, Depth, LineDepth, NLineIdx, NColIdx)|Acc], NeighWithRes).
getNeighList(SingularResBoard, w, (LineIdx, ColIdx), [(NLineIdx, NColIdx)|T], Acc, NeighWithRes):-
    resistance(SingularResBoard, (LineIdx, ColIdx) , (NLineIdx, NColIdx), ResValue),
    Depth is 7 - NColIdx,
    ColDepth is LineIdx - NLineIdx,
    getNeighList(SingularResBoard, w, (LineIdx, ColIdx), T, [(ResValue, Depth, ColDepth, NColIdx, NLineIdx)|Acc], NeighWithRes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates the paired resistance %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resistance(Board, (L1, C1), (L2, C2), Resistance) :-
    boardElem(Board, (L1, C1), Value1),
    boardElem(Board, (L2, C2), Value2),
    Resistance is Value1 + Value2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluation of the Board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluation(Board, Value):-
    updateSingularResistance(Board, b, BBoard),
    leastResPath(BBoard, b, Rb),
    updateSingularResistance(Board, w, WBoard),
    leastResPath(WBoard, w, Rw),
    Value is Rb/(Rw+0.01).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets the least resistance path %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
leastResPath(BoardWithRes, Color, ValueLeastRes) :-
    bagof((LineIdx,ColIdx),start((LineIdx,ColIdx),Color),StartList),
    getLeastResPath(StartList, BoardWithRes, Color, 100, ValueLeastRes).


getLeastResPath([], _, _, Value, Value).
getLeastResPath([(LineIdx,ColIdx)|Tail], BoardWithRes, Color, PossibleValue, Value) :-
    getPath(BoardWithRes, (LineIdx, ColIdx), Color, GetValue),
    updatebest(PossibleValue,GetValue, BestValue),
    !,
    getLeastResPath(Tail, BoardWithRes, Color, BestValue, Value).


getPath(BoardWithRes, (LineIdx,ColIdx), Color, Value) :-
    getPath(BoardWithRes, (LineIdx, ColIdx), Color, [], 0, Value).


getPath(_, (LineIdx, ColIdx), Color, _, Value, Value) :-
    end((LineIdx,ColIdx), Color).


getPath(BoardWithRes, (LineIdx,ColIdx), b, Path,  AccValue, Value) :-
    not(end((LineIdx,ColIdx), b)),
    neighList(BoardWithRes,b, (LineIdx, ColIdx), Neigh),
    sort(Neigh, SortedNeigh),
    member((R,_,_,L,C), SortedNeigh),
    not(member((L,C), Path)),
    New_AccValue is AccValue + R,
    getPath(BoardWithRes, (L,C), b, [(LineIdx, ColIdx)|Path] , New_AccValue, Value).
getPath(BoardWithRes, (LineIdx,ColIdx), w, Path,  AccValue, Value) :-
    not(end((LineIdx,ColIdx), w)),
    neighList(BoardWithRes, w, (LineIdx, ColIdx), Neigh),
    sort(Neigh, SortedNeigh),
    member((R,_,_,C,L), SortedNeigh),
    not(member((L,C), Path)),
    New_AccValue is AccValue + R,
    getPath(BoardWithRes, (L,C), w, [(LineIdx, ColIdx)|Path] ,New_AccValue, Value).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Starting and Ending Tiles %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start((0,0),b).
start((0,1),b).
start((0,2),b).
start((0,3),b).
start((0,4),b).
start((0,5),b).
start((0,6),b).
start((0,7),b).
start((0,0),w).
start((1,0),w).
start((2,0),w).
start((3,0),w).
start((4,0),w).
start((5,0),w).
start((6,0),w).
start((7,0),w).


end((7,0),b).
end((7,1),b).
end((7,2),b).
end((7,3),b).
end((7,4),b).
end((7,5),b).
end((7,6),b).
end((7,7),b).
end((0,7),w).
end((1,7),w).
end((2,7),w).
end((3,7),w).
end((4,7),w).
end((5,7),w).
end((6,7),w).
end((7,7),w).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updates for the best value %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updatebest(Value1, Value2, Value1) :-
    Value1 < Value2.
updatebest(Value1, Value2, Value2) :-
    Value1 > Value2.
updatebest(Value1, Value2, Value1) :-
    Value1 = Value2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets the possible moves of the AI %
% depending on the the vison range  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
moves(Board, Color, Vision):-
    listTiles(Board, Color, ColoredTiles),
    findMoves(Board, ColoredTiles, UnclearedVision),
    sort(UnclearedVision, Vision),
    !.


listTiles(Board, Color, ColoredTiles) :-
    findTiles(Board, 0, 0, Color, [], ColoredTiles),
    !.


findTiles(_, 8,_, _, BlackTiles, BlackTiles) :- !.
findTiles([L|Ls], LineIdx, ColIdx, Color, AccBlack, BlackTiles) :-
    findTilesColumn(LineIdx, ColIdx, L, Color, AccBlack, BColTiles),
    LineIdx < 8,
    NewLineIdx is LineIdx + 1,
    findTiles(Ls, NewLineIdx, ColIdx, Color, AccBlack, Black),
    append(BColTiles, Black, BlackTiles).


findTilesColumn(_,8,_, _, ColoredColumnTiles, ColoredColumnTiles) :- !.
findTilesColumn(LineIdx, ColIdx, [C|Cs], Color, AccBlack, BColTiles) :-
    C == Color,
    ColIdx < 8,
    append([(LineIdx,ColIdx)], AccBlack, NewAccBlack),
    NewColIdx is ColIdx + 1,
    findTilesColumn(LineIdx, NewColIdx, Cs, Color, NewAccBlack, BColTiles);
    ColIdx < 8,
    NewColIdx is ColIdx + 1,
    findTilesColumn(LineIdx, NewColIdx, Cs, Color, AccBlack, BColTiles),
    !.


%%%%%%%%%%%%%%%%%%%%
% AI field of view %
%%%%%%%%%%%%%%%%%%%%
findMoves(Board, ColoredTiles, TierNNeighs) :-
    findMoves(Board, ColoredTiles, [], Tier1Neighs),
    findMoves(Board, Tier1Neighs, [], Tier2Neighs),
    append(Tier1Neighs, Tier2Neighs, TierNNeighs).


findMoves(_, [], TierNNeighs, TierNNeighs). 
findMoves(Board, [Tile|Tiles], TilesAcc, TierNNeighs) :-
    getFreeElemNeighbours(Board, Tile, NeighList),
    append(NeighList, TilesAcc, NewTilesAcc),
    findMoves(Board, Tiles, NewTilesAcc, TierNNeighs),
    !.


%%%%%%%%%%%
% Minimax %
%%%%%%%%%%%
minimax(_, Board, 0, _, Val) :-
    evaluation(Board, Val),
    !.
minimax(b, Board, Depth, BestMove, BestValue) :-
    moves(Board, b, PossibleMoves), 
    !,
    NewDepth is Depth - 1,
    best(b, Board, PossibleMoves, NewDepth, nil, 100000, BestMove, BestValue),
    !.

minimax(w, Board, Depth, BestMove, BestValue) :-
    moves(Board, w, PossibleMoves), 
    !,
    NewDepth is Depth - 1,
    best(w, Board, PossibleMoves, NewDepth, nil, -100000, BestMove, BestValue),
    !.

best(_, _, [], _, BestMove, BestValue, BestMove, BestValue) :- !.
best(Player, Board, [(Line,Column)|Moves], Depth, RecordMove, RecordVal, BestMove, BestValue) :-
    setMoveOnBoard(Board, (Line, Column), Player, NextBoard),
    nextPlayer(Player, NextPlayer),
    !,
    minimax(NextPlayer, NextBoard, Depth, _, Value),
    betterof(Player, (Line,Column), Value, RecordMove, RecordVal, NewRecordMove, NewRecordVal),
    best(Player, Board, Moves, Depth, NewRecordMove, NewRecordVal, BestMove, BestValue).


betterof(b, Move, Value, _, RecordValue, Move, Value) :-
    Value < RecordValue.

betterof(b, _, Value, RecordMove, RecordValue, RecordMove, RecordValue) :-
    Value == RecordValue.

betterof(b, _, Value, RecordMove, RecordValue, RecordMove, RecordValue) :-
    Value > RecordValue.

betterof(w, Move, Value, _, RecordValue, Move, Value) :-
    Value > RecordValue.

betterof(w, _, Value, RecordMove, RecordValue, RecordMove, RecordValue) :-
    Value == RecordValue.

betterof(w, _, Value, RecordMove, RecordValue, RecordMove, RecordValue) :-
    Value < RecordValue.
    