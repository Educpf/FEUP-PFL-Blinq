
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- consult('board').



% Value

positive_infinity(1.0E300).
negative_infinity(-1.0E300).
epsilon(1.0E-6).

weight(numberPieces, 3).


% Computer logic to choose moves

choose_best_move(black, [Score-Move | Rest], Move).
choose_best_move(white, Moves, Move):-last(Moves, Score-Move).

choose_move([Board, _, _, ValidMoves, _], 1, [Position, Rotation]):-
    sleep(1),
    random_member(Position, ValidMoves),
    random(1, 2, RotationType),
    Rotation is RotationType * 2.


choose_move([Board, Player, BlocksLeft, ValidMoves, _], 2, BestMove):-
    sleep(1),
    positive_infinity(PosInf),
    negative_infinity(NegInf),
    min_max(Board, BlocksLeft, Player, 1, NegInf, PosInf, Score, BestMove), !.

choose_move([Board, Player, BlocksLeft, ValidMoves, _], 3, BestMove):-
    positive_infinity(PosInf),
    negative_infinity(NegInf),
    min_max(Board, BlocksLeft, Player, 2, NegInf, PosInf, Score, BestMove), !.



choose_move([Board, Player, BlocksLeft, ValidMoves, _], 3, BestMove):-
    possible_moves(ValidMoves, PossibleMoves),
    positive_infinity(PosInf),
    negative_infinity(NegInf),
    write(PossibleMoves),
    min_max(Board, BlocksLeft, Player, 2, NegInf, PosInf, Score),
    write(Score), !.


winner_evaluation(white, 1.0).
winner_evaluation(black, -1.0).
winner_evaluation(draw, 0.0).

% Base Cases

%MinMax Board Player Depth Alpha Beta Evaluation
min_max(Board, BlocksLeft, Player, _, _, _, Evaluation, []):-
    get_winner(Board, BlocksLeft, Winner),
    write('Someone won: '), write(Winner),
    winner_evaluation(Winner, Evaluation), !.

min_max(Board, BlocksLeft, Player, 0, Alpha, Beta, Evaluation, []):-
    value([Board, Player, BlocksLeft, _, _], white, Evaluation),
    write('Depth 0. Evaluation'), write(Evaluation), !.

min_max(Board, BlocksLeft, Player, Depth, Alpha, Beta, Evaluation, BestMove):-
    valid_moves(Board, ValidMoves),
    possible_moves(ValidMoves, PossibleMoves),
    initial_eval(Player, Eval),
    find_best(Board, BlocksLeft, Player, PossibleMoves, Depth, Alpha, Beta, Evaluation, Eval, BestMove, []),
    write('\n\nDepth: '), write(Depth),
    write('  The evaluation ->>> '), write(Evaluation), !.

initial_eval(white, Eval):-negative_infinity(Eval).
initial_eval(black, Eval):-positive_infinity(Eval).
/*
choose_move([[[[1,white],[1,black],[1,black],[1,black],[1,white],[1,black],[1,black],[1,black],[0,null],[0,null]],[[1,white],[1,black],[1,white],[1,white],[1,white],[1,black],[1,white],[1,white],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[1,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[1,white],[1,black],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]]],white,50,[[1,4],[1,6],[1,8],[1,10],[3,4],[3,6],[3,8],[3,10],[5,4],[5,8],[5,10],[7,4],[7,6],[7,8],[7,10],[9,2],[9,4],[9,6],[9,8],[9,10]],[[7,2],2]], 3, G).

[[[[1,white],[1,black],[1,white],[1,black],[1,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,black],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,black],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,black],[0,null],[0,null],[0,null],[0,null]],[[1,white],[2,white],[2,black],[2,white],[2,black],[1,white],[1,white],[1,white],[0,null],[0,null]],[[1,black],[1,black],[1,black],[1,black],[1,black],[1,black],[1,black],[1,black],[0,null],[0,null]]],black,31,[[3,4],[3,6],[3,8],[7,2],[7,4],[7,6],[7,8],[9,2],[9,4],[9,6],[9,8],[9,10]],[[8,3],1]]

*/

find_best(_, _,_, [], _, _, _, FinalEvaluation, FinalEvaluation, FinalMove, FinalMove):- !.
find_best(_, _,_, _, _, Alpha, Beta, FinalEvaluation, FinalEvaluation, FinalMove, FinalMove):-
    write('Checking for prune: '), write(Alpha), nl,write(Beta),nl,
    Beta =< Alpha, write('\nACTIVATED ECONOMYU MODEEEEEEEEEEEEEEEEEEEEEEEEEE\n'), !.

find_best(Board, BlocksLeft, Player, [[Position, Direction] | PossibleMoves], Depth, Alpha, Beta, FinalEvaluation, Evaluation, FinalMove, Move):-
    % Execute Move
    put_block(Board, Position, Direction, ChildBoard),
    ChildBlocksLeft is BlocksLeft - 1,
    NewDepth is Depth - 1,

    % Calculate value of following the chosen move
    change_player(Player, OtherPlayer), 
    min_max(ChildBoard, ChildBlocksLeft, OtherPlayer, NewDepth, Alpha, Beta, ChildEvaluation, _),
    % Update Evaluation and AlphaBeta
    best_evaluation(Player, ChildEvaluation, Evaluation, BestEvaluation),
    best_move([Position, Direction], Move, BestEvaluation, ChildEvaluation, BestMove),
    write('Before update: '), write(Player), nl,
    update_alpha_beta(Player, ChildEvaluation, Alpha, Beta, NewAlpha, NewBeta),
    % Try analyse another Move
    find_best(Board, BlocksLeft, Player, PossibleMoves, Depth, NewAlpha, NewBeta, FinalEvaluation, BestEvaluation, FinalMove, BestMove).

best_evaluation(black, Eval1, Eval2, Result):- Result is min(Eval1, Eval2).
best_evaluation(white, Eval1, Eval2, Result):- Result is max(Eval1, Eval2).

best_move(NewMove, OldMove, OldEvaluation, OldEvaluation, NewMove).
best_move(_, OldMove, _, _, OldMove).

    



update_alpha_beta(white, Evaluation, Alpha, Beta, NewAlpha, Beta):-
    NewAlpha is max(Alpha, Evaluation).
update_alpha_beta(black, Evaluation, Alpha, Beta, Alpha, NewBeta):-
    write('TRying updating Be\n'),
    NewBeta is min(Beta, Evaluation).



% Evaluation 

value(GameState, Player, Value):-
    write('Evaluating\n'),
    change_player(Player, Other),
    score(GameState, Player, Score1),
    score(GameState, Other, Score2),
    write('Score 1: '),
    write(Score1),nl,
    write('Score 2: '),
    write(Score2),nl,

    epsilon(SmallValue),
    Value is (Score1 - Score2) / (Score1 + Score2 + SmallValue).

score([Board, _, BlocksLeft, _, _], Player, Score):-
    number_pieces_score(Board, BlocksLeft, Player, PiecesScore),
    write(PiecesScore), nl,
    paths_quality(Board, Player, Paths, Score).

% path_proximity(Paths, Score).

count_squares(Board, Player, Amount):- 
    scanlist(append, Board, [], Flatten),
    count_squares(Flatten, Player, Amount, 0).

count_squares([], _, Amount, Acc):- Amount = Acc, !.
count_squares([[_, Player] | Rest], Player, Amount, Acc):-
    NewAcc is Acc + 1,
    count_squares(Rest, Player, Amount, NewAcc), !.
count_squares([_ | Rest], Player, Amount, Acc):-
    count_squares(Rest, Player, Amount, Acc).



    
    

number_pieces_score(Board, BlocksLeft, Player, PiecesScore):-
    count_squares(Board, Player, Amount),
    length(Board, Size),
    Max is ((Size * 2) ^ 2),
    Min is 2,
    weight(numberPieces, Weight),
    PiecesScore is (Amount - Min) / (Max - Min) * Weight.


% Increase the Score of a Path considering its size
path_score([Path, Score], Final):-
    length(Path, Size),
    Final is Score * (0.50 + (0.15 * Size)).

paths_quality(Board, Player, Paths, LimitedScore):-
    get_paths(Board, [1, 1], Player, [], PathList),
    maplist(path_score, PathList, FinalScores),
    sumlist(FinalScores, Score),
    LimitedScore is Score.
    


% Reached End 
get_paths(Board, [XPos, YPos], Player, Visited, []):-
    length(Board, Size),
    YPos > Size, !.
% Reached End of Row
get_paths(Board, [XPos, YPos], Player, Visited, IndividualScore) :-
    length(Board, Size),
    XPos > Size,
    NewYPos is YPos + 1, % Increase Row Number & Reset Column
    get_paths(Board, [0, NewYPos], Player, Visited, IndividualScore), !.

% Other cases
get_paths(Board, [XPos, YPos], Player, Visited, [[Path, PathScore] | IndividualScore]):-
    get_path(Board, [XPos, YPos], Player, Visited, Path, PathScore),
    NewXPos is XPos + 1,
    append(Visited, Path, NewVisited),
    get_paths(Board, [NewXPos, YPos], Player, NewVisited, IndividualScore).





% Not valid square
get_path(Board, [XPos, YPos], Player, Visited, [], 0):-
    length(Board, Size),
    ( 
    XPos < 1;
    XPos > Size;
    YPos < 1;
    YPos > Size
    ).

% Already visited
get_path(Board, [XPos, YPos], Player, Visited, [], 0):-
    member([XPos, YPos], Visited), !.

% Other Color
get_path(Board, [XPos, YPos], Player, Visited, [], 0):-
    change_player(Player, Other),
    get_square(Board, [XPos, YPos], [_, Color]),
    (Color == Other; Color == null), !.

/*
create_board(5, [5, 6], B, GK), paths_with_score(B, white, _, F).

paths_with_score([[[1,white],[1,black],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,black],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,black],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,black],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,black],[1,white],[1,black],[1,black],[1,white],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,black],[1,white],[1,black],[1,white],[1,black],[0,null],[0,null],[0,null],[0,null]],[[1,black],[1,black],[1,black],[1,black],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,white],[1,white],[1,white],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]]], white, Paths, G).

*/
get_path(Board, [XPos, YPos], Player, Visited, FinalPath, Value):-

    % Mark as visited
    UpdatedVisited = [[XPos, YPos] | Visited],
    Path = [[XPos, YPos]],

    % Check type of square
    height_square_points(Board, [XPos, YPos], HeightPoints),

    % See if its top square, or almost top
    XPosLeft is XPos - 1,
    XPosRight is XPos + 1,
    YPosDown is YPos - 1,
    YPosUp is YPos + 1,


    % Move Down
    get_path(Board, [XPos, YPosDown], Player, UpdatedVisited, PathDown, ValueDown),
    append(Path, PathDown, TempPath1),
    append(UpdatedVisited, PathDown, TempVisited1),
    % Move Left
    get_path(Board, [XPosLeft, YPos], Player, TempVisited1, PathLeft, ValueLeft),
    append(TempPath1, PathLeft, TempPath2),
    append(TempVisited1, PathLeft, TempVisited2),
    % Move Rigth
    get_path(Board, [XPosRight, YPos], Player, TempVisited2, PathRight, ValueRight),
    append(TempPath2, PathRight, TempPath3),
    append(TempVisited2, PathRight, TempVisited3),
    % Move Up
    get_path(Board, [XPos, YPosUp], Player, TempVisited3, PathUp, ValueUp),
    append(TempPath3, PathUp, FinalPath),

    path_direction_extra_points(Player, PathUp, PathDown, PathLeft, PathRight, ExtraPoints),

    Value is ValueDown + ValueLeft + ValueRight + ValueUp + ExtraPoints + HeightPoints, !.

path_direction_extra_points(white, PathUp, PathDown, _, _, ExtraPoints):-
    path_extra_points(PathUp, UpExtra),
    path_extra_points(PathDown, DownExtra),
    ExtraPoints is UpExtra + DownExtra.

path_direction_extra_points(black, _, _, PathRight, PathLeft, ExtraPoints):-
    path_extra_points(PathRight, RigthExtra),
    path_extra_points(PathLeft, LeftExtra),
    ExtraPoints is LeftExtra + RigthExtra.

% Square cannot be overwritten
height_square_points(Board, [XPos, YPos], 1):-
    get_square(Board, [XPos, YPos], [Height, Color]),
    length(Board, Size),
    Mid is Size // 2,
    TopHeight is min( XPos - (2 * XPos - 2 * Mid - 1) * ((XPos - 1) // Mid), YPos - (2 * YPos - 2 * Mid - 1) * ((YPos - 1) // Mid)),
    Height = TopHeight,
    !.

% Square cannot be immediatly overwritten
height_square_points(Board, [XPos, YPos], Points):-
    get_square(Board, [XPos, YPos], [Height, Color]),
    % Get side squares
    get_surrounding_squares(Board, [XPos, YPos], [[H1,_], [H2,_], [H3,_], [H4,_]]),
    include(>(Height), [H1, H2, H3, H4], SquaresLower),
    length(SquaresLower, Number),
    Points is Number / 3, !. % Value is either 1 or 2


path_extra_points([], 0).  
path_extra_points(_, 100).




