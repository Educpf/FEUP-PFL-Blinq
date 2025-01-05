
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- consult('board').



% Auxiliar predicates to retrieve specific values 
positive_infinity(1.0E300).
negative_infinity(-1.0E300).
epsilon(1.0E-300).

% Translation between game result and evaluation
winner_evaluation(white, 1.0).
winner_evaluation(black, -1.0).
winner_evaluation(draw, 0.0).


weight(numberPieces, 0.1).
weight(pathQuality, 0.9).


/*
    choose_move(+GameState, +Difficulty, -BestMove)
    Chooses a move for the current player based on the game state and difficulty level.
    
    Arguments:
        +GameState: A list representing the current game state [Board, Player, BlocksLeft, ValidMoves, _].
        +Difficulty: The difficulty level (1 = random move, 2 = Greddy Algorithm, 3 = MinMax algorithm).
        -BestMove: The chosen move, represented as [Position, Rotation] or a similar structure.
*/

% Difficulty 1: Select a random move
choose_move([Board, _, _, ValidMoves, _], 1, [Position, Rotation]):-
    sleep(1),
    random_member(Position, ValidMoves),
    random(1, 2, RotationType),
    Rotation is RotationType * 2.


% Difficulty 2: Greedy Algorithm using min_max with depth 1
choose_move([Board, Player, BlocksLeft, ValidMoves, _], 2, BestMove):-
    sleep(1),
    positive_infinity(PosInf),
    negative_infinity(NegInf),
    min_max(Board, BlocksLeft, Player, 1, NegInf, PosInf, Score, BestMove), !.

% Difficulty 3: More advanced min_max with depth 2
choose_move([Board, Player, BlocksLeft, ValidMoves, _], 3, BestMove):-
    positive_infinity(PosInf),
    negative_infinity(NegInf),
    min_max(Board, BlocksLeft, Player, 2, NegInf, PosInf, Score, BestMove), !.






/*
    min_max(+Board, +BlocksLeft, +Player, +Depth, +Alpha, +Beta, -Evaluation, -BestMove)
    Implements the Minimax algorithm with alpha-beta pruning to find the best move and its evaluation.
    
    Arguments:
        +Board: The current state of the board.
        +BlocksLeft: The number of blocks left to be placed.
        +Player: The current player (e.g., black or white).
        +Depth: The current depth of the search.
        +Alpha: The alpha value for pruning.
        +Beta: The beta value for pruning.
        -Evaluation: The evaluation score of the best move.
        -BestMove: The best move found at this depth, represented as [Position, Direction].
*/

% Initialize evaluation based on player color.
initial_eval(white, Eval):-negative_infinity(Eval).
initial_eval(black, Eval):-positive_infinity(Eval).


% Base case: If there's a winner, return the evaluation for that player.
min_max(Board, BlocksLeft, Player, _, _, _, Evaluation, []):-
    get_winner(Board, BlocksLeft, Winner),
    winner_evaluation(Winner, Evaluation), !.

% Base case: If the search depth is zero, evaluate the board state for the current player.
min_max(Board, BlocksLeft, Player, 0, Alpha, Beta, Evaluation, []):-
    value([Board, Player, BlocksLeft, _, _], white, Evaluation), !.

% Recursive case: Explore all valid moves and find the best move based on evaluation.
min_max(Board, BlocksLeft, Player, Depth, Alpha, Beta, Evaluation, BestMove):-
    valid_moves(Board, ValidMoves),
    possible_moves(ValidMoves, PossibleMoves),
    initial_eval(Player, Eval),
    find_best(Board, BlocksLeft, Player, PossibleMoves, Depth, Alpha, Beta, Evaluation, Eval, BestMove, []), !.


/*
    find_best(+Board, +BlocksLeft, +Player, +PossibleMoves, +Depth, +Alpha, +Beta, -FinalEvaluation, +Evaluation, -FinalMove, +Move)
    Evaluates all possible moves to find the best move using alpha-beta pruning.
    
    Arguments:
        +Board: The current state of the board.
        +BlocksLeft: The number of blocks left to be placed.
        +Player: The current player (black or white).
        +PossibleMoves: The list of possible moves to evaluate.
        +Depth: The current depth of the search.
        +Alpha: The alpha value for pruning.
        +Beta: The beta value for pruning.
        -FinalEvaluation: The final evaluation score after considering all moves.
        +Evaluation: The current best evaluation score.
        -FinalMove: The best move found.
        +Move: The current best move being considered.
*/

% Base case: No more moves to evaluate, return the final evaluation and move.
find_best(_, _,_, [], _, _, _, FinalEvaluation, FinalEvaluation, FinalMove, FinalMove):- !.

% Pruning case: Stop evaluating if Beta <= Alpha.
find_best(_, _,_, _, _, Alpha, Beta, FinalEvaluation, FinalEvaluation, FinalMove, FinalMove):-
    Beta =< Alpha, !.

% Recursive case: Evaluate the current move and continue with the rest.
find_best(Board, BlocksLeft, Player, [[Position, Direction] | PossibleMoves], Depth, Alpha, Beta, FinalEvaluation, Evaluation, FinalMove, Move):-
    % Execute Current Move
    put_block(Board, Position, Direction, ChildBoard),
    ChildBlocksLeft is BlocksLeft - 1,
    NewDepth is Depth - 1,

    % Calculate the value of the move recursively
    change_player(Player, OtherPlayer), 
    min_max(ChildBoard, ChildBlocksLeft, OtherPlayer, NewDepth, Alpha, Beta, ChildEvaluation, _),
    % Update Best Evaluation, Best Move and Alpha-Beta Values
    best_evaluation(Player, ChildEvaluation, Evaluation, BestEvaluation),
    best_move([Position, Direction], Move, BestEvaluation, ChildEvaluation, BestMove),
    update_alpha_beta(Player, ChildEvaluation, Alpha, Beta, NewAlpha, NewBeta),

    % Continue with other moves
    find_best(Board, BlocksLeft, Player, PossibleMoves, Depth, NewAlpha, NewBeta, FinalEvaluation, BestEvaluation, FinalMove, BestMove).


/*
    best_evaluation(+Player, +Eval1, +Eval2, -Result)
    Determines the best evaluation score based on the specified player (maximize or minimize).

    Arguments:
        +Player: The current player (white or black).
        +Eval1: The first evaluation value.
        +Eval2: The second evaluation value.
        -Result: The best evaluation value between Eval1 and Eval2.
*/
best_evaluation(black, Eval1, Eval2, Result):- Result is min(Eval1, Eval2).
best_evaluation(white, Eval1, Eval2, Result):- Result is max(Eval1, Eval2).

/*
    best_move(+NewMove, +OldMove, +OldEvaluation, +ChildEvaluation, -BestMove)
    Determines the best move between the current move and the previous best move.

    Arguments:
        +NewMove: The move being evaluated.
        +OldMove: The previous best move.
        +OldEvaluation: The evaluation score of the previous best move.
        +ChildEvaluation: The evaluation score of the current move.
        -BestMove: The best move between NewMove and OldMove.
*/
best_move(NewMove, OldMove, OldEvaluation, OldEvaluation, NewMove).
best_move(_, OldMove, _, _, OldMove).

/*
    update_alpha_beta(+Player, +Evaluation, +Alpha, +Beta, -NewAlpha, -NewBeta)
    Updates the alpha or beta values for pruning based on the specified player.

    Arguments:
        +Player: The current player (white or black).
        +Evaluation: The evaluation score of the current move.
        +Alpha: The current alpha value.
        +Beta: The current beta value.
        -NewAlpha: The updated alpha value.
        -NewBeta: The updated beta value.
*/
update_alpha_beta(white, Evaluation, Alpha, Beta, NewAlpha, Beta):-
    NewAlpha is max(Alpha, Evaluation).
update_alpha_beta(black, Evaluation, Alpha, Beta, Alpha, NewBeta):-
    NewBeta is min(Beta, Evaluation).



/*
    value(+GameState, +Player, -Value)
    Computes the heuristic value of a game state for a specific player, combining multiple 
    evaluation criteria using weighted scores.

    Arguments:
        +GameState: The current state of the game, including the board and other relevant parameters.
        +Player: The player for whom the evaluation is being computed.
        -Value: The resulting heuristic value, a float between -1 and 1, representing the favorability of 
                the game state for the specified player.

    Details:
        - The heuristic combines two components: the number of pieces advantage and the path quality advantage.
        - Each component is weighted using predefined weights (`numberPieces` and `pathQuality`), retrieved 
          using the `weight/2` predicate.
*/

value(GameState, Player, Value):-
    change_player(Player, Other),
    weight(numberPieces, PiecesWeight),
    weight(pathQuality, PathWeight),

    % Calculate the score for each player
    score(GameState, Player, Pieces1, Path1),
    score(GameState, Other, Pieces2, Path2),

    epsilon(SmallValue),
    % Normalize each component
    PathScore is (Path1 - Path2) / (Path1 + Path2 + SmallValue),
    PiecesScore is (Pieces1 - Pieces2) / (Pieces1 + Pieces2 + SmallValue),
    % Apply the weight to each one
    Value is PathScore * PathWeight + PiecesWeight * PiecesScore.

score([Board, _, BlocksLeft, _, _], Player, PiecesScore, PathScore):-
    count_squares(Board, Player, PiecesScore),
    paths_quality(Board, Player, PathScore).


/*
    count_squares(+Board, +Player, -Amount)
    Counts the number of squares on the board occupied by a specific player.

    Arguments:
        +Board: The current state of the board.
        +Player: The player whose squares are being counted.
        -Amount: The total number of squares occupied by the specified player.
*/
% Flateness the Board before calculating and retriving the number of squares
count_squares(Board, Player, Amount):- 
    scanlist(append, Board, [], Flatten),
    count_squares(Flatten, Player, Amount, 0).

/*
    count_squares(+Flatten, +Player, -Amount, +Acc)
    Helper predicate to recursively count the squares owned by the player.

    Arguments:
        +Flatten: A flattened list of all squares of the board.
        +Player: The player whose squares are being counted.
        -Amount: The total number of squares occupied by the specified player.
        +Acc: An accumulator to keep track of the count during recursion.
*/
% Base Case: Reached the End of List
count_squares([], _, Amount, Acc):- Amount = Acc, !.
% Recursive Case: square belongs to player
count_squares([[_, Player] | Rest], Player, Amount, Acc):-
    NewAcc is Acc + 1,
    count_squares(Rest, Player, Amount, NewAcc), !.
% Recursive Case: square does not belong to player
count_squares([_ | Rest], Player, Amount, Acc):-
    count_squares(Rest, Player, Amount, Acc).


/*
    path_score(+PathWithScore, -FinalScore)
    Adjusts the score of a path based on its size.

    Arguments:
        +PathWithScore: A pair [Path, Score], where Path is a list of positions and Score is its initial score.
        -FinalScore: The adjusted score of the path.
*/
path_score([Path, Score], Final):-
    length(Path, Size),
    Final is Score * (0.50 + (0.15 * Size)).

/*
    paths_quality(+Board, +Player, +Paths, -LimitedScore)
    Computes the overall quality of paths for a given player.

    Arguments:
        +Board: The current state of the board.
        +Player: The player for whose paths' quality is being evaluated.
        -LimitedScore: The aggregated score of all paths.
*/
paths_quality(Board, Player, LimitedScore):-
    % Calculate paths and their score
    get_paths(Board, [1, 1], Player, [], PathList),
    % Get final score of each path
    maplist(path_score, PathList, FinalScores),
    % Sum all scores
    sumlist(FinalScores, LimitedScore).
    

/*
    get_paths(+Board, +StartPos, +Player, +Visited, -Paths)
    Finds all valid paths on the board for a given player starting from a specified position.

    Arguments:
        +Board: The current state of the board.
        +StartPos: The starting position [X, Y] on the board.
        +Player: The player for whose paths are being evaluated.
        +Visited: A list of already visited positions to avoid revisiting.
        -Paths: A list of paths, each represented as [Path, PathScore].
*/

% Base Case: Reached End of Board
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
    % Get Path Starting at given position
    get_path(Board, [XPos, YPos], Player, Visited, Path, PathScore),
    % Update Starting position
    NewXPos is XPos + 1,
    % Update total visited 
    append(Visited, Path, NewVisited),
    % Recursively get others paths
    get_paths(Board, [NewXPos, YPos], Player, NewVisited, IndividualScore).



/*
    get_path(+Board, +Position, +Player, +Visited, -Path, -Score)
    Computes a single valid path starting from a specific position.

    Arguments:
        +Board: The current state of the board.
        +Position: The starting position [X, Y] on the board.
        +Player: The player for whose path is being evaluated.
        +Visited: A list of already visited positions.
        -Path: The computed path as a list of positions.
        -Score: The score of the computed path.
    Description:
        Recursively explore neighboring squares, evaluating the feasible path while 
        accumulating points based on the direction of movement and the type of squares.
*/

% Base Case: Not valid position
get_path(Board, [XPos, YPos], Player, Visited, [], 0):-
    length(Board, Size),
    ( 
    XPos < 1;
    XPos > Size;
    YPos < 1;
    YPos > Size
    ).

% Base Case: Already visited square
get_path(Board, [XPos, YPos], Player, Visited, [], 0):-
    member([XPos, YPos], Visited), !.

% Base Case: Square not belonging to player
get_path(Board, [XPos, YPos], Player, Visited, [], 0):-
    change_player(Player, Other),
    get_square(Board, [XPos, YPos], [_, Color]),
    (Color == Other; Color == null), !.

% Recursive Case
get_path(Board, [XPos, YPos], Player, Visited, FinalPath, Value):-

    % Mark as visited
    UpdatedVisited = [[XPos, YPos] | Visited],
    Path = [[XPos, YPos]],

    % Calculate Extra Height Points
    height_square_points(Board, [XPos, YPos], HeightPoints),

    XPosLeft is XPos - 1,
    XPosRight is XPos + 1,
    YPosDown is YPos - 1,
    YPosUp is YPos + 1,

    % Recursively call get_path for all directions, considering the squares visited

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

    % Consider Extra Points based on direction followed
    path_direction_extra_points(Player, PathUp, PathDown, PathLeft, PathRight, ExtraPoints),
    % Sum up all the Points
    Value is ValueDown + ValueLeft + ValueRight + ValueUp + ExtraPoints + HeightPoints, !.


/*
    path_direction_extra_points(+Player, +PathUp, +PathDown, +PathLeft, +PathRight, -ExtraPoints)
    Computes extra points based on path directions.

    Arguments:
        +Player: The player for whose extra points are computed.
        +PathUp, +PathDown, +PathLeft, +PathRight: Paths in specific directions.
        -ExtraPoints: The computed extra points.
*/

% Extra points given to paths following the desired direction
path_extra_points([], 0).  
path_extra_points(_, 100).

path_direction_extra_points(white, PathUp, PathDown, _, _, ExtraPoints):-
    path_extra_points(PathUp, UpExtra),
    path_extra_points(PathDown, DownExtra),
    ExtraPoints is UpExtra + DownExtra.

path_direction_extra_points(black, _, _, PathRight, PathLeft, ExtraPoints):-
    path_extra_points(PathRight, RigthExtra),
    path_extra_points(PathLeft, LeftExtra),
    ExtraPoints is LeftExtra + RigthExtra.



/*
    height_square_points(+Board, +Position, -Points)
    Computes the height-based points for a specific square.

    Arguments:
        +Board: The current state of the board.
        +Position: The position [X, Y] of the square.
        -Points: The computed height-based points.
*/

% Square cannot be overwritten
height_square_points(Board, [XPos, YPos], 1):-
    get_square(Board, [XPos, YPos], [Height, Color]),
    length(Board, Size),
    Mid is Size // 2,
    TopHeight is min( XPos - (2 * XPos - 2 * Mid - 1) * ((XPos - 1) // Mid), YPos - (2 * YPos - 2 * Mid - 1) * ((YPos - 1) // Mid)),
    Height = TopHeight,
    !.

% Points based on how many pieces are necessary until square can be overwritten 
height_square_points(Board, [XPos, YPos], Points):-
    get_square(Board, [XPos, YPos], [Height, Color]),
    % Get side squares
    get_surrounding_squares(Board, [XPos, YPos], [[H1,_], [H2,_], [H3,_], [H4,_]]),
    include(>(Height), [H1, H2, H3, H4], SquaresLower),
    length(SquaresLower, Number),
    Points is Number / 3, !. % Value is either 1 or 2
