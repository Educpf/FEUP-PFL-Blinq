:- consult('view').
:- consult('analysis').

% TODO

/*

    MENUS 
- Get Difficulty when CvC --- X
- Get starting square --- X
- Start menu option 4

    UI
- End screen -- X
- Evaluation
- Game Title
- Board                -- EDU

   MOVES
- Choose move ( AI )   -- EDU
- Value                -- EDU
- Select Move --- X
- getWinner -- X 

*/



% The Game
play:-
    % Menus
    initial_state(GameConfig, GameOptions, GameState),
    % Game Cycle
    game_loop(GameOptions, GameState).



initial_state(GameConfig, GameOptions, GameState):-
    start_menu(GameConfig),
    get_state(GameConfig, GameOptions, GameState).
    

get_state([Type, Difficulty, BoardSize, StartingSquare], [Type, Difficulty], [Board, white, BlocksNumber, ValidMoves, [[1, 2], 1]]):-
    create_board(BoardSize, StartingSquare, Board, BlocksNumber),
    valid_moves(Board, ValidMoves).


game_over(GameState, Winner):-
    get_winner(Board, Winner).

% Check game over
game_loop(_, GameState):-
    game_over(GameState, Winner),
    end_screen(Winner), !.

game_loop(GameOptions, GameState):-
    [Board, Player, Blocks, ValidMoves, Selected] = GameState,
    [Type, Level] = GameOptions,
    display_game(GameOptions, GameState),
    make_move(GameOptions, GameState, Move),
    move(GameState, Move, NewGameState),
    game_loop(GameOptions, NewGameState).


% REVIEW
display_game(GameOptions, GameState):-
    display_title(),
    display_game(GameState),
    show_evaluation(). % PUT IN GAMESTATE IF EVALUATION IS SHOWN OR NOT

display_title(GameOptions, GameState):-




make_move(['CvC', [Difficulty, _]], [Board, white, _, ValidMoves, _], Move):-
    choose_move([Board, white, _, ValidMoves, _], Difficulty, Move).
make_move(['CvC', [_, Difficulty]], [Board, black, _, ValidMoves, _], Move):-
    choose_move([Board, black, _, ValidMoves, _], Difficulty, Move).

make_move(['CvP', [_, Difficulty]], [Board, white, _, ValidMoves, _], Move):-
    choose_move([Board, white, _, ValidMoves, _], Difficulty, Move).
make_move(['PvC', [_, Difficulty]], [Board, black, _, ValidMoves, _], Move):-
    choose_move([Board, black, _, ValidMoves, _], Difficulty, Move).

make_move(GameOptions, [Board, _, _, ValidMoves, _], Move):-
    get_player_move([Board, _, _, ValidMoves, _], Move).


move(GameState, moveUp, [Board, Player, Blocks, ValidMoves, [[XPos, NewYPos], Rotation]]):-
    [Board, Player, Blocks, ValidMoves, [[XPos, YPos], Rotation]] = GameState,
    length(Board, BoardLenght),
    YPos < BoardLenght,
    NewYPos is YPos + 1.

move(GameState, moveDown, [Board, Player, Blocks, ValidMoves, [[XPos, NewYPos], Rotation]]):-
    [Board, Player, Blocks, ValidMoves, [[XPos, YPos], Rotation]] = GameState,
    YPos > 2 ,
    NewYPos is YPos - 1.

move(GameState, moveRight, [Board, Player, Blocks, ValidMoves, [[NewXPos, YPos], Rotation]]):-
    [Board, Player, Blocks, ValidMoves, [[XPos, YPos], Rotation]] = GameState,
    length(Board, BoardLenght),
    XPos < BoardLenght - 1,
    NewXPos is XPos + 1.

move(GameState, moveLeft, [Board, Player, Blocks, ValidMoves, [[NewXPos, YPos], Rotation]]):-
    [Board, Player, Blocks, ValidMoves, [[XPos, YPos], Rotation]] = GameState,
    XPos > 1,
    NewXPos is XPos - 1.

move(GameState, rotateRight, [Board, Player, Blocks, ValidMoves, [[NewXPos, YPos], NewRotation]]):-
    [Board, Player, Blocks, ValidMoves, [[XPos, YPos], Rotation]] = GameState,
    NewRotation is (Rotation mod 4 + 1).

move(GameState, rotateLeft, [Board, Player, Blocks, ValidMoves, [[XPos, YPos], NewRotation]]):-
    [Board, Player, Blocks, ValidMoves, [[XPos, YPos], Rotation]] = GameState,
    NewRotation is ((Rotation - 2) mod 4 + 1).

move(GameState, makeMove, NewGameState):-
    [Board, Player, Blocks, ValidMoves, Move] = GameState,
    move(GameState, Move, NewGameState).
    
move(GameState, [Position, Rotation], [NewBoard, NewPlayer, NewBlocks, NewValidMoves, Selected]):-
    [Board, Player, Blocks, ValidMoves, _] = GameState,
    member(Position, ValidMoves), % Validate Move
    put_block(Board, Position, Rotation, NewBoard), % Make Move
    valid_moves(NewBoard, NewValidMoves), % Redo validMoves generation
    change_player(Player, NewPlayer), % Alternate player
    NewBlocks is Blocks - 1. % Reduce number of total blocks
    
move(GameState, _, GameState).





value(GameState, Player, Value).



% Calculate what is the best move
choose_move(GameState, 1, Move).

choose_move(GameState, 2, Move).


