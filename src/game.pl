:- consult('view').
:- consult('analysis').
:- consult('board').
:- use_module(library(system)).

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
- Board     

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
    game_loop(GameOptions, GameState), !.



initial_state(GameConfig, GameOptions, GameState):-
    start_menu(GameConfig),
    get_state(GameConfig, GameOptions, GameState).
    

get_state([Type, Difficulty, BoardSize, StartingSquare], [Type, Difficulty], [Board, white, BlocksNumber, ValidMoves, [[1, 2], 1]]):-
    create_board(BoardSize, StartingSquare, Board, BlocksNumber),
    valid_moves(Board, ValidMoves).


game_over([Board, _, Blocks, _, _], _, Winner, _):-
    write('Thinking about winning\n'),
    get_winner(Board, Blocks, Winner), !.


% Check game over
game_loop(_, GameState):-
    game_over(GameState, _, Winner, _), !,
    end_screen(Winner).

end_screen(Winner).

game_loop(GameOptions, GameState):-
    [Board, Player, Blocks, ValidMoves, Selected] = GameState,
    [Type, Level] = GameOptions,
    display_game(GameOptions, GameState),
    write(GameOptions),
    nl,
    write(GameState),
    nl,
    make_move(GameOptions, GameState, Move),
    move(GameState, Move, NewGameState),
    game_loop(GameOptions, NewGameState).


% REVIEW
display_game(GameOptions, GameState):-
    % display_title(),
    % clear_screen,
    display_board(GameState).
    % show_evaluation(). % PUT IN GAMESTATE IF EVALUATION IS SHOWN OR NOT

display_title(GameOptions, GameState).




make_move(['CvC', [Difficulty, _]], [Board, white, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, white, BlocksLeft, ValidMoves, _], Difficulty, Move).
make_move(['CvC', [_, Difficulty]], [Board, black, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, black, BlocksLeft, ValidMoves, _], Difficulty, Move).

make_move(['CvP', [Difficulty]], [Board, white, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, white, BlocksLeft, ValidMoves, _], Difficulty, Move).
make_move(['PvC', [Difficulty]], [Board, black, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, black, BlocksLeft, ValidMoves, _], Difficulty, Move).

make_move(GameOptions, [Board, _, _, ValidMoves, _], Move):-
    get_player_move([Board, _, _, ValidMoves, _], Move).


player_move(w, moveUp).
player_move(s, moveDown).
player_move(a, moveLeft).
player_move(d, moveRight).
player_move(q, rotateLeft).
player_move(e, rotateRight).
player_move('\n', makeMove).

get_player_move(_, Move):-
    peek_char(Input),
    player_move(Input, Move),
    skip_line.



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

move(GameState, rotateRight, [Board, Player, Blocks, ValidMoves, [Position, NewRotation]]):-
    [Board, Player, Blocks, ValidMoves, [Position, Rotation]] = GameState,
    NewRotation is (Rotation mod 4 + 1).

move(GameState, rotateLeft, [Board, Player, Blocks, ValidMoves, [Position, NewRotation]]):-
    [Board, Player, Blocks, ValidMoves, [Position, Rotation]] = GameState,
    NewRotation is ((Rotation - 2) mod 4 + 1).

move(GameState, makeMove, NewGameState):-
    [Board, Player, Blocks, ValidMoves, Move] = GameState,
    move(GameState, Move, NewGameState).
    
move(GameState, [Position, Rotation], [NewBoard, NewPlayer, NewBlocks, NewValidMoves, Selected]):-
    [Board, Player, Blocks, ValidMoves, Selected] = GameState,
    member(Position, ValidMoves), % Validate Move
    put_block(Board, Position, Rotation, NewBoard), % Make Move
    valid_moves(NewBoard, NewValidMoves), % Redo validMoves generation
    change_player(Player, NewPlayer), % Alternate player
    NewBlocks is Blocks - 1. % Reduce number of total blocks
    
move(GameState, _, GameState).









