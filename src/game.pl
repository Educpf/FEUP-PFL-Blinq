:- consult('view').
:- consult('analysis').
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
    

get_state([Type, Difficulty, BoardSize, StartingSquare, Name1, Name2], [Type, Difficulty, Name1, Name2], [Board, white, BlocksNumber, ValidMoves, [[1, 2], 1]]):-
    create_board(BoardSize, StartingSquare, Board, BlocksNumber),
    valid_moves(Board, ValidMoves).


game_over([Board, Player, Blocks, _, _], GameOptions, Winner, WinnerName):-
    get_winner(Board, Winner),
    %get_winner_name(GameOptions, Winner, WinnerName),
    !.

game_over([_, white, -1, _, _], [_, _, Name1, Name2], Winner, WinnerName) :-
    WinnerName = Name2,
    Winner = 'black'.

game_over([_, black, -1, _, _], [_, _, Name1, Name2], Winner, WinnerName) :-
    WinnerName = Name1,
    Winner = 'white'.


get_winner(_, _):-fail.


get_winner_name([_, _, Name1, Name2], white, Name1).

get_winner_name([_, _, Name1, Name2], black, Name2).


% Check game over
game_loop(GameOptions, GameState):-
    game_over(GameState, GameOptions, Winner, WinnerName),
    display_endGame(Winner, WinnerName), !.

game_loop(GameOptions, GameState):-
    [Board, Player, Blocks, ValidMoves, Selected] = GameState,
    display_game(GameOptions, GameState),
    write(GameOptions),
    nl,
    write(GameState),
    nl,
    make_move(GameOptions, GameState, Move),
    move(GameState, Move, NewGameState),
    game_loop(GameOptions, NewGameState).



make_move(['CvC', [Difficulty, _]], [Board, white, _, ValidMoves, _], Move):-
    choose_move([Board, white, _, ValidMoves, _], Difficulty, Move).
make_move(['CvC', [_, Difficulty]], [Board, black, _, ValidMoves, _], Move):-
    choose_move([Board, black, _, ValidMoves, _], Difficulty, Move).

make_move(['CvP', [Difficulty]], [Board, white, _, ValidMoves, _], Move):-
    choose_move([Board, white, _, ValidMoves, _], Difficulty, Move).
make_move(['PvC', [Difficulty]], [Board, black, _, ValidMoves, _], Move):-
    choose_move([Board, black, _, ValidMoves, _], Difficulty, Move).

make_move(GameOptions, [Board, _, _, ValidMoves, _], Move):-
    get_player_move(Board, Move).


player_move(w, moveUp).
player_move(s, moveDown).
player_move(a, moveLeft).
player_move(d, moveRight).
player_move(q, rotateLeft).
player_move(e, rotateRight).
player_move(c, makeMove).
player_move(p,quit).
player_move(_,invalid).


get_player_move(Board, Move):-
    length(Board,L),
    M1 is L-1,
    peek_char(Input),
    char_code(Input, Code),
    between(48, 57, Code),
    read_position(1,M1,2,L,PosX, PosY, Rot), 
    Move = [select,[PosX,PosY],Rot],          
    !.

get_player_move(Board, Move):-
    peek_char(Input),
    player_move(Input, Move),
    Move \= invalid,
    skip_line,
    !.

get_player_move(Board, Move):-
    skip_line,
    write('Invalid\n'),
    get_player_move(Board,Move).



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

move([Board, Player, Blocks, ValidMoves, Selected], quit, [Board, Player, -1, ValidMoves, Selected]).

move([Board, Player, Blocks, ValidMoves, _],[select,Position,Rotation], [Board, Player, Blocks, ValidMoves, [Position, Rotation]]).

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





value(GameState, Player, Value).




