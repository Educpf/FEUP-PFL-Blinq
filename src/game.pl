:- consult('view').
:- consult('analysis').
:- consult('board').
:- use_module(library(system)).

% The Game
play:-
    repeat,
    % Show starting menus in order to get user configuration
    start_menu(GameConfig),
    % Initial GameState
    initial_state(GameConfig, GameOptions, GameState),
    % Game Cycle
    game_loop(GameOptions, GameState), !.


/*
    initial_state(+GameConfig, -GameOptions, -GameState)
    Get the initial game state, based on user configurations.
    
    Arguments:
        +GameConfig: A list with the configurations selected by the user
        -GameOptions: A list representing game options that are constant throught all the game [GameMode, Difficulty, PlayerName1, PlayerName2].
        -GameState: A list representing the current game state [Board, Player, BlocksLeft, ValidMoves, Position].
*/

initial_state([Type, Difficulty, BoardSize, StartingSquare, Name1, Name2], [Type, Difficulty, Name1, Name2], [Board, white, BlocksNumber, ValidMoves, [[1, 2], 1]]):-
    create_board(BoardSize, StartingSquare, Board, BlocksNumber),
    valid_moves(Board, ValidMoves).    


/*
    game_loop(+GameOptions, +GameState)
    Handles the game loop, where it manages the flow of the game

    Arguments:
        +GameOptions: A list representing game options [GameMode, Difficulty, PlayerName1, PlayerName2].
        +GameState: A list representing the current game state [Board, Player, BlocksLeft, ValidMoves, Position].
*/

% Checks game over. If the game is over displays end game screen.
game_loop(GameOptions, GameState):-
    game_over(GameState, GameOptions, Winner, WinnerName),
    display_endGame(GameState, Winner, WinnerName), !, fail.

% If game is not over, handles game logic, like display game, handle moves, make evaluation of current positions.
game_loop(GameOptions, GameState):-
    [Board, Player, Blocks, ValidMoves, Selected] = GameState,
    value(GameState, white, Evaluation),
    display_game(GameOptions, GameState, Evaluation),
    make_move(GameOptions, GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_loop(GameOptions, NewGameState).


/*
    game_over(+GameState, +GameOptions, -Winner, -WinnerName),
    Determines if the game has ended and identifies the winner and their name.

    Arguments:
        +GameState: A list representing the current game state [Board, Player, BlocksLeft, _, _].
        +GameOptions: A list representing game options that are constant throught all the game [_, _, PlayerName1, PlayerName2].
        -Winner: The color of the winning player ('white' or 'black'), or 'draw'.
        -WinnerName: The name of the winning player.

    Note: 
        In case of player surrend, the BlocksLeft has value surrender.
*/

game_over([Board, Player, Blocks, _, _], GameOptions, Winner, WinnerName):-
    get_winner(Board,Blocks, Winner),
    get_winner_name(GameOptions, Winner, WinnerName),
    !.

% White player surrends
game_over([_, white, surrender, _, _], [_, _, Name1, Name2], Winner, WinnerName) :-
    WinnerName = Name2,
    Winner = 'black', !.

% Black player surrends
game_over([_, black, surrender, _, _], [_, _, Name1, Name2], Winner, WinnerName) :-
    WinnerName = Name1,
    Winner = 'white', !.





/*
    get_winner_name(+GameOptions, +Winner, -WinnerName)
    Retrieves the name of the winning player based on the game options and winner color.

    Arguments:
        +GameOptions: A list representing the game options [_, _, PlayerName1, PlayerName2].
        +Winner: The color of the winning player ('white' or 'black'), or 'draw'.
        -WinnerName: The name of the winning player.

*/

% White wins
get_winner_name([_, _, Name1, Name2], white, Name1).

% Black wins
get_winner_name([_, _, Name1, Name2], black, Name2).

% Draw
get_winner_name([_, _, Name1, Name2], draw, '').




/*
    make_move(+GameOptions, +GameState, -Move)
    Determines the move to be made in the current turn based on the game mode and state.

    Arguments:
        +GameOptions: A list representing game options [GameMode, Difficulty, _, _].
        +GameState: A list representing the current game state [Board, Player, BlocksLeft, ValidMoves, _].
        -Move: The chosen move for the current turn.
*/

% Bot move based on bot expertise
make_move(['CvC', [Difficulty, _], _, _], [Board, white, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, white, BlocksLeft, ValidMoves, _], Difficulty, Move).
make_move(['CvC', [_, Difficulty], _, _], [Board, black, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, black, BlocksLeft, ValidMoves, _], Difficulty, Move).

make_move(['CvP', [Difficulty, _], _, _], [Board, white, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, white, BlocksLeft, ValidMoves, _], Difficulty, Move).
make_move(['PvC', [Difficulty, _], _, _], [Board, black, BlocksLeft, ValidMoves, _], Move):-
    choose_move([Board, black, BlocksLeft, ValidMoves, _], Difficulty, Move).

%Player move based on user input
make_move(_, [Board, _, _, _, _], Move):-
    get_player_move(Board, Move).



/*
    get_player_move(+Board, -Move)
    Reads and processes the user's move from the input, allowing either a positional move or an action.

    Arguments:
        +Board: The current state of the game board.
        -Move: The move selected by the user, represented as:
            - [select, [PosX, PosY], Rot]: A positional move with the specified position (PosX, PosY) and rotation (Rot).
            - Action: A pre-defined action based on user input.
*/

% Specified Position Case
get_player_move(Board, Move):-
    write('Your move ==> '),
    length(Board,L),
    M1 is L-1,
    peek_char(Input),
    char_code(Input, Code),
    between(48, 57, Code),
    read_position(1,M1,2,L,PosX, PosY, Rot), 
    Move = [select,[PosX,PosY],Rot],          
    !.

% Action Case
get_player_move(Board, Move):-
    peek_char(Input),
    player_move(Input, Move),
    Move \= invalid,
    skip_line,
    !.

% Invalid Input Case
get_player_move(Board, Move):-
    skip_line,
    write('Invalid\n'),
    get_player_move(Board,Move).


/*
    player_move(+Key, -Action)
    Maps user input keys to specific actions.

    Arguments:
        +Key: The input character provided by the user.
        -Action: The corresponding action for the given key.
*/

player_move(w, moveUp).
player_move(s, moveDown).
player_move(a, moveLeft).
player_move(d, moveRight).
player_move(q, rotateLeft).
player_move(e, rotateRight).
player_move(c, makeMove).
player_move(p,quit).
player_move(_,invalid).


/*
    move(+GameState, +Move, -NewGameState)
    Determines the effect of a move on the current game state and generates the resulting new game state.

    Arguments:
        +GameState: The current game state represented as [Board, Player, Blocks, ValidMoves, Selected].
        +Move: The action to be performed
        -NewGameState: The resulting game state after applying the move.
*/

% Positional Moves
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

% Rotation Moves
move(GameState, rotateRight, [Board, Player, Blocks, ValidMoves, [Position, NewRotation]]):-
    [Board, Player, Blocks, ValidMoves, [Position, Rotation]] = GameState,
    NewRotation is (Rotation mod 4 + 1).

move(GameState, rotateLeft, [Board, Player, Blocks, ValidMoves, [Position, NewRotation]]):-
    [Board, Player, Blocks, ValidMoves, [Position, Rotation]] = GameState,
    NewRotation is ((Rotation - 2) mod 4 + 1).

% Specified Position Move
move([Board, Player, Blocks, ValidMoves, _],[select,Position,Rotation], [Board, Player, Blocks, ValidMoves, [Position, Rotation]]).

% Surrend
move([Board, Player, Blocks, ValidMoves, Selected], quit, [Board, Player, surrender, ValidMoves, Selected]).

% Confirmation Move, updating game state
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
    
% Default Case, retains the current game state
move(GameState, _, GameState).









