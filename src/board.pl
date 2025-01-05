
:- use_module(library(lists)).
:- use_module(library(between)).

:-consult('utils').
% General Board utility predicates

% Block Rotation to Colors
normal_block_colors([white, black, black, white]).
valid_select_colors([valid_white, valid_black, valid_black, valid_white]).
unvalid_select_colors([invalid_white, invalid_black, invalid_black, invalid_white]).

% Alternate player
change_player(white, black).
change_player(black, white).

% Difficulty mapping
difficulty_map(1, easy).
difficulty_map(2, medium).
difficulty_map(3, hard).


/*
    create_board(+Size, +StartPos, -Board, -Score)
    Creates a board based on the given size and starting position, and assigns a score.
    
    Arguments:
        +Size: The size of the board (either 4 or 5).
        +StartPos: The starting position [Xpos, Ypos] on the board where the main action begins.
        -Board: The resulting list representing the game board.
        -TotalPieces: The number of pieces associated with the created board (either 30 or 54 based on the Size).
*/

% Creates a 4x4 board, without any starting piece.
create_board(4, _, Board, 30):-
    length(Board, 8),
    length(Line, 8),
    maplist(=([0, null]), Line),
    maplist(=(Line), Board).

% Creates a 5x5 board with a starting piece in a specific location
create_board(5, [Xpos, Ypos], Board, 54):-
    length(Line, 10),
    maplist(=([0, null]), Line),
    
    % Board above starting piece
    TopSize is 10 - Ypos,
    length(TopBoard, TopSize),
    maplist(=(Line), TopBoard),

    % Board below starting piece
    BottomSize is Ypos - 2,
    length(BottomBoard, BottomSize),
    maplist(=(Line), BottomBoard),

    % Lines containing starting piece
        % Constructing "Paddding"
    LeftPadSize is Xpos - 1,
    length(LeftPad, LeftPadSize),
    RightPadSize is 9 - Xpos,
    length(RightPad, RightPadSize),

    maplist(=([0, null]), LeftPad),
    maplist(=([0, null]), RightPad),

    Middle1 = [[1, white],[1, black] | RightPad],
    Middle2 = [[1, black],[1, white] | RightPad],
    append(LeftPad, Middle1, Line1),
    append(LeftPad, Middle2, Line2),

    % Join everything together
    append(BottomBoard, [Line2, Line1 | TopBoard], Board).








% MOVE VALIDATION

/*
    possible_moves(+ValidMoves, -PossibleMoves)
    Transforms a list of valid moves into a list of possible moves with rotation variation.

    Arguments:
        +ValidMoves: A list of valid moves.
        -PossibleMoves: The resulting list of possible moves with variations added.
*/   

% Auxiliar predicate to add the four possible variations of a move into a List
add_moves(Move, List, [[Move, 1], [Move, 2], [Move, 3], [Move, 4] | List]).

possible_moves(ValidMoves, PossibleMoves):-
    scanlist(add_moves, ValidMoves, [], PossibleMoves).



/*
    valid_moves(+Board, -ValidMoves)
    Generates a list of valid moves on the board based on the board state.
    
    Arguments:
        +Board: The current state of the board.
        -ValidMoves: A list of valid positions [XPos, YPos] where a move can be made.
*/
valid_moves(Board, ValidMoves):-
    length(Board, BoardLenght),
    % Check validity of each position
    findall([XPos, YPos], (between(1, BoardLenght, XPos),
                           between(1, BoardLenght, YPos),
                           validate_move(Board, [XPos, YPos])),
            ValidMoves).

/*
    validate_move(+Board, +Position)
    Checks whether a given position on the board is a valid move.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the coordinates of the move to be validated.
    
    Description:
        This predicate validates if the position [XPos, YPos] on the board satisfies the following conditions:
        1. The position is within the bounds of the board.
        2. All squares below the position are leveled (height-wise).
        3. The position satisfies the pyramidal structure of the board
        
        The predicate returns true if all conditions are satisfied, otherwise, false.
*/
validate_move(Board, [XPos, YPos]):-
    length(Board, BoardLenght),

    % Basic Validation ( check bounds )
    XPos >= 1,
    MaxX is BoardLenght - 1, XPos =< MaxX,
    YPos >= 2,
    YPos =< BoardLenght,

    % Check if all squares underneath are leveled
    check_height_level(Board, [XPos, YPos], Height),

    % Check pyramidal structue
    MaxSquare is (BoardLenght // 2) - Height,

    ((XPos - Height) mod 2 ) =:= 1,
    Xsquare is (XPos - 1 - Height) // 2,
    Xsquare >= 0,
    Xsquare < MaxSquare,
    ((YPos - Height) mod 2) =:= 0,
    Ysquare is (YPos - Height) // 2,
    Ysquare >= 1,
    Ysquare =< MaxSquare.



/*
    check_height_level(+Board, +Position, -Height)
    Checks if the height of the squares of a block piece, under a given position on the board is consistent.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the coordinates of the block to check.
        -Height: The height of the square at the given position, used for validation.
*/
check_height_level(Board, [XPos, YPos], Height):-

    YPos2 is YPos - 1,
    XPos2 is XPos + 1,
    get_square(Board, [XPos, YPos], [Height1, _]),
    get_square(Board, [XPos, YPos2], [Height2, _]),
    get_square(Board, [XPos2, YPos], [Height3, _]),
    get_square(Board, [XPos2, YPos2], [Height4, _]),
    Height1 = Height2,
    Height2 = Height3,
    Height3 = Height4,
    !,
    Height = Height1.



/*
    get_square(+Board, +Position, -Value)
    Retrieves the value of a square on the board at a given position.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the coordinates of the square to retrieve.
        -Value: The value at the given position [XPos, YPos] on the board, a list [Height, Color].
*/
get_square(Board, [XPos, YPos], Value):-
    nth1(YPos, Board, Line),
    nth1(XPos, Line, Value).

/*
    get_surrounding_squares(+Board, +Position, -Squares)
    Retrieves the values of the squares surrounding a given position on the board.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the coordinates of the square to check.
        -Squares: A list containing the values of the four surrounding squares in the order [Left, Right, Up, Down].
*/
get_surrounding_squares(Board, [XPos, YPos], [Square1, Square2, Square3, Square4]):-
    XPosLeft is XPos - 1,
    XPosRight is XPos + 1,
    YPosDown is YPos - 1,
    YPosUp is YPos + 1,
    get_square(Board, [XPosLeft, YPos], Square1), % Left
    get_square(Board, [XPosRight, YPos], Square2), % Right
    get_square(Board, [XPos, YPosUp], Square3), % Up
    get_square(Board, [XPos, YPosDown], Square4). %Down

/*
    put_square(+Board, +Position, +Color, -NewBoard)
    Places a square with a given color on the board at a specific position.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the position of the square to place.
        +Color: The color to set for the square at the given position.
        -NewBoard: The new board with the updated square at the specified position.
*/
put_square(Board, [XPos, YPos], Color, NewBoard):-
    nth1(YPos, Board, Line, WithoutLine),
    nth1(XPos, Line,  [Height, _], WithoutElem),
    NewHeight is Height + 1,
    nth1(XPos, NewLine, [NewHeight, Color], WithoutElem),
    nth1(YPos, NewBoard, NewLine, WithoutLine).

/*
    put_squares(+Board, +Position, +Colors, -NewBoard)
    Places a set of squares of a block on the board at a specific position with given colors.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the position of the block, the top-left square.
        +Colors: A list of four colors representing the squares to be placed at the position [TopLeft, TopRight, BottomRight, BottomLeft].
        -NewBoard: The new board with the updated squares.
    
*/
put_squares(Board, [XPos, YPos], [TopL, TopR, BotR, BotL], NewBoard):-
    YPos2 is YPos - 1,
    XPos2 is XPos + 1,
    put_square(Board, [XPos, YPos], TopL, Temp1),
    put_square(Temp1,[XPos2, YPos], TopR, Temp2),
    put_square(Temp2, [XPos, YPos2], BotL, Temp3),
    put_square(Temp3, [XPos2, YPos2], BotR, NewBoard).


/*
    put_block(+Board, +Position, +Rotation, -NewBoard)
    Places a block on the board at a specific position with a given rotation.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the position to place the block.
        +Rotation: The rotation amount (1, 2, 3, 4) indicating how the block should be oriented.
        -NewBoard: The new board with the placed block.
    
*/
% Puts block on the board ( Does not verify validity ) **REVIEW**
put_block(Board, [XPos, YPos], Rotation, NewBoard):-
    % Get block colors
    normal_block_colors(NormalBlock),
    RotateAmount is 1 - Rotation,
    % Get Colors in correct order
    rotate_list(RotateAmount, NormalBlock, Colors),
    % Update the Board 
    put_squares(Board, [XPos, YPos], Colors, NewBoard).

/*
    put_selected_block(+Board, +Position, +Rotation, -NewBoard)
    Places a selected block on the board at a specific position with a given rotation, considering if its valid or not.
    
    Arguments:
        +Board: The current state of the board.
        +Position: A list [XPos, YPos] representing the position to place the selected block.
        +Rotation: The rotation amount (1, 2, 3, 4) indicating how the block should be oriented.
        -NewBoard: The new board with the placed selected block.
    
*/

% Move is valid 
put_selected_block(Board, [XPos, YPos], Rotation, NewBoard):-
    % Move validation
    validate_move(Board, [XPos, YPos]),
    % Get block colors
    valid_select_colors(SelectColors),
    RotateAmount is 1 - Rotation,
    % Get Colors in correct order
    rotate_list(RotateAmount, SelectColors, Colors),
    % Update the Board
    put_squares(Board, [XPos, YPos], Colors, NewBoard), !.

% Move is invalid
put_selected_block(Board, [XPos, YPos], Rotation, NewBoard):-
    % Get block colors
    unvalid_select_colors(SelectColors),
    RotateAmount is 1 - Rotation,
    % Get Colors in correct order
    rotate_list(RotateAmount, SelectColors, Colors),
    % Change the squares
    put_squares(Board, [XPos, YPos], Colors, NewBoard).



/*
    get_winner(+Board, +Player, -Winner)
    Determines the winner of the game based on the current state of the board.
    
    Arguments:
        +Board: The current state of the board.
        +MovesLeft: The ammount of moves left
        -Winner: The player who won the game, or 'draw' if the game ended in a draw.
    
    Description:
        This predicate checks if the game reached the end, either if there are 0 moves left (draw),
        or either 'white' or 'black' have a valid path across the board.
        If board has no winner and are moves left the predicate fails
*/

% 0 moves left, draw
get_winner(Board, 0, draw).

% Check white paths
get_winner(Board, _, white):-
    find_complete_paths(Board, white, 1, [], ReachedEnd),
    or(ReachedEnd, true).

% Check black paths
get_winner(Board, _, black):-
    find_complete_paths(Board, black, 1, [], ReachedEnd),
    or(ReachedEnd, true).


/*
    find_complete_paths(+Board, +Player, +Position, +Visited, -ReachedEnd)
    Searches for a valid path for the specified player starting from the given position.
    
    Arguments:
        +Board: The current state of the board.
        +Player: The player to check for (either 'white' or 'black').
        +Position: The current position to start the search from.
        +Visited: A list of previously visited positions to avoid revisiting squares.
        -ReachedEnd: A list containing a boolean indicating whether a valid path was found.
    
    Description:
        This predicate recursively checks for a valid path for the player, exploring the board from
        the current position. The search is limited to valid squares of the appropriate color.
        If a valid path is found, `ReachedEnd` is set to true; otherwise, it is false.
*/
% Base Case: No more paths to check
find_complete_paths(Board, Player, Position, Visited, []):-
    length(Board, Size), 
    Position is Size + 1, !.

find_complete_paths(Board, black, Position, Visited, [Found | Rest]):-
    % Find Path
    complete_path(Board, black, [1, Position], Visited, NewVisited, Found),
    % Update Position and Visited 
    NewPosition is Position + 1,
    append(Visited, NewVisited, UpdatedVisited),
    % Recursively find more paths
    find_complete_paths(Board, black, NewPosition, UpdatedVisited, Rest).

find_complete_paths(Board, white, Position, Visited, [Found | Rest]):-
    % Find Path
    complete_path(Board, white, [Position, 1], Visited, NewVisited, Found),
    % Update Position and Visited 
    NewPosition is Position + 1,
    append(Visited, NewVisited, UpdatedVisited),
    % Recursively find more paths
    find_complete_paths(Board, white, NewPosition, UpdatedVisited, Rest).




/*
    complete_path(+Board, +Player, +Position, +Visited, -NewVisited, -FoundPath)
    Recursively searches for a valid path for the player from the given position.
    
    Arguments:
        +Board: The current state of the board.
        +Player: The player to check for (either 'white' or 'black').
        +Position: The current position to search from.
        +Visited: A list of positions that have already been visited.
        -NewVisited: A list of visited positions after processing the current position.
        -FoundPath: Boolean indicating whether a valid path was found (true or false).
    
    Description:
        This predicate checks if the player can complete a path from the current position. The search
        explores all four directions (up, down, left, right). The recursion continues until either a valid
        path is found or all directions are explored.
*/


/*
    check_end(+Board, +Player, +Position)
    Checks if the player has reached the end of the board at the given position.
    
    Arguments:
        +Board: The current state of the board.
        +Player: The player to check for (either 'white' or 'black').
        +Position: The current position to check.
    
*/
% White, check YPos
check_end(Board, white, [_, YPos]):-
    length(Board, Size),
    YPos > Size.
% Black, check XPos
check_end(Board, black, [XPos, _]):-
    length(Board, Size),
    XPos > Size.

% Check if end is reached
complete_path(Board, Type,[XPos, YPos], Visited, [], true):-
    check_end(Board, Type, [XPos, YPos]), !.

% Base Case: position outside boundaries
complete_path(Board, Type, [XPos, YPos], Visited, [], false):-
    % Check Bounds
    length(Board, Size),
    ( 
    XPos < 1;
    XPos > Size;
    YPos < 1;
    YPos > Size
    ), !.

% Base Case: square with invalid color
complete_path(Board, Type, Position, Visited, [], false):-
    % Check Color
    change_player(Type, Other),
    get_square(Board, Position, [_, Color]),
    (Color == Other; Color == null), !.


% Base Case: square already visited
complete_path(Board, _, Position, Visited, [], false):-
    % Check if not visited
    member(Position, Visited), !.

% Recursive Case: Explore all directions
complete_path(Board, Type, [XPos, YPos], Visited, FinalNewVisited, FoundPath):-
    
    % Update visited
    UpdatedVisited = [[XPos, YPos] | Visited],
    NewVisited = [[XPos, YPos]],

    % Try following paths to all direction
    XPosLeft is XPos - 1,
    XPosRight is XPos + 1,
    YPosDown is YPos - 1,
    YPosUp is YPos + 1,

    % Consider the Squares visited by each direction path

    complete_path(Board, Type, [XPos, YPosUp], UpdatedVisited, VisitedUp, FoundUp),
    append(UpdatedVisited, VisitedUp, TempVisited1),
    append(NewVisited, VisitedUp, TempNewVisited1),

    complete_path(Board, Type, [XPosRight, YPos], TempVisited1, VisitedRight, FoundRight),
    append(TempVisited1, VisitedRight, TempVisited2),
    append(TempNewVisited1, VisitedRight, TempNewVisited2),

    complete_path(Board, Type, [XPosLeft, YPos], TempVisited2, VisitedLeft, FoundLeft),
    append(TempVisited2, VisitedLeft, TempVisited3),
    append(TempNewVisited2, VisitedLeft, TempNewVisited3),

    complete_path(Board, Type, [XPos, YPosDown], TempVisited3, VisitedDown, FoundDown),
    append(TempNewVisited3, VisitedDown, FinalNewVisited),

    % Checks wether some path found the end
    or([FoundUp , FoundRight , FoundDown , FoundLeft], FoundPath), !.

    
