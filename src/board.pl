
:- use_module(library(lists)).
:- use_module(library(between)).

% GENERAL STUFF

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



% BOARD CREATION

create_board(4, _, Board, 30):-
    length(Board, 8),
    length(Line, 8),
    maplist(=([0, null]), Line),
    maplist(=(Line), Board).


create_board(5, [Xpos, Ypos], Board, 54):-
    length(Line, 10),
    maplist(=([0, null]), Line),
    % Above starting square   
    TopSize is 10 - Ypos,
    length(TopBoard, TopSize),
    maplist(=(Line), TopBoard),
    % Below starting square
    BottomSize is Ypos - 2,
    length(BottomBoard, BottomSize),
    maplist(=(Line), BottomBoard),
    % Lines containing square
        % Constructing Pads
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

add_moves(Move, List, [[Move, 1], [Move, 2], [Move, 3], [Move, 4] | List]).
    
possible_moves(ValidMoves, PossibleMoves):-
    scanlist(add_moves, ValidMoves, [], PossibleMoves).

valid_moves(Board, ValidMoves):-
    length(Board, BoardLenght),
    findall([XPos, YPos], (between(1, BoardLenght, XPos),
                           between(1, BoardLenght, YPos),
                           validate_move(Board, [XPos, YPos])),
            ValidMoves).


validate_move(Board, [XPos, YPos]):-
    length(Board, BoardLenght),
    % Basic Validation ( check bounds )
    XPos >= 1,
    MaxX is BoardLenght - 1, XPos =< MaxX,
    YPos >= 2,
    YPos =< BoardLenght,

    % Check if all squares underneath are leveled
    check_height_level(Board, [XPos, YPos], Height),
    % Check if position makes sense
    MaxSquare is (BoardLenght // 2) - Height,

    ((XPos - Height) mod 2 ) =:= 1,
    Xsquare is (XPos - 1 - Height) // 2,
    Xsquare >= 0,
    Xsquare < MaxSquare,
    ((YPos - Height) mod 2) =:= 0,
    Ysquare is (YPos - Height) // 2,
    Ysquare >= 1,
    Ysquare =< MaxSquare.



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




% Board Square change or retrieve


get_square(Board, [XPos, YPos], Value):-
    nth1(YPos, Board, Line),
    nth1(XPos, Line, Value).

get_surrounding_squares(Board, [XPos, YPos], [Square1, Square2, Square3, Square4]):-
    XPosLeft is XPos - 1,
    XPosRight is XPos + 1,
    YPosDown is YPos - 1,
    YPosUp is YPos + 1,
    get_square(Board, [XPosLeft, YPos], Square1), % Left
    get_square(Board, [XPosRight, YPos], Square2), % Right
    get_square(Board, [XPos, YPosUp], Square3), % Up
    get_square(Board, [XPos, YPosDown], Square4). %Down


put_square(Board, [XPos, YPos], Color, NewBoard):-
    nth1(YPos, Board, Line, WithoutLine),
    nth1(XPos, Line,  [Height, _], WithoutElem),
    NewHeight is Height + 1,
    nth1(XPos, NewLine, [NewHeight, Color], WithoutElem),
    nth1(YPos, NewBoard, NewLine, WithoutLine).

put_squares(Board, [XPos, YPos], [TopL, TopR, BotR, BotL], NewBoard):-
    YPos2 is YPos - 1,
    XPos2 is XPos + 1,
    put_square(Board, [XPos, YPos], TopL, Temp1),
    put_square(Temp1,[XPos2, YPos], TopR, Temp2),
    put_square(Temp2, [XPos, YPos2], BotL, Temp3),
    put_square(Temp3, [XPos2, YPos2], BotR, NewBoard).


% Puts block on the board ( Does not verify validity ) **REVIEW**
put_block(Board, [XPos, YPos], Rotation, NewBoard):-
    % Get block colors
    normal_block_colors(NormalBlock),
    RotateAmount is 1 - Rotation,
    rotate_list(RotateAmount, NormalBlock, Colors),
    % Change the squares
    put_squares(Board, [XPos, YPos], Colors, NewBoard).

put_selected_block(Board, [XPos, YPos], Rotation, NewBoard):-
    validate_move(Board, [XPos, YPos]),
    valid_select_colors(SelectColors),
    RotateAmount is 1 - Rotation,
    rotate_list(RotateAmount, SelectColors, Colors),
    % Change the squares
    put_squares(Board, [XPos, YPos], Colors, NewBoard), !.

put_selected_block(Board, [XPos, YPos], Rotation, NewBoard):-
    unvalid_select_colors(SelectColors),
    RotateAmount is 1 - Rotation,
    rotate_list(RotateAmount, SelectColors, Colors),
    % Change the squares
    put_squares(Board, [XPos, YPos], Colors, NewBoard).



% Winner detection

/*

*/
get_winner(Board, 0, draw).

get_winner(Board, _, white):-
    write('Trying white\n'),
    find_complete_paths(Board, white, 1, [], ReachedEnd),
    or(ReachedEnd, true).

get_winner(Board, _, black):-
    write('Trying black\n'),
    find_complete_paths(Board, black, 1, [], ReachedEnd),
    or(ReachedEnd, true).


find_complete_paths(Board, Player, Position, Visited, []):-
    length(Board, Size), 
    Position is Size + 1, !.

find_complete_paths(Board, black, Position, Visited, [Found | Rest]):-
    complete_path(Board, black, [1, Position], Visited, NewVisited, Found),
    NewPosition is Position + 1,
    append(Visited, NewVisited, UpdatedVisited),
    find_complete_paths(Board, black, NewPosition, UpdatedVisited, Rest).

find_complete_paths(Board, white, Position, Visited, [Found | Rest]):-
    complete_path(Board, white, [Position, 1], Visited, NewVisited, Found),
    NewPosition is Position + 1,
    append(Visited, NewVisited, UpdatedVisited),
    find_complete_paths(Board, white, NewPosition, UpdatedVisited, Rest).




check_end(Board, white, [_, YPos]):-
    length(Board, Size),
    YPos > Size.
check_end(Board, black, [XPos, _]):-
    length(Board, Size),
    XPos > Size.

/*

get_winner([[[1,black],[1,black],[1,black],[1,white],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,white],[1,black],[1,white],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[1,black],[1,white],[0,null],[0,null],[1,white],[1,white],[1,white],[1,white]],[[0,null],[0,null],[1,black],[1,white],[0,null],[0,null],[1,black],[1,black],[1,black],[1,black]],[[1,white],[1,black],[1,black],[1,black],[1,black],[1,white],[1,black],[1,white],[0,null],[0,null]],[[1,white],[1,black],[1,white],[1,white],[1,white],[1,black],[1,black],[1,white],[0,null],[0,null]],[[1,black],[1,black],[1,black],[1,black],[1,black],[1,black],[0,null],[0,null],[0,null],[0,null]],[[1,white],[1,white],[1,white],[1,white],[1,white],[1,white],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]],[[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null],[0,null]]], 23, G).

*/
complete_path(Board, Type,[XPos, YPos], Visited, [], true):-
    check_end(Board, Type, [XPos, YPos]), !.

complete_path(Board, Type, [XPos, YPos], Visited, [], false):-
    % Check Bounds
    length(Board, Size),
    ( 
    XPos < 1;
    XPos > Size;
    YPos < 1;
    YPos > Size
    ), !.

complete_path(Board, Type, Position, Visited, [], false):-
    % Check Color
    change_player(Type, Other),
    get_square(Board, Position, [_, Color]),
    (Color == Other; Color == null), !.


complete_path(Board, _, Position, Visited, [], false):-
    % Check if not visited
    member(Position, Visited), !.

complete_path(Board, Type, [XPos, YPos], Visited, FinalNewVisited, FoundPath):-
    
    % Update visited
    UpdatedVisited = [[XPos, YPos] | Visited],
    NewVisited = [[XPos, YPos]],

    % Try following paths to all direction
    XPosLeft is XPos - 1,
    XPosRight is XPos + 1,
    YPosDown is YPos - 1,
    YPosUp is YPos + 1,

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

    or([FoundUp , FoundRight , FoundDown , FoundLeft], FoundPath), !.

or(List, true):-
    member(true, List), !.
or(List, false).
    
