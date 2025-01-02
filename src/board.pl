
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
difficulty_map(2, hard).



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


    
