:- consult('utils').

menu_to_file(home, './res/home_page.txt').
menu_to_file(player_computer, './res/player_computer.txt').
menu_to_file(board_size, './res/board_size.txt').
menu_to_file(computer_computer, './res/computer_computer.txt').

options_to_file(difficulty, './res/difficulty.txt').
options_to_file(pieces, './res/pieces.txt').


get_game_config(Config):-
    start_menu(Config).


show_menu(HeaderType):-
    clear_screen,
    menu_to_file(HeaderType, Path),
    print_file(Path).

show_menu(HeaderType, OptionsType):-
    write('before clean'),
    clear_screen,
    menu_to_file(HeaderType, Path),
    print_file(Path),
    options_to_file(OptionsType, OptionsPath),
    print_file(OptionsPath).
    

start_menu(Config):-
    repeat,
    show_menu(home),
    read_digit(1,4,Input),
    start_menu(Input, Config),
    !.

start_menu(1, ['PvP', [], Size, Position]) :-
    size_menu(Size),
    position_menu(Size, Position).

start_menu(2, [Mode , [Level], Size, Position]):-
    player_computer([Level, Pieces, Size, Position]),
    nth1(Pieces,['PvC', 'CpV'], Mode).

start_menu(3, ['CvC', [Level1, Level2], Size, Position]) :-
    computer_computer(Level1, Level2, Size, Position).

start_menu(4, _):-halt(0).

computer_computer(Level1, Level2, Size, Position):-
    show_menu(computer_computer),
    read_digit(1,3,Level1),
    Level1 =\= 3,
    show_menu(computer_computer),
    read_digit(1,3,Level2),
    Level2 =\= 3,
    size_menu(Size),
    position_menu(Size, Position).



player_computer([Level, Pieces, Size, Position]):-
    show_menu(player_computer, difficulty),
    read_digit(1,3,Level),
    Level =\= 3,
    show_menu(player_computer, pieces),
    read_digit(1,3,Pieces),
    write('entering size_menu\n'),
    Pieces =\= 3,
    write('entering size_menu\n'),
    size_menu(Size),
    position_menu(Size, Position).


position_menu(4, nothing).
% TODO - Make this return based on user input.
position_menu(5, [5, 6]).


size_menu(Size):-
    show_menu(board_size),
    read_digit(1, 3, Input),
    Input =\= 3,
    Size is Input + 3.




display_square(Number, null):-
    gray_bgrnd,
    write_square(Number),
    clear_colors.

display_square(Number, white):-
    white_bgrnd,
    write_square(Number),
    clear_colors.
display_square(Number, black):-
    black_bgrnd,
    white_fgrnd,
    write_square(Number),
    clear_colors.
display_square(Number, valid_white):-
    light_blue_bgrnd,
    dark_blue_fgrnd,
    write_square(Number),
    clear_colors.

display_square(Number, valid_black):-
    dark_blue_bgrnd,
    light_blue_fgrnd,
    write_square(Number),
    clear_colors.
    
display_square(Number, invalid_white):-
    light_red_bgrnd,
    dark_red_fgrnd,
    write_square(Number),
    clear_colors.
display_square(Number, invalid_black):-
    dark_red_bgrnd,
    light_red_fgrnd,
    write_square(Number),
    clear_colors.



display_row([]):- !.
display_row([[Value, Color] | Row]):-
    display_square(Value, Color),
    display_row(Row).

display_rows([]):- !.
display_rows([Row | Board]):-
    length(Board, RemainingLines),
    CurrentLine is RemainingLines + 1,
    PadSize is -CurrentLine // 10,
    padding(PadSize),
    % Line Number
    write(CurrentLine),
    write(' '),
    
    display_square(' ', black),
    display_row(Row),
    display_square(' ', black),

    % Line Number
    write(' '),
    write(CurrentLine),
    nl,
    display_rows(Board).
    
% create_board(5, [4, 5], B, V), display_board([B, _, _, _, [[4, 5], 1]]).
display_board([Board, Player, BlocksLeft, ValidMoves, [Position, Rotation]]):-
    put_selected_block(Board, Position, Rotation, NewBoard),
    reverse(NewBoard, ReversedBoard),
    length(Board, Length),
    TotalLength is Length * 3,
    nl,nl,nl,nl,
    % Numbers
    padding(5),
    number_line(Length),
    nl,
    % White line
    padding(2),
    display_square(' ', null),
    white_line(TotalLength),
    display_square(' ', null),
    nl,
    % Actual rows
    display_rows(ReversedBoard),
    % White line
    padding(2),
    display_square(' ', null),
    white_line(TotalLength),
    display_square(' ', null),
    nl,
    % Numbers
    padding(5),
    number_line(Length), nl,nl,nl.




display_game(GameState). 
display_instructions(GameState).

display_title(WhitePLayer, BlackPlayer, BlocksLeft).

clear_screen:-
    write('\33\[2J').