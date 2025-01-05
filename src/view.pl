:- consult('utils').

menu_to_file(home_title, './res/home_page_title.txt').
menu_to_file(instructions_title, './res/instructions_title.txt').
menu_to_file(player1_title, './res/player1_title.txt').
menu_to_file(player2_title, './res/player2_title.txt').
menu_to_file(board_size_title, './res/board_size_title.txt').
menu_to_file(starting_piece_title, './res/starting_piece_title.txt').
menu_to_file(player_bot_title, './res/player_bot_title.txt').
menu_to_file(bot1_title, './res/bot1_title.txt').
menu_to_file(bot2_title, './res/bot2_title.txt').
menu_to_file(game_over, './res/game_over.txt').



options_to_file(home_options, './res/home_page_options.txt').
options_to_file(instructions_text, './res/instructions_text.txt').
options_to_file(player_options, './res/player_options.txt').
options_to_file(board_size_options, './res/board_size_options.txt').
options_to_file(starting_piece_options, './res/starting_piece_options.txt').
options_to_file(custom_starting_piece_options, './res/custom_starting_piece_options.txt').
options_to_file(bot_difficulty_options, './res/bot_difficulty_options.txt').
options_to_file(pieces_color_options, './res/pieces_color_options.txt').




get_game_config(Config):-
    start_menu(Config).


show_menu(HeaderType):-
    bold_on,
    purple_fgrnd,
    menu_to_file(HeaderType, Path),
    print_file(Path),
    reset_styling.

show_menu(HeaderType, OptionsType):-
    bold_on,
    purple_fgrnd,
    menu_to_file(HeaderType, Path),
    print_file(Path),
    reset_styling,
    options_to_file(OptionsType, OptionsPath),
    print_file(OptionsPath).
    

start_menu(Config):-
    repeat,
    show_menu(home_title, home_options),
    read_digit(0,4,Input),
    start_menu(Input, Config),
    !.

start_menu(0, _):-halt(0).

start_menu(1, ['PvP', [0,0], Size, Position, PlayerName1, PlayerName2]) :-
    name_menu(player1_title,PlayerName1),
    name_menu(player2_title,PlayerName2),
    size_menu(Size),
    position_menu(Size, Position).

start_menu(2, [Mode , [Level,0], Size, Position, PlayerName1, PlayerName2]):-
    player_computer([Level, Pieces, Size, Position, N]),
    nth1(Pieces,['PvC', 'CvP'], Mode),
    setName(Mode, N, PlayerName1, PlayerName2).


start_menu(3, ['CvC', [Level1, Level2], Size, Position, 'Computer1', 'Computer2']) :-
    computer_computer(Level1, Level2, Size, Position).

start_menu(4,_) :-
    show_menu(instructions_title, instructions_text),
    read_digit(0,0,Digit),
    Digit =\= 0.


setName('PvC', N, N, 'Computer').
setName('CvP', N, 'Computer', N).


player_computer([Level, Pieces, Size, Position, PlayerName1]):-
    name_menu(player1_title, PlayerName1),
    show_menu(player_bot_title, bot_difficulty_options),
    read_digit(0,3,Level),
    Level =\= 0,
    show_menu(player_bot_title, pieces_color_options),
    read_digit(0,2,Pieces),
    Pieces =\= 0,
    size_menu(Size),
    position_menu(Size, Position).


computer_computer(Level1, Level2, Size, Position):-
    show_menu(bot1_title, bot_difficulty_options),
    read_digit(0,3,Level1),
    Level1 =\= 0,
    show_menu(bot2_title, bot_difficulty_options),
    read_digit(0,3,Level2),
    Level2 =\= 0,
    size_menu(Size),
    position_menu(Size, Position).

position_menu(4, nothing).
 
position_menu(Size, Position):-
    show_menu(starting_piece_title, starting_piece_options),
    read_digit(0,6,Input),
    Input =\= 0,
    handle_pos(Position,Size,Input).

handle_pos(Position,Size,1):-
    K2 is Size * 2,
    Position = [1,K2].

handle_pos(Position,Size,2):-
    K1 is (Size*2) -1,
    K2 is Size * 2,
    Position = [K1,K2].

handle_pos(Position,Size,3):-
    Position = [1,2].

handle_pos(Position,Size,4):-
    K1 is (Size*2) -1,
    Position = [K1,2].

handle_pos(Position,Size,5):-
    K is Size +1,
    Position = [Size,K].

handle_pos(Position,Size, 6):-
    show_menu(starting_piece_title, custom_starting_piece_options),
    read_starting(Size, PosX, PosY), 
    Position = [PosX, PosY].


size_menu(Size):-
    show_menu(board_size_title, board_size_options),
    read_digit(0, 2, Input),
    Input =\= 0,
    Size is Input + 3.

name_menu(Player, Name):-
    show_menu(Player,player_options),
    read_line(Input),
    atom_codes(Name, Input).


number_white(1, white1).
number_white(2, white2).
number_white(3, white3).
number_white(4, white4).
number_white(5, white5).
number_white(' ', white1).

number_black(1, black1).
number_black(2, black2).
number_black(3, black3).
number_black(4, black4).
number_black(5, black5).
number_black(' ', black1).

display_square(Number, null):-
    gray_bgrnd,
    write_square(Number),
    clear_colors.

display_square(Number, white):-
    number_white(Number, Color),
    call(Color),
    write_square(Number),
    clear_colors.

display_square(Number, black):-
    number_black(Number, Color),
    call(Color),
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


check_player(Player, Color):-
    Player = Color,
    write('   <==| Your Turn'), nl. 

check_player(_,_):-
    nl.

display_player(Name, Player, Color):-
    display_square(' ', Color),write('   '), write(Name),
    check_player(Player, Color).

display_bot(Name, Level, Player, Color):-
    difficulty_map(Level,L),
    display_square(' ', Color),write('   '), write(Name), write('('), write(L), write(')'),
    check_player(Player, Color).

display_title([Mode, [L1,L2] , PlayerName1, PlayerName2], [_, Player, Blocks, _, _]):-
    nl,
    display_players(Mode, [L1,L2], PlayerName1, PlayerName2, Player),nl,
    write('Playing Now - '), write(Player),
    write('          Remaining Pieces: '), write(Blocks).  


display_players('PvP', _,  PlayerName1, PlayerName2, Player):-
    display_player(PlayerName1,Player,white),
    display_player(PlayerName2,Player,black).

display_players('PvC', [L1,_], PlayerName1, PlayerName2, Player):-
    display_player(PlayerName1,Player,white),
    display_bot(PlayerName2,L1,Player,black).

display_players('CvP', [L1,_], PlayerName1, PlayerName2, Player):-
    display_bot(PlayerName1,L1,Player,white),
    display_player(PlayerName2,Player,black).

display_players('CvC', [L1,L2], PlayerName1, PlayerName2, Player):-
    display_bot(PlayerName1,L1,Player,white),
    display_bot(PlayerName2,L2,Player,black).


display_game(GameOptions, GameState):-
    display_title(GameOptions, GameState),
    display_board(GameState).
    % show_evaluation(). % PUT IN GAMESTATE IF EVALUATION IS SHOWN OR NOT

display_endGame(Winner, WinnerName):-
    show_menu(game_over),
    write('Winner('), write(Winner), write(') -- '), 
    write(WinnerName). 


display_instructions(GameState).



clear_screen:-
    write('\33\[2J').