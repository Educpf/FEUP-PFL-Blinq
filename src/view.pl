:- consult('utils').



/*
    menu_to_file(+HeaderType, -Path)
    Maps a menu header to its corresponding file path.

    Arguments:
        +HeaderType: A term representing the menu header.
        -Path: The file path to the header's content file.
*/

menu_to_file(home_title, './res/home_page_title.txt').
menu_to_file(instructions_title, './res/instructions_title.txt').
menu_to_file(player1_title, './res/player1_title.txt').
menu_to_file(player2_title, './res/player2_title.txt').
menu_to_file(board_size_title, './res/board_size_title.txt').
menu_to_file(neutral_block_title, './res/neutral_block_title.txt').
menu_to_file(player_bot_title, './res/player_bot_title.txt').
menu_to_file(bot1_title, './res/bot1_title.txt').
menu_to_file(bot2_title, './res/bot2_title.txt').
menu_to_file(game_over, './res/game_over.txt').



/*
    options_to_file(+OptionsType, -Path)
    Maps a menu options to its corresponding file path.

    Arguments:
        +OptionsType: A term representing the menu options (e.g., `home_options`, `instructions_text`).
        -Path: The file path to the options' content file.
*/

options_to_file(home_options, './res/home_page_options.txt').
options_to_file(instructions_text, './res/instructions_text.txt').
options_to_file(player_options, './res/player_options.txt').
options_to_file(board_size_options, './res/board_size_options.txt').
options_to_file(neutral_block_options, './res/neutral_block_options.txt').
options_to_file(custom_neutral_block_options, './res/custom_neutral_block_options.txt').
options_to_file(bot_difficulty_options, './res/bot_difficulty_options.txt').
options_to_file(pieces_color_options, './res/pieces_color_options.txt').



/*
    show_menu(+HeaderType)
    Displays a menu with a specific header style.

    Arguments:
        +HeaderType: A term representing the menu header to display.
*/

show_menu(HeaderType):-
    clear_screen,
    bold_on,
    purple_fgrnd,
    menu_to_file(HeaderType, Path),
    print_file(Path),
    clear_colors.



/*
    show_menu(+HeaderType, +OptionsType)
    Displays a menu with a specific header and options style.

    Arguments:
        +HeaderType: A term representing the menu header to display.
        +OptionsType: A term representing the menu options to display.
*/

show_menu(HeaderType, OptionsType):-
    clear_screen,
    bold_on,
    purple_fgrnd,
    menu_to_file(HeaderType, Path),
    print_file(Path),
    clear_colors,
    options_to_file(OptionsType, OptionsPath),
    print_file(OptionsPath).



/*
    start_menu(-Config)
    Displays the main menu and processes user input to configure the game.

    Arguments:
        -Config: A list containing the game configuration based on the selected menu option.
*/

start_menu(Config):-
    repeat,
    show_menu(home_title, home_options),
    read_digit(0,4,Input),
    start_menu(Input, Config),
    !.



/*
    start_menu(+Input, -Config)
    Processes a specific menu option and generates the corresponding game configuration.

    Arguments:
        +Input: An integer representing the selected menu option.
        -Config: A list containing the generated game configuration.
*/

% Exits Program 
start_menu(0, _):-halt(0).

% Configures PvP game
start_menu(1, ['PvP', [0,0], Size, Position, PlayerName1, PlayerName2]) :-
    name_menu(player1_title,PlayerName1),
    name_menu(player2_title,PlayerName2),
    size_menu(Size),
    neutral_block_menu(Size, Position).

% Configures PvC game
start_menu(2, [Mode , [Level,0], Size, Position, PlayerName1, PlayerName2]):-
    player_computer(Level, Pieces, Size, Position, Name),
    nth1(Pieces,['PvC', 'CvP'], Mode),
    setName(Mode, Name, PlayerName1, PlayerName2).

% Configures CvC game
start_menu(3, ['CvC', [Level1, Level2], Size, Position, 'Computer1', 'Computer2']) :-
    computer_computer(Level1, Level2, Size, Position).

% Displays instructions menu
start_menu(4,_) :-
    show_menu(instructions_title, instructions_text),
    read_digit(0,0,Digit),
    Digit =\= 0.



/*
    setName(+Mode, +Name, -PlayerName1, -PlayerName2)
    Assigns player names based on the selected game mode in a PvC configuration.

    Arguments:
        +Mode: A string representing the game mode.
        +Name: A string representing the player's name.
        -PlayerName1: The name of the first player.
        -PlayerName2: The name of the second player.
*/

%PvC Case
setName('PvC', Name, Name, 'Computer').

%CvP Case
setName('CvP', Name, 'Computer', Name).



/*
    player_computer(-Level, -Pieces, -Size, -Position, -PlayerName1)
    Configures a Player vs Computer game mode.

    Arguments:
        - Level: Bot Expertise.
        - Pieces: User Pieces (black or white).
        - Size: The size of the game board.
        - Position: The neutral block position.
        - PlayerName1: The player's name
*/

player_computer(Level, Pieces, Size, Position, PlayerName1):-
    % Menu for entering player's name
    name_menu(player1_title, PlayerName1),
    % Menu to choose bot expertise
    show_menu(player_bot_title, bot_difficulty_options),
    read_digit(0,3,Level),
    Level =\= 0,
    % Menu to choose piece color
    show_menu(player_bot_title, pieces_color_options),
    read_digit(0,2,Pieces),
    Pieces =\= 0,
    % Menu to choose board size
    size_menu(Size),
    % Menu to choose neutral block position
    neutral_block_menu(Size, Position).



/*
    computer_computer(-Level1, -Level2, -Size, -Position)
    Configures a Computer vs Computer game mode.

    Arguments:
        - Level1: Bot 1 Expertise.
        - Level2: Bot 2 Expertise
        - Size: The size of the game board.
        - Position: The neutral block position 
*/

computer_computer(Level1, Level2, Size, Position):-
    % Menu to choose bot1 expertise
    show_menu(bot1_title, bot_difficulty_options),
    read_digit(0,3,Level1),
    Level1 =\= 0,
    % Menu to choose bot2 expertise
    show_menu(bot2_title, bot_difficulty_options),
    read_digit(0,3,Level2),
    Level2 =\= 0,
    % Menu to choose board size
    size_menu(Size),
    % Menu to choose neutral block position
    neutral_block_menu(Size, Position).



/*
    neutral_block_menu(+Size, -Position)
    Displays the neutral block selection menu, allowing the user to select a block.

    Arguments:
        +Size: An integer representing the size of the game board.
        -Position: A list representing the user selected position [PosX, PosY].
*/

% Size 4 case does not exist neutral block so, menu not shown
neutral_block_menu(4, nothing).

% Other sizes case
neutral_block_menu(Size, Position):-
    show_menu(neutral_block_title, neutral_block_options),
    read_digit(0,6,Input),
    Input =\= 0,
    handle_pos(Input,Size, Position).



/*
    handle_pos(+Input, +Size, -Position)
    Handles the position setup for a neutral block based on the user's input and the current game board size.

    Arguments:
        +Input: An integer representing the user's choice for the neutral block (ranging from 1 to 6).
        +Size: An integer representing the size of the game board.
        -Position: A list representing the user selected position [PosX, PosY].
*/

% Case input 1 - top-left block
handle_pos(1,Size,Position):-
    K2 is Size * 2,
    Position = [1,K2].

% Case input 2 - top-right block
handle_pos(2,Size,Position):-
    K1 is (Size*2) -1,
    K2 is Size * 2,
    Position = [K1,K2].

% Case input 3 - bottom-left block
handle_pos(3,Size,Position):-
    Position = [1,2].

% Case input 4 - bottom-right block
handle_pos(4,Size,Position):-
    K1 is (Size*2) -1,
    Position = [K1,2].

% Case input 5 - middle block
handle_pos(5,Size,Position):-
    K is Size +1,
    Position = [Size,K].

% Case input 6 - custom block
handle_pos(6,Size,Position):-
    show_menu(neutral_block_title, custom_neutral_block_options),
    read_starting(Size, PosX, PosY),
    Position = [PosX, PosY].



/*
    size_menu(-Size)
    Displays the board size menu and handles user input to select a game board size.

    Arguments:
        -Size: An integer representing the size of the game board selected by the user.
*/

size_menu(Size):-
    show_menu(board_size_title, board_size_options),
    read_digit(0, 2, Input),
    Input =\= 0,
    Size is Input + 3.



/*
    name_menu(+Player, -Name)
    Displays the name input menu and retrieves the player's name.

    Arguments:
        +Player: An string representing the menu title to be shown (player1 or player2)
        -Name: A string representing the player's name.
*/

name_menu(Player, Name):-
    show_menu(Player,player_options),
    read_line(Input),
    atom_codes(Name, Input).



/*
    number_white(+Number, -Color)
    Maps a number to the corresponding white piece color.

    Arguments:
        +Number: An integer representing the number associated with a white piece (1 to 5 or a space).
        -Color: The corresponding color for the white piece.
*/

number_white(1, white1).
number_white(2, white2).
number_white(3, white3).
number_white(4, white4).
number_white(5, white5).
number_white(' ', white1).



/*
    number_black(+Number, -Color)
    Maps a number to the corresponding black piece color.

    Arguments:
        +Number: An integer representing the number associated with a black piece (1 to 5 or a space).
        -Color: The corresponding color for the black piece.
*/

number_black(1, black1).
number_black(2, black2).
number_black(3, black3).
number_black(4, black4).
number_black(5, black5).
number_black(' ', black1).



/*
    display_square(+Number, +State)
    Displays a square on the game board with the specified color and number.

    Arguments:
        +Number: An integer representing the square number to be displayed, or nothing in case of space.
        +State: The color state of the square (`null`, `white`, `black`, `valid_white`, `valid_black`, `invalid_white`, `invalid_black`).
*/

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



/*
    display_row(+Row)
    Displays a single row of the game board with corresponding square values and colors.

    Arguments:
        +Row: A list of tuples `[Value, Color]` representing the square values and their associated colors for this row.
*/

display_row([]):- !.

display_row([[Value, Color] | Row]):-
    display_square(Value, Color),
    display_row(Row).



/*
    display_rows(+Board)
    Displays all rows of the game board, with row numbering and square formatting.

    Arguments:
        +Board: A list of rows where each row is a list of tuples `[Value, Color]` representing the squares.
*/

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
    
    

/*
    display_board(+Mode, +GameState)
    Displays the game board with specific formatting based on the game mode and the game state.

    Arguments:
        +Mode: The current game mode
        +GameState: A list representing the current game state [Board, Player, _, _, Position].
*/

display_board('CvC', [Board, _, _, _, _]):-
    display_board(Board), !.
 
display_board('PvC', [Board, black, _, _, _]):-
    display_board(Board), !.
 
display_board('CvP', [Board, white, _, _, _]):-
    display_board(Board), !.
 
display_board(_, [Board, _, _, _, [Position, Rotation]]):-
    put_selected_block(Board, Position, Rotation, NewBoard),
    display_board(NewBoard), !.



/*
    display_board(+Board)
    Displays the game board, including row numbers and squares with appropriate formatting.

    Arguments:
        +Board: The game board represented as a list of rows, where each row is a list of tuples `[Value, Color]` 
                representing the squares and their associated colors adn numbers.
*/

display_board(Board):-
    reverse(Board, ReversedBoard),
    length(Board, Length),
    TotalLength is Length * 3,
    nl,nl,
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



/*
    check_player(+Player, +Color)
    Checks if the current player matches the specified color and prints a message if it's the player's turn.

    Arguments:
        +Player: The current player to play (either `white` or `black`).
        +Color: The color to check against (either `white` or `black`).
*/

check_player(Player, Color):-
    Player = Color,
    write('   <==| Your Turn'), nl. 

check_player(_,_):-
    nl.



/*
    display_title([+Mode, +Players, +PlayerName1, +PlayerName2], +GameState)
    Displays the title menu with the current players informations, and remaining pieces.

    Arguments:
        +Mode: The game mode.
        +Players: A list containing bot expertise level.
        +PlayerName1: The name of the first player.
        +PlayerName2: The name of the second player.
        +GameState: A list representing the current game state [_, Player, Blocks, _, _].
*/

display_title([Mode, [L1,L2] , PlayerName1, PlayerName2], [_, Player, Blocks, _, _]):-
    show_menu(home_title),
    display_players(Mode, [L1,L2], PlayerName1, PlayerName2, Player),nl,
    padding(2),
    write('Remaining Pieces: '), write(Blocks).  



/*
    display_players(+Mode, +Level, +PlayerName1, +PlayerName2, +Player)
    Displays the players information based on the game mode.

    Arguments:
        +Mode: The game mode.
        +Level: A list with the bot level expertise.
        +PlayerName1: The name of the first player.
        +PlayerName2: The name of the second player.
        +Player: The current player to play (either `white` or `black`).
*/

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



/*
    display_player(+Name, +Player, +Color)
    Displays the player informations, piece color, name and if it his its turn.

    Arguments:
        +Name: The player's name.
        +Player: The current player to play (either `white` or `black`).
        +Color: The player piece color.
*/

display_player(Name, Player, Color):-
    padding(2),
    display_square(' ', Color),write('   '), write(Name),
    check_player(Player, Color).



/*
    display_bot(+Name, +Level, +Player, +Color)
    Displays the player informations, piece color, name and if it his its turn.

    Arguments:
        +Name: The bot's name.
        +Level: The bot's expertise level.
        +Player: The current player to play (either `white` or `black`).
        +Color: The bot piece color.
*/

display_bot(Name, Level, Player, Color):-
    padding(2),
    difficulty_map(Level,L),
    display_square(' ', Color),write('   '), write(Name), write('('), write(L), write(')'),
    check_player(Player, Color).



/*
    display_game(+GameOptions, +GameState, +Evaluation)
    Displays the full game interface, including the title, board, and evaluation.

    Arguments:
        +GameOptions: A list representing game options [GameMode, Difficulty, PlayerName1, PlayerName2].
        +GameState: A list representing the current game state [Board, Player, BlocksLeft, ValidMoves, Position].
        +Evaluation: The current evaluation of the game.
*/

display_game([Mode, Difficulty, Name1, Name2], GameState, Evaluation):-
    clear_screen,
    display_title([Mode, Difficulty, Name1, Name2], GameState),
    display_board(Mode, GameState),
    display_evaluation(GameState, Evaluation).



/*
    display_evaluation(+Board, +Evaluation)
    Displays the evaluation of the game

    Arguments:
        +Board: The current game board
        +Evaluation: The evaluation score of the game.
*/

display_evaluation([Board | _], Evaluation):-
    length(Board, Size),
    TotalSize is Size * 3 + 6,
    White is ((Evaluation + 1) / 2) * TotalSize,
    padding(2),
    write('Evaluation: '), format('~3f', Evaluation), nl,
    padding(2), display_bar(TotalSize, White), nl,nl.



/*
    display_bar(+TotalSize, +White)
    Displays a graphical bar to represent the evaluation, with white and black portion.

    Arguments:
        +TotalSize: The total size of the bar to be displayed.
        +White: The number of white segments in the bar, representing the evaluation score.
*/

display_bar(0, _):- !.

display_bar(TotalSize, White):-
    White =< 0,
    black_bgrnd,
    write(' '),
    clear_colors,
    NewSize is TotalSize - 1,
    display_bar(NewSize, 0).

display_bar(TotalSize, White):-
    white_bgrnd,
    write(' '),
    clear_colors,
    NewSize is TotalSize - 1,
    NewWhite is White - 1,
    display_bar(NewSize, NewWhite).



/*
    display_endGame(+GameState, +Winner, +WinnerName)
    Displays the end game screen, including the winner's details.

    Arguments:
        +GameState: A list representing the current game state [Board, Player, BlocksLeft, ValidMoves, Position].
        +Winner: The winner of the game, represented as either `white` or `black`.
        +WinnerName: The name of the winning player.
*/

display_endGame(GameState, Winner, WinnerName):-
    show_menu(game_over),
    display_board('CvC', GameState),
    padding(5),
    write('Winner('), write(Winner), write(') -- '), 
    write(WinnerName),nl,nl,
    write('Return to Home Page [ Press Any Key ] '),
    read_line(_). 



clear_screen:-
    write('\33\[2J').