
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(file_systems)).
:- consult('colors').




/* 
   print_file(+FilePath)
   Opens a file and prints its content to the standard output.
   
   Arguments:
        +FilePath (string): The path of the file you want to print.

*/

print_file(FilePath):-
    open(FilePath, read, Stream),
    show_file_content(Stream),
    close(Stream),
    nl.

show_file_content(Stream):-
    peek_code(Stream, -1), !.

show_file_content(Stream):-
    get_char(Stream, Data),
    write(Data),
    show_file_content(Stream).
    

/*
    read_digit(+Min, +Max, -Value)
    Reads a single digit from the user, ensuring that it is within the specified range.

    Arguments:
        +Min: The minimum valid value (inclusive).
        +Max: The maximum valid value (inclusive).
        -Value: The valid digit entered by the user.

*/
read_digit(Min, Max, Value):-
    peek_code(Code),
    Number is Code - 48,
    % Check validity of input
    \+ (
        between(48, 57, Code),
        between(Min, Max, Number)
      ),
    skip_line,
    write('Not a valid option\n'),
    !,
    read_digit(Min, Max, Value).

% Valid input
read_digit(_, _, Digit):-
    % Consume it
    get_code(Code),
    Digit is Code - 48,
    skip_line.


/*
    read_starting(+Size, -PosX, -PosY)
    Reads the starting position of the neutral block ensuring it is valid

    Arguments:
        +Size: The size of the board, which affects the valid range for the starting position.
        -PosX: The X-coordinate of the starting position
        -PosY: The Y-coordinate of the starting position
    
*/

read_starting(Size, PosX, PosY) :- 
    read(X-Y),
    skip_line,
    integer(X),
    integer(Y),
    M1 is (Size*2)-1,
    M2 is Size *2,
    between(1, M1, X),
    between(2, M2, Y),
    X mod 2 =:= 1,
    Y mod 2 =:= 0,
    PosX is X,
    PosY is Y,
    !.    

read_starting(Size, PosX, PosY) :-
    write('Invalid Position\n'),
    read_starting(Size, PosX, PosY).

/*
    read_position(+MinX, +MaxX, +MinY, +MaxY, -PosX, -PosY, -Rot)
    Reads the position and rotation for a game move, ensuring they are within the valid range.

    Arguments:
        +MinX: The minimum valid value for the X-coordinate.
        +MaxX: The maximum valid value for the X-coordinate.
        +MinY: The minimum valid value for the Y-coordinate.
        +MaxY: The maximum valid value for the Y-coordinate.
        -PosX: The X-coordinate of the move.
        -PosY: The Y-coordinate of the move.
        -Rot: The rotation value for the move (must be between 1 and 4).

*/
read_position(MinX, MaxX, MinY, MaxY, PosX, PosY, Rot):-
    read(PosX-PosY-Rot),
    skip_line,
    between(MinX, MaxX, PosX),
    between(MinY, MaxY, PosY),
    between(1,4,Rot),
    write('afterbetgame\n'),
    !.





% Display utility predicates

/*
    write_line(+Line, +Size, +Symbol)
    Writes a line of symbols to the output with the specified size.

    Arguments:
        +Line: A list representing the sequence of symbols to be written. It can be a list of atoms or characters.
        +Size: The number of times to print the `Symbol` in the line.
        +Symbol: The symbol to be printed in each position of the line.

*/
write_line([], 0, _):- !.
write_line([Symbol | Line], Size, Symbol):-
    NewSize is Size - 1,
    put_char(Symbol),
    write_line(Line, NewSize, Symbol).

% Writes a white line with the specified size
white_line(Size):-
    white_bgrnd,
    TotalSize is Size + 3,
    write_line(Line, Size, ' '),
    clear_colors.

% Writes a square ( 3 characters with background color )
write_square(Number):-
    bold_on,
    write(' '),
    write(Number),
    write(' ').

/*
    padding(+Difference)
    Prints a line of spaces (padding) based on the given difference value.

    Arguments:
        +Difference: The number of spaces to be added to the total padding length.
        
*/
padding:-padding(0).
padding(Difference):-
    Total is 54 + Difference,
    length(Padding, Total),
    maplist(=(' '), Padding),
    format('~s', [Padding]).

/*
    number_line(+Number)
    Prints a sequence of numbers from 1 to the given number, formatted to fit two spaces.

    Arguments:
        +Number: The maximum number to print in the sequence. 
                The sequence starts from 1 and goes up to `Number`.

*/
number_line(0).
number_line(Number):-number_line(Number, 1).
number_line(Number, Current):- Current > Number, !.
number_line(Number, Current):-
    write(' '),
    number_chars(Current, CharList),
    format('~2s',[CharList]),
    Next is Current + 1,
    number_line(Number, Next).





/*
    or(+List, -Result)
    Checks if there is a true value in the list, setting Result to true if found, otherwise false.
    
    Arguments:
        +List: A list of boolean values (true or false).
        -Result: A boolean value (true or false).

*/
or(List, true):-
    member(true, List), !.
or(List, false).
