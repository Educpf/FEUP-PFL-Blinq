
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(file_systems)).
:- consult('colors').




% INPUT OUTPUT

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
    


read_digit(Min, Max, Value):-
    peek_code(Code),
    Number is Code - 48,
    \+ (
        between(48, 57, Code),
        between(Min, Max, Number)
      ),
    skip_line,
    write('Not a valid option\n'),
    !,
    read_digit(Min, Max, Value).

read_digit(_, _, Digit):-
    get_code(Code),
    Digit is Code - 48,
    skip_line.
    

% ANSI COLOR CHANGES


% BOARD DRAW HELPERS

write_line([], 0, _):- !.
write_line([Symbol | Line], Size, Symbol):-
    NewSize is Size - 1,
    put_char(Symbol),
    write_line(Line, NewSize, Symbol).

write_square(Number):-
    bold_on,
    write(' '),
    write(Number),
    write(' ').


padding(Difference):-
    Total is 30 + Difference,
    length(Padding, Total),
    maplist(=(' '), Padding),
    format('~s', [Padding]).

white_line(Size):-
    white_bgrnd,
    TotalSize is Size + 3,
    write_line(Line, Size, ' '),
    clear_colors.

number_line(0).
number_line(Number):-number_line(Number, 1).
number_line(Number, Current):- Current > Number, !.
number_line(Number, Current):-
    write(' '),
    number_chars(Current, CharList),
    format('~2s',[CharList]),
    Next is Current + 1,
    number_line(Number, Next).

