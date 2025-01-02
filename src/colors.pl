




reset_styling:- write('\33\[0m').

% Effects
clear_effects:-write('\33\[22m').

bold_on:-write('\33\[1m').

% COLORS 

clear_colors:- write('\33\[0m').
% Foreground Colors

white_fgrnd:- write('\33\[38;5;15m').
black_fgrnd:- write('\33\[38;5;16m').
light_blue_fgrnd:- write('\33\[38;2;201;232;255m').
dark_blue_fgrnd:- write('\33\[38;2;0;18;31m').
dark_red_fgrnd:- write('\33\[38;5;52m').
light_red_fgrnd:- write('\33\[38;5;1m').
% Background Colors

white_bgrnd:- write('\33\[48;5;15m').
black_bgrnd:- write('\33\[48;5;16m').
gray_bgrnd:- write('\33\[48;2;73;77;79m').

light_blue_bgrnd:- write('\33\[48;2;201;232;255m').
dark_blue_bgrnd:- write('\33\[48;2;0;18;31m').

dark_red_bgrnd:- write('\33\[48;2;51;8;2m').
light_red_bgrnd:- write('\33\[48;2;237;147;147m').


