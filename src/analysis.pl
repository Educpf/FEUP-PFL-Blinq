


choose_move([Board, _, _, ValidMoves, _], 1, [Position, Rotation]):-
    random_member(Position, ValidMoves),
    random(1, 4, Rotation).
    
