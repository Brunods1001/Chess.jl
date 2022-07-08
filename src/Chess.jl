module Chess

using StaticArrays
import Base: getindex, isempty, setindex!, size, axes, display, string, (:)

export
    Board, Cell,
    str, display,
    Color, White, Black,
    Position, @p_str,
    RowNumber, ColLetter,
    Pawn, Knight, Bishop, Rook, Queen, King,
    size, isempty, getindex,
    move!, is_capturable,
    CapturableTrait,
    geticon, chess_num_to_idx, num,
    getpoints, getstats,
    incheck


abstract type Piece end

@enum Color White Black
@enum RowNumber _1 _2 _3 _4 _5 _6 _7 _8
@enum ColLetter A B C D E F G H
iswhite(p::Piece) = p.color == White
isblack(p::Piece) = p.color == Black
num(r::RowNumber) = parse(Int, string(r)[2])
num(c::ColLetter) = (Int(string(c)[1]) - 64)


# Structs
mutable struct Cell
    piece::Union{Piece,Nothing}
    color::Color

    Cell(color::Color) = new(nothing, color)
    Cell(piece::Piece, color::Color) = new(piece, color)
end
iswhite(c::Cell) = c.color == White

mutable struct Board
    cells::SMatrix{8,8,Cell}
    Board() = new(create_board())
end

struct Position
    col::ColLetter
    row::RowNumber
end

"Keeps track of game data, allowing to go back to history"
struct Game
    moves
    player1_stats  # player info and pieces captured
    player2_stats
end

"Get string representation for a cell accounting for its color and piece"
function cell_contents(cell::Cell, pad="/")::String
    padding = iswhite(cell) ? " " : pad
    content = isempty(cell) ? padding : geticon(cell.piece)
    "$padding$content$padding"
end

function display(board::Board)
    ncells = size(board)[2]
    leftpad = "  -"
    numbers = "87654321"
    for (num, row) in zip(numbers, eachrow(board.cells))
        top = leftpad * repeat("•---", ncells)
        println(top, "•")
        print(" $num ")
        for cell in row
            print("|")
            # print(cell_contents(cell, code_point_to_char("U+2593")))
            print(cell_contents(cell, "/"))
        end
        println("|")
    end
    println(leftpad * repeat("•---", ncells), "•")
    bottom = let
        pad = " "
        pad * join([" $(letter) " for letter in "ABCDEFGH"], pad) * pad
    end
    print("   ")
    println(bottom)
end
function display(cell::Cell)
    if isempty(cell)
        println("•---•")
        iswhite(cell) ? println("|    |") : println("|/  /|")
        println("•---•")
    else
        println("•---•")
        iswhite(cell) ? println("| $(geticon(cell.piece)) |") : println("|/$(geticon(cell.piece))/|")
        println("•---•")
    end
end
function display(cells::Matrix{Cell})
    ncells = size(cells)[2]
    leftpad = ""
    for row in eachrow(cells)
        top = leftpad * repeat("•---", ncells)
        println(top, "•")
        for cell in row
            print("|")
            print(cell_contents(cell, "/"))
        end
        println("|")
    end
    println(leftpad * repeat("•---", ncells), "•")
    print("   ")
end
function display(piece::Piece)
    geticon(piece) |> println
end
function display(pos::Position)
    println(
        replace("$(string(pos.col))$(string(pos.row))", "_" => "")
    )
end
function display(vec::Vector{Position})
    print("[ ")
    for pos in vec
        print(
            replace("$(string(pos.col))$(string(pos.row)) ", "_" => "")
        )
    end
    print("]")
end
function string(pos::Position)
    replace("$(string(pos.col))$(string(pos.row))", "_" => "")
end
function getindex(board::Board, i::Int, j::Int)
    board.cells[i, j]
end
function chess_num_to_idx(cellstr::String)::Tuple{Int,Int}
    r, c = cellstr
    j = (Int(r) - 64)
    i = 9 - parse(Int, c)
    (i, j)
end
function getindex(board::Board, cellstr::String)
    i, j = chess_num_to_idx(cellstr)
    board.cells[i, j]
end
function getindex(board::Board, pos::Position)
    cellstr = string(pos)
    board[cellstr]
end
function getindex(board::Board, rng::Tuple{UnitRange{Int},UnitRange{Int}})
    irng, jrng = rng
    board[irng, jrng]
end
function getindex(board::Board, rng::StepRange{Int,Int})
    board.cells[rng]
end

function (:)(start::Position, stop::Position)
    i1, j1 = chess_num_to_idx(string(start))
    i2, j2 = chess_num_to_idx(string(stop))
    if i2 < i1
        i2, i1 = i1, i2
    end
    if j2 < j1
        j2, j1 = j1, j2
    end
    (i1:i2, j1:j2)
end
function (:)(start::Position, step::Int, stop::Position)
    i1, j1 = chess_num_to_idx(string(start))
    i2, j2 = chess_num_to_idx(string(stop))
    if i2 < i1
        i2, i1 = i1, i2
    end
    if j2 < j1
        j2, j1 = j1, j2
    end
    (i1:step:i2, j1:step:j2)
end
function getindex(board::Board, ir, jr)
    board.cells[ir, jr]
end
function axes(board::Board, i)
    axes(board.cells, i)
end
function setindex!(board::Board, value, inew, jnew)
    cells = SMatrix{8,8,Cell}(
        [((i == inew) & (j == jnew)) ? value : board[i, j]
         for i = 1:size(board)[1], j = 1:size(board)[2]])
    board.cells = cells
end

function create_board()::SMatrix{8,8,Cell}
    cells = SMatrix{8,8,Cell}(
        [(isodd(i) & isodd(j)) | (iseven(i) & iseven(j)) ?
         Cell(White) :
         Cell(Black)
         for i = 1:8, j = 1:8])

    cells = SMatrix{8,8,Cell}(
        [
        (i == 1) ? (
            (j == 1) ? (
                Cell(Rook(White), White)
            ) :
            (j == 2) ? (
                Cell(Knight(White), Black)
            ) :
            (j == 3) ? (
                Cell(Bishop(White), White)
            ) :
            (j == 4) ? (
                Cell(King(White), Black)
            ) :
            (j == 5) ? (
                Cell(Queen(White), White)
            ) :
            (j == 6) ? (
                Cell(Bishop(White), Black)
            ) :
            (j == 7) ? (
                Cell(Knight(White), White)
            ) :
            (j == 8) ? (
                Cell(Rook(White), Black)
            ) :
            @error "Position not implemented"
        ) :
        (i == 2) ? (
            iseven(j) ? (Cell(Pawn(White), White)) : (Cell(Pawn(White), Black))
        ) :
        (i == 7) ? (
            iseven(j) ? (Cell(Pawn(Black), Black)) : (Cell(Pawn(Black), White))
        ) :
        (i == 8) ? (
            (j == 1) ? (
                Cell(Rook(Black), Black)
            ) :
            (j == 2) ? (
                Cell(Knight(Black), White)
            ) :
            (j == 3) ? (
                Cell(Bishop(Black), Black)
            ) :
            (j == 4) ? (
                Cell(King(Black), White)
            ) :
            (j == 5) ? (
                Cell(Queen(Black), Black)
            ) :
            (j == 6) ? (
                Cell(Bishop(Black), White)
            ) :
            (j == 7) ? (
                Cell(Knight(Black), Black)
            ) :
            (j == 8) ? (
                Cell(Rook(Black), White)
            ) :
            @error "Position not implemented"
        ) :
        (isodd(i) & isodd(j)) | (iseven(i) & iseven(j)) ?
        Cell(White) :
        Cell(Black)
        for i = 1:8, j = 1:8])
end

function str(cell::Cell)::String
    isnothing(cell.piece) ? "|_|" : icon(cell.piece)
end
function str(board::Board)::String
    vec1 = [join(repeat(".", 16), "_")]
    vec2 = str.(board.cells)
    append!(vec1, vec2)
    join(vec1, "\n")


end

function Position(txt::String)
    i, j = chess_num_to_idx(txt)

    col = ColLetter(j - 1)
    row = RowNumber(8 - i)
    Position(col, row)
end
function Position(num::Int)
    !(1 <= num <= 64) && throw("Position must be between 1 and 64")
    num -= 1
    ncols = num ÷ 8
    nrows = num % 8
    row = RowNumber(nrows)
    col = ColLetter(ncols)
    Position(col, row)
end

macro p_str(txt::String)
    Position(txt)
end

"Get the size of the board"
function size(b::Board)::Tuple{Int,Int}
    size(b.cells)
end

"Create a Pawn"
struct Pawn <: Piece
    color::Color
end

"Create a Knight"
struct Knight <: Piece
    color::Color
end

"Create a Bishop"
struct Bishop <: Piece
    color::Color
end

"Create a Rook"
struct Rook <: Piece
    color::Color
end

"Create a Queen"
struct Queen <: Piece
    color::Color
end

"A King"
struct King <: Piece
    color::Color
end

function code_point_to_char(code_point::String)::Char
    delim = occursin(code_point, "+") ? "+" : "U"
    num = split(uppercase(code_point), delim) |> last
    Char(parse(Int, num, base=16))
end
"Create an icon for a cell"
function icon(piece::Piece)::String
    @error "$piece not implemented"
end

# Why not this instead of traits?
#icon(::Piece) = @error "Not implemented"
#icon(p::Pawn) = p.color == White ? code_point_to_char("U+265F") : code_point_to_char("U+2659")
#icon(p::Knight) = p.color == White ? code_point_to_char("U+265F") : code_point_to_char("U+2659")
#icon(p::Bishop) = p.color == White ? code_point_to_char("U+265F") : code_point_to_char("U+2659")
#icon(p::Rook) = p.color == White ? code_point_to_char("U+265F") : code_point_to_char("U+2659")
#icon(p::Queen) = p.color == White ? code_point_to_char("U+265F") : code_point_to_char("U+2659")
#icon(p::King) = p.color == White ? code_point_to_char("U+265F") : code_point_to_char("U+2659")

# Methods

## Cells
function isempty(cell::Cell)::Bool
    isnothing(cell.piece)
end

## Board | Movement

"Finds all pieces of a type"
function find_piece_positions(board::Board, ::Type{T})::Vector{Position} where {T<:Piece}
    positions = []
    for (i, cell) in enumerate(board.cells)
        isempty(cell) && continue
        if cell.piece isa T
            push!(positions, Position(i))
        end
    end
    positions
end
function find_piece_positions(board::Board, ::Type{T}, color::Color)::Vector{Position} where {T<:Piece}
    positions = []
    for (i, cell) in enumerate(board.cells)
        isempty(cell) && continue
        (cell.piece.color != color) && continue
        if cell.piece isa T
            push!(positions, Position(i))
        end
    end
    positions
end

"Checks if a position has a clear path to the opposing king"
function checks_king(board::Board, pos::position)

end

"Check if King is in check"
function incheck(board::Board)::Bool
    # find kings
    vec_pos = find_piece_positions(board, King)
    white_piece_pos = find_piece_positions(board, Piece, White)
    black_piece_pos = find_piece_positions(board, Piece, Black)
    # for each king
    for king_pos in vec_pos
        king = board[king_pos].piece
        if iswhite(king)
            # iterate over black pieces
            for pos in black_piece_pos
                checks_king(board, pos)
            end
        else
        end

    end
    # iterate over other color's pieces
    # check if a piece has a clear path to king
    true
end

"Evaluates the board for check/checkmate"
function validate_board(board::Board)::Bool
    # find king
    # iterate over opponent pieces
    # check if a piece has a clear path to king
end
function validate_board(board::Board, pos1::Position, pos2::Position)::Bool end

function move!(board::Board, pos1::Position, pos2::Position)::Board
    cell = board[pos1]
    if isempty(cell)
        @warn "There is no piece on this cell"
        return board
    end
    piece = cell.piece

    if validate_move(board, piece, pos1, pos2) && validate_board(board, pos1, pos2)
        board[pos2].piece = piece
        board[pos1].piece = nothing
    else
        @warn "$piece cannot move from $(string(pos1)) to $(string(pos2))"
    end

    # check if pawn reached last row
    # check for check
    # check for checkmate

    w, b = getstats(board)
    # println("Points:\nWhite:$w\nBlack:$b")
    board
end


### Piece movement rules
"Checks if a two positions are immediately diagonal from each other"
function isdiagone(pos1::Position, pos2::Position)
    (
        (
            (num(pos2.row) == (num(pos1.row) + 1)) |
            (num(pos2.row) == (num(pos1.row) - 1))
        )
        &
        (
            (num(pos2.col) == (num(pos1.col) + 1)) |
            (num(pos2.col) == (num(pos1.col) - 1))
        )
    )
end

"Checks if two positions are diagonal from each other"
function isdiag(pos1::Position, pos2::Position)::Bool
    (abs(num(pos1.row) - num(pos2.row))) == (abs(num(pos1.col) - num(pos2.col)))
end
"Checks if two positions are horizontal"
function ishorz(pos1::Position, pos2::Position)
    pos1.row == pos2.row
end
"Checks if two positions are vertical"
function isvert(pos1::Position, pos2::Position)
    pos1.col == pos2.col
end

"Gets flat indices from board and pos"
function flat_pos_from_mat(board::Board, pos1::Position, pos2::Position)::Tuple{Int,Int}
    m, n = size(board)
    r1, c1 = (num(pos1.row), num(pos1.col))
    r2, c2 = (num(pos2.row), num(pos2.col))
    # r1, r2 = (r2 < r1) ? (r2, r1) : (r1, r2)
    # c1, c2 = (c2 < c1) ? (c2, c1) : (c1, c2)
    start = m + 1 - r1 + (c1 - 1) * m
    stop = m + 1 - r2 + (c2 - 1) * m
    if start > stop
        stop, start
    else
        start, stop
    end
end

"Checks if straight or diagonal path on board is clear"
function isclear(board::Board, pos1::Position, pos2::Position)::Bool
    if !(isdiag(pos1, pos2) | ishorz(pos1, pos2) | isvert(pos1, pos2))
        @error "Can only scan vertical, horizontal, and diagonal paths"
        return false
    end
    # StaticArray is column major
    start, stop = flat_pos_from_mat(board, pos1, pos2)
    m, n = size(board)
    rise = num(pos2.row) - num(pos1.row)
    run = num(pos2.col) - num(pos1.col)
    step = if pos1.col == pos2.col
        1
    elseif pos1.row == pos2.row
        m
    elseif (rise * run) > 0
        m - 1 - abs(num(pos2.row) - num(pos1.row)) + abs(num(pos2.col) - num(pos1.col))
    else
        m + 1 - abs(num(pos2.row) - num(pos1.row)) + abs(num(pos2.col) - num(pos1.col))
    end
    vec = board.cells[start:step:stop]

    vec_clean = if length(vec) > 1
        # get path from pos1 to pos2 non-inclusively
        vec[2:end-1]
    else
        @error "Path must be greater than one square"
        return false
    end
    all(isempty.(vec_clean))
end

"Manhattan distance"
function dist(pos1::Position, pos2::Position)::Int
    rise = num(pos2.row) - num(pos1.row)
    run = num(pos2.col) - num(pos1.col)
    abs(rise) + abs(run)
end

### Move validations
"Uses board to determine if move is capture"
function validate_move(board::Board, ::Piece, ::Position, ::Position)
    throw("Not implemented")
end

function validate_move(board::Board, p::Pawn, pos1::Position, pos2::Position)::Bool
    # assert pawn moves forward. White goes down, Black goes up
    if iswhite(p)
        if (num(pos1.row) <= num(pos2.row))
            @error "Pawn must move forward"
            return false
        end
    end
    if isblack(p)
        if (num(pos1.row) >= num(pos2.row))
            @error "Pawn must move forward"
            return false
        end
    end

    # check if cell movement path is occupied
    # check if capture
    if !isempty(board[pos2]) && isdiagone(pos1, pos2) && (board[pos2].piece.color == p.color)
        @error "Cannot capture your own pieces"
        return false
    elseif !isempty(board[pos2]) && isdiagone(pos1, pos2)
        @info "Capturing $(board[pos2].piece)"
        return true
    end


    # assert path is empty
    let
        rowdiff = abs(num(pos2.row) - num(pos1.row))
        isstart = iswhite(p) ? (num(pos1.row) == 7) : (num(pos1.row) == 2)
        if isstart & (rowdiff == 2)
            i, j = chess_num_to_idx(string(pos2))
            if iswhite(p)
                if i > 1 && !isempty(board[i-1, j])
                    @error "Path is blocked"
                    return false
                end
            else
                if i < 8 && !isempty(board[i+1, j])
                    @error "Path is blocked"
                    return false
                end
            end
        elseif rowdiff == 1
            if !isempty(board[pos2])
                @error "Path is blocked"
                return false
            end
        else
            @error "Pawn cannot move beyond one or two moves"
            return false
        end
    end

    if !isempty(board[pos2])
        @error "Cell $(board[pos2]) is occupied"
        return false
    end

    # assert same column
    if (pos1.col != pos2.col)
        @error "Pawns must move within its same column unless capturing an opponent"
        return false
    end

    # if piece starts at init, allow move forward one or two cells
    if (num(pos1.row) == 2) & isblack(p)
        @assert (2 < num(pos2.row) <= 4) "Pawn can move one or two spaces forward"
    elseif (pos1.row == 7) & iswhite(p)
        false
    else
        true
    end

    true
end

function validate_move(board::Board, p::Bishop, pos1::Position, pos2::Position)::Bool
    # check path is diagonal
    if !isdiag(pos1, pos2)
        @error "Bishop can only move diagonally"
        return false
    end

    # check that path is clear
    if !isclear(board, pos1, pos2)
        @error "Path is not clear"
        return false
    end

    # check pos2 is empty
    if !isempty(board[pos2])
        # check piece is not same color
        if board[pos2].piece.color == board[pos1].piece.color
            @error "Cannot capture your own pieces"
            return false
        end
    end

    # check position colors are equal
    if board[pos1].color != board[pos2].color
        @error "Bishop must stay on its initial color"
        return false
    end
    true
end

function validate_move(board::Board, p::Rook, pos1::Position, pos2::Position)::Bool
    # check path ishorz or isvert
    if !(ishorz(pos1, pos2) || isvert(pos1, pos2))
        @error "Rooks can only move horizontally or vertically"
        return false
    end

    # check that path is clear
    if !isclear(board, pos1, pos2)
        @error "Path is not clear"
        return false
    end

    # cannot capture own piece
    if !isempty(board[pos2])
        # check piece is not same color
        if board[pos2].piece.color == board[pos1].piece.color
            @error "Cannot capture your own pieces"
            return false
        end
    end

    # king cannot be in check

    true
end
function validate_move(board::Board, p::Queen, pos1::Position, pos2::Position)::Bool
    # check path ishorz, isvert, or isdiag
    if !(ishorz(pos1, pos2) || isvert(pos1, pos2) || isdiag(pos1, pos2))
        @error "Queens can only move diagonally, horizontally, or vertically"
        return false
    end

    # check that path is clear
    if !isclear(board, pos1, pos2)
        @error "Path is not clear"
        return false
    end

    # cannot capture own piece
    if !isempty(board[pos2])
        # check piece is not same color
        if board[pos2].piece.color == board[pos1].piece.color
            @error "Cannot capture your own pieces"
            return false
        end
    end

    # king cannot be in check

    true
end
function validate_move(board::Board, p::Knight, pos1::Position, pos2::Position)::Bool

    # assert col and row are diff
    if (pos1.col == pos2.col) || (pos1.row == pos2.row)
        @error "Knight must land on a different row and column"
        return false
    end

    # assert color is different
    if board[pos1].color == board[pos2].color
        @error "Knight must land on a different color"
        return false
    end

    # assert manhattan distance is 4
    if dist(pos1, pos2) != 3
        @error "Knight must land three squares away from starting point"
        return false
    end

    # check if cell movement path is occupied
    # check if capture
    if (
        !isempty(board[pos2]) &&
        (board[pos2].piece.color == p.color)
    )
        @error "Cannot capture your own pieces"
        return false
    end

    true
end
function validate_move(board::Board, p::King, pos1::Position, pos2::Position)::Bool

    # check if cell movement path is occupied
    # check if capture
    if (
        !isempty(board[pos2]) &&
        (board[pos2].piece.color == p.color)
    )
        @error "Cannot capture your own pieces"
        return false
    end

    # king can only move one square
    let
        d = dist(pos1, pos2)
        if !(d == 1) && !(((d == 2) && isdiag(pos1, pos2)))
            @error "King must only move one square"
            return false
        end
    end

    true
end

## Pieces
function is_capturable(x::Piece)
    !(x isa King)
end

function getpoints(p::Piece)
    throw("Not implemented for $p")
end
getpoints(p::Pawn)::Int = 1
getpoints(p::Knight)::Int = 3
getpoints(p::Bishop)::Int = 3
getpoints(p::Rook)::Int = 5
getpoints(p::Queen)::Int = 9

## Stats

"Print points"
function getstats(board::Board)
    whitepoints = 0
    blackpoints = 0
    for cell in board.cells
        if !isempty(cell)
            if cell.piece isa King
            elseif iswhite(cell.piece)
                whitepoints += getpoints(cell.piece)
            else
                blackpoints += getpoints(cell.piece)
            end
        end
    end
    whitepoints, blackpoints
end

# Traits
## Icon
abstract type IconTrait end
struct IconPawn <: IconTrait end
struct IconKnight <: IconTrait end
struct IconBishop <: IconTrait end
struct IconRook <: IconTrait end
struct IconQueen <: IconTrait end
struct IconKing <: IconTrait end
IconTrait(::Type{<:Piece}) = @error "Not Implemented"
IconTrait(p::T) where {T<:Piece} = IconTrait(typeof(p))
IconTrait(::Type{<:Pawn}) = IconPawn()
IconTrait(::Type{<:Knight}) = IconKnight()
IconTrait(::Type{<:Bishop}) = IconBishop()
IconTrait(::Type{<:Rook}) = IconRook()
IconTrait(::Type{<:Queen}) = IconQueen()
IconTrait(::Type{<:King}) = IconKing()
function geticon(p::T) where {T<:Piece}
    geticon(IconTrait(p), p)
end
function geticon(::IconPawn, p)
    cw = "U+2659"
    cb = "U+265F"
    iswhite(p) ? code_point_to_char(cw) : code_point_to_char(cb)
end
function geticon(::IconKnight, p)
    cw = "U+2658"
    cb = "U+265E"
    iswhite(p) ? code_point_to_char(cw) : code_point_to_char(cb)
end
function geticon(::IconBishop, p)
    cw = "U+2657"
    cb = "U+265D"
    iswhite(p) ? code_point_to_char(cw) : code_point_to_char(cb)
end
function geticon(::IconRook, p)
    cw = "U+2656"
    cb = "U+265C"
    iswhite(p) ? code_point_to_char(cw) : code_point_to_char(cb)
end
function geticon(::IconQueen, p)
    cw = "U+2655"
    cb = "U+265B"
    iswhite(p) ? code_point_to_char(cw) : code_point_to_char(cb)
end
function geticon(::IconKing, p)
    cw = "U+2654"
    cb = "U+265A"
    iswhite(p) ? code_point_to_char(cw) : code_point_to_char(cb)
end



## Capturability
abstract type CapturableTrait end
struct Capturable <: CapturableTrait end
struct NotCapturable <: CapturableTrait end
CapturableTrait(::Type{<:Piece}) = Capturable()
CapturableTrait(::Type{<:King}) = NotCapturable()

function capture(p::T) where {T}
    capture(CapturableTrait(T), p)
end
function capture(::NotCapturable, p)
    @error "Cannot capture $(p)!"
end
function capture(::Capturable, p)
    @error "Not implemented"
end

## Points

## Movement


end # module
