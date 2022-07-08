using Test
using Chess
import Chess: size, isblack, isdiag, isclear

@testset "Utils" begin
    board = Board()
    pos1 = p"C1"
    pos2 = p"F4"
    @test isdiag(pos1, pos2)
    @test !isclear(board, pos1, pos2)
    @test isclear(board, p"F4", p"C7")
end


@testset "Representation" begin
    @testset "Create a cell" begin
        cell = Cell(White)
        @test isempty(cell)
        @test cell.color == White
    end
    @testset "Create board and cell" begin
        # a board is an 8x8 array of cells
        board = Board()
        @test size(board) == (8, 8)
    end
    @testset "Create a pawn" begin
        pawn = Pawn(White)  # Pawn is a struct that takes a color enum
        @test pawn.color == White
    end
    @testset "Create a rook" begin
        rook = Rook(White)
        @test rook.color == White
    end
    @testset "Create a bishop" begin
        bishop = Bishop(Black)
        @test bishop.color == Black
    end
    @testset "Piece traits" begin
        # create traits for each piece to define its movements
        # use the Holy Traits pattern
        # what behavior should a piece have? 
        # It should move.
        # It should be captured or not if it is King
        @testset "Capturable trait" begin end

        @testset "Movement trait" begin end

        @testset "Points trait" begin end
    end
end

@testset "Move pieces" begin
    @testset "Pawns" begin
        board = Board()
        @test board["E2"].piece isa Pawn
        @test board["E2"].piece |> isblack
        @testset "Move pawn E2 to E3" begin
            move!(board, p"E2", p"E3")
            @test board["E3"].piece isa Pawn
        end
        @testset "Move pawn E2 to E4" begin
            move!(board, p"D2", p"D4")
            @test board["D4"].piece isa Pawn
            move!(board, p"D7", p"D6")
            @test board["D6"].piece isa Pawn
        end
        @testset "Raise error: pawn E2 to E5" begin
            move!(board, p"C2", p"C5")
            @test !(board["C5"].piece isa Pawn)
            @test board["C2"].piece isa Pawn
        end

        @testset "Capture" begin
            board = Board()
            move!(board, p"E7", p"E5")
            move!(board, p"E5", p"E4")
            move!(board, p"E4", p"E3")
            @test getstats(board) == (39, 39)
            move!(board, p"D2", p"E3")
            @test getstats(board) == (38, 39)

            # cannot capture teammate
            move!(board, p"F2", p"E3")
            @test board["F2"].piece isa Pawn
            @test getstats(board) == (38, 39)

        end

    end

    @testset "Bishops" begin
        board = Board()
        move!(board, p"D2", p"D3")
        move!(board, p"C1", p"G5")
        @test board["G5"].piece isa Bishop
        move!(board, p"G5", p"F4")
        @test board["F4"].piece isa Bishop

        # capture a piece
        move!(board, p"F4", p"C7")
        @test board["C7"].piece isa Bishop
        @test getstats(board) == (38, 39)
    end

    @testset "Rooks" begin
        board = Board()
        # test rook can't move
        @test board["A1"].piece isa Rook
        move!(board, p"A1", p"A5")
        @test !(board["A5"].piece isa Rook)
        @test board["A1"].piece isa Rook

        # remove pawn so rook can move forward
        board["A2"].piece = nothing
        move!(board, p"A1", p"A5")
        @test board["A5"].piece isa Rook
        # rook can't capture piece behind pawn
        move!(board, p"A5", p"A8")
        @test board["A5"].piece isa Rook
        # rook captures pawn
        move!(board, p"A5", p"A7")
        @test board["A5"].piece isa Nothing
        @test board["A7"].piece isa Rook
    end

    @testset "Queen" begin
        board = Board()
        move!(board, p"E1", p"E3")
        @test board["E1"].piece isa Queen
        move!(board, p"D2", p"D3")
        move!(board, p"E1", p"B4")
        @test board["E1"].piece isa Nothing
        @test board["B4"].piece isa Queen
        move!(board, p"B4", p"B7")
        @test board["B7"].piece isa Queen
        @test getstats(board) == (38, 39)
        move!(board, p"B7", p"A8")
        @test getstats(board) == (33, 39)
        # remove queen and test points
        @test board["A8"].piece isa Queen
        board["A8"].piece = nothing
        @test getstats(board) == (33, 30)
    end

    @testset "Knight" begin
        board = Board()
        move!(board, p"B1", p"B4")
        @test board["B1"].piece isa Knight
        @test board["B4"].piece isa Nothing
        move!(board, p"B1", p"C3")
        @test board["B1"].piece isa Nothing
        @test board["C3"].piece isa Knight
        move!(board, p"C3", p"B1")
        @test board["B1"].piece isa Knight
        @test board["C3"].piece isa Nothing
        move!(board, p"B1", p"A3")
        @test board["B1"].piece isa Nothing
        @test board["A3"].piece isa Knight
    end

    @testset "King" begin
        board = Board()
        move!(board, p"D1", p"D2")
        @test board["D1"].piece isa King
        # remove pawn so king can move
        board["D2"].piece = nothing
        # king cannot move more than one square
        move!(board, p"D1", p"D4")
        @test board["D1"].piece isa King
        @test board["D4"].piece isa Nothing
        move!(board, p"D1", p"D2")
        @test board["D2"].piece isa King
        @test board["D1"].piece isa Nothing

        # king can move forward

        # king can move diagonally
        move!(board, p"D2", p"E3")

    end

    @testset "Check" begin
        board = Board()
        @test !incheck(board)
        # create a board that checks the king
        board["D2"].cell = nothing
        # own piece does not check the king
        board["D6"].cell = Rook(Black)
        @test !incheck(board)
        # opponent does
        board["D6"].cell = Rook(Black)
        @test incheck(board)
    end
end
