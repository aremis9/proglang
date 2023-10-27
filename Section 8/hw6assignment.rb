# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

    def initialize(point_array, board)
        super(point_array, board)
    end

    # class array holding all the pieces and their rotations
    
    All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                    [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),  # Z
                    rotations([[-1, 0], [0, 0], [1, 0], [1, 1], [0, 1]]),  # thick L
                    [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],  # very long (only needs two)
                    [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
                    rotations([[0, 0], [0, 1], [1, 0]])]  # short L

    # class method to choose the next piece
    def self.next_piece (board)
        if board.cheat
            board.cheat = false
            MyPiece.new([[[0, 0]]], board)
        else
            MyPiece.new(All_My_Pieces.sample, board)
        end
    end
end

class MyBoard < Board
    # your enhancements here

    def initialize(game)
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @delay = 500
        @cheat = false
    end

    # gets the next piece
    def next_piece()
        @current_block = MyPiece.next_piece(self)
        @current_pos = nil
    end

    def store_current
        locations = @current_block.current_rotation
        displacement = @current_block.position
        size = locations.size
        (0..(size - 1)).each{|index| 
            current = locations[index];
            @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
            @current_pos[index]
        }
        remove_filled
        @delay = [@delay - 2, 80].max
    end

    def cheat
        @cheat
    end

    def cheat= x
        @cheat = x
    end

    def cheated
        if @score >= 100 and @cheat == false
            @score -= 100
            @cheat = true
        end
    end
end

class MyTetris < Tetris
    # your enhancements here

    def initialize
        super
        set_board
        key_bindings
    end

    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw
    end

    def key_bindings
        super
        @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
        @root.bind('c', proc {@board.cheated})
    end
end


