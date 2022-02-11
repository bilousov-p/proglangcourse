# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),
               rotations([[0, 0], [0, 0], [0, 1], [1, 1]]), # third figure
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), # first figure
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]],
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]]] # second figure

  Cheat_Piece = [[[0,0]]]           
 
  # class method to choose the next piece
  def self.next_piece (board, cheating)
    if cheating
      MyPiece.new(Cheat_Piece, board)
    else  
      MyPiece.new(All_My_Pieces.sample, board)
    end  
  end
  

end

class MyBoard < Board
  def initialize (game)
    super(game)
    @cheating = false
    @current_block = MyPiece.next_piece(self, @cheating)
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self, @cheating)
    @current_pos = nil
    @cheating = false
  end

  
  def try_cheat
    if @score > 99 and !@cheating
      cheat()
    end  
  end

  # lets you cheat
  def cheat
   @score = @score - 100
   @game.update_score
   @cheating = true
  end

  def store_current
     locations = @current_block.current_rotation 
     displacement = @current_block.position
     (0..(@current_block.current_rotation.size - 1)).each{|index|
       current = locations[index];
       @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
       @current_pos[index]
    }
    remove_filled
     @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings 
    super()
    @root.bind('u', proc { @board.rotate_clockwise; @board.rotate_clockwise })
    @root.bind('c', proc { @board.try_cheat })
  end

end


