class AI(private var player: Player, private var depth: Int) extends Solver {
  
  override def getMoves(b: Board): Array[Move] = {
    val state = new State(player, b, null)
    AI.createGameTree(state, depth)
    AI.minimax(this, state)
    state.value = max(state.children)
    var bestMoves = Array[Move]()
    for (i <- state.children) {
      if (state.value == i.value) {
        bestMoves :+= i.getLastMove()
      }
    }
    return bestMoves
  }

  def minimax(s: State) {

    if(s.children(0).children.length == 0) { 
      for (leaf <- s.children) {
        leaf.value = evaluateBoard(leaf.board)
      }
    } else {
      for (i <- s.children) {
        minimax(i)       
        if (player != s.player) {
          i.value = max(i.children)
        } else {
          i.value = min(i.children)
        }
      }
    }
  }
  
  def max(children: Array[State]): Int = {
    var maxVal = Int.MinValue
    for (i <- children) {
      if (i.value > maxVal)
        maxVal = i.value
    }
    maxVal
  }

  def min(children: Array[State]): Int = {
    var minVal = Int.MaxValue
    for (i <- children) {
      if (i.value < minVal)
        minVal = i.value
    }
    minVal
  }

  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

object AI {

  def createGameTree(s: State, d: Int) {
    if(d == 0){
      return
    } else {
      s.initializeChildren()
      var arr: Array[State] = null
      arr = s.children
      for(i <- arr) {
        createGameTree(i, d - 1)
      }    
    }
  }

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

