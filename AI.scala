import java.util.List
import AI._
//remove if not needed
import scala.collection.JavaConversions._

object AI {

  def createGameTree(s: State, d: Int) {
    if(d == 0){
      //allocate a weighting to that move
      //minimax()
      //AI.
      //evaluateBoard
      //println(s)
      //println("fINISH")
      //val ai = new AI(s.player.opponent,1)
      //AI.minimax(ai, s)
      //println(s)
    } else {
      s.initializeChildren()
      var arr: Array[State] = null
      arr = s.children
      for(i <- arr) {
        //println("new tree")
        //println(i)
        createGameTree(i, d - 1)
      }
    
    }
  }

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

class AI(private var player: Player, private var depth: Int) extends Solver {

  override def getMoves(b: Board): Array[Move] = {
    //val lastMove = b.
    val state = new State(player.opponent, b, null)
    //state.initializeChildren()
    createGameTree(state, depth)

    //println(state.children(2).getLastMove)
    minimax(state)
    val choice = state.value
    println(choice)
    var moves = Array[Move]()
    var counter = 0
    for (i<-state.children) {

      if (choice == i.value) {
        println(i.getLastMove.toString())
        moves :+= i.children(counter).getLastMove
      }
      counter = counter+1
    }

    //val a = b.getPossibleMoves(player)
    return moves
  }

  def minimax(s: State): Unit = {
    if (s.children(0).children.length == 0) {
      for (i <- s.children) {
        i.value = evaluateBoard(i.board)
      }
      if (s.player == player) {
        //max of s.children
        s.value = max(s.children)

      } else {
        //min of s.children
        s.value = min(s.children)
      }
    } else {
      for (i<-s.children) {
        //println("getting there")
        minimax(i)
      }
      if (s.player == player) {
        //max of s.children
        s.value = max(s.children)
      } else {
        //min of s.children
        s.value = min(s.children)
      }
    }
  }

  def max(children: Array[State]): Int = {
    var maxVal = Int.MinValue
    for (i<-children) {
      if (i.value > maxVal)
        maxVal = i.value
    }
    maxVal
  }

  def min(children: Array[State]): Int = {
    var minVal = Int.MaxValue
    for (i<-children) {
      if (i.value < minVal)
        minVal = i.value
    }
    minVal
  }

  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (winner == null) {
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
      value = (if (winner == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}