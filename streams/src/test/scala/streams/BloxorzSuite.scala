package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-oo-------
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level6 extends SolutionChecker {

    val level =
      """-----oooooo
      |-----o--ooo
      |-----o--ooooo
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo
      |------ooooo
      |------ooooo
      |-------ooo""".stripMargin

    val optsolution = List(Right, Right, Right, Down, Right, Down, Down,
      Right, Down, Down, Right, Up, Left, Left, Left, Up, Up, Left, Up, Up,
      Up, Right, Right, Right, Down, Down, Left, Up, Right, Right, Down,
      Right, Down, Down, Right)
  }

  trait Level11 extends SolutionChecker {
    /*terrain for level 11*/

    // note the button is taken out as it makes it impossible

    val level =
      """-oooo
      |-oToo
      |-ooo
      |-o---oooooo
      |-o---oo--oo
      |Soooooo--ooo
      |-----o-----o
      |-----oooo--o
      |-----ooooooo
      |--------ooo
    """.stripMargin

    val optsolution = List(Right, Right, Right, Right, Up, Left, Down, Down, Down, Right, Right, Right, Down, Left, Up, Left, Left, Left, Up, Up, Right, Up, Right, Right, Down, Right, Up, Left, Left, Left, Down, Down, Left, Left, Left, Up, Up, Right, Up, Up, Left, Down, Right, Up, Right, Down, Left)
  }

  trait Level33a extends SolutionChecker {
    val level =
      """-----oo-ooo----        
	  |-----oooooo----        
	  |ooo---oo-ooooo-        
	  |oSooooooo--oo--        
	  |-----oo-oo-ooo-        
	  |-----oooooo-oo-        
	  |ooo--oooooo-ooo        
	  |o-oooo-o--ooo-T        
	  |ooo--ooo---oooo        
	  |ooo---------ooo""".stripMargin
  }

  trait Level33b extends SolutionChecker {
    val level =
      """-----oo-ooo----        
		  |-----ooooooo---        
		  |ooo---oo-ooooo-        
		  |ooooooooo--oo--        
		  |-----oo-oo-ooo-        
		  |-----oooooo-oo-        
		  |ooo--oooooo-ooo        
		  |oToooo-o--ooo-S        
		  |ooo--ooo---oooo        
		  |ooo---------ooo""".stripMargin
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("Neighbours with history") {
    new Level1 {
      val neighbours = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))

      assert(expected == neighbours)
    }
  }

  test("New Neighbours only") {
    new Level1 {

      val neighbours = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))

      val expected = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream

      assert(expected == neighbours)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("empty solution for level 2") {
    new Level2 {
      assert(solution === Nil)
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("solution for level 6") {
    new Level6 {
      assert(solution === optsolution)
    }
  }

  test("optimal solution for level 11") {
    new Level11 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("solution for level 11") {
    new Level11 {
      assert(solution === optsolution)
    }
  }

  test("optimal solution length for level 11") {
    new Level11 {
      assert(solution.length === optsolution.length)
    }
  }

  test("solution for level 33a") {
    new Level33a {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("solution for level 33b") {
    new Level33b {
      assert(solve(solution) === Block(goal, goal))
    }
  }
}
