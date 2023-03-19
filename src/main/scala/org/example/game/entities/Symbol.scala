package org.example.game.entities

sealed trait Symbol {
    protected  val beats: List[Symbol]

    def wins(other:Symbol): Boolean = 
        beats.contains(other)
}

/**
  * Rock crushes lizard
  * Rock crushes Scissors
  */
case object Rock extends Symbol {
    protected val beats: List[Symbol] = List(Lizard, Scissors)
}

/**
  * Paper covers rock
  * Paper disapproves Spock
  */

case object Paper extends Symbol {
    protected val beats: List[Symbol] = List(Rock, Spock)
}

/**
* Scissors cuts Paper
* Scissors Decapitates Lizard
*/

case object Scissors extends Symbol {
    protected val beats: List[Symbol] = List(Paper, Lizard)
}

/**
* Lizard poisons Spock
* Lizard eats paper
*/

case object Lizard extends Symbol {
    protected val beats: List[Symbol] = List(Paper, Spock)
}

/**
* Spock smashes Scissors
* Spock vaporizes Rock
*/

case object Spock extends Symbol {
    protected val beats: List[Symbol] = List(Scissors, Rock)
}

object Symbol {
    def fromString(text: String): Symbol = 
        text.trim.toLowerCase match {
            case "rock" => Rock
            case "paper" => Paper
            case "scissors" => Scissors
            case "spock" => Spock
            case "lizard" => Lizard 
            case unknown =>
                val errorMsg = s"Unknown Symbol $unknown." +
                "Please pick a valid symbol [Rock, Paper, Scissors, Lizard, Spock]"
                throw new IllegalArgumentException(errorMsg)
        }
}

class Player(name: String, val symbol: Symbol) {
    override def toString(): String = s"Player $name with symbol $symbol"
}

object Player {
    // valid example: "Daniela: Spock"
    def apply(text: String): Player = 
        text.split(":", 2) match {
            case Array(name, symbol)=> 
                new Player(name.trim, Symbol.fromString(symbol))
            case _ => 
                val errorMsg = s"Invalid player $text." + 
                "Please, use the format <name>: <symbol>" 
                throw new IllegalArgumentException(errorMsg)
        }
    
    def unapply(player: Player): Option[Symbol] = Some(player.symbol)

}

case class Game(playerA: Player, playerB: Player) {
    private val winner: Option[Player] = 
        (playerA, playerB) match {
            case (pA @ Player(sA), Player(sB)) if sA.wins(sB) => Some(pA)
            case (Player(sA), pB @ Player(sB)) if sB.wins(sA) => Some(pB)
            case _ => None

        }
    val result: String = winner.map(player => s"$player wins!\n").getOrElse("It's a draw!\n")
}

object Game {
    // valid example: "Daniela: Spock - Martin: Paper"
    def apply(text: String): Game = 
        text.split("-", 2) match
            case Array(playerA, playerB) =>
                apply(Player(playerA), Player(playerB))
            case _  => 
                val errorMsg = s"Invalid game $text. " +
                s"Please, use the format <name>: <symbol> - <name>: <symbol>"
                throw new IllegalArgumentException(errorMsg)
        
        
}