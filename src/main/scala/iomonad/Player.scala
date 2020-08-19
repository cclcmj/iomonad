package iomonad

final case class Player(name:String,score:Int)
object Player{
    def winner(p1:Player,p2:Player): Option[Player] = 
        if(p1.score > p2.score) Some(p1)
        else if (p1.score < p2.score) Some(p2)
        else None
    def winnerMsg(p: Option[Player]):String = p map {
        case Player(name,_) => s"$name is the winner"
    } getOrElse "it's a draw"
    def contest(p1:Player,p2:Player): Unit = 
        println(winnerMsg(winner(p1,p2)))
}