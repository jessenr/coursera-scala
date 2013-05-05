package forcomp

object Test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = "Robert".groupBy(identity) map((cs) => (cs._1, cs._2.length()))
                                                  //> x  : scala.collection.immutable.Map[Char,Int] = Map(e -> 1, t -> 1, b -> 1, 
                                                  //| r -> 1, R -> 1, o -> 1)
  
  
  
  val z = (List[(Char,Int)]() /:  "Robert".groupBy(identity)) ((acc,characters) => acc :+ (characters._2 head, characters._2 length)) sortWith((x:(Char,Int), y:(Char,Int)) => x._1 < y._1)
                                                  //> z  : List[(Char, Int)] = List((R,1), (b,1), (e,1), (o,1), (r,1), (t,1))
  
}