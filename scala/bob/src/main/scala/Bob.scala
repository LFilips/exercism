object Bob {

  def response(statement: String): String = ???

  def bobStateMachine: String => Unit = (statement: String) => {
    statement.foldRight(Nothing: State)((char: Char, state: State) => nextState(state,char))
  }

  def nextState = (state: State,char: Char) => {
    char match {
      case x if x.isUpper => state match {
        case Yell => YellQuestion
        case _ => Question
      }
      case '?' =>  state match {
        case Yell => YellQuestion
        case _ => Question
      }
      case x if x.isLetterOrDigit => state match {
        case Yell => Yell
        case YellQuestion => YellQuestion
        case Question => Question
        case Nothing => Whatever
        case _ => Whatever
      }
      case _ => state
    }

  }


  trait State

  object Question extends State {
    val answer= "Sure."
  }

  object Yell extends State

  object YellQuestion extends State

  object Whatever extends State

  object Nothing extends State

}
