object Bob {

  def response(statement: String): String = {
    bobStateMachine(statement)
  }

  def bobStateMachine: String => String = (statement: String) => {
    val state = statement.foldLeft(Nothing: State)((state: State, char: Char) => nextState(state, char))
    state.answer
  }

  /**
    * This function calculate the next state according to the actual and the input char.
    * @return the next state
    */
  def nextState: (State, Char) => State = (state: State, char: Char) => {
    def notEmpty(x: Char): Boolean = {
      x != ' '
    }

    char match {
      case x if x.isUpper => state match {
        case Yell => YellQuestion
        case _ => Yell
      }
      case x if x.isLower => state match {
        case _ => Whatever
      }
      case '?' => state match {
        case Yell => YellQuestion
        case _ => Question
      }
      case x if notEmpty(x) => state match {
        case Yell => Yell
        case YellQuestion => YellQuestion
        case Question => Question
        case Nothing => Whatever
        case _ => Whatever
      }
      case _ => state
    }

  }

  sealed abstract class State {
    val answer: String
  }

  object Question extends State {
    override val answer: String = "Sure."
  }

  object Yell extends State {
    override val answer: String = "Whoa, chill out!"
  }

  object YellQuestion extends State {
    override val answer: String = "Calm down, I know what I'm doing!"
  }

  object Whatever extends State {
    override val answer: String = "Whatever."
  }

  object Nothing extends State {
    override val answer: String = "Fine. Be that way!"
  }

}
