package pms.core

import pms.effects._
import pms.effects.implicits._

/**
  *
  * @author Lorand Szakacs, https://github.com/lorandszakacs
  * @since 20 Jun 2018
  *
  */
object PlainTextPassword {

  //TODO: make these restrictions configurable
  def apply(pw: String): Attempt[PlainTextPassword] =
    if (pw.length < 6)
      Attempt.raiseError(Fail.invalid("Password needs to have at least 6 characters"))
    else Attempt.pure(new PlainTextPassword(pw))

}

final class PlainTextPassword private (val plainText: String) {

  //generated by IntelliJ
  override def equals(other: Any): Boolean = other match {
    case that: PlainTextPassword =>
      plainText == that.plainText
    case _ => false
  }

  //generated by IntelliJ
  override def hashCode(): Int =
    31 * plainText.hashCode
}
