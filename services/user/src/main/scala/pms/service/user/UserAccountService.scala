package pms.service.user

import pms.core._
import pms.effects._
import pms.logger._
import pms.effects.implicits._

import pms.email._

import pms.algebra.user._

/**
  *
  * @author Lorand Szakacs, https://github.com/lorandszakacs
  * @since 26 Jun 2018
  *
  */
final class UserAccountService[F[_]] private (
  private val userAuth:     UserAuthAlgebra[F],
  private val userAccount:  UserAccountAlgebra[F],
  private val userAlgebra:  UserAlgebra[F],
  private val emailAlgebra: EmailAlgebra[F],
)(
  implicit private val F:     Concurrent[F],
  implicit private val timer: Timer[F],
) {

  private val logger: PMSLogger[F] = PMSLogger.getLogger[F]

  def invitationStep1(inv: UserInvitation)(implicit authCtx: AuthCtx): F[Unit] =
    for {
      inviteToken <- userAccount.invitationStep1(inv)
      logToken    <- F.delay(scala.util.Random.nextLong())
      _ <- {
        val sendEmail = emailAlgebra
          .sendEmail(
            to = inv.email,
            //TODO: resolve this data from an email content algebra or something
            subject = s"You have been invited to join Pure Movie Server as a :${inv.role.productPrefix}",
            //TODO: resolve this data from an email content algebra or something
            content = s"Please click this link to finish registration: [link_to_frontend]/$inviteToken",
          )

        //TODO: make this retry stuff generic and parameterized on how many
        //TODO: retries, and how much to wait between retries
        import scala.concurrent.duration._
        BracketAttempt[F].guaranteeCase(sendEmail) {
          case ExitCase.Completed => F.unit
          case ExitCase.Canceled  => userAccount.deleteUserInvitation(inviteToken)
          case ExitCase.Error(e) =>
            val toWait = 500.millis //TODO: read from a config file

            logger.warn(e)(s"$logToken — Failed to send email — waiting $toWait") >>
              timer.sleep(toWait) >>
              sendEmail.handleErrorWith { fe =>
                logger.warn(fe)(s"$logToken Failed to send email a second time. No longer retrying") >>
                  userAccount.deleteUserInvitation(inviteToken)
              }
        }
      }.forkAndForget
    } yield ()

  def invitationStep2(conf: UserConfirmation): F[User] =
    for {
      user <- userAccount.invitationStep2(conf.invitationToken, conf.plainTextPassword)
    } yield user

  def resetPasswordStep1(email: Email): F[Unit] =
    for {
      resetToken <- userAccount.resetPasswordStep1(email)
      _ <- emailAlgebra
        .sendEmail(
          to      = email,
          subject = "Password reset for Pure Movie Server",
          content = s"Please click the following link to reset your account password: [link_to_FE]$resetToken",
        )
        .forkAndForget
    } yield ()

  def resetPasswordStep2(pwr: PasswordResetCompletion): F[Unit] =
    userAccount.resetPasswordStep2(pwr.token, pwr.newPws)

}

object UserAccountService {

  def concurrent[F[_]: Concurrent: Timer](
    userAuth:     UserAuthAlgebra[F],
    userAccount:  UserAccountAlgebra[F],
    userAlgebra:  UserAlgebra[F],
    emailAlgebra: EmailAlgebra[F],
  ): UserAccountService[F] = new UserAccountService[F](
    userAuth,
    userAccount,
    userAlgebra,
    emailAlgebra,
  )
}
