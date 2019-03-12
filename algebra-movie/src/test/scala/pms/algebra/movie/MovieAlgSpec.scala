package pms.algebra.movie

import java.time.LocalDate

import pms.core.PlainTextPassword
import pms.effects._
import pms.server.bootstrap.ModuleServerBootstrap
import pms.db.config._
import doobie.util.transactor.Transactor
import pms.algebra.user._
import pms.core.Email
import spire.math.Interval

import scala.concurrent.ExecutionContext

class MovieAlgSpec
    extends org.specs2.mutable.Specification with ModuleMovieAsync[IO] with ModuleServerBootstrap[IO]
    with ModuleUserAsync[IO] with ModuleUserBootstrap[IO] {

  val databaseConfig =
    DatabaseConfig("org.postgresql.Driver", "jdbc:postgresql:testmoviedatabase", "busyuser", "qwerty", false)

  implicit val cs = IO.contextShift(ExecutionContext.global)
  implicit val transactor:       Transactor[IO]                  = DatabaseConfigAlgebra.transactor(databaseConfig).unsafeRunSync()
  implicit val userAccount:      UserAccountAlgebra[IO]          = UserAccountAlgebra.async(async, transactor)
  implicit val userBootstrapAlg: UserAccountBootstrapAlgebra[IO] = UserAccountBootstrapAlgebra.impl(userAccount)

  DatabaseConfigAlgebra.initializeSQLDb(databaseConfig).unsafeRunSync()

  val (user: User, email: Email, userRole: UserRole, userPw: PlainTextPassword) =
    TestHelpersFunctions.unsafeCreateUser("SuperAdmin_Movie@yahoo.com", "SuperAdmin", "password1234")

  implicit def async: Async[IO] = Concurrent.apply[IO]

  val userAuth = UserAuthAlgebra.async(async, transactor)

  implicit val authCtx = userAuth.authenticate(email, userPw).unsafeRunSync()

  "MovieAlgebra" should {

    """create a movie""" in {
      val movie = MovieCreation(MovieTitle("Title"), Option(ReleaseDate(LocalDate.now())))

      val result = movieAlgebra.createMovie(movie)

      result.unsafeRunSync().name mustEqual movie.name
    }

    """find all movies in an interval""" in {

      val intervalQuery: QueryInterval = Interval(ReleaseDate(LocalDate.of(2000, 7, 12)), ReleaseDate(LocalDate.now()))
      val result = movieAlgebra.findMoviesBetween(intervalQuery)

      result.unsafeRunSync().isEmpty should_== (false)

    }
  }
}