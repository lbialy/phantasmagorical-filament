package example.tf

// Dummy effect
trait IO[A]

// Obligatory base typeclasses
trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]

  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

object Monad:
  def apply[F[_]](using F: Monad[F]): Monad[F] = F

trait MonadError[F[_], E]:
  def raiseError[A](e: E): F[A]
  extension [A](fa: F[A]) def handleErrorWith(f: E => F[A]): F[A]

// Base services (capabilities) as algebras
trait FileSystem[F[_]]:
  def read(path: String): F[String]
  def write(path: String, content: String): F[Unit]

object FileSystem:
  def apply[F[_]](using F: FileSystem[F]): FileSystem[F] = F
  def make[F[_]: Monad](using MonadError[F, Throwable]): F[FileSystem[F]] =
    Monad[F].pure:
      new FileSystem[F]:
        def read(path: String): F[String] = ???
        def write(path: String, content: String): F[Unit] = ???

// Infrastructural services as algebras
trait Trace[F[_]]:
  def span[A](name: String)(tracedEffect: F[A]): F[A]

object Trace:
  def apply[F[_]](using F: Trace[F]): Trace[F] = F
  // usually would require network access capability here
  def make[F[_]: Monad]: F[Trace[F]] =
    Monad[F].pure:
      new Trace[F]:
        def span[A](name: String)(tracedEffect: F[A]): F[A] = tracedEffect

trait Hash[F[_]]:
  def hash(value: String): F[String]

object Hash:
  def apply[F[_]](using F: Hash[F]): Hash[F] = F
  def make[F[_]: Monad]: F[Hash[F]] =
    Monad[F].pure:
      new Hash[F]:
        def hash(value: String): F[String] = ???

trait Cache[K, V, F[_]]:
  def get(key: K): F[Option[V]]
  def put(key: K, value: V): F[Unit]

object Cache:
  def apply[K, V, F[_]](using F: Cache[K, V, F]): Cache[K, V, F] = F
  def file[K, V, F[_]: Monad: FileSystem](using MonadError[F, Throwable]): F[Cache[K, V, F]] =
    Monad[F].pure:
      new Cache[K, V, F]:
        def get(key: K): F[Option[V]] =
          FileSystem[F]
            .read(key.toString) // dummy
            .map(deserialize(_))
            .handleErrorWith(_ => Monad[F].pure(None))

        def put(key: K, value: V): F[Unit] =
          // dummy
          FileSystem[F].write(key.toString(), serialize(value))

        // usually a typeclass would be used here
        private def serialize(value: V): String = ???
        private def deserialize(value: String): Option[V] = ???

case class User(email: String, passwordHash: String)

trait UserRepository[F[_]]:
  def createUser(email: String, passwordHash: String): F[User]
  def getUser(email: String): F[User]

object UserRepository:
  def apply[F[_]](using F: UserRepository[F]): UserRepository[F] = F
  // usually would require database transactor here
  def make[F[_]: Monad: Trace]: F[UserRepository[F]] =
    Monad[F].pure:
      new UserRepository[F]:
        def createUser(email: String, passwordHash: String): F[User] =
          Trace[F].span("createUserInDb"):
            ???
        def getUser(email: String): F[User] =
          Trace[F].span("getUserFromDb"):
            ???

// Business services as algebras

trait UserHttpService[F[_]]:
  def createUser(email: String, password: String): F[Unit]
  def authenticateUser(email: String, password: String): F[User]

object UserHttpService:
  def make[F[_]: Monad: UserRepository: Trace: Hash](using ME: MonadError[F, Throwable], C: Cache[String, User, F]): F[UserHttpService[F]] =
    Monad[F].pure:
      new UserHttpService[F]:
        def createUser(email: String, password: String): F[Unit] =
          Trace[F].span("createUser"):
            for
              passwordHash <- Hash[F].hash(password)
              user <- UserRepository[F].createUser(email, passwordHash)
              _ <- Cache[String, User, F].put(email, user)
            yield ()

        def authenticateUser(email: String, password: String): F[User] =
          Trace[F].span("authenticateUser"):
            for
              cachedUser <- Cache[String, User, F].get(email)
              user <- cachedUser.fold(UserRepository[F].getUser(email))(Monad[F].pure)
              receivedPwdHash <- Hash[F].hash(password)
              _ <- if receivedPwdHash == user.passwordHash then Monad[F].pure(()) else ME.raiseError(Exception("Invalid password"))
            yield user

// Wiring
@main def tfMain =
  given Monad[IO] = ???
  given MonadError[IO, Throwable] = ???

  val program: IO[Unit] = for
    given FileSystem[IO] <- FileSystem.make[IO]
    given Trace[IO] <- Trace.make[IO]
    given Hash[IO] <- Hash.make[IO]
    given Cache[String, User, IO] <- Cache.file[String, User, IO]
    given UserRepository[IO] <- UserRepository.make[IO]
    userHttpService <- UserHttpService.make[IO]
    _ <- userHttpService.createUser("lukasz", "mypasswd23")
  yield ()
