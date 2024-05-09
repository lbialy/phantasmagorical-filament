package example.caps

import scala.annotation.capability

// base capabilities

@capability trait Async:
  def suspend[A](block: => A): A // dummy

object Async:
  given Async = new Async:
    def suspend[A](block: => A): A = block // dummy

@capability trait FileSystem:
  def read(path: String): String
  def write(path: String, content: String): Unit

object FileSystem:
  given FileSystem = new FileSystem:
    def read(path: String): String = ???
    def write(path: String, content: String): Unit = ???

// infrastructural services

// should this be a capability? it does store a state
@capability trait Trace:
  def span[A](name: String)(block: => A): A // would this be a sensible signature?

object Trace:
  // would this capture a Network capability here? it will need it for the whole lifetime
  given Trace = new Trace:
    def span[A](name: String)(block: => A): A = block // dummy

trait Hash:
  def hash(value: String): String

class HashImpl() extends Hash:
  def hash(value: String): String = ???

// our trait is clean of any capabilities
trait Cache[K, V]:
  def get(key: K): Option[V]
  def put(key: K, value: V): Unit

// is this a correct way to capture the FileSystem capability?
class FileCache[K, V](using fs: FileSystem) extends Cache[K, V]:
  def get(key: K): Option[V] =
    val content = fs.read(key.toString())
    deserialize(content) match
      case Some(v) => Some(v)
      case None    => None

  def put(key: K, value: V): Unit =
    val content = serialize(value)
    fs.write(key.toString(), content)

  // usually a typeclass would be used here
  private def serialize(value: V): String = ???
  private def deserialize(content: String): Option[V] = ???

case class User(email: String, passwordHash: String)

// our trait is generic in capabilities
trait UserRepository[C]:
  def createUser(email: String, passwordHash: String)(using C): Either[Throwable, User]
  def getUser(email: String)(using C): Either[Throwable, User]

// what if we need more than one capability here?
class UserRepositoryImpl extends UserRepository[Trace]:
  def createUser(email: String, passwordHash: String)(using Trace): Either[Throwable, User] =
    summon[Trace].span("createUserInDb") {
      ???
    }
  def getUser(email: String)(using Trace): Either[Throwable, User] =
    summon[Trace].span("getUserFromDb") {
      ???
    }

// alternative encoding with context functions
trait CtxUserRepository[F[_]]:
  def createUser(email: String, passwordHash: String): F[User]
  def getUser(email: String): F[User]

class CtxUserRepositoryImpl extends CtxUserRepository[CtxUserRepositoryImpl.Eff]:
  import CtxUserRepositoryImpl.Eff

  def createUser(email: String, passwordHash: String): Eff[User] =
    summon[Trace].span("createUserInDb") {
      ???
    }
  def getUser(email: String): Eff[User] =
    summon[Trace].span("getUserFromDb") {
      ???
    }

object CtxUserRepositoryImpl:
  type Eff[A] = Trace ?=> Either[Throwable, A]

// Business services

trait UserHttpService[C]:
  def createUser(email: String, password: String)(using C): Either[Throwable, Unit]
  def authenticate(email: String, password: String)(using C): Either[Throwable, User]

class UserHttpServiceImpl(userRepo: UserRepository[Trace], cache: Cache[String, User], hash: Hash) extends UserHttpService[Trace & Async]:
  def createUser(email: String, password: String)(using Trace & Async): Either[Throwable, Unit] =
    summon[Trace].span("createUser") {
      userRepo.createUser(email, password) match
        case Left(ex)    => Left(ex)
        case Right(user) => cache.put(user.email, user)

      Right(())
    }
  def authenticate(email: String, password: String)(using Trace & Async): Either[Throwable, User] =
    summon[Trace].span("authenticate") {
      cache
        .get(email)
        .fold(userRepo.getUser(email))(Right(_))
        .flatMap: user =>
          val pwdHash = hash.hash(password)

          if user.passwordHash == pwdHash then Right(user)
          else Left(Exception("Invalid password"))
    }

// alternative encoding with context functions

trait CtxUserHttpService[F[_]]:
  def createUser(email: String, password: String): F[Unit]
  def authenticate(email: String, password: String): F[User]

import CtxUserHttpServiceImpl.Eff
import CtxUserRepositoryImpl.{Eff => CtxUserRepositoryEff}

class CtxUserHttpServiceImpl(userRepo: CtxUserRepository[CtxUserRepositoryEff], cache: Cache[String, User], hash: Hash)
    extends CtxUserHttpService[Eff]:

  def createUser(email: String, password: String): Eff[Unit] =
    summon[Trace].span("createUser") {
      userRepo.createUser(email, password) match
        case Left(ex)    => Left(ex)
        case Right(user) => cache.put(user.email, user)

      Right(())
    }

  def authenticate(email: String, password: String): Eff[User] =
    summon[Trace].span("authenticate") {
      cache
        .get(email)
        .fold(userRepo.getUser(email))(Right(_))
        .flatMap: user =>
          val pwdHash = hash.hash(password)

          if user.passwordHash == pwdHash then Right(user)
          else Left(Exception("Invalid password"))
    }

object CtxUserHttpServiceImpl:
  type Eff[A] = Trace ?=> Async ?=> Either[Throwable, A]

@main def main =
  val userRepo: UserRepository[Trace] = UserRepositoryImpl()
  val cache: Cache[String, User] = FileCache[String, User]
  val hash: Hash = HashImpl()

  val userHttpService = UserHttpServiceImpl(userRepo, cache, hash)
  locally:
    // No given instance of type example.caps.Trace & example.caps.Async was found for parameter x$3 of method createUser in class UserHttpServiceImpl
    // without this:
    type Ctx = Trace & Async
    given Ctx = new Trace with Async:
      def span[A](name: String)(block: => A): A = Trace.given_Trace.span(name)(block)
      def suspend[A](block: => A): A = Async.given_Async.suspend(block)

      userHttpService.createUser("lukasz", "mypasswd23")

  val ctxUserRepo = CtxUserRepositoryImpl()
  val ctxUserHttpService = CtxUserHttpServiceImpl(ctxUserRepo, cache, hash)
  locally:
    // this takes givens from companion objects which may be more convenient
    // but also quite confusing when there are multiple instances
    ctxUserHttpService.createUser("lukasz", "myPasswd23")
