package example.zio

import zio.*

// Base services (capabilities) as interfaces with ZIO methods

trait FileSystem:
  def read(path: String): Task[String]
  def write(path: String, content: String): Task[Unit]

object FileSystemImpl:
  val layer = ZLayer.succeed(new FileSystem:
    def read(path: String): Task[String] = ???
    def write(path: String, content: String): Task[Unit] = ???
  )

// Infrastructural services as interfaces with ZIO methods

trait Trace:
  def span[R, E, A](name: String)(tracedEffect: ZIO[R, E, A]): ZIO[R, E, A]

object TraceImpl:
  // usually would require network access capability here
  val layer = ZLayer.succeed(
    new Trace:
      def span[R, E, A](name: String)(tracedEffect: ZIO[R, E, A]): ZIO[R, E, A] = tracedEffect // no-op
  )

trait Hash:
  def hash(value: String): Task[String]

object HashImpl:
  val layer = ZLayer.succeed(
    new Hash:
      def hash(value: String): Task[String] = ???
  )

trait Cache[K, V]:
  def get(key: K): Task[Option[V]]
  def put(key: K, value: V): Task[Unit]

object CacheImpl:
  def file[K, V](fileSystem: FileSystem): Cache[K, V] = new Cache:
    def get(key: K): Task[Option[V]] =
      fileSystem.read(key.toString()).map(deserialize(_)).catchAll(_ => ZIO.succeed(None))

    def put(key: K, value: V): Task[Unit] =
      fileSystem.write(key.toString(), serialize(value))

    // usually a typeclass would be used here
    private def serialize(value: V): String = ???
    private def deserialize(value: String): Option[V] = ???

  def fileLayer[K: Tag, V: Tag] = ZLayer.fromFunction(file[K, V])

case class User(id: String, email: String, passwordHash: String)

trait UserRepository:
  def createUser(email: String, passwordHash: String): Task[User]
  def getUser(email: String): Task[User]

object UserRepositoryImpl:
  def make(trace: Trace): UserRepository = new UserRepository:
    def createUser(email: String, passwordHash: String): Task[User] =
      trace.span("createUserInDb"):
        ???

    def getUser(email: String): Task[User] =
      trace.span("getUserFromDb"):
        ???

  // usually would require database transactor here
  val layer = ZLayer.fromFunction(make)

// Business services as interfaces with ZIO methods

trait UserHttpService:
  def createUser(email: String, password: String): Task[Unit]
  def authenticateUser(email: String, password: String): Task[User]

object UserHttpServiceImpl:
  def make(userRepository: UserRepository, cache: Cache[String, User], trace: Trace, hash: Hash): UserHttpService = new UserHttpService:
    def createUser(email: String, password: String): Task[Unit] =
      trace.span("createUser"):
        for
          pwdHash <- hash.hash(password)
          user <- userRepository.createUser(email, pwdHash)
          _ <- cache.put(email, user)
        yield ()

    def authenticateUser(email: String, password: String): Task[User] =
      trace.span("authenticateUser"):
        for
          cachedUser <- cache.get(email)
          user <- cachedUser.fold(userRepository.getUser(email))(ZIO.succeed)
          receivedPwdHash <- hash.hash(password)
          _ <- if receivedPwdHash == user.passwordHash then ZIO.unit else ZIO.fail(Exception("Invalid password"))
        yield user

  val layer = ZLayer.fromFunction(make)

// Wiring
@main def zioMain =
  val program =
    for
      svc <- ZIO.service[UserHttpService]
      _ <- svc.createUser("lukasz", "mypasswd23")
    yield ()

  val runnableZIO: ZIO[Any, Throwable, Unit] =
    program.provide(
      CacheImpl.fileLayer[String, User],
      FileSystemImpl.layer,
      UserRepositoryImpl.layer,
      UserHttpServiceImpl.layer,
      HashImpl.layer,
      TraceImpl.layer
    )
