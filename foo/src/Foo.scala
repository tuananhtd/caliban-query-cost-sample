package foo

import caliban.*
import caliban.schema.ArgBuilder.auto.*
import caliban.schema.Schema
import caliban.schema.Schema.auto.*
import caliban.wrappers.CostEstimation
import caliban.wrappers.CostEstimation.*
import zio.*

object Foo extends ZIOAppDefault {

  case class FriendsArgs(limit: Int)

  @GQLCost(1)
  case class Character(
    name: String,
    age: Int,
    @GQLCost(1, multipliers = List("limit"))
    friends: FriendsArgs => UIO[List[Character]]
  )

  given Schema[Any, Character] = Schema.gen[Any, Character]

  def getFriends(characterName: String, limit: Int): UIO[List[Character]] = {
    // Mock data: each character has some friends (no friends to avoid infinite recursion)
    val allFriends = characterName match {
      case "John" => List(
        Character("Jane", 25, _ => ZIO.succeed(Nil)),
        Character("Bob", 35, _ => ZIO.succeed(Nil)),
        Character("Alice", 28, _ => ZIO.succeed(Nil))
      )
      case "Jane" => List(
        Character("John", 30, _ => ZIO.succeed(Nil)),
        Character("Alice", 28, _ => ZIO.succeed(Nil))
      )
      case "Bob" => List(
        Character("John", 30, _ => ZIO.succeed(Nil))
      )
      case "Alice" => List(
        Character("Jane", 25, _ => ZIO.succeed(Nil)),
        Character("Bob", 35, _ => ZIO.succeed(Nil))
      )
      case _ => Nil
    }
    ZIO.succeed(allFriends.take(limit))
  }

  def getCharacters(limit: Int): List[Character] = List(
    Character("John", 30, args => getFriends("John", args.limit)),
    Character("Jane", 25, args => getFriends("Jane", args.limit)),
    Character("Bob", 40, args => getFriends("Bob", args.limit)),
    Character("Alice", 35, args => getFriends("Alice", args.limit))
  ).take(limit)

  // schema
  case class CharacterLimit(limit: Int)

  case class Queries(
    @GQLCost(1, List("limit"))
    getCharacters: CharacterLimit => List[Character]
  )

  // resolver
  val queries = Queries(args => getCharacters(args.limit))

  override def run: ZIO[Any, Any, Unit] = {
    val api = graphQL(RootResolver(queries)) @@
      queryCost @@
      maxCost(1000)(CostEstimation.costDirective)

    for {
      interpreter <- api.interpreter
      result <- interpreter.execute("{ getCharacters(limit: 2) { friends(limit: 5) { name } } }")
      _ <- Console.printLine(s"Result: $result")
      _ <- Console.printLine(s"Cost: ${result.extensions}\n") // this will show queryCost = 9
    } yield ()
  }
}

