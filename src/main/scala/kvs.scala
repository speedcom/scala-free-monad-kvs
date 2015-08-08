
import scalaz.Free._
import scalaz._

// This example based on the one in Runar Bjarnason's "Dead Simple Dependency Injection" talk.
// http://www.youtube.com/watch?v=ZasXwtTRkio

// RECIPE
// 1. ADT
// 2. Functor definition
// 3. Lifting functions
// 4. Composite functions
// 5. Write scripts
// 6. Interpreters

// Fantasy API
// def put(key: String, value: String): Unit
// def get(key: String): String
// def delete(key: String): Unit

// 1. ADT
trait KVS[Next]

// def put(key: String, value: String): Unit
case class Put[Next](key: String, value: String, next: Next) extends KVS[Next]

// def get(key: String): String
case class Get[Next](key: String, onResult: String => Next) extends KVS[Next]

// def delete(key: String): Unit
case class Delete[Next](key: String, next: Next) extends KVS[Next]


object KVS {

  // 2. Functor definition
  implicit val functorKvs = new Functor[KVS] {
    override def map[A, B](kvs: KVS[A])(f: (A) => B): KVS[B] = kvs match {
      case Put(key, value, next) => Put(key, value, f(next))
      case Get(key, h) => Get(key, result => f(h(result)))
      case Delete(key, next) => Delete(key, f(next))
    }
  }

  // 3. Lifting functions - rise our ADT + companion Functor to create API using FreeMonad
  def put(key: String, value: String): Free[KVS, Unit] = liftF(Put(key, value, ()))

  def get(key: String): Free[KVS, String] = liftF(Get(key, identity))

  def delete(key: String): Free[KVS, Unit] = liftF(Delete(key, ()))

  // 4. Composite functions
  def modify(key: String, f: String => String): Free[KVS, Unit] = for {
    s <- get(key)
    _ <- put(key, f(s))
  } yield ()

  // 5. Write scripts
  type Script[A] = Free[KVS, A]

  val script: Script[(String, String)] = for {
    _ <- put("key_1", "10000")
    _ <- put("key_2", "999")
    _ <- modify("key_1", _ + "0000")
    _ <- delete("key_3")
    k1_v <- get("key_1")
    k2_v <- get("key_2")
  } yield ((k1_v, k2_v))

  // 6. Interpreters
  def pureInterpreter(script: Script[(String, String)],
                      data: Map[String, String] = Map.empty): Map[String, String] = {
    script.resume.fold({
      case Get(key, h) => pureInterpreter(h(data(key)), data)
      case Put(key, value, next) => pureInterpreter(next, data + (key -> value))
      case Delete(key, next) => pureInterpreter(next, data - key)
    }, _ => data
    )
  }

}

object KvsApp extends App {

  val data = Map("key_30" -> "1")
  val dataAfter = KVS.pureInterpreter(KVS.script, data)
  println(s"dataAfter: $dataAfter")
}
