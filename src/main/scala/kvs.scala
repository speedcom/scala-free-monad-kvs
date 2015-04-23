
import scalaz._
import Scalaz._
import Free._

// This example is based off the one in Runar Bjarnason's "Dead Simple Dependency Injection" talk.
// http://www.youtube.com/watch?v=ZasXwtTRkio

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
case class Put[Next](key: String, value: String, next: Next) extends KVS[Next]     // <----  def put(key: String, value: String): Unit
case class Get[Next](key: String, onResult: String => Next) extends KVS[Next]      // <----  def get(key: String): String
case class Delete[Next](key: String, next: Next) extends KVS[Next]                 // <----  def delete(key: String): Unit
