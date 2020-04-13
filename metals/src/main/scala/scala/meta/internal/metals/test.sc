import scala.concurrent.{Await, Future}
import scala.concurrent.Future._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Failure

val list = Seq(1, 2, 3)

def mafunction(list: Seq[Int], filter: Int => Boolean) = list.filter(filter)

def filter(value: Int, b: Boolean) = value > 1

mafunction(list, filter(_, true))

Future.successful(1)
//Future(1)

//Await.result(Future(Nil.head), 5.seconds)

val k = Future(Nil.head).onComplete {
  case Failure(e) => println("ok")
  case _ => println("ca")
}

k
