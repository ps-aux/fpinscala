
/**
  * Created by arkonix on 12/19/16.
  */
object MyApp extends App {

  def matchTest(x: Int): String = x match {
    case 1 => "one"
    case 2 => "two"
    case _ => "many"
  }

}
