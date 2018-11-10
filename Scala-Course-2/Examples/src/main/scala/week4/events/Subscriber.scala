package week4.events

trait Subscriber {

  def handler(pub: Publisher): Unit

}
