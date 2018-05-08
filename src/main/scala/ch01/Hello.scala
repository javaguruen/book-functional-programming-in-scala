package ch01

object Hello{
  def main(args: Array[String]): Unit ={
    println("Initiating greeting...")
    println(new Hello().greet)
  }
}
class Hello {
  def greet = "Hello from scala"
}
