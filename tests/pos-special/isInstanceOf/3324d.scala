class Test {
  val x: Any = ???

  x match {
    case _: List[Int @unchecked] => 5
    case _: List[Int] @unchecked => 5
  }
}