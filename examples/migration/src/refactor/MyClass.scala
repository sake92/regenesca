package refactor

@Wither
case class MyClass(x: Int, y: String) {
  def withX(x: Int): MyClass = copy(x = x)
}

class Wither extends scala.annotation.Annotation
