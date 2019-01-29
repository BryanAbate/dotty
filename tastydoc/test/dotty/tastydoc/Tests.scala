package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

class Tests {
  @Test def testAccess(): Unit = {
    ConsumeTasty(
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.13/classes",
      List("example.Access"),
      new dotty.tastydoc.TastydocConsumer
    )
  }
  @Test def testExample(): Unit = {
    ConsumeTasty(
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.13/classes",
      List("example.Example"),
      new dotty.tastydoc.TastydocConsumer
    )
  }
  @Test def testExample2(): Unit = {
    ConsumeTasty(
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.13/classes",
      List("example.OExample"),
      new dotty.tastydoc.TastydocConsumer
    )
  }
  @Test def testClasses(): Unit = {
    ConsumeTasty(
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.13/classes",
      List("example.C2"),
      new dotty.tastydoc.TastydocConsumer
    )
  }
}