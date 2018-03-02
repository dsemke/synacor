import java.net.URI
import java.nio.file.{Files, Paths}

import VM.{State, exec}

import scala.io.Source

object Main extends App {
  val code: Array[Byte] = Files.readAllBytes(Paths.get(getClass.getResource("/challenge.bin").toURI))
  exec(code, State(0, List(), Array.fill(8)(0)))
}
