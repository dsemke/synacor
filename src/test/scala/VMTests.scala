import org.scalatest._
import java.nio.file.{Files, Paths}
import VM._

class VMTests extends FlatSpec with Matchers {

  lazy val code: Array[Byte] = Files.readAllBytes(Paths.get(getClass.getResource("/challenge.bin").toURI))
  val testCode: Array[Byte] = Array(0x15, 0x00, 0x15, 0x00, 0x13, 0x00, 0x57, 0x00)

  val state = State(0, List(), Array.fill(8)(0))

  "read" should "read bytes" in {
    println(read(testCode, state))
  }

  "exec" should "exec code" in {
    exec(code, state)
  }
}
