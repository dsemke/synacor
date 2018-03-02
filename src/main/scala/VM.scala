import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

object VM extends LazyLogging {
  val INTEGER_MOD  = 0x8000
  val REGISTER_MIN = 0x8000
  val REGISTER_MAX = 0x8007

  case class State(var pc: Int, var stack: List[Int], reg: Array[Int])

  @tailrec
  def exec(code: Array[Byte], state: State): Unit = {
    var shouldTick = true

    val totalBytes = code.length / 2
    val instr = read(code, state)
    logger.trace(s"pc = ${state.pc} instr = $instr reg = ${state.reg.mkString(", ")}")

    instr match {
      case 0 => // halt
        logger.debug("[HALT]")
        state.pc = totalBytes + 1

      case 1 => // set a b
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val value = read(code, tick(state))
        logger.debug(s"[SET] reg = $register value = $value")
        state.reg(register) = value

      case 2 => // push a
        val a = read(code, tick(state))
        logger.debug(s"[PUSH] $a")
        push(state, a)

      case 3 => // pop a
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val value = pop(state)
        logger.debug(s"[POP] reg = $register val = $value")
        state.reg(register) = value

      case 4 => // eq a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[EQ] reg = $register b = $b c = $c")
        state.reg(register) = if (b == c) 1 else 0

      case 5 => // gt a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[GT] reg = $register $b > $c")
        state.reg(register) = if (b > c) 1 else 0

      case 6 => // jmp a
        val addr = read(code, tick(state))
        logger.debug(s"[JMP] $addr")
        state.pc = addr
        shouldTick = false

      case 7 => // jt a b
        val a = read(code, tick(state))
        val addr = read(code, tick(state))
        logger.debug(s"[JT] a = $a addr = $addr")
        if (a != 0) {
          state.pc = addr
          shouldTick = false
        }

      case 8 => // jf a b
        val a = read(code, tick(state))
        val addr = read(code, tick(state))
        logger.debug(s"[JF] a = $a addr = $addr")
        if (a == 0) {
          state.pc = addr
          shouldTick = false
        }

      case 9 => // add a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[ADD] reg = $register b = $b c = $c val = ${(b + c) % INTEGER_MOD}")
        state.reg(register) = (b + c) % INTEGER_MOD

      case 10 => // mult a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[MULT] reg = $register b = $b c = $c val = ${(b * c) % INTEGER_MOD}")
        state.reg(register) = (b * c) % INTEGER_MOD

      case 11 => // mod a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[MOD] reg = $register b = $b c = $c val = ${b % c}")
        state.reg(register) = b % c

      case 12 => // and a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[AND] reg = $register b = $b c = $c and = ${b & c}")
        state.reg(register) = b & c

      case 13 => // or a b c
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val c = read(code, tick(state))
        logger.debug(s"[OR] reg = $register b = $b c = $c or = ${b | c}")
        state.reg(register) = b | c

      case 14 => // not a b
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        logger.debug(s"[NOT] reg = $register b = $b not = ${~b}")
        state.reg(register) = ~b & 0x7FFF

      case 15 => // rmem a b
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val b = read(code, tick(state))
        val value = read(code, state.copy(b))
        logger.debug(s"[RMEM] reg = $register b = $b value = $value")
        state.reg(register) = value

      case 16 => // wmem a b
        val a = read(code, tick(state))
        val b = read(code, tick(state))
        writemem(code, a, b)
        logger.debug(s"[WMEM] a = $a b = $b 843 = ${read(code, state.copy(843))}")

      case 18 => // ret
        try {
          val addr = pop(state)
          logger.debug(s"[RET] addr = $addr")
          state.pc = addr
          shouldTick = false
        } catch {
          case _: IllegalStateException =>
            logger.debug(s"[RET] nothing on stack, halting.")
            state.pc = totalBytes + 1
        }

      case 17 => // call a
        val addr = read(code, tick(state))
        logger.debug(s"[CALL] $addr")
        // push next addr to the stack
        push(state, state.pc + 1)
        // jump to addr
        state.pc = addr
        shouldTick = false

      case 19 => // print a
        val char = read(code, tick(state)).toChar
        print(char)

      case 20 => // in a
        val register = rawread(code, tick(state)) - REGISTER_MIN
        val ch = Console.in.read.toChar
        logger.debug(s"[IN] reg = $register ch = $ch")
        state.reg(register) = ch

      case 21 => // noop
        logger.debug("[NOOP]")

      case n => logger.warn(s"[???] $n")
    }

    // we manually set the pc, so we don't want to tick
    if (shouldTick) tick(state)

    if (state.pc < totalBytes) exec(code, state)
    else logger.debug("exiting program...")
  }

  def peek(state: State, reg: Int): Int = {
    logger.debug(s"[PEEK] reg: $reg val = ${state.reg(reg)}")
    state.reg(reg)
  }

  def rawread(bytes: Array[Byte], state: State): Int = {
    val offset = state.pc * 2
    val b1: Byte = bytes.lift(offset) getOrElse 0x00
    val b2: Byte = bytes.lift(offset + 1) getOrElse 0x00
    (b1 & 0xFF) + ((b2 & 0xFF) << 8)
  }

  def read(bytes: Array[Byte], state: State): Int = {
    val value = rawread(bytes, state)

    // check to see if this is a register lookup
    if (value >= REGISTER_MIN && value <= REGISTER_MAX) {
      peek(state, value - REGISTER_MIN)
    } else value
  }

  def tick(state: State): State = {
    state.pc = state.pc + 1
    state
  }

  def push(state: State, value: Int): Unit = state match {
    case State(_, stack, _) => state.stack = value :: stack
  }

  def pop(state: State): Int = state match {
    case State(_, x :: xs, _) =>
      state.stack = xs
      x
    case _ =>
      throw new IllegalStateException("called pop without anything in the stack")
  }

  def writemem(bytes: Array[Byte], offset: Int, value: Int): Unit = {
    val b1 = (value & 0xFF).toByte
    val b2 = ((value & (0xFF << 8)) >> 8).toByte
    bytes(offset * 2)       = b1
    bytes((offset * 2) + 1) = b2
  }
}
