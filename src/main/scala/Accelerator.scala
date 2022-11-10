import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  // State enum and register
  val idle :: xLoop :: yLoop :: checkBorder :: isBlackPixel :: writeBlack :: checkLeft :: checkRight :: checkUp :: checkDown :: writeWhite :: yInc :: xInc :: done :: Nil = Enum(14)
  val stateReg = RegInit(idle)

  // Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val xReg = RegInit(0.U(16.W))
  val yReg = RegInit(0.U(16.W))

  // Default values
  io.done := false.B
  io.address := 0.U
  io.writeEnable := false.B
  io.dataWrite := 0.U
  xReg := 0.U
  yReg := 0.U

  // State machine
  switch (stateReg) {
    is (idle) {
      when (io.start) {
        stateReg := xLoop
      }
    }
    is (xLoop) {
      when (xReg > 19.U) {
        stateReg := done
      } .otherwise {
        stateReg := yLoop
        yReg := 0.U
        addressReg := 0.U // TODO: why?
      }
    }
    is (yLoop) {
      when (yReg > 19.U) {
        stateReg := xInc
      } .otherwise {
        stateReg := checkBorder
      }
    }
    is (checkBorder) {
      when (xReg === 0.U || xReg === 19.U || yReg === 0.U || yReg === 19.U) {
        stateReg := writeBlack
        addressReg := xReg + 20 * yReg + 400.U // Output address
      } .otherwise {
        stateReg := isBlackPixel
        addressReg := xReg + 20 * yReg // Input address
        io.address := addressReg
      }
    }
    is (isBlackPixel) {
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
        addressReg := xReg + 20 * yReg + 400.U // Output address
      } .otherwise {
        stateReg := checkLeft
        io.address := addressReg - 1.U // Get pixel to the left
      }
    }
    is (writeBlack) {
      stateReg := yInc
      io.writeEnable := true.B
      io.dataWrite := 0.U
      io.address := addressReg
    }
    is (checkLeft) {
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
        addressReg := addressReg + 400.U // Output address
      } .otherwise {
        stateReg := checkRight
      }
    }
    is (checkRight) {
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
        addressReg := addressReg + 400.U // Output address
      } .otherwise {
        stateReg := checkUp
        io.address := addressReg - 20.U // Get pixel above
      }
    }
    is (checkUp) {
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
        addressReg := addressReg + 400.U // Output address
      } .otherwise {
        stateReg := checkDown
        io.address := addressReg + 20.U // Get pixel below
      }
    }
    is (checkDown) {
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
        addressReg := addressReg + 400.U // Output address
      } .otherwise {
        stateReg := writeWhite
        addressReg := addressReg + 400.U // Output address
      }
    }
    is (writeWhite) {
      stateReg := yInc
      io.writeEnable := true.B
      io.dataWrite := 255.U
      io.address := addressReg // Output address
    }
    is (yInc) {
      stateReg := xInc
      io.writeEnable := false.B
      yReg := yReg + 1.U
    }
    is (xInc) {
      stateReg := xLoop
      xReg := xReg + 1.U
    }
    is (done) {
      io.done := true.B
    }
  }


}
