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
  val xReg = RegInit(0.U(16.W))
  val yReg = RegInit(0.U(16.W))
  val inReg = RegInit(0.U(16.W))

  // Default values
  io.done := false.B
  io.address := 0.U
  io.writeEnable := false.B
  io.dataWrite := 0.U

  // State machine
  switch (stateReg) {
    is (idle) {
      when (io.start) {
        stateReg := xLoop
      }
    }
    is (xLoop) {
      when (xReg <= 19.U) {
        stateReg := yLoop
      } .otherwise {
        stateReg := done
      }
    }
    is (yLoop) {
      when (yReg <= 19.U) {
        inReg := xReg + (20.U * yReg)
        stateReg := checkBorder
      } .otherwise {
        stateReg := xInc
      }
    }
    is (checkBorder) {
      when (xReg === 0.U || xReg === 19.U || yReg === 0.U || yReg === 19.U) {
        stateReg := writeBlack
      } .otherwise {
        stateReg := isBlackPixel
      }
    }
    is (isBlackPixel) {
      io.address := inReg
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
      } .otherwise {
        io.address := inReg + 20.U // Get pixel below
        dataRead := io.dataRead
        stateReg := checkDown
      }
    }
    is (writeNextBlack) {
      io.address := inReg + 400.U + 20.U
      yReg := yReg + 2.U
      stateReg := yLoop
    }
    is (writeBlack) {
      io.address := inReg + 400.U
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := yInc
    }
    is(checkDown) {
      when(dataRead === 0.U) {
        io.address := inReg + 400.U
        io.writeEnable := true.B
        stateReg := writeNextBlack
      }.otherwise {
        io.address := inReg - 1.U // Get pixel to the left
        dataRead := io.dataRead
        stateReg := checkLeft
      }
    }
    is (checkLeft) {
      when (dataRead === 0.U) {
        stateReg := writeBlack
      } .otherwise {
        io.address := inReg + 1.U // Get pixel to the right
        dataRead := io.dataRead
        stateReg := checkRight
      }
    }
    is (checkRight) {
      io.address := inReg + 1.U // Get pixel to the right
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
      } .otherwise {
        io.address := inReg - 20.U // Get pixel above
        dataRead := io.dataRead
        stateReg := checkUp
      }
    }
    is (checkUp) {
      io.address := inReg - 20.U // Get pixel above
      when (io.dataRead === 0.U) {
        stateReg := writeBlack
      } .otherwise {
        stateReg := checkDown
      }
    }
    is (yInc) {
      io.writeEnable := false.B
      yReg := yReg + 1.U
      stateReg := yLoop
    }
    is (xInc) {
      yReg := 0.U
      xReg := xReg + 1.U
      stateReg := xLoop
    }
    is (done) {
      io.done := true.B
    }
  }


}
