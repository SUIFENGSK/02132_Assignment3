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
  val tempReg = RegInit(0.U(16.W))
  val inReg = RegInit(0.U(16.W))
  val outReg = RegInit(0.U(16.W))

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
        outReg := inReg + 400.U // Output address
        stateReg := writeBlack
      } .otherwise {
        stateReg := isBlackPixel
      }
    }
    is (isBlackPixel) {
      io.address := inReg
      when (io.dataRead === 0.U) {
        outReg := xReg + (20.U * yReg) + 400.U // Output address
        stateReg := writeBlack
      } .otherwise {
        tempReg := inReg - 1.U // Get pixel to the left
        stateReg := checkLeft
      }
    }
    is (writeBlack) {
      io.address := outReg
      io.writeEnable := true.B
      io.dataWrite := 0.U
      stateReg := yInc
    }
    is (checkLeft) {
      io.address := tempReg
      when (io.dataRead === 0.U) {
        outReg := inReg + 400.U // Output address
        stateReg := writeBlack
      } .otherwise {
        tempReg := inReg + 1.U // Get pixel to the right
        stateReg := checkRight
      }
    }
    is (checkRight) {
      io.address := tempReg
      when (io.dataRead === 0.U) {
        outReg := inReg + 400.U // Output address
        stateReg := writeBlack
      } .otherwise {
        tempReg := inReg - 20.U // Get pixel above
        stateReg := checkUp
      }
    }
    is (checkUp) {
      io.address := tempReg
      when (io.dataRead === 0.U) {
        outReg := inReg + 400.U // Output address
        stateReg := writeBlack
      } .otherwise {
        tempReg := inReg + 20.U // Get pixel below
        stateReg := checkDown
      }
    }
    is (checkDown) {
      io.address := tempReg
      when (io.dataRead === 0.U) {
        outReg := inReg + 400.U // Output address
        stateReg := writeBlack
      } .otherwise {
        outReg := inReg + 400.U // Output address
        stateReg := writeWhite
      }
    }
    is (writeWhite) {
      io.writeEnable := true.B
      io.dataWrite := 255.U
      io.address := outReg // Output address
      stateReg := yInc
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
