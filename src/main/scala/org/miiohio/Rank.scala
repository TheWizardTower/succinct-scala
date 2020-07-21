package org.miiohio

import scodec.bits._

case class RankCache (
    val countAtWord: Int,
    val countAtByteZero: Int,
    val countAtByteOne: Int,
    val countAtByteTwo: Int,
    val countAtByteThree: Int
)

class Rank(bitArg: BitVector, bitStepSizeArg: Int = 512) {
  val bits: BitVector = bitArg
  val bitStepSize: Int = bitStepSizeArg
//   val cache: Vector[RankCache] = Rank.populateCache(bits)
}

object Rank {
    def countOnesInByte(byte: Byte): Int = {
        return countOnesInByteUpToIndex(byte, 7)
    }
    def countOnesInByteUpToIndex(byte: Byte, bitIndex: Int): Int = {
        var result: Int = 0
        for(index <- 0 to bitIndex){
            val shiftValue = 7 - index
            if (byte.&(1 << shiftValue) >= 1) {
                result += 1
            }
        }
        return result
    }
    // def populateCache(bits: BitVector): Vector[RankCache] = {
    //     val bits_count = bits.length
    //     val bytes_count = java.lang.Math.ceil(bits_count / 8.0)
    //     var result: Vector[RankCache] = Vector()
    //     return result
    // }
}