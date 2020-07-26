package org.miiohio

import scodec.bits._

case class RankCache (
    var countAtStepEdge: Int,
    var countAtByte: Vector[Int]
)

class Rank(bitArg: BitVector, bitStepSizeArg: Int = 4) {
  val bits: BitVector = bitArg
  val bitStepSize: Int = bitStepSizeArg
  val cache: Vector[RankCache] = Rank.populateCache(bits, bitStepSizeArg)
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
    def populateCache(bits: BitVector, stepSize: Int): Vector[RankCache] = {
        val bitsCount = bits.length
        val stepCount = java.lang.Math.ceil(bitsCount.toDouble / (8.0 * stepSize.toDouble)).toInt
        var result: Vector[RankCache] = Vector()
        var runningBitCount: Int = 0

        for (index <- 0 until stepCount) {
            printf("Index: %d\n", index)
            var stepTotal: Int = 0
            var temp: RankCache = new RankCache(runningBitCount, Vector())
            val bytesLeft = ((bitsCount / 8.0) - (index * stepSize)).toInt
            val iteratorBoundary = java.lang.Math.min(stepSize, bytesLeft).toInt
            printf("BytesLeft: %d, iteratorBoundary: %d\n", bytesLeft, iteratorBoundary)
            if (iteratorBoundary == 0) {
                val bytesCounted = bitsCount / 8
                val bitsToGo: Int = (bitsCount % 8).toInt
                var byteCount = 0
                printf("    Found bit fragments. Bytes counted: %d, bitsToGo: %d\n", bytesCounted, bitsToGo)
                for (bitFragment <- 0 until bitsToGo) {
                    printf("      bitFragment: %d\n", bitFragment)
                    val bitIndex = (index * stepSize * 8) + bitFragment
                    val bitIsHigh = bits.get(bitIndex)
                    if (bitIsHigh) {
                        byteCount += 1
                    }
                }
                stepTotal += byteCount
                temp.countAtByte = temp.countAtByte :+ byteCount
            } else {
                for (byteIndex <- 0 until iteratorBoundary) {
                    printf("  byteIndex: %d\n", byteIndex)
                    val arrayIndex: Long = ((index * stepSize) + byteIndex).toLong
                    var byteCount = 0
                    if (((arrayIndex + 1) * 8) > bitsCount) {
                        val bytesCounted = bitsCount / 8
                        val bitsToGo: Int = (bitsCount % 8).toInt
                        printf("    Found bit fragments. Bytes counted: %d, bitsToGo: %d\n", bytesCounted, bitsToGo)
                        for (bitFragment <- 0 until bitsToGo) {
                            printf("      bitFragment: %d\n", bitFragment)
                            val bitIndex = (arrayIndex * 8) + bitFragment
                                val bitIsHigh = bits.get(bitIndex)
                            if (bitIsHigh) {
                                byteCount += 1
                            }
                        }
                    } else {
                        byteCount = countOnesInByte(bits.getByte(arrayIndex))
                    }
                        stepTotal += byteCount
                    temp.countAtByte = temp.countAtByte :+ byteCount
                }
            }
            result = result :+ temp
            runningBitCount += stepTotal
        }

        printf("Done.\n\n")
        return result
    }
}
