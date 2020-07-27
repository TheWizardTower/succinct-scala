package org.miiohio

import scodec.bits._

case class RankCache(
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
    for (index <- 0 to bitIndex) {
      val shiftValue = 7 - index
      if (byte.&(1 << shiftValue) >= 1) {
        result += 1
      }
    }
    return result
  }
  def getLastCountAtByte(byteCounts: Vector[Int]): Int = {
    if (byteCounts.length == 0) {
      return 0
    }
    return byteCounts.last
  }
  def loopHelper(bits: BitVector, index: Int, stepSize: Int): Int = {
    val bitsCount = bits.length
    val bytesCounted = bitsCount / 8
    val bitsToGo: Int = (bitsCount % 8).toInt
    var byteCount = 0
    printf(
      "    Found bit fragments. Bytes counted: %d, bitsToGo: %d\n",
      bytesCounted,
      bitsToGo
    )
    for (bitFragment <- 0 until bitsToGo) {
      printf("      bitFragment: %d\n", bitFragment)
      val bitIndex = (index * stepSize * 8) + bitFragment
      val bitIsHigh = bits.get(bitIndex.toLong)
      if (bitIsHigh) {
        byteCount += 1
      }
    }
    return byteCount
  }
  def countBlocks(
      bits: BitVector,
      stepCount: Int,
      stepSize: Int
  ): Vector[RankCache] = {
    var result: Vector[RankCache] = Vector()
    val bitsCount = bits.length
    var runningBitCount: Int = 0
    for (index <- 0 until stepCount) {
      printf("Index: %d\n", index)
      var stepTotal: Int = 0
      var temp: RankCache = new RankCache(runningBitCount, Vector())
      val bytesLeft = ((bitsCount / 8) - (index * stepSize)).toInt
      printf(
        "stepSize: %d, index * stepSize: %d, bitsCount / 8: %d\n",
        stepSize,
        index * stepSize,
        bitsCount / 8
      )
      val iteratorBoundary = java.lang.Math.min(stepSize, bytesLeft).toInt
      printf(
        "BytesLeft: %d, iteratorBoundary: %d\n",
        bytesLeft,
        iteratorBoundary
      )
      var byteCount = 0
      var byteIndex = 0
      for (byteIndex <- 0 until iteratorBoundary) {
        printf("  byteIndex: %d\n", byteIndex)
        val arrayIndex: Long = ((index * stepSize) + byteIndex).toLong
        var byteCount = 0
        byteCount = countOnesInByte(bits.getByte(arrayIndex))
        stepTotal += byteCount
        temp.countAtByte :+= stepTotal
      }
      result :+= temp
      runningBitCount += stepTotal
    }
    return result
  }
  def getRunningCount(cache: Vector[RankCache]): Int = {
    if (cache.length == 0) {
      return 0
    }
    return cache.last.countAtStepEdge + getLastCountAtByte(
      cache.last.countAtByte
    )
  }
  def countTrailingBytes(
      bits: BitVector,
      stepCount: Int,
      stepSize: Int,
      currentTotal: Int
  ): Vector[RankCache] = {
    var result: Vector[RankCache] = Vector()
    if (stepCount * 8 * stepSize < bits.length / 8) {
      var temp: RankCache = RankCache(currentTotal, Vector[Int]())
      var byteCount = 0;
      for (index <- (stepCount * stepSize) until (bits.length / 8).toInt) {
        val byte = bits.getByte(index.toLong)
        byteCount += countOnesInByte(byte)
        temp.countAtByte :+= byteCount
      }
      val lastCountedIndex = (bits.length / 8) * 8

      if (lastCountedIndex % stepSize != 0) {}
      result :+= temp
    }
    return result
  }

  def populateCache(bits: BitVector, stepSize: Int): Vector[RankCache] = {
    if (bits.length == 0) {
      return Vector[RankCache]()
    }
    printf("BitVector: %s\n", bits.toHex)
    val stepCount = (bits.length / (8 * stepSize)).toInt
    var runningBitCount: Int = 0
    printf(
      "bit length: %d, calculated step count: %d\n",
      bits.length,
      stepCount
    )

    var result: Vector[RankCache] = countBlocks(bits, stepCount, stepSize)
    // Check if we have bytes left over, but less bytes than a whole step.
    result ++= countTrailingBytes(
      bits,
      stepCount,
      stepSize,
      getRunningCount(result)
    )

    // We've counted everything that's in byte sized chunks, now to see if there are any bits left over that aren't byte-aligned.
    val lastCountedIndex = (bits.length / 8) * 8
    if (lastCountedIndex < bits.length) {
      printf(
        "    Final if tripped. Step count: %d, stepSize: %d, bitsCount: %d, result length: %d\n",
        stepCount,
        stepSize,
        bits.length,
        result.length
      )

      var byteCount = 0
      for (index <- lastCountedIndex until bits.length) {
        if (bits.get(index)) {
          byteCount += 1
        }
      }
      if (result.length == 0 || result.last.countAtByte.length >= stepSize) {
        printf("derp 1\n")
        result :+= RankCache(0, Vector(byteCount))
      } else {
        printf("    Populated Result Vector\n")
        val stepTotal =
          getLastCountAtByte(result.last.countAtByte) + byteCount
        printf("    stepTotal: %d, byteCount: %d\n", stepTotal, byteCount)
        result.last.countAtByte :+= stepTotal
      }
    }

    printf("Done.\n\n")
    return result
  }
}
