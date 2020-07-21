package org.miiohio

import org.scalatest.flatspec.AnyFlatSpec
import org.miiohio.Rank

class RankTest extends AnyFlatSpec {

  "countOnesInByte" should "Be able to count high bits in a byte" in {
    assert(Rank.countOnesInByte(0) == 0)
    assert(Rank.countOnesInByte(1) == 1)
    assert(Rank.countOnesInByte(2) == 1)
    assert(Rank.countOnesInByte(3) == 2)
    assert(Rank.countOnesInByte(127) == 7)
    assert(Rank.countOnesInByte(-128) == 1)
    assert(Rank.countOnesInByte(-1) == 8)
  }

  "countOnesInByteUpToIndex" should "Stop at the correct place." in {
      assert(Rank.countOnesInByteUpToIndex(127, 0) == 0)
      assert(Rank.countOnesInByteUpToIndex(-128, 0) == 1)
      assert(Rank.countOnesInByteUpToIndex(-1, 0) == 1)
      assert(Rank.countOnesInByteUpToIndex(-1, 1) == 2)
  }

}