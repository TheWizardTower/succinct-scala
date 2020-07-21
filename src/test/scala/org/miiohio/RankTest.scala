package org.miiohio

import org.scalatest.flatspec.AnyFlatSpec
import org.miiohio.Rank
import scodec.bits._

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

  "populateCache" should "Count correctly." in {
      assert(Rank.populateCache(hex"00000000".toBitVector, 4) == Vector(RankCache(0, Vector(0,0,0,0))))
      assert(Rank.populateCache(hex"0000000000000000".toBitVector, 4) == Vector(RankCache(0, Vector(0,0,0,0)), RankCache(0, Vector(0,0,0,0))))
      assert(Rank.populateCache(hex"deadbeef".toBitVector, 4) == Vector(RankCache(0, Vector(6,5,6,7))))
      assert(Rank.populateCache(hex"deadbabe".toBitVector, 4) == Vector(RankCache(0, Vector(6,5,5,6))))
      assert(Rank.populateCache(hex"deadbeefdeadbabe".toBitVector, 4) == Vector(RankCache(0, Vector(6,5,6,7)), RankCache(24, Vector(6,5,5,6))))
  }

  "populateCache" should "Handle not-byte-aligned arrays." in {
      assert(Rank.populateCache(bin"00", 4) == Vector(RankCache(0, Vector(0))))
  }

}