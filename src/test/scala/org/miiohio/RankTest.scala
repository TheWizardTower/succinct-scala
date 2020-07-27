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
    assert(
      Rank.populateCache(bin"", 4) == Vector()
    )
    assert(
      Rank.populateCache(hex"00000000".toBitVector, 4) == Vector(
        RankCache(0, Vector(0, 0, 0, 0))
      )
    )
    assert(
      Rank.populateCache(hex"0000000000000000".toBitVector, 4) == Vector(
        RankCache(0, Vector(0, 0, 0, 0)),
        RankCache(0, Vector(0, 0, 0, 0))
      )
    )
    assert(
      Rank.populateCache(hex"deadbeef".toBitVector, 4) == Vector(
        RankCache(0, Vector(6, 11, 17, 24))
      )
    )
    assert(
      Rank.populateCache(hex"deadbabe".toBitVector, 4) == Vector(
        RankCache(0, Vector(6, 11, 16, 22))
      )
    )
    assert(
      Rank.populateCache(hex"deadbeefdeadbabe".toBitVector, 4) == Vector(
        RankCache(0, Vector(6, 11, 17, 24)),
        RankCache(24, Vector(6, 11, 16, 22))
      )
    )
    assert(
      Rank.populateCache(hex"0000".toBitVector, 4) == Vector(
        RankCache(0, Vector(0, 0))
      )
    )
    assert(
      Rank.populateCache(hex"00FF".toBitVector, 4) == Vector(
        RankCache(0, Vector(0, 8))
      )
    )
    assert(
      Rank.populateCache(hex"00".toBitVector, 4) == Vector(
        RankCache(0, Vector(0))
      )
    )
    assert(
      Rank.populateCache(hex"FF".toBitVector, 4) == Vector(
        RankCache(0, Vector(8))
      )
    )
  }

  "populateCache" should "Handle not-byte-aligned arrays." in {
    assert(Rank.populateCache(bin"00", 4) == Vector(RankCache(0, Vector(0))))
    assert(Rank.populateCache(bin"11", 4) == Vector(RankCache(0, Vector(2))))
    assert(
      Rank.populateCache(
        BitVector.concat(
          Vector[BitVector](hex"00000000".toBitVector, (bin"11"))
        ),
        4
      ) == Vector(RankCache(0, Vector(0, 0, 0, 0, 2)))
    assert(
      Rank.populateCache(
        BitVector.concat(
          Vector[BitVector](hex"00 00".toBitVector, (bin"11"))
        ),
        4
      ) == Vector(RankCache(0, Vector(0, 0, 2)))
    )
    assert(
      Rank.populateCache(
        BitVector.concat(
          Vector[BitVector](hex"00 00".toBitVector, (bin"11"))
        ),
        4
      ) == Vector(RankCache(0, Vector(0, 0, 2)))
    )
    assert(
      Rank.populateCache(
        BitVector.concat(
          Vector[BitVector](hex"00 00".toBitVector, (bin"1111"))
        ),
        4
      ) == Vector(RankCache(0, Vector(0, 0, 4)))
    )
  }

}
