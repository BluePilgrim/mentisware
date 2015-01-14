package com.mentisware.ds

import com.mentisware.test.UnitSpec

trait HashTableBehaviors[Key, Entry] { this: UnitSpec =>
  def sequencedOperations(tableSize: Int, hashFunc: (Int => Int), testSet: List[Any]) {
    it should "process the operation sequence correctly" in {
      val hashTable = new HashTable[Key, Entry](tableSize, hashFunc)
      
      testSet foreach {
        _ match {
          case ('i', key: Key, data: Entry, result) =>
            (hashTable.insert(key, data)) should equal (result)
          case ('s', key: Key, result) =>
            (hashTable.search(key)) should equal (result)
          case ('d', key: Key, result) =>
            (hashTable.delete(key)) should equal (result)
          case _ => assert(false)
        }
      }
      
      println("For table size = " + tableSize)
      println(hashTable)
    }
  }
}



class HashTableTest1 extends UnitSpec with HashTableBehaviors[Int, String] {
  def testSet1 = List(
      ('i', 3, "Test", None), ('i', 4, "Hello", None), ('i', 6, "this", None), ('i', 100, "aaa", None), ('i', 34, "bbbb", None),
      ('s', 6, Some("this")), ('s', 100, Some("aaa")), ('s', 20, None), ('s', 3, Some("Test")),
      ('d', 4, Some("Hello")), ('d', 100, Some("aaa")), ('d', 30, None),
      ('i', 4, "Help", None), ('i', 34, "ccc", Some("bbbb")))

  "A division hash method" should behave like sequencedOperations(7, HashFuncGen.divisionHash(7), testSet1)
  "A multiplication hash method" should behave like
      sequencedOperations(12, HashFuncGen.multipicationHash(12), testSet1)
  "A universal hash method" should behave like sequencedOperations(13, HashFuncGen.universalHash(13), testSet1)
}



class HashTableTest2 extends UnitSpec with HashTableBehaviors[String, Int] {
  def testSet2 = List(
      ('i', "This", 3, None), ('i', "is", 7, None), ('i', "what a strange day!", 100, None),
      ('s', "is", Some(7))
  )

  "A division hash method" should behave like sequencedOperations(13, HashFuncGen.divisionHash(13), testSet2)
  "A multiplication hash method" should behave like
      sequencedOperations(101, HashFuncGen.multipicationHash(101), testSet2)
  "A universal hash method" should behave like sequencedOperations(47, HashFuncGen.universalHash(47), testSet2)
}
