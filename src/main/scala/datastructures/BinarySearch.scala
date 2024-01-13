package org.leetcode.course

import scala.util.boundary, boundary.break
import scala.annotation.tailrec
import java.util.Arrays

object BinarySearch:
  def binarySearch(arr: Array[Int], target: Int): Int =
    var left = 0
    var right = arr.size - 1
    while left <= right do
      val mid = left + (right - left) / 2
      if arr(mid) == target then return mid
      if arr(mid) > target then
        right = mid - 1
      else
        left = mid + 1
    left

  def binarySearchLeftFirst(arr: Array[Int], k: Int): Int =
    var left = 0
    var right = arr.size

    while left < right do
      val mid = left + (right - left) / 2
      if arr(mid) >= k then
        right = mid
      else
        left = mid + 1
    left

  def binarySearchRightFirst(arr: Array[Int], k: Int): Int =
    var left = 0
    var right = arr.size

    while left < right do
      val mid = left + (right - left) / 2
      if arr(mid) > k then
        right = mid
      else
        left = mid + 1
    left

  // Example 1: 704. Binary Search
  def search(nums: Array[Int], target: Int): Int =
    var left = 0
    var right = nums.size - 1

    while left <= right do // can be re-written in tail recursion
      val mid = left + (right - left) / 2
      val curr = nums(mid)

      if curr == target then return mid

      if curr > target then 
        right = mid - 1
      else
        left = mid + 1
    -1

  def searechMatrix(matrix: Array[Array[Int]], target: Int): Boolean =
    var n = matrix.size
    var m = matrix(0).size
    var left = 0
    var right = n * m - 1

    while left <= right do
      val mid = left + (right - left) / 2
      val row = mid / n
      val col = mid % n
      val num = matrix(row)(col)

      if num == target then return true

      if num < target then
        left = mid + 1
      else
        right = mid - 1
    false

  def successfulPairs(spells: Array[Int], potions: Array[Int], success: Long): Array[Int] =
    def bs(arr: Array[Int], k: Double): Int =
      var left = 0
      var right = arr.size - 1

      while left <= right do
        val mid = left + (right - left) / 2

        if arr(mid) < k then
          left = mid + 1
        else
          right = mid - 1
      left
    end bs

    Arrays.sort(potions)
    val ans = new Array[Int](potions.size)
    val m = potions.size

    for i <- spells.indices do
      val j = bs(potions, success / spells(i).toDouble)
      ans(i) = m - j
    ans

  def searchInsert(nums: Array[Int], target: Int): Int =
    @tailrec
    def find(left: Int, right: Int): Int =
      if left <= right then
        val mid = left + (right - left) / 2
        if nums(mid) < target then
          find(left = mid + 1, right)
        else
          find(left, right = mid - 1)
      else left
    find(left = 0, right = nums.size - 1)

  def answerQueries(nums: Array[Int], queries: Array[Int]): Array[Int] =
    ???

object TestBinarySearch:
  import BinarySearch.*

  //testAnswerQueries(answerQueries)
  testBinarySearch(searchInsert)
  testSuccessfulPairs(successfulPairs)
  testSearechMatrix(searechMatrix)

  testBinarySearch(search)

  testBinarySearchRightFirst(binarySearchRightFirst)

  testBinarySearchLeftFirst(binarySearchLeftFirst)
  testBinarySearch(binarySearchLeftFirst)

  testBinarySearch(binarySearch)

  def testAnswerQueries(fn: (Array[Int], Array[Int]) => Array[Int]) =
    val call = fn(Array(4, 5, 2, 1), Array(3, 10, 21)).toSeq
    val call2 = fn(Array(2, 3, 4, 5), Array(1)).toSeq
    assert(call == Seq(2, 3, 4), call)
    assert(call2 == Nil, call2)

  def testSuccessfulPairs(fn: (Array[Int], Array[Int], Long) => Array[Int]) =
    val call = fn(
      Array(5, 1, 3), // spells
      Array(1, 2, 3, 4, 5), // potions
      7 // success
    ).toSeq
    assert(call == Seq(4, 0, 3, 0, 0), call)

  def testSearechMatrix(fn: (Array[Array[Int]], Int) => Boolean) =
    val xs = Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
    assert(fn(xs, 5))
    assert(!fn(xs, 10))

  def testBinarySearchRightFirst(fn: (Array[Int], Int) => Int) =
    val call = fn(
      Array(1, 2, 3, /*target ->*/3, 4, 5, 6, 6, 7, 8), 3
    )
    val call2 = fn(
      Array(1, 2, 3, 3, 4, /*missing*/ 6, 6, 7, 8), 5
    )
    assert(call == 4, call)
    assert(call2 == 5, call2)

  def testBinarySearchLeftFirst(fn: (Array[Int], Int) => Int) =
    val call = fn(
      Array(1, 2, /*target ->*/3, 3, 3, 4, 5, 6, 6, 7, 8), 3
    )
    val call2 = fn(
      Array(1, 2, 3, 3, 4, /*missing*/ 6, 6, 7, 8), 5
    )
    assert(call == 2, call)
    assert(call2 == 5, call2)

  def testBinarySearch(fn: (Array[Int], Int) => Int) =
    val xs = (1 to 10).toArray

    for (x, idx) <- xs.zipWithIndex do
      val call = fn(xs, x)
      assert(call == idx, s"[actual $call] - [expected $idx]")
