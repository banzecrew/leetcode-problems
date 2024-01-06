package org.leetcode.course

import scala.util.boundary, boundary.break

object BinarySearch:
  def binarySearch(arr: Array[Int], target: Int): Int =
    var left = 0
    var right = arr.size - 1
    while left <= right do
      val mid = left +
        (right - left) / 2

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







  def binarySearchLeftFirst2(arr: Array[Int], k: Int): Int =
    var left = 0
    var right = arr.size

    while left < right do
      val mid = left + (right - left) / 2
      if arr(mid) >= k then right = mid
      else left = mid + 1
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

object TestBinarySearch:
  import BinarySearch.*

  testBinarySearchRightFirst(binarySearchRightFirst)
  testBinarySearch(binarySearchRightFirst)

  testBinarySearchLeftFirst(binarySearchLeftFirst)
  testBinarySearch(binarySearchLeftFirst)
  testBinarySearchLeftFirst(binarySearchLeftFirst2)

  testBinarySearch(binarySearch)

  def testBinarySearchRightFirst(fn: (Array[Int], Int) => Int) =
    val call = fn(
      Array(1, 2, 3, /*target ->*/3, 4, 5, 6, 6, 7, 8), 3
    )
    val call2 = fn(
      Array(1, 2, 3, 3, 4, /*missing*/ 6, 6, 7, 8), 5
    )
    assert(call == 3, call)
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
    val call = fn((1 to 10).toArray, 7)
    assert(call == 6, call)
