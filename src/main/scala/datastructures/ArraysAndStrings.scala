package org.leetcode.course

import scala.collection.mutable
import java.util.Arrays

object ArraysAndStrings:
/**
  * Example 1: Given a string s, return true if it is a palindrome, false otherwise.
  * A string is a palindrome if it reads the same forward as backward. That means, after reversing it, it is still the same string. For example: "abcdcba", or "racecar".
  */
  def isPlndr(s: String): Boolean =
    var left = 0
    var right = s.size - 1
    for _ <- s if left < right do
      if s(left) == s(right) then
        left += 1
        right -= 1
      else return false
    true

/**
  * Example 2: Given a sorted array of unique integers and a target integer, return true if there exists a pair of numbers that sum to target, false otherwise.
  * This problem is similar to Two Sum. (In Two Sum, the input is not sorted).
  *
  * For example, given nums = [1, 2, 4, 6, 8, 9, 14, 15] and target = 13, return true because 4 + 9 = 13.
  */
  def checkIsTarget(nums: Array[Int], target: Int): Boolean =
    var left = 0
    var right = nums.size - 1
    while left < right do
      val sum = nums(left) + nums(right)
      if sum == target then return true
      else if sum > target then right -= 1 else left += 1
    false

/**
  * Example 3: Given two sorted integer arrays arr1 and arr2, return a new array that combines both of them and is also sorted.
  */
  def combine(xs: Array[Int], xy: Array[Int]): Array[Int] =
    val res = mutable.ArrayBuffer[Int]()

    var i, j = 0

    while i < xs.size && j < xy.size do
      if xs(i) < xy(j) then
        res += xs(i)
        i += 1
      else
        res += xy(j)
        j += 1

    while i < xs.size do
      res += xs(i)
      i += 1

    while j < xy.size do
      res += xy(j)
      j += 1

    res.toArray

/**
  * Example 4: 392. Is Subsequence.
  * 
  * Given two strings s and t, return true if s is a subsequence of t, or false otherwise.
  * 
  * A subsequence of a string is a sequence of characters that can be obtained by deleting some (or none) of the characters from the original string,
  * while maintaining the relative order of the remaining characters.
  * For example, "ace" is a subsequence of "abcde" while "aec" is not.
  */
  def isSubsequence(s: String, sub: String): Boolean =
    var i, j = 0

    while i < s.size && j < sub.size do
      if s(i) == sub(j) then i += 1
      j += 1

    i == s.length()

/**
  * Write a function that reverses a string. The input string is given as an array of characters s.
  * 
  * You must do this by modifying the input array in-place with O(1) extra memory.
  */
  def reverseString(s: Array[Char]): Array[Char] =
    var left = 0
    var right = s.size - 1
    while left < right do
      s(left) = (s(left) + s(right)).toChar
      s(right) = (s(left) - s(right)).toChar
      s(left) = (s(left) - s(right)).toChar
      left += 1
      right -= 1
    s

/**
  * Given an integer array nums sorted in non-decreasing order, return an array of the squares of each number sorted in non-decreasing order.
  */
  def sortedSquares(nums: Array[Int]): Array[Int] =
    def sq(x: Int): Int = scala.math.pow(x, 2).toInt

    var left = 0
    var right = nums.size - 1
    val result = new Array[Int](nums.size)
    var i = nums.size - 1
    
    while (i >= 0) {
      val leftVal = sq(nums(left))
      val rightVal = sq(nums(right))

      if (leftVal < rightVal) {
        result(i) = rightVal
        right -= 1
      } else {
        result(i) = leftVal
        left += 1
      }
      i -= 1
    }
    result

  // -4,-1,0,3,10
  def sortedSquares2(nums: Array[Int]): Array[Int] =
    def square(x: Int): Int = scala.math.pow(x, 2).toInt
    var res = new Array[Int](nums.size)
    var left = 0
    var right, i = nums.size - 1

    while i >= 0 do
      val leftValue = square(nums(left))
      val rightValue = square(nums(right))

      if leftValue > rightValue then
        res(i) = leftValue
        left += 1
      else
        res(i) = rightValue
        right -= 1
      i -= 1
    res

  def maxValidSubArray(nums: Array[Int], k: Int): Int =
    var left, right, curr, answ = 0

    while right < nums.size do
      curr += nums(right)
      while curr > k do
        curr -= nums(left)
        left += 1
      answ = scala.math.max(answ, right - left + 1)
      right += 1
    answ

  def maxValidSubArrayNonStrict(nums: Array[Int], k: Int): Int =
    var left, right, sum, answ = 0

    while right < nums.size do
      sum += nums(right)
      while sum > k do
        sum -= nums(left)
        left += 1
      answ = scala.math.max(answ, right - left + 1)
      right += 1
    answ

  def maxValidSubArrayNonStrict2(nums: Array[Int], k: Int): Int =
    var total, curr = 0
    var left, right = 0
    while right < nums.size do
      total += nums(right)
      while total > k do
        total -= nums(left)
        left += 1
      curr = scala.math.max(curr, right - left + 1)
      right += 1
    curr

/**
  * Example 1: Given an array of positive integers nums and an integer k, find the length of the longest subarray whose sum is less than or equal to k.
  * This is the problem we have been talking about above. We will now formally solve it.
  */
  def findLength(s: String): Int =
    var left, right, count, answ = 0
    while right < s.size do
      if s(right) == '0' then count += 1
      while count > 1 do
        if s(left) == '0' then count -= 1
        left += 1

      answ = scala.math.max(answ, right - left + 1)
      right += 1
    answ

/**
  * Example 2: You are given a binary string s (a string containing only "0" and "1"). You may choose up to one "0" and flip it to a "1". What is the length of the longest substring achievable that contains only "1"?
  *
  * For example, given s = "1101100111", the answer is 5. If you perform the flip at index 2, the string becomes 1111100111
  */
  def findLength2(s: String): Int =
    // 1101100111
    var left, right, answ, count = 0
    
    while right < s.length() do
      if s(right) == '0' then count += 1

      while count > 1 do
        if s(left) == '0' then count -= 1
        left += 1
      answ = scala.math.max(answ, right - left + 1)
      right += 1
    answ

/**
  * Example 3: 713. Subarray Product Less Than K.
  * 
  * Given an array of positive integers nums and an integer k, return the number of subarrays where the product of all the elements in the subarray is strictly less than k.
  * 
  * For example, given the input nums = [10, 5, 2, 6], k = 100, the answer is 8. The subarrays with products less than k are:
  * 
  * [10], [5], [2], [6], [10, 5], [5, 2], [2, 6], [5, 2, 6]
  */
  def numSubarrayProductLessThenK(nums: Array[Int], k: Int): Int =
    if k <= 1 then return 0 // base case

    var ans = 0
    var left, right = 0
    var curr = 1
  
    while right < nums.size do
      curr *= nums(right)
      while curr >= k do
        curr /= nums(left)
        left += 1
      ans += right - left + 1
      right += 1
    ans

/**
  * Example 4: Given an integer array nums and an integer k, find the sum of the subarray with the largest sum whose length is k.
  */
  def findBestSubarray(nums: Array[Int], k: Int): Int =
    var i, sum = 0

    while i < k do
      sum += nums(i)
      i += 1

    var ans = sum
    i = k
    while i < nums.size do
      sum += nums(i) - nums(i - k)
      ans = scala.math.max(ans, sum)
      i += 1
    ans

  def findMaxAverage(nums: Array[Int], k: Int): Double =
    if nums.size <= k then return nums.sum / k.toDouble

    var i, sum = 0

    while i < k do
      sum += nums(i)
      i += 1
    i = k

    var ans: Double = sum

    while i < nums.size do
      sum += nums(i) - nums(i - k)
      ans = scala.math.max(sum, ans)
      i += 1
    ans / k

  def findMaxAverage2(nums: Array[Int], k: Int): Double =
    if nums.size <= k then nums.sum / k.toDouble
    
    var i, sum = 0
    
    while i < k do
      sum += nums(i)
      i += 1

    var ans: Double = sum / k

    while i < nums.size do
      sum += nums(i) - nums(i - k)
      ans = scala.math.max(sum, ans)
      i += 1
    ans / k

// 1,1,1,0,0,0,1,1,1,1,0
  def longestOnes(nums: Array[Int], k: Int): Int =
    var left, right, count = 0
    var ans = 0

    while right < nums.size do
      if nums(right) == 0 then count += 1

      while count > k do
        if nums(left) == 0 then
          count -= 1
        left += 1

      ans = scala.math.max(ans, right - left + 1)
      right += 1
    ans

  def longestOnes2(nums: Array[Int], k: Int): Int =
    var left, right, count, ans = 0

    while right < nums.size do
      if nums(right) == 0 then count += 1
      
      while count > k do
        if nums(left) == 0 then count -= 1
        left += 1

      ans = scala.math.max(ans, right - left + 1)
      right += 1
    ans

  def answerQueries(nums: Array[Int], queries: Array[Array[Int]], limit: Int): Array[Boolean] =
    val prefix = new Array[Int](nums.size)
    prefix(0) = nums(0)

    var i = 1
    while i < nums.length do
      prefix(i) = prefix(i - 1) + nums(i)
      i += 1

    val ans = new Array[Boolean](queries.size)

    i = 0

    while i < queries.size do
      val x = queries(i)(0)
      val y = queries(i)(1)
      val curr = prefix(y) - prefix(x) + nums(x)
      ans(i) = curr < limit
      i += 1
    ans

  def waysToSplitArray(nums: Array[Int]): Int =
    val prefix = new Array[Int](nums.size)
    prefix(0) = nums(0)
    var i = 1
    
    while i < nums.size do
      prefix(i) = nums(i) + prefix(i - 1)
      i += 1

    var ans = 0
    i = 0

    while i < nums.size - 1 do
      val leftPart = prefix(i)
      val rightPart = prefix(nums.size - 1) - prefix(i)
      if leftPart >= rightPart then ans += 1
      i += 1
    ans

  def waysToSplitArray2(nums: Array[Int]): Int =
    var total = 0
    var leftPart = 0
    var i = 0

    for num <- nums do total += num

    var ans = 0

    while i < nums.size - 1 do
      leftPart += nums(i)
      val rightPart = total - leftPart
      if leftPart >= rightPart then ans += 1
      i += 1
    ans

  def runningSumOf1dArray(nums: Array[Int]): Array[Int] =
    var i = 1
    
    while i < nums.size do
      nums(i) = nums(i - 1) + nums(i)
      i += 1
    nums

  def minStartValue(nums: Array[Int]): Int =
    var prefixSum = 0
    var startValue = 0
    
    for i <- nums.indices do
      prefixSum += nums(i)
      startValue = scala.math.min(startValue, prefixSum)

    -startValue + 1

  def getAverages(nums: Array[Int], k: Int): Array[Int] =
    val prefix = new Array[Int](nums.size)
    prefix(0) = nums(0)

    for i <- 1 until nums.size do
      prefix(i) = prefix(i - 1) + nums(i)

    val averages = new Array[Int](nums.size)

    for i <- nums.indices do
      val left = i - k
      val right = i + k
      val isValid = left >= 0 && right < nums.size
      if isValid then
        val sum = prefix(right) - prefix(left) + nums(left)
        averages(i) = sum / (k * 2 + 1)
      else averages(i) = -1
    averages

  def getAverages2(nums: Array[Int], k: Int): Array[Int] =
    val averages = Array.fill(nums.size)(-1)
    var windowSum = 0L

    for i <- 0 until k * 2 + 1 do
      windowSum += nums(i)
    
    averages(k) = (windowSum / (2 * k + 1)).toInt

    for i <- (2 * k + 1) until nums.size do
      windowSum = windowSum - nums(i - (2 * k + 1)) + nums(i)
      averages(i - k) = (windowSum / (k * 2 + 1)).toInt
    averages

end ArraysAndStrings

object TestArraysAndStrings:
  import ArraysAndStrings.*

  testCheckIsTarget(checkIsTarget)
  testIsPlndr(isPlndr)
  testCombine(combine)
  testIsSubsequence(isSubsequence)
  testReverseString(reverseString)
  testMaxValidSubArray(maxValidSubArray)
  testMaxValidSubArrayNonStrict(maxValidSubArrayNonStrict)
  testMaxValidSubArrayNonStrict(maxValidSubArrayNonStrict2)
  testFindLength(findLength)
  testFindLength(findLength2)
  testNumSubarrayProductLessThenK(numSubarrayProductLessThenK)
  testSortedSquares(sortedSquares)
  testSortedSquares(sortedSquares2)
  testFindBestSubarray(findBestSubarray)
  testFindMaxAverage(findMaxAverage)
  testFindMaxAverage(findMaxAverage2)
  testLongestOnes(longestOnes)
  testLongestOnes(longestOnes2)
  testAnswerQueries(answerQueries)
  testWaysToSplitArray(waysToSplitArray)
  testWaysToSplitArray(waysToSplitArray2)
  testRunningSumOf1dArray(runningSumOf1dArray)
  testMinStartValue(minStartValue)
  testGetAverages(getAverages)
  testGetAverages(getAverages2)

  def testGetAverages(fn: (Array[Int], Int) => Array[Int]) =
    val call = fn(Array(7,4,3,9,1,8,5,2,6), 3).toSeq
    assert(call == Seq(-1,-1,-1,5,4,4,-1,-1,-1), call)

  def testMinStartValue(fn: Array[Int] => Int) =
    val call = fn(Array(-3,2,-3,4,2))
    val call2 = fn(Array(1, -6, 5, 3, 0))
    assert(call == 5, call)
    assert(call2 == 6, call2)

  def testRunningSumOf1dArray(fn: Array[Int] => Array[Int]) =
    val call = fn(Array(1,2,3,4)).toSeq
    val call2 = fn(Array(1,1,1,1,1)).toSeq
    assert(call == Seq(1,3,6,10), call)
    assert(call2 == Seq(1,2,3,4,5), call2)

  def testWaysToSplitArray(fn: Array[Int] => Int) =
    val call = fn(Array(10, 4, -8, 7)) // (10, 14, 6, 13)
    val call2 = fn(Array(8, 3, 15, 50)) // (8, 11, 26, 76)
    assert(call == 2, call)
    assert(call2 == 0, call2)

  def testAnswerQueries(fn: (Array[Int], Array[Array[Int]], Int) => Array[Boolean]) =
    val call =
      fn(
        Array(1, 6, 3, 2, 7, 2),
        Array(
          Array(0, 3),
          Array(2, 5),
          Array(2, 4),
        ),
        13
      ).toSeq
    assert(call == Seq(true, false, true), call)

  def testLongestOnes(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(1,1,1,0,0,0,1,1,1,1,0), 2)
    assert(call == 6, call)

  def testFindMaxAverage(fn: (Array[Int], Int) => Double) =
    val call = fn(Array(1,12,-5,-6,50,3), 4)
    val call2 = fn(Array(0,4,0,3,2), 1)
    assert(call == 12.75, call)
    assert(call2 == 4.0, call2)

  def testFindBestSubarray(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(2, 5, 11, 4, 9), 3)
    assert(call == 24, call)

  def testSortedSquares(fn: Array[Int] => Array[Int]) =
    val call = fn(Array(-4,-1,0,3,10)).toSeq
    assert(call == Seq(0,1,9,16,100), call)

  def testNumSubarrayProductLessThenK(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(10, 5, 2, 6), 100)
    assert(call == 8, call)

  def testFindLength(fn: String => Int) =
    val call = fn("1101100111")
    assert(call == 5, call)

  def testMaxValidSubArrayNonStrict(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(3, 1, 2, 7, 4, 2, 1, 1, 5), 8)
    assert(call == 4, call)

  def testMaxValidSubArray(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(1, 1, 1, 3), 3)
    assert(call == 3, call)

  def testCheckIsTarget(fn: (Array[Int], Int) => Boolean) =
    assert(fn(Array(1, 2, 4, 6, 8, 9, 14, 15), 13))
    assert(!fn(Array(1, 2, 3, 6, 8, 9, 14, 15), 13))

  def testIsPlndr(fn: String => Boolean) =
    assert(fn("abcdcba"))
    assert(fn("racecar"))
    assert(!fn("hello"))

  def testCombine(fn: (Array[Int], Array[Int]) => Array[Int]) =
    val call = fn(Array(1, 4, 7, 20), Array(3, 5, 6)).toSeq
    val call2 = fn(Array(2,3,5), Array(4,7,9,11)).toSeq
    assert(call == Seq(1, 3, 4, 5, 6, 7, 20), call)
    assert(call2 == Seq(2, 3, 4, 5, 7, 9, 11), call2)

  def testIsSubsequence(fn: (String, String) => Boolean) =
    val call = fn("ace", "abcde")
    val call2 = fn("helc", "helloworld")
    assert(call, call)
    assert(!call2, call2)

  def testReverseString(fn: Array[Char] => Array[Char]) =
    val call = fn(Array('h', 'e', 'l', 'l', 'o')).toSeq
    assert(call == Seq('o', 'l', 'l', 'e', 'h'), call)
