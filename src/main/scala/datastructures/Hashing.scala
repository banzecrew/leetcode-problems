package org.leetcode.course

import scala.math
import scala.collection.mutable
import scala.util.boundary, boundary.break
import java.util.Arrays
import scala.language.experimental
import scala.collection.mutable.ListBuffer

object Hashing:
  def twoSum(nums: Array[Int], target: Int): Array[Int] =
    val dic = mutable.HashMap[Int, Int]()

    for i <- nums.indices do
      val num = nums(i)
      val complement = target - num
      if dic.contains(complement) then
        return Array(i, dic(complement))
      dic += num -> i
    Array(-1, -1)

  def repeatedCharacter(s: String): Char =
    val seen = mutable.HashSet[Char]()
    boundary:
      for i <- s.indices do
        val char = s(i)
        if seen.contains(char) then break(char)
        seen += char
      ' '

  def findNumbers(nums: Array[Int]): List[Int] =
    val res = mutable.ListBuffer[Int]()

    for i <- nums.indices do
      val cur = nums(i)
      var j = 0
      var isValid = true
      while j < nums.size do
        if cur - 1 == nums(j) || cur + 1 == nums(j) then isValid = false
        j += 1
      if isValid then res += cur
    
    res.toList

  def findNumbers2(nums: Array[Int]): List[Int] =
    val ans = mutable.ListBuffer[Int]()
    val numSet = mutable.HashSet.from(nums)

    for num <- nums do
      if !numSet.contains(num - 1) && !numSet.contains(num + 1) then ans += num

    ans.toList

  def checkIfPangram(sentence: String): Boolean =
    val chars = mutable.HashSet[Char]()
    for el <- sentence do chars += el
    chars.size == 26

  def missingNumber(nums: Array[Int]): Int =
    val sorted = nums.sorted
    var left = 0
    var right = nums.size - 1

    boundary:
      for i <- nums.indices do
        if sorted(i) != i then break(i)

      val n = nums.size
      if nums(n - 1) != n then break(n)
      -1

  def missingNumber2(nums: Array[Int]): Int =
    Arrays.sort(nums) // O(n log n)
    
    boundary:
      if nums(0) != 0 then break(0)
      if nums(nums.size - 1) != nums.size then break(nums.size)

      var i = 1
      while i < nums.size do
        val expectedCurr = nums(i - 1) + 1
        if nums(i) != expectedCurr then break(expectedCurr)
        i += 1
      -1

  def missingNumber3(nums: Array[Int]): Int =
    val set = mutable.Set[Int]()
    for num <- nums do set += num
    
    val expectedNumCount = nums.size + 1
    boundary:
      var i = 0
      while i < expectedNumCount do
        if !set.contains(i) then break(i)
        i += 1
      -1

  // 3,0,1
  def missingNumber4(nums: Array[Int]): Int =
    var missingNum = nums.length
    for i <- nums.indices do
      missingNum ^= i ^ nums(i)

    missingNum

  // ar = 0, 1, 2, 3, 4
  // ar.size = 5
  // expectedSum = 0 + 1 + 2 + 3 + 4 = 10
  def missingNumber5(nums: Array[Int]): Int =
    var sum = nums.size * (nums.size + 1) / 2
    for num <- nums do sum -= num
    sum

  def countElements(arr: Array[Int]): Int =
    val set = mutable.Set[Int]()
    for n <- arr do set += n
    var count = 0
    for n <- arr do
      if set.contains(n + 1) then count += 1
    count

  def findLongestSubstring(s: String, k: Int): Int =
    val counts = mutable.Map[Char, Int]()
    var left, right = 0
    var ans = 0

    while right < s.length() do
      val ch = s.charAt(right)
      counts += ch -> (counts.getOrElse(ch, 0) + 1)
      while counts.size > k do
        val remove = s.charAt(left)
        counts += remove -> (counts.getOrElse(remove, 0) - 1)
        if counts.getOrElse(remove, 0) == 0 then counts -= remove
        left += 1
      ans = scala.math.max(ans, right - left + 1)
      right += 1
    ans

  def findLongestSubstring2(s: String, k: Int): Int =
    val counts = mutable.Map[Char, Int]()
    var left, right = 0
    var ans = 0

    while right < s.length() do
      val add = s.charAt(right)
      counts += add -> (counts.getOrElse(add, 0) + 1)
      while counts.size > k do
        val del = s.charAt(left)
        counts += del -> (counts.getOrElse(del, 0) - 1)
        if counts.getOrElse(del, 0) == 0 then counts -= del
        left += 1
      ans = scala.math.max(ans, right - left + 1)
      right += 1
    ans

  // time O(m⋅(n+logm))
  // space O(n * m): n = nums.size; m = nums(0).size
  def intersection(nums: Array[Array[Int]]): List[Int] =
    val counts = mutable.Map[Int/*num*/, Int/*count*/]()
    for ar <- nums do
      for num <- ar do
        counts += num -> (counts.getOrElse(num, 0) + 1)

    val ans = mutable.ListBuffer[Int]()

    for key <- counts.keySet do
      if counts(key) == nums.size then ans += key

    ans.toList.sorted

  def areOccurrencesEqual(s: String): Boolean =
    val counts = mutable.Map[Char, Int]()

    for ch <- s do
      counts += ch -> (counts.getOrElse(ch, 0) + 1)

    counts.values.toSet.size == 1

  def subarraySum(nums: Array[Int], k: Int): Int =
    val counts = mutable.Map[Int, Int]()
    counts += 0 -> 1

    var ans = 0
    var curr = 0

    for num <- nums do
      curr += num
      ans += counts.getOrElse(curr - k, 0)
      counts += curr -> (counts.getOrElse(curr, 0) + 1)
    ans

  def numberOfSubarrays(nums: Array[Int], k: Int): Int =
    val counts = mutable.Map[Int, Int]()
    counts += 0 -> 1

    var ans = 0
    var curr = 0
    // Array(1, 1, 2, 1, 1), 3)
    for num <- nums do
      curr += num % 2
      ans += counts.getOrElse(curr - k, 0)
      counts += curr -> (counts.getOrElse(curr, 0) + 1)
    ans

 /*  Find Players With Zero or One Losses
  *    You are given an integer array matches where matches[i] = [winneri, loseri] indicates that the player winneri defeated player loseri in a match.
  *
  *    Return a list answer of size 2 where:
  *
  *    - answer[0] is a list of all players that have not lost any matches.
  *    - answer[1] is a list of all players that have lost exactly one match.
  *    The values in the two lists should be returned in increasing order.
  *
  *    Note:
  *
  *    You should only consider the players that have played at least one match.
  *    The testcases will be generated such that no two matches will have the same outcome.
  */
  def findWinners(matches: Array[Array[Int]]): List[List[Int]] =
    val stats = mutable.Map[Int/*player*/, Int/*loses*/]()
  
    for game <- matches do
      val winner = game(0)
      val loser = game(1)
      stats += winner -> (stats.getOrElse(winner, 0))
      stats += loser -> (stats.getOrElse(loser, 0) + 1)

    val zeroLose = mutable.ListBuffer[Int]()
    val oneLose = mutable.ListBuffer[Int]()

    for (player, loses) <- stats do
      if loses == 0 then zeroLose += player
      if loses == 1 then oneLose += player

    List(
      zeroLose.toList.sorted,
      oneLose.toList.sorted
    )

  def findWinners2(matches: Array[Array[Int]]): List[List[Int]] =
    val zeroLoss = mutable.HashSet[Int]()
    val oneLoss = mutable.HashSet[Int]()
    val moreLosses = mutable.HashSet[Int]()

    for (game <- matches) {
      val winner = game(0)
      val loser = game(1)
      if (!oneLoss.contains(winner) && !moreLosses.contains(winner))
        zeroLoss += winner

      if (zeroLoss.contains(loser)) {
        zeroLoss -= loser
        oneLoss += loser
      } else if (oneLoss.contains(loser)) {
        oneLoss -= loser
        moreLosses += loser
      } else if (moreLosses.contains(loser)) ()
      else oneLoss += loser
    }
    List(zeroLoss.toList.sorted, oneLoss.toList.sorted)

/** Largest Unique Number
  *
  * Given an integer array nums, return the largest integer that only occurs once.
  * If no integer occurs once, return -1.
  * 
  * 1 <= nums.length <= 2000
  * 0 <= nums[i] <= 1000
  */
  def largestUniqueNumber(nums: Array[Int]): Int =
    val counts = mutable.Map[Int, Int]()

    for num <- nums do
      counts += num -> (counts.getOrElse(num, 0) + 1)

    var ans = -1
    for (num, count) <- counts do
      if count == 1 then
        ans = scala.math.max(ans, num)
    ans

  def maxNumberOfBalloons(text: String): Int =
    val counts = mutable.Map[Char, Int]()
    var set = Set.from("balloon")

    for lt <- text do
      if set.contains(lt) then
        counts += lt -> (counts.getOrElse(lt, 0) + 1)

    if counts.size < set.size then 0
    else
      counts('l') /= 2
      counts('o') /= 2
      var ans = counts('l')
      for count <- counts.values do
        ans = scala.math.min(ans, count)
      ans

  def findMaxLength(nums: Array[Int]): Int =
    val map = mutable.Map[Int, Int]()
    var ans, count = 0

    for i <- nums.indices do
      val curr = if nums(i) == 1 then 1 else -1
      count += curr
      if count == 0 then
        ans = i + 1
      else if map.contains(count) then
        ans = scala.math.max(ans, i - map(count))
      else
        map.put(count, i)
    ans

  def groupAnagrams(strs: Array[String]): List[List[String]] =
    val groups = mutable.Map[String, List[String]]()

    for s <- strs do
      val arr = s.toArray
      Arrays.sort(arr)
      val key = arr.mkString
      groups += key -> (s :: groups.getOrElse(key, Nil))

    groups.values.toList

  def minimumCardPickup(cards: Array[Int]): Int =
    val dic = mutable.Map[Int, List[Int]]()
    for i <- cards.indices do
      val num = cards(i)
      dic += num -> (dic.getOrElse(num, Nil) :+ i) // fixme: use prepend for O(1)

    var ans = Int.MaxValue

    for key <- dic.keySet do
      val arr = dic(key)
      var i = 0
      while i < arr.size - 1 do
        ans = scala.math.min(ans, arr(i + 1) - arr(i) + 1)
        i += 1

    if ans == Int.MaxValue then -1 else ans

  def minimumCardPickup2(cards: Array[Int]): Int =
    val counts = mutable.Map[Int/*number*/, List[Int]/*ids of number*/]()

    for i <- cards.indices do /* find all positions for every card */
      val card = cards(i)
      counts += card -> (counts.getOrElse(card, Nil) :+ i) /* fixme: use prepend for O(1) */

    var ans = Int.MaxValue

    for (_, ids) <- counts do
      var i = 0
      while i < ids.size - 1 /*because at last index we'll use i + 1*/ do // for ...
        val rightBound = ids(i + 1) /* right bound always be bigger since ids sorted in asc order */
        val leftBound = ids(i)
        ans = scala.math.min(ans, rightBound - leftBound + 1) /* general formula `right - left + 1` */
        i += 1

    if ans == Int.MaxValue then -1 else ans

  def maximumSum(nums: Array[Int]): Int =
    def sumOfDigits(n: Int): Int =
      if n == 0 then 0
      else n % 10 + sumOfDigits(n / 10)

    val counts = mutable.HashMap[Int, Int]()
    var ans = -1

    for num <- nums do
      val digitSum = sumOfDigits(num)
      if counts.contains(digitSum) then // sum already exists, save answer
        ans = scala.math.max(ans, num + counts(digitSum))
      else // record new sum of digits as unique key
        counts += digitSum -> (scala.math.max(counts.getOrElse(digitSum, 0), num))
    ans

  def equalPairs(grid: Array[Array[Int]]): Int =
    val rowsMap, colsMap = mutable.Map[String, Int]()

    for row <- grid do
      val key = row.mkString
      rowsMap += key -> (rowsMap.getOrElse(key, 0) + 1)
    
    for col <- grid.indices do
      val currentCol = new Array[Int](grid.size)
      for row <- grid.indices do
        currentCol(row) = grid(row)(col)

      val key = currentCol.mkString
      colsMap += key -> (colsMap.getOrElse(key, 0) + 1)

    var ans = 0

    for (k, v) <- rowsMap do
      ans += v * colsMap.getOrElse(k, 0)
    ans

  def canConstruct(ransomNote: String, magazine: String): Boolean =
    boundary:
      if ransomNote.length() > magazine.length() then break(false)

      val counts = mutable.Map[Char, Int]()

      for i <- magazine.indices do
        if i < ransomNote.length then
          val lt = ransomNote(i)
          counts += lt -> (counts.getOrElse(lt, 0) - 1)
        val lt = magazine(i)
        counts += lt -> (counts.getOrElse(lt, 0) + 1)
      counts.forall(_._2 >= 0)

  def numJewelsInStones(jewels: String, stones: String): Int =
    val counts = new Array[Int](52)

    for stone <- stones do
      if stone < 'a' then
        counts(stone - 'A' + 26) += 1
      else
        counts(stone - 'a') += 1

    var ans = 0

    for jewel <- jewels do
      if jewel < 'a' then
        ans += counts(jewel - 'A' + 26)
      else
        ans += counts(jewel - 'a')
    ans

  def lengthOfLongestSubstring(s: String): Int =
    if s.isEmpty then return 0 // boundary
    if s.size < 2 then return 1

    var left, right = 0
    var set = mutable.Set[Char]()
    var ans = 0

    while right < s.size do
      val curr = s(right)
      while (set.contains(curr)) {
        set -= s(left)
        left += 1
      }
      set += curr
      ans = scala.math.max(ans, set.size)
      right += 1
    ans

  def lengthOfLongestSubstring2(s: String): Int =
    if s.isEmpty then return 0
    if s.size == 1 then return 1

    val map = mutable.Map[Char, Int/*idx*/]()
    var ans = 0

    var left = 0
    for right <- s.indices do
      val curr = s(right)
      left = math.max(left, map.getOrElse(curr, 0))
      ans = math.max(ans, right - left + 1)
      map += curr -> (right + 1)
    ans

object TestHashing:
  import Hashing.*

  testTwoSum(twoSum)
  testRepeatedCharacter(repeatedCharacter)
  testFindNumbers(findNumbers)
  testFindNumbers(findNumbers2)
  testCheckIsPangram(checkIfPangram)
  testMissingNumber(missingNumber)
  testMissingNumber(missingNumber2)
  testMissingNumber(missingNumber3)
  testMissingNumber(missingNumber4)
  testMissingNumber(missingNumber5)
  testCountElements(countElements)
  testFindLongestSubstring(findLongestSubstring)
  testFindLongestSubstring(findLongestSubstring2)
  testIntersection(intersection)
  testAreOccurrencesEqual(areOccurrencesEqual)
  testSubarraySum(subarraySum)
  testNumberOfSubarrays(numberOfSubarrays)
  testFindWinners(findWinners)
  testFindWinners(findWinners2)
  testLargestUniqueNumber(largestUniqueNumber)
  testMaxNumberOfBalloons(maxNumberOfBalloons)
  testFindMaxLength(findMaxLength)
  testGroupAnagrams(groupAnagrams)
  testMinimumCardPickup(minimumCardPickup)
  testMinimumCardPickup(minimumCardPickup2)
  testMaximumSum(maximumSum)
  testEqualPairs(equalPairs)
  testCanConstruct(canConstruct)
  testNumJewelsInStones(numJewelsInStones)
  testLengthOfLongestSubstring(lengthOfLongestSubstring)
  testLengthOfLongestSubstring(lengthOfLongestSubstring2)

  def testLengthOfLongestSubstring(fn: String => Int) =
    val call = fn("abcabcbb")
    val call2 = fn("bbbbb")
    val call3 = fn("pwwkew")
    val call4 = fn("aab")
    val call5 = fn("qrsvbspk")
    val call6 = fn("abcabcbb")
    val call7 = fn("dvdf")
    assert(call == 3, call)
    assert(call2 == 1, call2)
    assert(call3 == 3, call3)
    assert(call4 == 2, call4)
    assert(call5 == 5, call5)
    assert(call6 == 3, call6)
    assert(call7 == 3, call7)

  def testNumJewelsInStones(fn: (String, String) => Int) =
    val call = fn("aA", "aAAbbbb")
    val call2 = fn("z", "ZZ")
    assert(call == 3, call)
    assert(call2 == 0, call2)

  def testCanConstruct(fn: (String, String) => Boolean) =
    assert(!fn("a", "b"))
    assert(!fn("aa", "ab"))
    assert(fn("aa", "aab"))

  def testEqualPairs(fn: Array[Array[Int]] => Int) =
    val call = fn(
      Array(
        Array(3, 2, 1),
        Array(1, 7, 6),
        Array(2, 7, 7)
      )
    )
    assert(call == 1, call)

  def testMaximumSum(fn: Array[Int] => Int) =
    val call = fn(Array(22, 43, 5, 111, 12, 7, 43, 4))
    assert(call == 123, call)

  def testMinimumCardPickup(fn: Array[Int] => Int) =
    val call = fn(Array(1, 2, 6, 2, 1))
    val call2 = fn(Array(1, 1, 4))
    val call3 = fn(Array(1, 2, 6, 2, 1, 0, 1, 5))
    assert(call == 3, call)
    assert(call2 == 2, call2)
    assert(call3 == 3, call3)

  def testGroupAnagrams(fn: Array[String] => List[List[String]]) =
    val call = fn(
      Array("eat","tea","tan","ate","nat","bat")
    )

    assert(call.size == 3, call.size)
    assert(call.exists(_ == List("bat")))
    assert(call.exists(group =>  group == List("nat","tan") || group == List("tan", "nat")))
    assert(
      call.exists { group =>
        group == List("ate","eat","tea") ||
        group == List("tea", "eat", "ate") ||
        group == List("eat", "tea", "ate") ||
        group == List("ate", "tea", "eat")
      }
    )

  def testFindMaxLength(fn: Array[Int] => Int) =
    val call = fn(Array(0, 1))
    val call2 = fn(Array(0, 1, 0))
    val call3 = fn(Array(1, 0, 1, 0))
    val call4 = fn(Array(1, 0, 0, 1, 1, 0))
    assert(call == 2, call)
    assert(call2 == 2, call2)
    assert(call3 == 4, call3)
    assert(call4 == 6, call4)

  def testMaxNumberOfBalloons(fn: String => Int) =
    val call = fn("loonbalxballpoon")
    val call2 = fn("leetcode")
    val call3 = fn("balon")
    val call4 = fn("nlaebolko")
    val call5 = fn("balllllllllllloooooooooon")
    assert(call == 2, call)
    assert(call2 == 0, call2)
    assert(call3 == 0, call3)
    assert(call4 == 1, call4)
    assert(call5 == 1, call5)

  def testLargestUniqueNumber(fn: Array[Int] => Int) =
    val call = fn(Array(5,7,3,9,4,9,8,3,1))
    val call2 = fn(Array(9,9,8,8))
    assert(call == 8, call)
    assert(call2 == -1, call2)

  def testFindWinners(fn: Array[Array[Int]] => List[List[Int]]) =
    val call = fn(
      Array(
        Array(1, 3), Array(2, 3), Array(3, 6),
        Array(5, 6), Array(5, 7), Array(4, 5),
        Array(4, 8), Array(4, 9), Array(10, 4),
        Array(10, 9)
      )
    )
    val expected = List(
      List(1,2,10),
      List(4,5,7,8)
    )
    assert(call == expected, call)

  def testNumberOfSubarrays(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(1, 1, 2, 1, 1), 3)
    assert(call == 2, call)

  def testSubarraySum(fn: (Array[Int], Int) => Int) =
    val call = fn(Array(1, 2, 1, 2, 1), 3)
    assert(call == 4, call)

  def testAreOccurrencesEqual(fn: String => Boolean) =
    val call = fn("abacbc")
    val call2 = fn("aaabb")
    
    assert(call)
    assert(!call2)

  def testIntersection(fn: Array[Array[Int]] => List[Int]) =
    val call = fn(
      Array(
        Array(3,1,2,4,5),
        Array(1,2,3,4),
        Array(3,4,5,6)
      )
    )
    val call2 = fn(
      Array(
        Array(1, 2, 3, 1000),
        Array(4, 5, 6, 100, 1000)
      )
    )

    assert(call == List(3, 4), call)
    assert(call2 == List(1000), call2)

  def testFindLongestSubstring(fn: (String, Int) => Int) =
    val call = fn("eceba", 2)
    assert(call == 3, call)

  def testCountElements(fn: Array[Int] => Int) =
    val call = fn(Array(2,3,4))
    assert(call == 2, call)

  def testMissingNumber(fn: Array[Int] => Int) =
    val call = fn(Array(3,0,1))
    val call2 = fn(Array(0, 1))
    val call3 = fn(Array(9,6,4,2,3,5,7,0,1))
    val call4 = fn(Array(9,8,6,4,2,3,5,7,0,1))

    assert(call == 2, call)
    assert(call2 == 2, call2)
    assert(call3 == 8, call3)
    assert(call4 == 10, call4)

  def testCheckIsPangram(fn: String => Boolean) =
    val call = fn("thequickbrownfoxjumpsoverthelazydog")
    val call2 = fn("leetcode")
    assert(call)
    assert(!call2)

  def testFindNumbers(fn: Array[Int] => List[Int]) =
    val call = fn(Array(2,3,6))
    assert(call == List(6), call)

  def testTwoSum(fn: (Array[Int], Int) => Array[Int]) =
    val call = fn(Array(5, 2, 7, 10, 3, 9), 8).toSeq
    assert(call == Seq(4, 0), call)

  def testRepeatedCharacter(fn: String => Char) =
    val call = fn("abcdeda")
    val call2 = fn("qwe")
    assert(call == 'd', call)
    assert(call2 == ' ', call2)
