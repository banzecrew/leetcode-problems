package org.leetcode.course

import org.leetcode.course.TreesAndGraphs.maxDepth

// Simplified tree representation
final case class TreeNode(
  var value: Int,
  var left: TreeNode = null,
  var right: TreeNode = null
)

object TreeNode:
  def preorderDfs(node: TreeNode): Unit =
    if node == null then
      return ()

    println(node.value) // logic before traversal
    preorderDfs(node.left)
    preorderDfs(node.right)

  def inorderDfs(node: TreeNode): Unit =
    if node == null then
      return ()

    inorderDfs(node.left)
    println(node.value) // logic between traversal
    inorderDfs(node.right)

  def postorderDfs(node: TreeNode): Unit =
    if node == null then
      return ()

    postorderDfs(node.left)
    postorderDfs(node.right)
    println(node.value) // logic after traversal

  /**
    *    0
    *  /   \
    * 1     2
    */
  val root = TreeNode(0)
  val one = TreeNode(1)
  val two = TreeNode(2)

  root.left = one
  root.right = two

object TreesAndGraphs:
  import TreeNode.*

  def maxDepth(node: TreeNode): Int =
    if node == null then
      return 0
    
    val left = maxDepth(node.left)
    val right = maxDepth(node.right)
    scala.math.max(left, right) + 1

object TestTreesAndGraphs:
  /**
    *   Tree
    *       0
    *     /   \
    *    1     2
    *  /  \     \
    * 3    4     5
    *       \
    *        6
    */
  val tree = TreeNode(
    value = 0,
    TreeNode(value = 1,
      TreeNode(value = 3),
      TreeNode(value = 4, right = TreeNode(value = 6))
    ),
    TreeNode(
      value = 2,
      right = TreeNode(value = 5)
    )
  )

  testMaxDepth(maxDepth)

  def testMaxDepth(fn: TreeNode => Int) =
    val call = fn(tree)
    assert(call == 4, call)
