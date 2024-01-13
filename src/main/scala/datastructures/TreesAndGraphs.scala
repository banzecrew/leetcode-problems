package org.leetcode.course

// Simplified tree representation
final class TreeNode(
  var value: Int,
  left: TreeNode = null,
  right: TreeNode = null
)

object TreeNode:
  def apply(value: Int): TreeNode = new TreeNode(value)

object TreesAndGraphs