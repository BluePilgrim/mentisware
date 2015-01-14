package com.mentisware.ds

abstract class TreeNode {
  var parent: TreeNode
  var left: TreeNode
  var right: TreeNode
}

trait BinarySearchTree[Key, Node] {
  def search(k: Key): Option[Node]
  def predecessor(n: Node): Option[Node]
  def successor(n: Node): Option[Node]
  def minimum(): Option[Node]
  def maximum(): Option[Node]
  def insert(n: Node): Unit
  def delete(n: Node): Unit
  def remove(k: Key): Option[Node]
}