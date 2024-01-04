package org.leetcode.course

import scala.collection.mutable
import scala.annotation.tailrec

sealed trait MyList[+A]:
  // we want to have ability to chaining over next
  def next: MyList[A]

object MyList:
  // ADT
  case object Empty extends MyList[Nothing]:
    final override def next: MyList[Nothing] = throw new NoSuchElementException("next on empty list")
  final case class Node[A](var value: A, var next: MyList[A]) extends MyList[A]

  def apply[A](value: A): Node[A] = Node(value, Empty)

  // auxilary function: possible buffer overflow
  def apply[A](xs: A*): MyList[A] =
    if xs.isEmpty then Empty
    else Node(xs.head, apply(xs.tail: _*))

  def getSum(head: MyList[Int]): Int =
    // no head-recursion due to possible stack overflow
    @tailrec def f(h: MyList[Int], ac: Int): Int = h match
      case Empty => ac
      case Node(value, next) => f(next, ac + value)
    f(head, 0)

  def addNode[A](prevNode: MyList[A], nodeToAdd: MyList[A]): Unit =
    // prevNode: n1(4)
    // nodeToAdd: n4(66)
    // n1(4) -> n2(6) -> n3(8)
    (prevNode, nodeToAdd) match
      case (old: Node[A], added: Node[A]) =>
        added.next = old.next
        old.next = added
      case _ => ()

  def deleteNode[A](node: MyList[A]): Unit = node match
    case Empty => ()
    case n: Node[A] =>
      n.next = n.next.next

object TestMyList:
  import MyList.*

  testAddNode(addNode)
  testChaining()

  def testChaining() =
    val n1 = MyList(1)
    val n2 = MyList(2)
    val n3 = MyList(3)

    n1.next = n2
    n2.next = n3

    assert(n1.next.next == n3, n1.next.next)
    assert(n1.next.next eq n3) // same object ref

  def testAddNode[A](fn: (MyList[A], MyList[A]) => Unit) =
    val n1 = MyList(1)
    val n2 = MyList(3)
    val n3 = MyList(4)
    val n4 = MyList(7)

    n1.next = n2
    n2.next = n3
    n3.next = n4

    assert(n1 == MyList(1,3,4,7), n1)
    addNode(n2, MyList(555))
    assert(n1 == MyList(1, 3, 555, 4, 7), n1)

final class ListNode:
  var value: Int = 0
  var next: ListNode | Null = null

object ListNode:
  def apply(x: Int): ListNode =
    var node = new ListNode
    node.value = x
    node

  def getSum(head: ListNode): Int =
    var h = head
    var sum = 0
    while h != null do
      sum += h.value
      h = h.next
    sum
  
  def getSum2(head: ListNode): Int =
    if head == null then 0
    else head.value + getSum(head.next)
  
// Let prevNode be the node at position i - 1
  def addNode(prevNode: ListNode, nodeToAdd: ListNode): Unit =
    nodeToAdd.next = prevNode.next
    prevNode.next = nodeToAdd

  def deleteNode(prevNode: ListNode): Unit =
    prevNode.next = prevNode.next.next

object TestListNode:
  import ListNode.*

  testGetSum(getSum)
  testGetSum(getSum2)
  testAddNote(addNode)
  testDeleteNode(deleteNode)

  def testDeleteNode(fn: ListNode => Unit) =
    val node1 = ListNode(2)
    val node2 = ListNode(4)
    val node3 = ListNode(7)
    node1.next = node2
    node2.next = node3
    
    fn(node1)

    assert(node1.value == 2)
    assert(node2.next.value == 7)
    
  def testAddNote(fn: (ListNode, ListNode) => Unit) =
    val node1 = ListNode(2)
    val node2 = ListNode(4)
    val node3 = ListNode(7)
    node1.next = node2
    node2.next = node3

    assert(node1.value == 2, node1.value)
    assert(node1.next.value == 4, node1.next.value)
    assert(node1.next.next.value == 7, node1.next.next.value)

    fn(node2, ListNode(66))

    assert(node1.value == 2, node1.value)
    assert(node1.next.value == 4, node1.next.value)
    assert(node1.next.next.value == 66, node1.next.next.value)
    assert(node1.next.next.next.value == 7, node1.next.next.next.value)

  def testGetSum(fn: ListNode => Int) =
    val node1 = ListNode(2)
    val node2 = ListNode(4)
    val node3 = ListNode(7)
    node1.next = node2
    node2.next = node3
    val call = fn(node1)
    assert(call == 13, call)

sealed trait Listt[+A]:
  def head: A
  def tail: Listt[A]
  def ::[S >: A](elem: S): Listt[S]

object Listt:
  def apply[A](xs: A*): Listt[A] =
    if xs.isEmpty then Empty
    else Cons(xs.head, apply(xs.tail: _*))

  case object Empty extends Listt[Nothing]:
    override def head: Nothing = throw new NoSuchElementException("head of empty list")
    override def tail: Listt[Nothing] = throw new NoSuchElementException("tail of empty list")
    override def ::[S >: Nothing](elem: S): Listt[S] = Cons(elem, Empty)

  final case class Cons[A](override val head: A, var tail: Listt[A]) extends Listt[A]:
    self =>
    override def ::[S >: A](elem: S): Listt[S] = Cons(elem, self)

object Variance:
  class Animal
  class Dog extends Animal:
    def sound: Unit = println("gaw-gaw!")
  class Cat extends Animal
  class Crocodile extends Animal

  class SList[+T]

  // if dogs are animal then is a SList[Dog] also a SList[Animal]
  // the variance question

  // 1 - yes => generic type is COVARIANT
  val animal: Animal = new Dog
  val animals: SList[Animal] = new SList[Dog]

  // 2 - no => generic type is INVARIANT 

  // 3 - hell no! backwards => generic type is CONTRAVARIANT
  
  class Vet[-T]
  val lassiesVet: Vet[Dog] = new Vet[Animal]
  
  // variance problem
  // abstract class MyList2[+A]:
  //   def head: A
  //   def tail: MyList2[A]
  //   def add(el: A): MyList2[A]

  // types of val fields are in COVARIANT position
  // class Vet2[-A](val favoriteAnimal: A)
  
  // val garfield = new Cat
  // val theVet: Vet2[Animal] = new Vet2[Animal](garfield)
  // val lassiesVet2: Vet2[Dog] = theVet

  // val lassie: Dog = lassiesVet2.favoriteAnimal // type conflict

  // types of var fields are in COVARIANT position
  // class Vet3[-A](var favoriteAnimal: A)

  // class MutableOpt[+A](var contents: A)

  // types of var fields are also in CONTRAVARIANT position
  // class MutableOpt[+A](var contents: A)
  // val maybeAnimal: MutableOpt[Animal] = new MutableOpt[Dog](new Dog)
  // maybeAnimal.contents = new Cat // type conflict

  // type of method arguments are in CONTRAVARIANT position
  // class MyList2[+A]:
  //   def add(el: A): MyList2[A] = ???

  // val animals2: MyList2[Animal] = new MyList2[Cat]
  // val moreAnimals = animals2.add(new Dog) // type conflict

  class Vet3[-A]:
    def heal(animal: A): Boolean = true

  val lassiesVet3: Vet3[Dog] = new Vet3[Animal]
  lassiesVet3.heal(new Dog)
  // lassiesVet3.heal(new Cat) // legit error

  // method return types are in COVARIANT position
  // abstract class Vet4[-A]:
  //   def rescueAnimal(): A

  // val vet4: Vet4[Animal] = new Vet4[Animal]:
  //   override def rescueAnimal(): Animal = new Cat

  // val lassiesVet4: Vet4[Dog] = vet4
  // val rescuedDog: Dog = lassiesVet4.rescueAnimal() // Dog but actually cat = type conflict!

  class MyListC[+T]:
    def add[S >: T](elem: S): MyListC[S] = new MyListC[S]

  class VetC[-T]:
    def rescueAnimal[S <: T](): S = ???

  //val lassiesVetC: VetC[Dog] = new VetC[Animal]
  //lazy val rescuedDogC: Dog = lassiesVetC.rescueAnimal() // rescueAnimal[Dog]
