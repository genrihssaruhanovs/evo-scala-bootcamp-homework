package evo.homework.akka

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  sealed private trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case i @ Insert(_, _, _)   => doInsert(i)
    case c @ Contains(_, _, _) => doContains(c)
    case r @ Remove(_, _, _)   => doRemove(r)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) m.requester ! OperationFinished(m.id)
    else {
      val position = if (m.elem > elem) Right else Left
      subtrees.get(position) match {
        case Some(childActor) => childActor ! m
        case None =>
          subtrees += (position -> context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false)))
          m.requester ! OperationFinished(m.id)
      }
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem && !removed) m.requester ! ContainsResult(m.id, result = true)
    else {
      val position = if (m.elem > elem) Right else Left
      subtrees.get(position) match {
        case Some(childActor) => childActor ! m
        case None =>
          m.requester ! ContainsResult(m.id, result = false)
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    } else {
      val position = if (m.elem > elem) Right else Left
      subtrees.get(position) match {
        case Some(childActor) => childActor ! m
        case None =>
          m.requester ! OperationFinished(m.id)
      }
    }
  }
}
