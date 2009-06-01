package net.lshift.feedshub.management.controller


import scala.collection.mutable.Queue

trait RollingQueue {
  type T
  val maximumSize: Int

  private val queue: Queue[T] = new Queue[T]()

  def enqueue(item: T) = {
    queue.enqueue(item)
    while(queue.size > maximumSize) queue.dequeue
  }

  def dequeue: Option[T] = if(queue.isEmpty) None else Some(queue.dequeue)

  def contents: Queue[T] = queue.clone
}