
class ListNode(_x: Int, _next: ListNode = null){
  val x = _x
  var next = _next
}

object ReverseLinkedList {

  def go = {
    val v1 = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))
    println(s"The original list: ${viewList(v1)}")
    val r_imperative = reverseListImperative(v1)
    println(s"The reversed list (imperative): ${viewList(r_imperative)}")
    val v2 = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))
    println(s"The original list again: ${viewList(v2)}")
    val r_functional = reverseListFunctional(v2)
    println(s"The reversed list (functional): ${viewList(r_functional)}")
  }

  def viewList(head: ListNode): String = {
    def view(node: ListNode): String = 
      if (node == null) "(null)" else s"(${node.x})-->>" + view(node.next)
    view(head)
  }

  def reverseListImperative(head: ListNode): ListNode = {
    var prev: ListNode = null
    var curr = head

    while (curr != null) {
      val temp = curr.next
      curr.next = prev
      prev = curr
      curr = temp
    }

    prev
  }

  def reverseListFunctional(head: ListNode): ListNode = {
    def recurse(prev: ListNode, curr: ListNode): ListNode =
      if (curr == null) prev
      else {
        val temp = curr.next

        curr.next = prev

        recurse(curr, temp)
      }

    recurse(null, head)
  }

}
