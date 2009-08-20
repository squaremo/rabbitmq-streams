package com.rabbitmq.streams.management.utils.paging

class Pager(val pageSize: Int, val totalSize: Int) {
  require(pageSize > 0, "pageSize cannot be zero or negative, pageSize provided was " + pageSize)
  require(totalSize >= 0, "total size cannot be negative, totalSize provided was " + totalSize)

  def numberOfPages: Int = totalSize match {
    case 0 => 0
    case _ => pageSize >= totalSize match {
      case true => 1
      case false => totalSize / pageSize + 1
    }
  }
}