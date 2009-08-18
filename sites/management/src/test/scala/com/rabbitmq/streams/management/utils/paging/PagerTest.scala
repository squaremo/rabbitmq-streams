package com.rabbitmq.streams.management.utils.paging

import org.scalatest.{FunSuite, Suite}

class PagerTest extends FunSuite {
  test("0 is not a valid page size") {
    intercept[IllegalArgumentException] {
      new Pager(0, 0)
    }
  }

  test("-1 is not a valid page size") {
    intercept[IllegalArgumentException] {
      new Pager(-1, 0)
    }
  }

  test("-1 is not a valid total size")  {
    intercept[IllegalArgumentException]  {
      new Pager(1, -1)
    }
  }

  test("A pager with a total size of 0 will have 0 pages")  {
    expect(0) {
      val p = new Pager(10, 0)
      p.numberOfPages
    }
  }

  test("A pager with total size of 10 and a page size of 10 will have 1 page")  {
    expect(1) {
      val p = new Pager(10, 10)
      p.numberOfPages
    }
  }

  test("A pager with total size of 8 and a page size of 10 will have 1 page")  {
    expect(1) {
      val p = new Pager(10, 8)
      p.numberOfPages
    }
  }

  test("A pager with total size of 24 and a page size of 10 will have 3 pages") {
    expect(3) {
      val p = new Pager(10, 24)
      p.numberOfPages
    }
  }
}

