package com.rabbitmq.streams.management.utils.paging

import org.scalatest.junit.JUnit3Suite

class PagerTest extends JUnit3Suite {
  def testZeroIsNotAValidPageSize() {
    intercept[IllegalArgumentException] {
      new Pager(0, 0)
    }
  }

  def testMinusOneIsNotAValidPageSize() {
    intercept[IllegalArgumentException] {
      new Pager(-1, 0)
    }
  }

  def testMinusOneIsNotAValidTotalSize()  {
    intercept[IllegalArgumentException]  {
      new Pager(1, -1)
    }
  }

  def testAPagerWithATotalSizeOfZeroWillHaveZeroPages()  {
    expect(0) {
      val p = new Pager(10, 0)
      p.numberOfPages
    }
  }

  def testAPagerWithTotalSizeOfTenAndAPageSizeOfTenWillHaveOnePage()  {
    expect(1) {
      val p = new Pager(10, 10)
      p.numberOfPages
    }
  }

  def testAPagerWithTotalSizeOfEightAndAPageSizeOfTenWillHaveOnePage()  {
    expect(1) {
      val p = new Pager(10, 8)
      p.numberOfPages
    }
  }

  def testAPagerWithTotalSizeOfTwentyFourAndAPageSizeOfTenWillHaveThreePages() {
    expect(3) {
      val p = new Pager(10, 24)
      p.numberOfPages
    }
  }
}

