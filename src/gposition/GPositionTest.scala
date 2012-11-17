package gposition

import java.util.ArrayList

class GPositionTest {
  private var diffStringList: ArrayList[String] = new ArrayList[String]

  def getDiffStringList: ArrayList[String] = {
    return diffStringList
  }
  def setDiffStringList(diffStringList: ArrayList[String]) {
    this.diffStringList = diffStringList
  }

}