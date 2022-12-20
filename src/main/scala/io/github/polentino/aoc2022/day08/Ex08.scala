package io.github.polentino.aoc2022.day08

object Ex08 {

  def solve(lines: List[String]) = {
    val treeMap        = lines.map(_.toCharArray.map(_.asDigit)).toArray
    val dimensionY     = treeMap.length
    val dimensionX     = treeMap.head.length
    val alreadyVisible = (dimensionX + dimensionY - 2) * 2

    val innerVisible = for {
      y <- 1 until dimensionY - 1
      x <- 1 until dimensionX - 1
    } yield isVisible(treeMap, x, y)

    alreadyVisible + innerVisible.sum
  }

  def solve2(lines: List[String]) = {
    val treeMap    = lines.map(_.toCharArray.map(_.asDigit)).toArray
    val dimensionY = treeMap.length
    val dimensionX = treeMap.head.length

    val scores = for {
      y <- 1 until dimensionY - 1
      x <- 1 until dimensionX - 1
    } yield treeScore(treeMap, x, y)

    scores.max
  }

  private def treeScore(treeMap: Array[Array[Int]], x: Int, y: Int) = {
    val height = treeMap(y)(x)
    val maxY   = treeMap.length
    val maxX   = treeMap.head.length

    def count(trees: Array[Int]) = {
      val idx = trees.indexWhere(_ >= height)
      if ( idx == -1 ) trees.length else idx + 1
    }

    val top = count(treeMap.slice(0, y).map(_ (x)).reverse)
    val bottom = count(treeMap.slice(y + 1, maxY).map(_ (x)))

    val left = count(treeMap(y).slice(0, x).reverse)
    val right = count(treeMap(y).slice(x + 1, maxX))
    top * bottom * left * right
  }

  private def isVisible(treeMap: Array[Array[Int]], x: Int, y: Int): Int = {
    val height = treeMap(y)(x)
    val maxY   = treeMap.length
    val maxX   = treeMap.head.length
    if (
      isVisibleVertically(treeMap, height, x, 0, y) ||
      isVisibleVertically(treeMap, height, x, y + 1, maxY) ||
      isVisibleHorizontally(treeMap, height, y, 0, x) ||
      isVisibleHorizontally(treeMap, height, y, x + 1, maxX)
    ) 1
    else 0
  }

  private def isVisibleVertically(treeMap: Array[Array[Int]], height: Int, x: Int, fromY: Int, toY: Int) =
    treeMap.slice(fromY, toY).map(_(x)).forall(_ < height)

  private def isVisibleHorizontally(treeMap: Array[Array[Int]], height: Int, y: Int, fromX: Int, toX: Int) =
    treeMap(y).slice(fromX, toX).forall(_ < height)
}
