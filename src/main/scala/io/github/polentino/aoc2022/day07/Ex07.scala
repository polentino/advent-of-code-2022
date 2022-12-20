package io.github.polentino.aoc2022.day07

import zio.*

import io.github.polentino.aoc2022.day07.Ex07.FS.*

object Ex07 {

  // lines that triggers a state change, aka commands
  private val cdToDir      = "\\$ cd (/|\\w+)".r
  private val listDir      = "$ ls"
  private val cdUp         = "$ cd .."
  // lines that updates current state
  private val lsDirOutput  = "dir\\s(\\w+)".r
  private val lsFileOutput = "(\\d+)\\s(\\p{Graph}+)".r

  def solve(lines: List[String]): Int = {
    buildTree(lines) { tree =>
      TreeBuilder.getDirs(tree)
        .filter(_.size <= 100000).map(_.size).sum
    }
  }

  def solve2(lines: List[String]): Int = {
    buildTree(lines) { tree =>
      val unusedSpace    = 70000000 - tree.size
      val spaceToBeFreed = 30000000 - unusedSpace
      TreeBuilder.getDirs(tree)
        .map(_.size)
        .filter(_ >= spaceToBeFreed)
        .min
    }
  }

  private def buildTree(lines: List[String])(mapTree: Dir => Int) = {
    val allPaths = lines.foldLeft(Accumulator.empty)((acc, line) => acc.advance(line)).getPaths
    val reduced  = TreeBuilder.buildFromPaths(allPaths)
    mapTree(reduced)
  }

  sealed trait FS {
    val name: String
    val size: Int
  }

  object FS {
    final case class File(name: String, size: Int) extends FS

    final case class Dir(name: String, children: List[FS] = Nil) extends FS {

      val size = {
        def loop(fs: FS): Int = fs match {
          case dir: Dir => dir.children.map(loop).sum
          case f: File  => f.size
        }
        children.map(loop).sum
      }
    }
  }

  sealed trait Accumulator {
    def advance(line: String): Accumulator
    def getPaths: List[List[FS]]
  }

  object Accumulator {
    def empty: Accumulator = Empty

    case object Empty extends Accumulator {

      override def advance(line: String): Accumulator = line match
        case cdToDir(newDir) => Valid(List.empty, List(Dir(newDir)))
        case _               => Empty

      override def getPaths = List.empty
    }

    final case class Valid(paths: List[List[FS]], currentPath: List[FS]) extends Accumulator {

      override def advance(line: String): Accumulator = line match {
        case cdToDir(dirName)         => copy(currentPath = currentPath :+ Dir(dirName))
        case `listDir`                => this
        case `cdUp`                   => copy(currentPath = currentPath.dropRight(1))
        case lsDirOutput(_)           => this
        case lsFileOutput(size, name) => copy(paths = paths :+ (currentPath :+ File(name, Integer.parseInt(size))))
      }

      override def getPaths = paths
    }
  }

  object TreeBuilder {

    def buildFromPaths(paths: List[List[FS]]) = {
      val (start, rest) = paths match {
        case head :: tail => (reducePath(head), tail)
      }
      rest.foldLeft(start)(merge).asInstanceOf[Dir]
    }

    private def merge(main: FS, other: List[FS]): FS = {
      def loop(node: Dir, path: List[FS]): Dir = {
        if (hasLeavesOnly(node)) {
          node.copy(children = node.children :+ reducePath(path.tail))
        } else {
          val canBeTaversed = node.children.exists {
            case _: File => false
            case d: Dir  => containsPath(d, path.tail)
          }

          if (canBeTaversed) {
            node.copy(
              children = node.children.map {
                case f: File                              => f
                case d: Dir if containsPath(d, path.tail) => loop(d, path.tail)
                case d: Dir                               => d
              }
            )
          } else {
            node.copy(children = node.children :+ reducePath(path.tail))
          }
        }
      }

      loop(main.asInstanceOf[Dir], other)
    }

    def getDirs(root: Dir): List[Dir] =
      root.children.collect { case d: Dir => d +: getDirs(d) }.flatten

    private def containsPath(root: Dir, path: List[FS]): Boolean = path match {
      case (d: Dir) :: tail => root.name == d.name || d.children.exists {
          case _: File => false
          case d: Dir  => containsPath(d, tail)
        }
      case _                => false
    }

    def hasLeavesOnly(node: Dir): Boolean = node.children.forall {
      case _: File => true
      case _: Dir  => false
    }

    private def reducePath(path: List[FS]): FS = path.reduceRight((l, r) =>
      l match {
        case f: File => f
        case d: Dir  => d.copy(children = d.children :+ r)
      }
    )
  }
}
