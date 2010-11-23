package tree

import java.io.File

object Tree {
  
  def main(args: Array[String]) {
    val dirName = args match {
      case Array(hd) => hd
      case Array(hd, _) => hd
      case _ => "."
    }
    new File(dirName) match {
      case d if d.isDirectory => new RootDir(d).print
      case d => {
        println("[Error] " + d + " is not directory!")
        exit(1)
      }
    }
  }
}

abstract class Node(file : File, indent: String = "") {
  
  val branch : String
  val childNode : String
  
  def print {
    println(indent + branch + file.getName)
    if (file.isDirectory)
      ls.foreach(_.print)
  }

  private def ls : Array[Node] = {
    try {
      val l = file.listFiles.zipWithIndex
      l.collect { 
        case (f, i) if f.isDirectory & i == l.length - 1 => new LastDir(f, indent + childNode)
        case (f, i) if f.isFile & i == l.length - 1 => new LastFile(f, indent + childNode)
        case (f, i) if f.isDirectory => new NormalDir(f, indent + childNode)
        case (f, i) if f.isFile => new NormalFile(f, indent + childNode)
      }
    } catch {
      case e: NullPointerException => println("Permission Denied. [%s]".format(file))
      Array()
    }
  }
}

class RootDir(file : File, indent : String = "") extends Node(file, indent) {
  val branch = ""
  val childNode = ""
}

class NormalDir(file : File, indent : String) extends Node(file, indent) {
  val branch = "|-- "
  val childNode = "|   "
}

class NormalFile(file : File, indent : String) extends Node(file, indent) {
  val branch = "|-- "
  val childNode = ""
}

class LastDir(file : File, indent : String) extends Node(file, indent) {
  val branch = "`-- "
  val childNode = "    "
}

class LastFile(file : File, indent : String) extends Node(file, indent) {
  val branch = "`-- "
  val childNode = ""
}



