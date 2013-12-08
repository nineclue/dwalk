package compdir

import java.nio.file.{Path, Files, Paths, FileSystems, LinkOption}
// import java.nio.file.attribute.FileTime

object Run extends SPath {
  def main(args: Array[String]):Unit = {
    val startPath = if (args.isEmpty) "" else args(0)
    println(s"method process($startPath)")
    process(startPath)
    println(s"method tree($startPath)")
    tree(startPath)
  }

  def process(path:String) = {
    def printer(o:DObject):Unit = 
      if (o.isDirectory) { println(s"Dir : ${o.path}")  }
      else println(f"File : ${o.name} - ${o.size}(${o.pseudochecksum}%x)- ${o.modifiedTime}")
    Dir.process(path)(printer)
  }

  def tree(path:Path) = {
    val r = Dir(path.toAbsolutePath)
    val iniCompCount = path.toAbsolutePath.getNameCount
    val compIndent = 3
    def helper(d:Dir):Unit = {
      val curCompCount = d.path.getNameCount
      println(" " * (curCompCount - iniCompCount) * compIndent + d.name + " " + d.files.map(_.size).sum)
      d.dirs.foreach(helper(_))
    }
    helper(r)
  }
}
