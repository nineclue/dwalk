package compdir

import java.nio.file.{Path, Files, Paths, FileSystems, LinkOption}
// import java.nio.file.attribute.FileTime

object Run extends SPath {
  def main(args: Array[String]):Unit = {
    val startPath = if (args.isEmpty) "" else args(0)
    println(s"checking ${startPath}")
    // walk(startPath)
    tree(startPath)
  }

  def walk(path:String) = {
    def printer: PartialFunction[DObject, Unit] = {
      case f:File => println(f"File : ${f.name} - ${f.size}(${f.pseudochecksum}%x)- ${f.modifiedTime}")
      case d:Dir => { println(s"Dir : ${d.path}"); d.walk(printer) }
    }
    Dir.walk(path)(printer)
  }

  def tree(path:Path) = {
    val iniCompCount = path.toAbsolutePath.getNameCount
    val compIndent = 3
    def printer(dir:Dir, files:Iterator[File]) = {
      val curCompCount = dir.path.getNameCount
      val fsizeSum = files.foldLeft(0L)((acc, f) => acc + f.size)
      println(" " * (curCompCount - iniCompCount) * compIndent + dir.name + " : " + fsizeSum)
    }
    Dir.process(path)(printer)
  }
}
