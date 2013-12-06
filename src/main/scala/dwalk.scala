package compdir

// import java.io.File
import java.nio.file.{Path, Files, Paths, FileSystems}
import java.nio.file.attribute.FileTime

object Run {
  def main(args: Array[String]) = {
    val startPath = if (args.isEmpty) Paths.get("").toAbsolutePath else Paths.get(args(0))
    println(s"checking ${startPath.toString}")
    new Directory(startPath)
  }
}

trait DObject {
  val name:String
  type FileInfo = (String, Long)

  def pseudochecksum
  def nameSum:Long = name.foldLeft(0)((acc, c) => acc + c)
}

class File(val name:String, val size:Long, val mTime:FileTime) extends DObject{
  def pseudochecksum = nameSum + size + mTime.toMillis
}

class Directory(val path:Path) extends DObject {
  assert(path.isAbsolute)

  var files = Set[FileInfo]()
  val name = path.getFileName.toString
  def pseudochecksum = nameSum
  def walk = {
    val stream = Files.newDirectoryStream(path)
    val it = stream.iterator
    while (it.hasNext) {
      val nObj = it.next
      if (!Directory.ignore(nObj)) {
        if (Files.isDirectory(nObj)) new Directory(nObj)
        println(nObj.toString)
      }
    }
    stream.close
  }
  walk
}

object Directory {
  def ignore:Path => Boolean = defaultIgnore

  def defaultIgnore(f:Path) = f.toString.apply(0) == '.' || Files.isHidden(f)
}