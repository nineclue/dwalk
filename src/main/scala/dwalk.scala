package compdir

// import java.io.File
import java.nio.file.{Path, Files, Paths, FileSystems, LinkOption}
import java.nio.file.attribute.FileTime

object Run extends DObject {
  def main(args: Array[String]):Unit = {
    val startPath = if (args.isEmpty) Paths.get("").toAbsolutePath else Paths.get(args(0))
    println(s"checking ${startPath.toString}")
    def helper:Path=>Unit = { p:Path => 
      if (isDirectory(p)) dprint(p)
      println(p.toString)
    }
    def dprint = walkPath(helper)_
    dprint(startPath)
    /* def dprint(p:Path):Unit = {
      if (Files.isDirectory(p)) walk(p)(dprint)
        println(p.toString)      
    } */
  }
}

trait SPath {
  type Ignore = Path => Boolean

  implicit def path2Iterator(p:Path) = new Iterator[Path] {
    private var stream = Files.newDirectoryStream(p)
    private var it = stream.iterator
    private var finished = false
    def hasNext:Boolean = { 
      if (finished) {
        stream = Files.newDirectoryStream(p)
        it = stream.iterator
        finished = false
      }
      val r = it.hasNext
      if (r == false) { finished = true; stream.close }
      r
    }
    def next = it.next
  }

  implicit def defaultIgnore:Ignore = { f => f.toString.apply(0) == '.' || Files.isHidden(f) }

  def walkPath(code:Path=>Unit)(p:Path)(implicit ignore:Ignore) = {
    path2Iterator(p).filter(!ignore(_)).foreach(code(_))
  }

}

trait DObject extends SPath {
  type DInfo = Tuple3[String, Boolean, Long]

  def walk(code:Path=>Unit)(implicit p:Path, ignore:Ignore) = {
    path2Iterator(p).filter(!ignore(_)).foreach(code(_))
  }

  def size(implicit p:Path) = Files.size(p)

  def isDirectory(implicit p:Path) = Files.isDirectory(p)

  def isHidden(implicit p:Path) = Files.isHidden(p)

  def name(implicit p:Path) = p.getFileName.toString

  def modifiedTime(implicit p:Path) = Files.getLastModifiedTime(p)

  def nameSum(name:String):Long = name.foldLeft(0)((acc, c) => acc + c)
}

case class File(path:Path) extends DObject{
  assert(!isDirectory(path))
  implicit val self = path

  def pseudochecksum = nameSum(name) + size + modifiedTime.toMillis
}

class Directory(val path:Path) extends DObject {
  assert(Files.isDirectory(path) && path.isAbsolute)
  implicit val self = path

  def pseudochecksum = nameSum(name)
}
