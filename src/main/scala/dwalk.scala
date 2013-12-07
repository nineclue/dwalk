package compdir

import java.nio.file.{Path, Files, Paths, FileSystems, LinkOption}
import java.nio.file.attribute.FileTime

object Run extends SPath {
  def main(args: Array[String]):Unit = {
    val startPath = if (args.isEmpty) Paths.get("").toAbsolutePath else Paths.get(args(0))
    println(s"checking ${startPath.toString}")

    def printer: PartialFunction[DObject, Unit] = {
      case f:File => println(s"File : ${f.name} - ${f.size} - ${f.modifiedTime}")
      case d:Dir => { println(s"Dir : ${d.path}"); d.walk(printer) }
    }
    Dir(startPath).walk(printer)
  }
}

/* primitive scala functions about Java's newDirectoryStream */
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

  implicit def defaultIgnore:Ignore = 
      { f => f.toString.apply(0) == '.' || Files.isHidden(f) }

  def walkPath(code:Path=>Unit)(p:Path)(implicit ignore:Ignore) =
      path2Iterator(p).filter(!ignore(_)).foreach(code(_))
}

trait DObject extends SPath {
  /* name, directory, checksum */
  type DInfo = Tuple3[String, Boolean, Long]

  val path:Path
  implicit def path2DObject(p:Path):DObject = 
      if (Files.isDirectory(p)) Dir(p)
      else File(p)
  implicit def dObject2Path(d:DObject):Path = d.path

  def pseudochecksum(implicit p:Path):Long

  /* don't follow symbolic link, if don't want, override it to true */
  val followLink:Boolean = false    

  def size = Files.size(this.path)

  def isDirectory = Files.isDirectory(this.path)

  def isHidden = Files.isHidden(this.path)

  def name = this.path.getFileName.toString

  def modifiedTime = 
      if (followLink) Files.getLastModifiedTime(this.path)
      else Files.getLastModifiedTime(this.path, LinkOption.NOFOLLOW_LINKS)

  def nameSum(name:String):Long = name.foldLeft(0)((acc, c) => acc + c)
}

case class File(path:Path) extends DObject {
  assert(!Files.isDirectory(path))
  def pseudochecksum(implicit p:Path):Long = 
      nameSum(p.name) + p.size + p.modifiedTime.toMillis
}

case class Dir(path:Path) extends DObject {
  assert(Files.isDirectory(path) && path.isAbsolute)
  def pseudochecksum(implicit p:Path):Long = nameSum(p.name)

  def walk(code:PartialFunction[DObject,Unit])(implicit ignore:Ignore) = 
    path2Iterator(path).filter(!ignore(_)).foreach(p => code(p))
}
