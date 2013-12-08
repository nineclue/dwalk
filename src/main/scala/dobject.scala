package compdir

import java.nio.file.{Path, Files, Paths, FileSystems, LinkOption}
import scala.collection.mutable.ListBuffer

/* primitive scala functions about Java's newDirectoryStream */
trait SPath {
  type Ignore = Path => Boolean
  type DirInfo = Tuple2[Iterator[Dir], Iterator[File]]

  implicit def str2Path(p:String):Path = Paths.get(p)

  implicit def path2Iterator(p:Path) = new Iterator[Path] {
    private var stream = Files.newDirectoryStream(p)
    private var it = stream.iterator
    private var finished = false
    def hasNext:Boolean = { 
      if (finished) false
      else {
	      val r = it.hasNext
	      if (r == false) { finished = true; stream.close }
	      r
	    }
    }
    def next = it.next
  }

  implicit def defaultIgnore:Ignore = 
      { f => f.toString.apply(0) == '.' || Files.isHidden(f) }

  def walkPath(code:Path=>Unit)(p:Path)(implicit ignore:Ignore) =
      path2Iterator(p).filter(!ignore(_)).foreach(code(_))
}

trait DObject extends SPath {
  val path:Path
  implicit def path2DObject(p:Path):DObject = 
      if (Files.isDirectory(p)) Dir(p)
      else File(p)
  implicit def dObject2Path(d:DObject):Path = d.path

  def pseudochecksum():Long

  /* don't follow symbolic link, if don't want, override it to true */
  val followLink:Boolean = false    

  def size = Files.size(this.path)

  def isDirectory = Files.isDirectory(this.path)

  def isHidden = Files.isHidden(this.path)

  def name = this.path.getFileName.toString

  def modifiedTime = 
      if (followLink) Files.getLastModifiedTime(this.path)
      else Files.getLastModifiedTime(this.path, LinkOption.NOFOLLOW_LINKS)

  def nameSum(name:String):Long = name.foldLeft(0)((acc, c) => acc << 1 + c)
}

case class File(path:Path) extends DObject {
  assert(!Files.isDirectory(path))
  def pseudochecksum:Long = 
      nameSum(name) + size + modifiedTime.toMillis
}

case class Dir(path:Path) extends DObject {
  assert(Files.isDirectory(path) && path.isAbsolute)
  def pseudochecksum:Long = nameSum(name)

  def walk(code:PartialFunction[DObject,Unit])(implicit ignore:Ignore) = 
    path2Iterator(path).filter(!ignore(_)).foreach(p => code(p))
}

object Dir extends SPath {
	def walk(path:Path)(code:PartialFunction[DObject, Unit])(implicit ignore:Ignore) = {
		Dir(path.toAbsolutePath).walk(code)(ignore)
	}

	def process(path:Path)(code:(Dir, Iterator[File]) => Unit)(implicit ignore:Ignore) = {
		def helper(d:Dir):Unit = {
			val fs = ListBuffer[File]()
			path2Iterator(d.path).withFilter(!ignore(_)).foreach(p =>
				if (Files.isDirectory(p)) helper(Dir(p))
				else fs += File(p))
			code(d, fs.toIterator)
		}
		helper(Dir(path.toAbsolutePath))
	}

	def summary(path:Path)(implicit ignore:Ignore) = ???
}
