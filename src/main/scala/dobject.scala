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
      path2Iterator(p).withFilter(!ignore(_)).foreach(code(_))
}

class DObject(val path:Path) extends SPath {
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

  def pseudochecksum:Long = nameSum(name)
}

class File(override val path:Path) extends DObject(path) {
  assert(!Files.isDirectory(path))
  override def pseudochecksum:Long = 
      nameSum(name) + size + modifiedTime.toMillis
}

class Dir(override val path:Path, children:Tuple2[Iterator[Dir], Iterator[File]]) extends DObject(path) {
  assert(Files.isDirectory(path) && path.isAbsolute)
  val (dirs, files) = children
  override def pseudochecksum:Long = nameSum(name)
}

object Dir extends SPath {
	/* 
	 * calls code with each file or directory recursively 
	 */
	def process(path:Path)(code:DObject => Unit)(implicit ignore:Ignore):Unit = {
		path2Iterator(path).withFilter(!ignore(_)).foreach(p => { 
			code(new DObject(p));
			if (Files.isDirectory(p)) process(p)(code)(ignore)
		})
	}

	/* 
	 * returns Dir object which contains subdirectory / file informations
	 */
	def apply(path:Path)(implicit ignore:Ignore):Dir = {
		assert(Files.isDirectory(path))
		def helper(p:Path):(Iterator[Dir], Iterator[File]) = {
			val ds = ListBuffer[Dir]()
			val fs = ListBuffer[File]()
			path2Iterator(p).withFilter(!ignore(_)).foreach(o =>
				if (Files.isDirectory(o)) {
					ds += new Dir(o, helper(o))
				} else {
					fs += new File(o)
				})
			(ds.toIterator, fs.toIterator)
		}
		new Dir(path, helper(path))
	}
}
