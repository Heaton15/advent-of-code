package work

object day7 extends App {
  import scala.io.Source
  import scala.collection.mutable.ArrayStack
  import scala.collection.mutable.Stack
  import scala.collection.mutable.Queue
  import scala.collection.*

  val fileName = "inputs/elf_storagedata.txt"
  val storageData = Source.fromFile(fileName).getLines

// When looking at the outputs, we see 2 kinds of issued commands
  enum Command {
    case ChangeDirectory(dir: String)
    case ListFiles
  }

// When we parse through, there are 3 kinds of outputs we can see in the list
  enum TerminalOutput {
    case Cmd(cmd: Command)
    case Directory(name: String)
    case File(size: Int, name: String)
  }

// Import the enums to get their data values
  import Command.*
  import TerminalOutput.*

// Let's create a enumeration list of what is happening in the text file
  def enumerateList(line: String) = line.linesIterator.map {
    case s"$$ cd $directory" =>
      Cmd(ChangeDirectory(directory)) // Example of variable pattern matching
    case s"$$ ls"          => Cmd(ListFiles)
    case s"dir $directory" => Directory(directory)
    case s"$size $file"    => File(size.toInt, file)
  }.toList

  // Convert the terminal output to something with enumerated types
  val terminalOutput = storageData.flatMap { str => enumerateList(str) }.toList

  // For a given directory, let's add all of the files and other directories to
  // the tree. Then if we need a specific directory we can just find it by name.

  class DirectoryStructure(
      val name: String,
      val subDirectories: mutable.Map[String, DirectoryStructure],
      val files: mutable.Map[String, Int],
      val parent: DirectoryStructure | Null
  )

  // If we change to a directory that does not exist, make it
  // If we list files, we are adding to out current structure

  def initDirectoryStructure(name: String, parent: DirectoryStructure | Null) =
    new DirectoryStructure(name, mutable.Map.empty, mutable.Map.empty, parent)

  val root = initDirectoryStructure("/", null)

  def changeDir(ds: DirectoryStructure, dir: String): DirectoryStructure = {
    dir match {
      case r: "/"  => root
      case u: ".." => ds.parent
      case dir @ _ => ds.subDirectories(dir)
    }
  }

  def processCommand(ds: DirectoryStructure, c: Command): DirectoryStructure = {
    c match {
      case ChangeDirectory(dir) => changeDir(ds, dir)
      case ListFiles            => ds
    }
  }

  def processInputs(
      ds: DirectoryStructure,
      cmdList: List[TerminalOutput]
  ): Unit = {
    val newDs = cmdList.head match {
      case Cmd(cmd) => processCommand(ds, cmd)
      case Directory(name) => {
        ds.subDirectories += (name -> initDirectoryStructure(name, ds))
        ds
      }
      case File(size, name) => {
        ds.files += (name -> size)
        ds
      }
    }
    if (!cmdList.tail.isEmpty) processInputs(newDs, cmdList.tail)
  }

  // root now contains everything 
  processInputs(root, terminalOutput)

  val maxSize = 100000

  def solvePuzzle(): Unit = {}




}
