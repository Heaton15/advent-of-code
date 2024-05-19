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

  val terminalOutput = storageData.flatMap { str => enumerateList(str) }.toList

// We want the name of the current directory, the subDirectories in the level,
// files in our current level, and a parent if one exists. We want our top
// level class to be able to contain all sub classes.

  class DirectoryStructure(
      name: String,
      subDirectories: mutable.Map[String, DirectoryStructure],
      files: mutable.Map[String, Int],
      parent: DirectoryStructure | Null
  )
}
