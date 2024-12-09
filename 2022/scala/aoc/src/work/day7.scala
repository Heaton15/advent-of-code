package work

object day7 extends App {
  import scala.io.Source
  import scala.collection.*

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

  // For a given directory, let's add all of the files and other directories to
  // the tree. Then if we need a specific directory we can just find it by name.

  class DirectoryStructure(
      val name: String,
      val subDirectories: mutable.Map[String, DirectoryStructure],
      val files: mutable.Map[String, Int],
      val parent: DirectoryStructure | Null
  )

  val fileName = "inputs/elf_storagedata.txt"
  val storageData = Source.fromFile(fileName).getLines

  // Convert the terminal output to something with enumerated types
  val terminalOutput = storageData.flatMap { str => enumerateList(str) }.toList

  // If we change to a directory that does not exist, make it
  // If we list files, we are adding to out current structure

  def initDs(name: String, parent: DirectoryStructure | Null) =
    new DirectoryStructure(name, mutable.Map.empty, mutable.Map.empty, parent)

  val root = initDs("/", null)

  def processCommand(ds: DirectoryStructure, c: Command): DirectoryStructure = {
    c match {
      case ChangeDirectory(dir) =>
        dir match {
          case r: "/"  => root
          case u: ".." => ds.parent
          case dir @ _ => ds.subDirectories(dir)
        }
      case ListFiles => ds
    }
  }

  def processInputs(ds: DirectoryStructure, cmdList: List[TerminalOutput]): Unit = {
    val newDs = cmdList.head match {
      case Cmd(cmd) => processCommand(ds, cmd)
      case Directory(name) => {
        ds.subDirectories += (name -> initDs(name, ds))
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

  // Find all child nodes with no subdirectories
  def lowestNodes(d: DirectoryStructure, lst: List[DirectoryStructure]): List[DirectoryStructure] = {
    if (d.subDirectories.isEmpty) lst :+ d
    else d.subDirectories.foldLeft(lst)((x, y) => lowestNodes(y._2, x))
  }

  val maxSize = 100000

  // Calculates the file size of the current DirectoryStructure and all children
  def fileSize(d: DirectoryStructure): Int = d.files.foldLeft(0)((a, b) => a + b._2)

  // Calculates the file size of the current directory and all children
  def dirFileSize(d: DirectoryStructure, currSize: Int = 0): Int = {
    if (d.subDirectories.isEmpty) fileSize(d) + currSize
    else fileSize(d) + d.subDirectories.foldLeft(currSize)((acc, child) => dirFileSize(child._2, acc))
  }

  def sumOfMaxSizes(d: DirectoryStructure, acc: Int): Int = {
    val size = dirFileSize(d)
    val newSize = if (size < maxSize) acc + size else acc

    if (d.subDirectories.isEmpty) newSize
    else d.subDirectories.foldLeft(newSize)((a, child) => sumOfMaxSizes(child._2, a))
  }

  val bottomOfTree = lowestNodes(root, List.empty[DirectoryStructure])

  val part1_solution = sumOfMaxSizes(root, 0)
  println(s"part1 solution: $part1_solution")

  val totalDiskSpace = 70000000
  val neededUnusedSpace = 30000000
  val totalUsedSpace = dirFileSize(root)

  val neededSpace = neededUnusedSpace - (totalDiskSpace - totalUsedSpace)

  def findSmallestFreeSpaceDirOptions(d: DirectoryStructure, lst: List[DirectoryStructure]): List[DirectoryStructure] = {
    val size = dirFileSize(d)
    val newList = if (size > neededSpace) lst :+ d else lst

    if (d.subDirectories.isEmpty) newList
    else d.subDirectories.foldLeft(newList)((a, child) => a ++ findSmallestFreeSpaceDirOptions(child._2, List.empty[DirectoryStructure]))

  }

  val possible_options = findSmallestFreeSpaceDirOptions(root, List.empty[DirectoryStructure])
  val part2_solution = possible_options.reduce((a,b) => if (dirFileSize(a) < dirFileSize(b)) a else b )
  println(s"part2 solution: ${dirFileSize(part2_solution)}")

}
