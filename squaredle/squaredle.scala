val board = Vector("exio", "xmhn", "zaep", "valu")

class Cell(val letter: Char, var neighbors: Seq[Cell] = Nil) {
  override def toString(): String = letter.toString()
}

val localDict = scala.io.Source.fromFile("dictionary.txt")
val dictURLs = List("https://www.andrew.cmu.edu/course/15-121/dictionary.txt",
                    "https://raw.githubusercontent.com/redbo/scrabble/master/dictionary.txt")
val dictSources = localDict :: dictURLs.map(scala.io.Source.fromURL(_))
val dictLines = dictSources.flatMap(_.getLines())
                           .map(_.toLowerCase)
val dict = scala.collection.immutable.TreeSet.from(dictLines)

def isWordPrefix(path: Seq[Cell]): Boolean = {
  val prefix = path.mkString
  dict.rangeFrom(prefix)
      .headOption
      .exists(_.startsWith(prefix))
}

val cells = board.map(_.map(new Cell(_)))

val seeds = for {
  (row, i) <- cells.zipWithIndex
  (cell, j) <- row.zipWithIndex if cell.letter != ' '
} yield {
  cell.neighbors = cells.slice(i - 1, i + 2)
                        .flatMap(_.slice(j - 1, j + 2))
                        .filter(_ != cell)
                        .filter(_.letter != ' ')
  List(cell)
}

val paths = LazyList.iterate(seeds)(_.flatMap(path => {
  path.last
      .neighbors
      .filterNot(path.contains)
      .map(path.appended)
      .filter(isWordPrefix)
}))

val words = paths.takeWhile(!_.isEmpty)
                 .flatten
                 .map(_.mkString)
                 .filter(dict.contains)
                 .distinct
                 .sortBy(s => (s.length, s))

words.filter(_.length >= 4)
     .foreach(println)
