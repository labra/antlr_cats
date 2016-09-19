package es.weso.shex.main
import es.weso.shex.compact._

object MainApp {
  def main(args: Array[String]): Unit = {
        var inputText: String = """
        |prefix : <http://example.org/>
        |prefix a: <http://pepe.s/>
        |base <http://example.org/>
        |base <http://kiko.org/>
        |
        |start = :S
        |a:S {
        | :p [ 1 2 ]
        |}
        |""".stripMargin

        if (args.length > 0) {
            inputText = args(0)
        }

       val schema = Parser.parseSchema(inputText)
       println(s"Schema parsed: $schema")
    }

}
