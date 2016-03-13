import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val directory =
      if (args.length == 1)
        new File(args.head)
      else
        new File(System.getProperty("user.home"), "SwissCommercialRegister")

    if (!directory.exists()) {
      directory.mkdirs()
    }

    val querier = Querier(directory)

    querier.run()
  }
}
