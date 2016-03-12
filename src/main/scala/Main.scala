import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val directory =
      if (args.length == 1)
        new File(args.head)
      else
        new File(System.getProperty("user.home"), "SwissCommercialRegister")

    val downloader = Downloader(directory)

    downloader.run()
  }
}
