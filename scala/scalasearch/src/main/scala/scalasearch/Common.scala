package scalasearch

object Common {
  def log(message: String): Unit = {
    println(message)
  }

  def logError(message: String): Unit = {
    log("ERROR: " + message)
  }
}
