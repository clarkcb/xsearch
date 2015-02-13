package scalasearch

class SearchException(val message: String) extends Exception {
  override def getMessage : String = message
}
