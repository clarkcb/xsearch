package ktsearch

/**
 * @author cary on 7/23/16.
 */
fun log(message: String) {
    println(message)
}

fun logError(message: String) {
    println("ERROR: $message")
}

class SearchException(err: String) : Exception(err)
