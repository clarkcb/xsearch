package scalasearch

import java.io._
import scala.io._

object SearchMain {

    val DEBUG = true

    def usage(status: Int) = {
        val usage = """
Usage:
search [-x ext[,ext]] [-f '<regex>'] -s '<searchstring>' <startpath>
        """
        println(usage)
        sys.exit(status)
    }

    def main(args: Array[String]) = {
        if (args.length == 0) usage(1)
        val arglist = args.toList

        if (DEBUG) {
            println("args:")
            args.foreach(println)
        }

        //val searchSettings = getSearchSettings(options)
        //println("searchSettings: " + searchSettings)
        //search(searchSettings)
    }
}
