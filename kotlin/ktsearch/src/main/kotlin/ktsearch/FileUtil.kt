package ktsearch

/**
 * @author cary on 7/24/16.
 */
object FileUtil {
    private val dotDirs: Set<String> = setOf(".", "..", "./", "../")
    //private val DEFAULT_ENCODING = "UTF-8"

    fun isDotDir(f: String): Boolean {
        return dotDirs.contains(f)
    }

    fun isHidden(f: String): Boolean {
        return f.isNotEmpty() && f[0] == '.' && !isDotDir(f)
    }
}
