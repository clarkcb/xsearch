package ktsearch

import java.io.File

/**
 * @author cary on 7/24/16.
 */
class SearchFile(val containers: List<String>, val file: File, val fileType: FileType) {
    val CONTAINER_SEPARATOR = "!"

    constructor(file: File, fileType: FileType) : this(listOf(), file, fileType)

    override fun toString(): String {
        val sb = StringBuilder()
        if (containers.isNotEmpty()) {
            for (i in containers.indices) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR)
                }
                sb.append(containers[i])
            }
            sb.append(CONTAINER_SEPARATOR)
        }
        sb.append(file.path)
        return sb.toString()
    }
}
