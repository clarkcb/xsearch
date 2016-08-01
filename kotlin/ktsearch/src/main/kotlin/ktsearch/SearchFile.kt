package ktsearch

import java.io.File

/**
 * @author cary on 7/24/16.
 */
class SearchFile(containers: List<String>, file: File, fileType: FileType) {
    val CONTAINER_SEPARATOR = "!"

    val containers: List<String> = containers
    val file: File = file
    val fileType: FileType = fileType

    constructor(file: File, fileType: FileType) : this(listOf(), file, fileType) {

    }

    override fun toString(): String {
        val sb = StringBuilder()
        if (containers.size > 0) {
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
