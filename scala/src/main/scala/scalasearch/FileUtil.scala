package scalasearch

import java.io.File

object FileUtil {
  def getExtension(f: File): String = {
    getExtension(f.getName)
  }

  def getExtension(f: SearchFile): String = {
    getExtension(f.fileName)
  }

  def getExtension(name: String): String = {
    val lastIndex = name.lastIndexOf('.')
    if (lastIndex > 0 && lastIndex < name.length-1)
      name.split('.').last
    else
      ""
  }

  def isHiddenFile(f: File): Boolean = {
    isHiddenFile(f.getName)
  }

  def isHiddenFile(f: SearchFile): Boolean = {
    isHiddenFile(f.fileName)
  }

  def isHiddenFile(name: String): Boolean = {
    name.startsWith(".") && !Set(".", "..").contains(name)
  }
}
