import org.gradle.api.Plugin
import org.gradle.api.Project
import java.io.File
import java.net.URL
import java.security.MessageDigest
import java.time.Instant

class DependencyUpdatePlugin : Plugin<Project> {
    override fun apply(project: Project) {
        project.tasks.register("checkForDependencyUpdates") {
            doLast {
                val cacheDir = File(project.layout.buildDirectory.asFile.get(), "dependencyUpdateCache")
                val cacheTtlMinutes = 60L

                project.configurations.forEach { config ->
                    config.dependencies.forEach { dep ->
                        val group = dep.group ?: return@forEach
                        val artifact = dep.name
                        val version = dep.version ?: return@forEach

                        val groupPath = group.replace(".", "/")
                        val metadataUrl = "https://repo1.maven.org/maven2/$groupPath/$artifact/maven-metadata.xml"

                        // Simple hashed filename for safe file naming
                        val cacheKey = "$group:$artifact"
                        val hash = MessageDigest.getInstance("MD5")
                            .digest(cacheKey.toByteArray())
                            .joinToString("") { "%02x".format(it) }

                        val cacheFile = File(cacheDir, "$hash.xml")
                        val now = Instant.now()

                        val xml: String = if (cacheFile.exists() &&
                            now.epochSecond - cacheFile.lastModified() / 1000 < cacheTtlMinutes * 60) {
                            cacheFile.readText()
                        } else {
                            try {
                                val content = URL(metadataUrl).readText()
                                cacheFile.parentFile.mkdirs()
                                cacheFile.writeText(content)
                                content
                            } catch (e: Exception) {
                                println("Failed to fetch metadata for $group:$artifact: ${e.message}")
                                return@forEach
                            }
                        }

                        val latest = Regex("<latest>(.*?)</latest>").find(xml)?.groupValues?.get(1)
                        if (latest != null && latest != version) {
                            println("Update available: $group:$artifact $version -> $latest")
                        }
                    }
                }
            }
        }
    }
}
