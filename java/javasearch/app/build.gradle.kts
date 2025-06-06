/*
 * This file was generated by the Gradle 'init' task.
 */

plugins {
    id("buildlogic.java-application-conventions")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}

repositories {
    mavenLocal()
    maven {
        url = uri("https://repo.maven.apache.org/maven2/")
    }
}

dependencies {
    implementation("org.apache.commons:commons-text")
    implementation("xfind:javafind:0.1.0-SNAPSHOT")
    implementation(project(":lib"))
}

application {
    // Define the main class for the application.
    mainClass = "javasearch.JavaSearch"
}
