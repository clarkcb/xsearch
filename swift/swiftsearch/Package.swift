// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "swiftsearch",
    // products already default to swiftsearch.framework, swiftsearchApp and swfitsearchTests.xctest
    products: [
        .library(name: "swiftsearch", targets: ["swiftsearch"]),
        .executable(name: "swiftsearchApp", targets: ["swiftsearchApp"]),
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .target(
            name: "swiftsearch",
            dependencies: [],
            resources: [
                .process("Resources/filetypes.json"),
                .process("Resources/filetypes.xml"),
                .process("Resources/searchoptions.json"),
                .process("Resources/searchoptions.xml")

            ]),
        .target(
            name: "swiftsearchApp",
            dependencies: ["swiftsearch"]),
        .testTarget(
            name: "swiftsearchTests",
            dependencies: ["swiftsearch"]),
    ]
)
