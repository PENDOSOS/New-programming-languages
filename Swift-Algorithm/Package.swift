// swift-tools-version:5.5
import PackageDescription

let package = Package(
    name: "swift_algorithm",
    products: [
        .executable(name: "swift_algorithm", targets: ["swift_algorithm"]),
    ],
    targets: [
        .target(
            name: "swift_algorithm",
            dependencies: [
                // Add your target's dependencies here
            ]
        ),
    ]
)