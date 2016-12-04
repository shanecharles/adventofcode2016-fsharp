// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"

let testDir = "./test_build/"

// Filesets
let appReferences  =
    !! "/**/app/*.csproj"
    ++ "/**/app/*.fsproj"

let testReferences = 
    !! "/**/tests/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir; testDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "BuildTests" (fun _ ->    
    MSBuildDebug testDir "Build" testReferences 
    |> Log "TestBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "Test" (fun _ ->
  !! (testDir + "*.tests.dll")
    |> NUnit3 (fun p -> 
          {p with 
              ToolPath = "packages/NUnit.ConsoleRunner/tools/nunit3-console.exe"
              })
)

// Build order
"Clean"
  ==> "Build"
  ==> "BuildTests"
  ==> "Test"
  ==> "Deploy"

// start build
RunTargetOrDefault "Test"
