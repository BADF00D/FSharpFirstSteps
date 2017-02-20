open System;
open System.IO;
open System.Collections.Generic;
open System.Linq;


let getAllDirectories (path :string) = 
    let rec getSubPath (parts : IEnumerable<string>, basePath : string) =
        if parts.Any()
            then 
                let newBasePath = basePath+"/"+parts.First()
                let remaining = parts.Skip(1)
                let paths = getSubPath (remaining, newBasePath)
                seq [basePath] |> Seq.append paths
            else
                seq [basePath]
        
    let partsOfPath = path.Split ([| '/' |], StringSplitOptions.RemoveEmptyEntries)
    getSubPath (partsOfPath.Skip(1), "/"+partsOfPath.First())


//let relativePath = @"C:\Gits\FSharpFirstSteps\FileFixIt\tests\sample.txt"
let relativePath = @"X:\FSharpFirstSteps\FileFixIt\tests\sample.txt"



let lines = 
    File.ReadAllLines relativePath

let numberOfTests = lines.[0]

type TestSource  = { ExistingDirectories : string[] ; PotentialNewDirectories : string[]}

let rec readTestCasesRec (lines : IEnumerable<string>) : IEnumerable<TestSource> =
    let header = lines.First().Split(' ');
    let numberOfExistingDirectories = Int32.Parse(header.[0])
    let numberOfNewDirectories = Int32.Parse(header.[1])
    let exisiting = lines.Skip(1).Take(numberOfExistingDirectories).ToArray()
    let newDirectories = lines.Skip(1+numberOfExistingDirectories).Take(numberOfNewDirectories).ToArray()
    let testCase =  {ExistingDirectories = exisiting; PotentialNewDirectories = newDirectories;}
    
    let remainingLines = lines.Skip(1 + numberOfExistingDirectories + numberOfNewDirectories)
    if remainingLines.Any()
        then 
            let testCases = readTestCasesRec remainingLines
            testCases |> Seq.append (seq [testCase])
        else seq [testCase]


let readTestCases (lines : string[]) =
    (readTestCasesRec (lines.Skip(1))).ToArray()


let solve (test : TestSource) =
    let lookup = List<string>()
    let addInitialDirectories (existing:string[]) = 
        existing
            |> Seq.map getAllDirectories
            |> Seq.collect (fun x -> seq { for i in x do yield i})
            |> Seq.map (fun dir -> 
                if not (lookup.Contains(dir))
                        then 
                            do lookup.Add(dir)
                            1
                        else
                            0)
            |> Seq.sum
    let added = addInitialDirectories (test.ExistingDirectories)
    test.PotentialNewDirectories
        |> Seq.map getAllDirectories
        |> Seq.collect (fun x -> seq { for i in x do yield i})
        |> Seq.map (fun dir -> 
            if not (lookup.Contains(dir))
                    then 
                        do lookup.Add(dir)
                        1
                    else
                        0)
        |> Seq.sum
    
let solveAll (tests : IEnumerable<TestSource>) =
    tests
    |> Seq.map solve
    |> Seq.map (fun x -> printf "Case #i: %i \r\n" x)
    |> Seq.iter (fun x -> printf "")

let allTestCases = readTestCases lines

solveAll allTestCases

Console.ReadLine