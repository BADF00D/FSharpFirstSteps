open System;
open System.IO;
open System.Collections.Generic;
open System.Linq;

let relativePath = @"C:\Gits\FSharpFirstSteps\FileFixIt\tests\sample.txt"
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
            seq [testCase] |> Seq.append testCases
        else seq [testCase]


let readTestCases (lines : string[]) =
    (readTestCasesRec (lines.Skip(1))).ToArray()

let solve (test : TestSource) =
    let lookup = List<string>()
    for path in test.ExistingDirectories do
        let parts = path.Split('/')
        for part in parts do
            if (lookup.Contains(part))
                then do lookup.Add(part)     
    let mutable count = 0
    for path in test.PotentialNewDirectories do
        let parts = path.Split('/')
        for part in parts do
            if(lookup.Contains(part))
                then 
                    count = count+1;
                    do lookup.Add(part)

    
    count

let solveAll (tests : IEnumerable<TestSource>) =
    tests
    |> Seq.map solve
    |> Seq.iter (fun x -> printf "%i" x)

let allTestCases = readTestCases lines

solveAll allTestCases