open System;
open System.IO;
open System.Collections.Generic;
open System.Linq;

let file = @"X:\F#\FileFixIt\tests\sample.txt"
let lines = File.ReadAllLines file

let numberOfTests = lines.[0]

type TestSource  = { ExistingDirectories : string[] ; PotentialNewDirectories : string[]}


let readTestCase (lines : IEnumerable<string>) =
    let header = lines.First().Split(' ');
    let numberOfExistingDirectories = Int32.Parse(header.[0])
    let numberOfNewDirectories = Int32.Parse(header.[1])
    let exisiting = lines.Skip(1).Take(numberOfExistingDirectories).ToArray()
    let newDirectories = lines.Skip(1+numberOfExistingDirectories).Take(numberOfNewDirectories).ToArray()
    (
        1+numberOfExistingDirectories+numberOfNewDirectories,
        {
            ExistingDirectories = exisiting;
            PotentialNewDirectories = newDirectories;
        }
    )


let readTestCases (lines : string[]) =
    let numberOfTestCases = Int32.Parse(lines.First())
    let result = new List<TestSource>();
    let remaining = lines
    for i=0 to numberOfTestCases do
        let intermediate = readTestCase(lines.Skip(1));
        result.Add(snd intermediate)
        remaining <| lines.Skip(fst intermediate)
        ignore

    " "

        

