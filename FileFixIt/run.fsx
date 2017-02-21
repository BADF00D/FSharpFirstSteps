open System;
open System.IO;
open System.Collections.Generic;
open System.Linq;

let split (ch : char) (s : string) = s.Split(ch)

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


//let pathToTests = @"C:\Gits\FSharpFirstSteps\FileFixIt\tests\"
let pathToTests = @"C:\Git\FSharpFirstSteps\FileFixIt\tests"
let currentFile = @"A-large-practice.in"
//let pathToTests = @"X:\FSharpFirstSteps\FileFixIt\tests\sample.txt"



let lines = 
    let fullPath = Path.Combine (pathToTests, currentFile)
    File.ReadAllLines fullPath

let numberOfTests = lines.[0]

type TestSource  = { ExistingDirectories : string list ; PotentialNewDirectories : string list}

let rec readTestCasesRec (acc : TestSource list) (lines : string list) : TestSource list =
    match lines with
    | [] -> acc
    | _ ->
        let [numExisting; numNew] = 
            lines 
            |> Seq.head 
            |> split ' ' 
            |> List.ofArray
            |> List.map int
        
        let (exisitingDirs, newDirs, rest) = 
            lines
            |> List.skip 1
            |> List.splitAt numExisting
            |> fun (ex, re) -> (ex, re |> List.splitAt numNew)
            |> fun (ex, (ne, re)) -> (ex, ne, re)

        let newDirectories = lines.Skip(1+numExisting).Take(numNew).ToArray()
        let testCase =  { ExistingDirectories = exisitingDirs; PotentialNewDirectories = newDirs; }
        
        readTestCasesRec (testCase :: acc) rest

let readTestCases (lines : string[]) =
    lines 
    |> List.ofSeq
    |> List.skip 1
    |> readTestCasesRec []
    |> List.rev
    //(readTestCasesRec (lines.Skip(1))).ToArray()


let solve (test : TestSource) =
    let lookup = List<string>()
    let addInitialDirectories (existing:string list) = 
        existing
            |> Seq.map getAllDirectories
            |> Seq.collect (fun x -> seq { for i in x do yield i})
            |> Seq.map (fun dir -> 
                if not (lookup.Contains dir)
                        then 
                            do lookup.Add dir
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
    
let solveAll (tests : TestSource seq) =
    let fullPath = Path.Combine(pathToTests, currentFile+".result.txt")
    use stream = new FileStream(fullPath, FileMode.Create)
    use writer = new StreamWriter(stream)
    tests
    |> Seq.map solve
    |> Seq.iteri (fun i x -> 
        //let result = "Case #"+(i+1).ToString()+": "+x.ToString()
        // writer.WriteLine result
        sprintf "Case #%i: %i" (i + 1) x
        |> writer.WriteLine
        //ignore writer.Flush 
    )
    writer.Flush |> ignore
    

let allTestCases = readTestCases lines

solveAll allTestCases

Console.ReadLine ()