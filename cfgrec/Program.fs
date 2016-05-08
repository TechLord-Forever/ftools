﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open cfgrecon

[<EntryPoint>]
let main argv =
  if Array.length argv < 1 then
    Printf.printf "give some trace file"
    0
  else
    try
      use trace_reader = new System.IO.BinaryReader(System.IO.File.OpenRead(argv.[1]))
      let machine_arch = ProtobufExtraction.extract_machine_architecture trace_reader
      match machine_arch with
        | Some arch ->
          match arch with
            | Machine.Architecture.X86 ->
              let trace = ProtobufExtraction.extract_instructions<uint32> trace_reader
              List.iter (fun ins -> Printf.printfn "%s" (Machine.instruction_to_string ins)) trace
              1
            | Machine.Architecture.X86_64 ->
              let trace = ProtobufExtraction.extract_instructions<uint64> trace_reader
              List.iter (fun ins -> Printf.printfn "%s" (Machine.instruction_to_string ins)) trace
              1
        | None -> failwith "cannot read machine architecture"
    with
      | :? System.IO.FileNotFoundException -> Printf.printfn "cannot found file %s" argv.[1]; 0
      | _ -> Printf.printfn "unknown error"; 0

