// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
[<EntryPoint>]
let main argv =
  if Array.length argv < 1 then
    Printf.printf "give some trace file"
  else
    try
      use trace_reader = new System.IO.BinaryReader(System.IO.File.OpenRead(argv.[1]))
      let machine_arch = ProtobufExtraction.extract_machine_architecture trace_reader
      match machine_arch with
        | Some arch ->
          match arch with
            | Machine.X86 
    with
      | :?
  // printfn "%A" argv
    0 // return an integer exit code

