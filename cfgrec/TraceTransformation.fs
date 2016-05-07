namespace cfgrecon
  module TraceTransformation =
    let inline print_trace (trace:Machine.Instruction<'T> list) =
      List.map (fun ins -> Printf.printfn "%s" <| Machine.instruction_to_string ins) trace
