namespace cfgrecon
  module TraceManipulation =
    let print_trace (trace:Machine.Instruction<'T> list) =
      List.map (fun ins -> Printf.printfn "%s" <| Machine.instruction_to_string ins) trace

