namespace cfgrecon

  [<AutoOpen>]
  module Utility =
    let convert_generic<'T> (x:uint64) =
      match box Unchecked.defaultof<'T> with
        | :? uint32 -> uint32 x |> box
        | :? uint64 -> uint64 x |> box
        | _ -> failwith "unknown type of address"
      :?> 'T

    let cast_generic<'T> (x:uint64) : 'T =
      match box Unchecked.defaultof<'T> with
        | :? uint32 -> uint32 x |> unbox<'T>
        | :? uint64 -> uint64 x |> unbox<'T>
        | _ -> failwith "unknown type of address"

    let convert_static (x:uint64) : int = FSharpPlus.Operators.explicit x
