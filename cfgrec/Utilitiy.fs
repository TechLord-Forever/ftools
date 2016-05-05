namespace cfgrecon

  // [<AutoOpen>]
  module Utility =
    let convert_generic<'T> (x:uint64) =
      match box Unchecked.defaultof<'T> with
        | :? int32 -> int32 x |> box
        | :? uint32 -> uint32 x |> box
        | :? int64 -> int64 x |> box
        | :? uint64 -> uint64 x |> box
        | _ -> failwith "unknown type of address"
      :?> 'T

    let cast_generic<'T> (x:uint64) : 'T =
      match box Unchecked.defaultof<'T> with
        | :? int32 -> int32 x |> unbox<'T>
        | :? uint32 -> uint32 x |> unbox<'T>
        | :? int64 -> int64 x |> unbox<'T>
        | :? uint64 -> uint64 x |> unbox<'T>
        | _ -> failwith "unknown type of address"

    let inline convert_static (x:uint64) : 'T = FSharpPlus.Operators.explicit x
