namespace cfgrecon

  module DataPresentation =
    type PairT<'T> = 'T * 'T

    let print_generic<'T> cat =
      if (cat = 0) then
        let pair_int : PairT<int> = (3, 3)
        Printf.printfn "%d %d" (fst pair_int) (snd pair_int)
      else
        let pair_string : PairT<string> = ("3", "3")
        Printf.printfn "%s %s" (fst pair_string) (snd pair_string)
