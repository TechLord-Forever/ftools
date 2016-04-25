namespace cfgrecon

  [<AutoOpen>]
  module Machine =
    type Architecture =
      | X86
      | X86_64

    type MachineInfo = { arch: Architecture;
                         address_size: uint32 }

    type Instruction (ins_addr, ins_disas, ins_tid, ins_opc,
                      ins_read_regs, ins_write_regs, ins_read_addrs, ins_write_addrs) =
      member v.address         = ins_addr
      member v.disassemble     = ins_disas
      member v.thread_id       = ins_tid
      member v.opcode          = ins_opc
      member v.read_registers  = ins_read_regs
      member v.write_registers = ins_write_regs
      member v.read_addresses  = ins_read_addrs
      member v.write_address   = ins_write_addrs

   type TraceInfo (parsed_machine_info, parsed_instructions) =
     member v.machine_info = parsed_machine_info
     member v.instructions = parsed_instructions

  module DataExtraction =
    // first read a uint32 value since it specifies the length of json serialized data, next read the data
    let private extract_packed_data (reader:System.IO.BinaryReader) =
      reader.ReadUInt32() |> int |> reader.ReadBytes

    // an example of json form for the machine's information:
    // {
    //   "header": {
    //     "architecture": 0,
    //     "address size": 32
    //    }
    //  }
    let private get_machine_information (header:byte[]) =
      try
        let json_header                            = Chiron.Parsing.Json.parse (string header)
        let header_map : Map<string, obj>          = unbox json_header
        let header_info_map : Map<string, decimal> = Map.find "header" header_map |> unbox
        let parsed_arch                            = Map.find "architecture" header_info_map |> unbox<decimal> |> System.Decimal.ToUInt32
        let parsed_addrlen                         = Map.find "address size" header_map |> unbox<decimal> |> System.Decimal.ToUInt32
        match (int parsed_arch) with
          | 0 -> Some { arch = X86; address_size = parsed_addrlen }
          | 1 -> Some { arch = X86_64; address_size = parsed_addrlen }
          | _ -> None
      with
        | _ -> None

    let private extract_json_register_object reg_obj =
      let single_reg_map:Map<string, decimal> = unbox reg_obj
      let single_reg_array = Map.toArray single_reg_map
      (fst single_reg_array.[0], snd single_reg_array.[0] |> System.Decimal.ToUInt64)

    let private extract_json_address_object addr_obj =
      let single_addr_map:Map<string, decimal> = unbox addr_obj
      let single_addr_array = Map.toArray single_addr_map
      (fst single_addr_array.[0] |> System.UInt64.Parse,
       snd single_addr_array.[0] |>  System.Decimal.ToUInt64 |> uint8)

    let private parse_json_chunk_element chunk_elem =
      let elem_map:Map<string, obj> = unbox chunk_elem
      let ins_addr                  = Map.find "address" elem_map |> unbox<decimal> |>  System.Decimal.ToUInt64
      let ins_disas                 = Map.find "disassemble" elem_map |> unbox<string>
      let ins_tid                   = Map.find "thread_id" elem_map |> unbox<decimal> |> System.Decimal.ToUInt32
      let ins_opc                   = Map.find "opcode" elem_map |> unbox<string> |> System.Convert.FromBase64String
      let ins_read_regs             = Map.find "read registers" elem_map |> unbox |> List.map extract_json_register_object |> Map.ofList
      let ins_write_regs            = Map.find "write registers" elem_map |> unbox |> List.map extract_json_register_object |> Map.ofList
      let ins_read_addrs            = Map.find "read addresses" elem_map |> unbox |> List.map extract_json_address_object |> Map.ofList
      let ins_write_addrs           = Map.find "write address" elem_map |> unbox |> List.map extract_json_address_object |> Map.ofList
      (ins_addr, ins_disas, ins_tid, ins_opc, ins_read_regs, ins_write_regs, ins_read_addrs, ins_write_addrs)

    let private normalize_parsed_instruction base_ins =
      let (ins_addr, ins_disas, ins_tid, ins_opc, ins_read_regs, ins_write_regs, ins_read_addrs, ins_write_addrs) = base_ins
      Instruction(Utility.cast_generic<'T> ins_addr, ins_disas, ins_tid, ins_opc,
                  Map.map (fun _ reg_value -> Utility.cast_generic<'T> reg_value) ins_read_regs,
                  Map.map (fun _ reg_value -> Utility.cast_generic<'T> reg_value) ins_write_regs,
                  Map.toList ins_read_addrs |> List.map (fun (address, value) -> (Utility.cast_generic<'T> address, value)) |> Map.ofList,
                  Map.toList ins_write_addrs |>  List.map (fun (address, value) -> (Utility.cast_generic<'T> address, value)) |> Map.ofList)

    let private get_instructions_from_chunk (chunk:byte[]) =
      try
        let json_chunk = Chiron.Parsing.Json.parse(string chunk)
        let chunk_map : Map<string, obj> = unbox json_chunk
        let chunk_info_list = Map.find "chunk" chunk_map |> unbox |> Map.toList
        List.map (parse_json_chunk_element >> normalize_parsed_instruction) chunk_info_list |> Some
      with
        | _ -> None

    let parse_trace_file filename =
      use trace_reader = new System.IO.BinaryReader(System.IO.File.OpenRead(filename))
      let parsed_machine_info = extract_packed_data trace_reader |> get_machine_information
      match parsed_machine_info with
        | None -> None
        | Some machine_info ->
          let trace_instructions = ref Seq.empty
          let should_continue_parsing = ref true
          while !should_continue_parsing do
            let new_chunk_ins  = extract_packed_data trace_reader |> get_instructions_from_chunk
            match new_chunk_ins with
              | None -> should_continue_parsing := false
              | Some chunk_ins -> trace_instructions := Seq.ofList chunk_ins |> Seq.append !trace_instructions
          TraceInfo(machine_info, trace_instructions) |> Some


