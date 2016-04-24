namespace cfgrecon

  [<AutoOpen>]
  module Machine =
    type Architecture =
      | X86
      | X86_64

    type MachineInfo = { arch: Architecture;
                         address_size: uint32 }

    type MemoryMap<'T when 'T : comparison> = Map<'T, uint8>
    type RegisterMap<'T> = Map<string, 'T>

    type Instruction<'T when 'T : comparison> =
      { address           : 'T;
        disassemble       : string;
        thread_id         : uint32;
        opcode            : byte[];
        read_registers    : RegisterMap<'T>;
        written_registers : RegisterMap<'T>;
        read_addresses    : MemoryMap<'T>;
        written_addresses : MemoryMap<'T> }

    type TraceInfo<'T when 'T : comparison> = { machine: MachineInfo;
                                                trace: seq<Instruction<'T>> }

    type BaseInstruction (ins_addr : uint64,
                          ins_disas : string,
                          ins_tid : uint32,
                          ins_opc : byte[],
                          ins_read_regs : RegisterMap<uint64>,
                          ins_write_regs : RegisterMap<uint64>,
                          ins_read_addrs : MemoryMap<uint64>,
                          ins_write_addrs : MemoryMap<uint64>) =
      member v.address = ins_addr
      member v.disassemble = ins_disas


  module DataExtraction =
    let make_instruction (ins_addr, ins_disas, ins_thread_id, ins_opcode,
                          read_regs, written_regs, read_addrs, written_addrs) =
      { address           = ins_addr;
        disassemble       = ins_disas;
        thread_id         = ins_thread_id;
        opcode            = ins_opcode;
        read_registers    = read_regs;
        written_registers = written_regs;
        read_addresses    = read_addrs;
        written_addresses = written_addrs }

    // first read a uint32 value since it specifies the length of json serialized data, next read the data
    let private extract_packed_data (reader:System.IO.BinaryReader) =
      try
        reader.ReadUInt32() |> int |> reader.ReadBytes
      with
        | :? _ as ex -> failwith (Printf.sprintf "extract_packed_data: %s" ex.Message)

    // an example of json form for the machine's information:
    // {
    //   "header": {
    //     "architecture": 0,
    //     "address size": 32
    //    }
    //  }
    let private parse_machine_information (header:byte[]) =
      try
        let json_header                          = Chiron.Parsing.Json.parse (string header)
        let header_map:Map<string, obj>          = unbox json_header
        let header_info_map:Map<string, decimal> = unbox <| Map.find "header" header_map
        let parsed_arch                          = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "architecture" header_info_map)
        let parsed_addrlen                       = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "address size" header_map)
        match (int parsed_arch) with
          | 0 -> { arch = X86; address_size = parsed_addrlen }
          | 1 -> { arch = X86_64; address_size = parsed_addrlen }
          | _ -> failwith "parse_marchine_information: unknown architecture"
      with
        | :? _ as ex -> failwith (Printf.sprintf "parse_marchine_information: %s" ex.Message)


    // an example of the json form for a chunk of instructions:
    // {
    //   "chunk": {
    //     [
    //        {
    //           "address": ...,
    //           "disassemble": ...,
    //           "thread_id": ...,
    //           "opcode":
    //           "read_registers": [
    //             "eax": 39048,
    //             "ebx": ...
    //           ],
    //           "write_registers": [ ... ],
    //           "read_addresses": [
    //             "0x1343": 843451,
    //             ...
    //           ],
    //           "write_addresses": ...
    //        },
    //        ...
    //     ]
    //   }
    // }

    let private extract_json_register_object reg_obj =
      let single_reg_map:Map<string, decimal> = unbox reg_obj
      let single_reg_array = Map.toArray single_reg_map
      (fst single_reg_array.[0], snd single_reg_array.[0] |> System.Decimal.ToUInt64)

    let private extract_json_address_object addr_obj =
      let single_addr_map:Map<string, decimal> = unbox addr_obj
      let single_addr_array = Map.toArray single_addr_map
      (fst single_addr_array.[0] |> System.UInt64.Parse,
       snd single_addr_array.[0] |>  System.Decimal.ToUInt64 |> uint8)

    let private extract_json_chunk_element chunk_elem =
      let elem_map:Map<string, obj> = unbox chunk_elem
      let address = Map.find "address" elem_map |> unbox<decimal> |>  System.Decimal.ToUInt64
      let disassemble = Map.find "disassemble" elem_map |> unbox<string>
      let thread_id = Map.find "thread_id" elem_map |> unbox<decimal> |> System.Decimal.ToUInt32
      let opcode = Map.find "opcode" elem_map |> unbox<string> |> System.Convert.FromBase64String
      let read_registers =
        let reg_obj_list = Map.find "read registers" elem_map |> unbox
        Map.ofList <| List.map extract_json_register_object reg_obj_list
      let write_registers =
        let reg_obj_list = Map.find "write registers" elem_map |> unbox
        Map.ofList <| List.map extract_json_register_object reg_obj_list
      let read_addresses =
        let addr_obj_list = Map.find "read addresses" elem_map |> unbox
        Map.ofList <| List.map extract_json_address_object addr_obj_list
      let write_addresses =
        let addr_obj_list = Map.find "write address" elem_map |> unbox
        Map.ofList <| List.map extract_json_address_object addr_obj_list
      (address, disassemble, thread_id, opcode, read_registers, write_registers, read_addresses, write_addresses)

    let private parseget_base_instructions (chunk:byte[]) =
      let json_chunk = Chiron.Parsing.Json.parse (string chunk)
      let chunk_map : Map<string, obj> = unbox json_chunk
      let chunk_info_list = Map.toList (unbox <| Map.find "chunk" chunk_map)
      List.map extract_json_chunk_element chunk_info_list

    // let private convert<'T> = Utility.cast_generic<'T>
    let private convert = Utility.convert_static

    let private convert_base_instruction<'T when 'T: comparison> base_ins =
      match base_ins with
        | (address, disassemble, thread_id, opcode, read_registers, write_registers, read_addresses, write_addresses) ->
          (convert address, disassemble, thread_id, opcode,
           Map.map (fun _ reg_value -> convert reg_value) read_registers,
           Map.map (fun _ reg_value -> convert reg_value) write_registers,
           Map.toList read_addresses |> List.map (fun (address, value) -> (convert address, value)) |> Map.ofList,
           Map.toList write_addresses |>  List.map (fun (address, value) -> (convert address, value)) |> Map.ofList)

    let private get_instruction_from_chunk<'T when 'T: comparison> (chunk:byte[]) =
      try
        parseget_base_instructions chunk
        |> List.map convert_base_instruction
        |> List.map make_instruction
      with
        | :? _ as ex -> failwith (Printf.sprintf "get_instruction_from_chunk: %s" ex.Message)


    // let parse_trace_file filename =
    //   use trace_reader = new System.IO.BinaryReader(System.IO.File.OpenRead(filename))
    //   let machine_info = extract_packed_data trace_reader |> parse_machine_information
