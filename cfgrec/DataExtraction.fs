namespace cfgrecon

  [<AutoOpen>]
  module Instruction =
    type MachineArchitecture =
      | X86
      | X86_64

    type MachineInfo = { arch: MachineArchitecture;
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

  module DataExtraction =

    // first read a uint32 value since it specifies the length of json serialized data, next read the data
    let private extract_packed_data (reader:System.IO.BinaryReader) =
      reader.ReadBytes (int (reader.ReadUInt32()))

    // an example of json form for the machine's information:
    // {
    //   "header": {
    //     "architecture": 0,
    //     "address size": 32
    //    }
    //  }
    let get_machine_information_from_header (data:byte[]) =
      try
        let json_header = Chiron.Parsing.Json.parse (string data)
        let header_map:Map<string, obj> = unbox json_header
        let header_info_map:Map<string, decimal> = unbox <| Map.find "header" header_map
        let parsed_arch = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "architecture" header_info_map)
        let parsed_addrlen = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "address size" header_map)
        match (int parsed_arch) with
          | 0 -> Some { arch = X86; address_size = parsed_addrlen }
          | 1 -> Some { arch = X86_64; address_size = parsed_addrlen }
          | _ -> failwith "unknown architecture"
      with
        | _ -> None


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

    let get_base_instruction_from_chunk (data:byte[]) =
      let json_chunk = Chiron.Parsing.Json.parse (string data)
      let chunk_map : Map<string, obj> = unbox json_chunk
      let chunk_info_list = Map.toList (unbox <| Map.find "chunk" chunk_map)
      List.map (fun chunk_elem ->
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
                 ) chunk_info_list


