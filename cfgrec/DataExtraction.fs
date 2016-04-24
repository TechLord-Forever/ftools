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

    // type MemoryMap<'T when 'T : comparison> = Map<'T, uint8>
    // type RegisterMap<'T> = Map<string, 'T>

    // type Instruction<'T when 'T : comparison> =
    //   { address           : 'T;
    //     disassemble       : string;
    //     thread_id         : uint32;
    //     opcode            : byte[];
    //     read_registers    : RegisterMap<'T>;
    //     written_registers : RegisterMap<'T>;
    //     read_addresses    : MemoryMap<'T>;
    //     written_addresses : MemoryMap<'T> }

    // type TraceInfo<'T when 'T : comparison> = { machine: MachineInfo;
    //                                             trace: seq<Instruction<'T>> }

    // type BaseInstruction (ins_addr : uint64,
    //                       ins_disas : string,
    //                       ins_tid : uint32,
    //                       ins_opc : byte[],
    //                       ins_read_regs : RegisterMap<uint64>,
    //                       ins_write_regs : RegisterMap<uint64>,
    //                       ins_read_addrs : MemoryMap<uint64>,
    //                       ins_write_addrs : MemoryMap<uint64>) =
    //   member x.address = ins_addr
    //   member x.disassemble = ins_disas

  module DataExtraction =
    // let make_instruction (ins_addr, ins_disas, ins_thread_id, ins_opcode,
    //                       read_regs, written_regs, read_addrs, written_addrs) =
    //   { address           = ins_addr;
    //     disassemble       = ins_disas;
    //     thread_id         = ins_thread_id;
    //     opcode            = ins_opcode;
    //     read_registers    = read_regs;
    //     written_registers = written_regs;
    //     read_addresses    = read_addrs;
    //     written_addresses = written_addrs }

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
      let json_header                            = Chiron.Parsing.Json.parse (string header)
      let header_map : Map<string, obj>          = unbox json_header
      let header_info_map : Map<string, decimal> = Map.find "header" header_map |> unbox
      let parsed_arch                            = Map.find "architecture" header_info_map |> unbox<decimal> |> System.Decimal.ToUInt32
      let parsed_addrlen                         = Map.find "address size" header_map |> unbox<decimal> |> System.Decimal.ToUInt32
      match (int parsed_arch) with
        | 0 -> { arch = X86; address_size = parsed_addrlen }
        | 1 -> { arch = X86_64; address_size = parsed_addrlen }
        | _ -> failwith "parse_marchine_information: unknown architecture"


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
      Instruction(ins_addr, ins_disas, ins_tid, ins_opc,
                  Map.map (fun _ reg_value -> Utility.cast_generic<'T> reg_value) ins_read_regs,
                  Map.map (fun _ reg_value -> Utility.cast_generic<'T> reg_value) ins_write_regs,
                  Map.toList ins_read_addrs |> List.map (fun (address, value) -> (Utility.cast_generic<'T> address, value)) |> Map.ofList,
                  Map.toList ins_write_addrs |>  List.map (fun (address, value) -> (Utility.cast_generic<'T> address, value)) |> Map.ofList)

    let private get_instructions_from_chunk (chunk:byte[]) =
      let json_chunk = Chiron.Parsing.Json.parse(string chunk)
      let chunk_map : Map<string, obj> = unbox json_chunk
      let chunk_info_list = Map.find "chunk" chunk_map |> unbox |> Map.toList
      List.map (parse_json_chunk_element >> normalize_parsed_instruction) chunk_info_list


    // let private parse_base_instructions (chunk:byte[]) =
    //   let json_chunk = Chiron.Parsing.Json.parse (string chunk)
    //   let chunk_map : Map<string, obj> = unbox json_chunk
    //   let chunk_info_list = Map.toList (unbox <| Map.find "chunk" chunk_map)
    //   List.map parse_json_chunk_element chunk_info_list

    // let private convert<'T> = Utility.cast_generic<'T>
    // let private inline convert = Utility.convert_static

    // let private convert_base_instruction base_ins =
    //   match base_ins with
    //     | (address, disassemble, thread_id, opcode, read_registers, write_registers, read_addresses, write_addresses) ->
    //       Instruction(address, disassemble, thread_id, opcode,
    //                   Map.map (fun _ reg_value -> reg_value) read_registers,
    //                   Map.map (fun _ reg_value -> reg_value) write_registers,
    //                   Map.toList read_addresses |> List.map (fun (address, value) -> (address, value)) |> Map.ofList,
    //                   Map.toList write_addresses |>  List.map (fun (address, value) -> (address, value)) |> Map.ofList)

    // let private get_instruction_from_chunk (chunk:byte[]) =
    //   parse_base_instructions chunk |> List.map convert_base_instruction



    // let parse_trace_file filename =
    //   use trace_reader = new System.IO.BinaryReader(System.IO.File.OpenRead(filename))
    //   let machine_info = extract_packed_data trace_reader |> parse_machine_information
