namespace cfgrecon
  module DataExtraction =
    type MachineArchitecture =
      | X86
      | X86_64

    type MachineInfo = { arch: MachineArchitecture;
                         address_size: uint32 }

    (*===
       first read a uint32 value since it specifies the length of json serialized data, next read the data
    ===*)
    let private extract_packed_data (reader:System.IO.BinaryReader) =
      reader.ReadBytes (int (reader.ReadUInt32()))

    (*===
      an example of json form for the machine's information:
      {
        "header": {
          "architecture": 0,
          "address size": 32
         }
       }
    ===*)
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


    (*===
      an example of the json form for a chunk of instructions:
      {
        "chunk": {
          [
             {
                "address": ...,
                "disassemble": ...,
                "thread_id": ...,
                "opcode":
                "read_registers": [
                  "eax": 39048,
                  "ebx": ...
                ],
                "write_registers": [ ... ],
                "read_addresses": [
                  "0x1343": 843451,
                  ...
                ],
                "write_addresses": ...
             },
             ...
          ]
        }
      }
    ===*)
    let get_instruction_from_chunk<'T when 'T : comparison> (data:byte[]) =
      let json_chunk = Chiron.Parsing.Json.parse (string data)
      let chunk_map : Map<string, obj> = unbox json_chunk
      let chunk_info_list = Map.toList (unbox <| Map.find "chunk" chunk_map)
      List.iter (fun chunk_elem ->
                 let elem_map:Map<string, obj> = unbox chunk_elem
                 let address = Map.find "address" elem_map |> unbox<decimal> |>  System.Decimal.ToUInt64
                 let disassemble = Map.find "disassemble" elem_map |> unbox<string>
                 let opcode = Map.find "opcode" elem_map |> unbox<string> |> System.Convert.FromBase64String
                 let read_registers : Instruction.RegisterMap<'T> =
                   let reg_obj_list = Map.find "read registers" elem_map
                   Map.ofList <| List.map (fun reg_obj ->
                                           let single_reg_map:Map<string, decimal> = unbox reg_obj
                                           let single_reg_array = Map.toArray single_reg_map
                                           (fst single_reg_array.[0],
                                            typeof<'T> (System.Decimal.ToUInt64 <| snd single_reg_array.[0]))
                                           ) reg_obj_list
                 let write_registers : Instruction.RegisterMap<'T> =
                   let reg_obj_list = Map.find "write registers" elem_map
                   Map.ofList <| List.map (fun reg_obj ->
                                           let single_reg_map:Map<string, decimal> = unbox reg_obj
                                           let single_reg_array = Map.toArray single_reg_map
                                           (fst single_reg_array.[0],
                                            typeof<'T> (System.Decimal.ToUInt64 <| snd single_reg_array.[0]))
                                           ) reg_obj_list
                 let read_addresses : Instruction.MemoryMap<'T> =
                   let addr_obj_list = Map.find "read addresses" elem_map
                   Map.ofList <| List.map (fun addr_obj ->
                                           let single_addr_map:Map<string, decimal> = unbox addr_obj
                                           let single_addr_array = Map.toArray single_addr_map
                                           (typeof<'T> (System.UInt64.Parse <| fst single_addr_array.[0]),
                                            uint8 (System.Decimal.ToUInt64 <| snd single_addr_array.[0]))
                                           ) addr_obj_list
                 let write_addresses : Instruction.MemoryMap<'T> =
                   let addr_obj_list = Map.find "write address" elem_map
                   Map.ofList <| List.map (fun addr_obj ->
                                           let single_addr_map:Map<string, decimal> = unbox addr_obj
                                           let single_addr_array = Map.toArray single_addr_map
                                           (typeof<'T> (System.UInt64.Parse <| fst single_addr_array.[0]),
                                            uint8 (System.Decimal.ToUInt64 <| snd single_addr_array.[0]))
                                           ) addr_obj_list

                 ) chunk_info_list


