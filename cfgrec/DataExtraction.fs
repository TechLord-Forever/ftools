namespace cfgrecon
  module DataExtraction =
    type MachineArchitecture =
      | X86
      | X86_64

    type MachineInfo = { arch: MachineArchitecture;
                         address_size: uint32 }

    (*==========================================================================*)

    let private extract_packed_data (reader:System.IO.BinaryReader) =
      reader.ReadBytes (int (reader.ReadUInt32()))

    let get_machine_information (data: byte[]) =
      let json_header = Chiron.Parsing.Json.parse (string data)
      let header_map:Map<string, obj> = unbox json_header
      let header_arch = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "architecture" header_map)
      let header_addr_size = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "address size" header_map)
      match (int header_arch) with
        | 0 -> { arch = X86; address_size = header_addr_size }
        | 1 -> { arch = X86_64; address_size = header_addr_size }
        | _ -> failwith "unknown architecture"


    // let get_instructions (data: byte[]) =
    //   use data_as_string = (string data)
    //   use json_instructions = Chiron.Json.parse data_as_string




