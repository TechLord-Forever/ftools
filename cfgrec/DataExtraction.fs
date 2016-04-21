namespace cfgrecon
  module DataExtraction =
    type MachineArchitecture =
      | X86
      | X86_64

    type MachineInfo = { arch: MachineArchitecture;
                         address_size: uint32 }

    (*================================================================================================================*)

    (* first read a uint32 value since it specifies the length of json serialized data,  next read the data *)
    let private extract_packed_data (reader: System.IO.BinaryReader) =
      reader.ReadBytes (int (reader.ReadUInt32()))

    (*
       an example of machine's information json object:
       {
         "header": {
           "architecture": 0,
           "address size": 32
         }
       }
    *)
    let get_machine_information (data: byte[]) =
      let json_header = Chiron.Parsing.Json.parse (string data)
      let header_map:Map<string, obj> = unbox json_header
      let header_info_map = unbox <| Map.find "header" header_map
      let parsed_arch = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "architecture" header_info_map)
      let parsed_addr_length = System.Decimal.ToUInt32 (unbox<decimal> <| Map.find "address size" header_map)
      match (int parsed_arch) with
        | 0 -> { arch = X86; address_size = parsed_addr_length }
        | 1 -> { arch = X86_64; address_size = parsed_addr_length }
        | _ -> failwith "unknown architecture"


    let get_instructions (data: byte[]) =
      let json_instructions = Chiron.Parsing.Json.parse (string data)





