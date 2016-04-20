namespace cfgrecon
  module DataExtraction =
    // open System.IO
    // open Chiron

    type architecture_t =
      | X86 = 0
      | X86_64 = 1

    type typeid_address_t =
      | Bit32 = 0
      | Bit64 = 1

    type value_address_t =
      | Value_32 of uint32
      | Value_64 of uint64

    type address_t =
      { typeid: typeid_address_t;
        value: value_address_t }

    type register_t =
      { name: string;
        value: address_t }

    type memory_t =
      { address: address_t;
        value: address_t }

    type typeid_con_info_t =
      | RegRead = 0
      | RegWrite = 1
      | MemLoad = 2
      | MemStore = 3
      | Comment = 255

    type value_con_info_t =
      | Read_register of register_t
      | Write_register of register_t
      | Load_memory of memory_t
      | Store_memory of memory_t

    type con_info_t =
      { typeid: typeid_con_info_t;
        value: value_con_info_t }

    type instruction_t =
      { thread_id: address_t;
        address: address_t;
        opcode: string;
        concrete_info: con_info_t list;
        disassemble: string }

    type header_t =
      { architecture: architecture_t;
        address_size: typeid_address_t }

    type chunk_t = instruction_t list

    (*==========================================================================*)

    let private extract_packed_data (reader:System.IO.BinaryReader) =
      use data_size = reader.ReadUInt32 ()
      reader.ReadBytes (int data_size)

    let get_instructions (data: byte[]) =
      use data_as_string = System.String (data)
      use json_instructions = Chiron.Json.parse data_as_string



