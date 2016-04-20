namespace cfgrecon
  module DataExtraction =
    type MachineArchitecture =
      | X86
      | X86_64

    type RegisterAccess<'T> = { name: string; value: 'T }
    type MemoryAccess<'T> = { address: 'T; value: 'T }


    type MachineInfo = { arch: MachineArchitecture;
                         address_size: uint32 }

    type Instruction<'T> = { address: 'T;
                             disassemble: string;
                             thread_id: uint32;
                             opcode: byte[];
                             read_registers: RegisterAccess list;
                             write_registers: RegisterAccess list;
                             read_addresses: MemoryAccess list;
                             write_addresses: MemoryAccess list }

    (*==========================================================================*)

    let private extract_packed_data (reader:System.IO.BinaryReader) =
      use data_size = reader.ReadUInt32 ()
      reader.ReadBytes (int data_size)

    let get_machine_information (data: byte[]) =
      

    let get_instructions (data: byte[]) =
      use data_as_string = System.String (data)
      use json_instructions = Chiron.Json.parse data_as_string




