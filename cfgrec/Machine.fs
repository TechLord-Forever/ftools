namespace cfgrecon

  // [<AutoOpen>]
  module Machine =
    type Architecture =
      | X86
      | X86_64

    type MachineInfo = { arch: Architecture;
                         address_size: uint32 }

    type MemoryMap<'T when 'T : comparison> = Map<'T, uint8>
    type RegisterMap<'T> = Map<string, 'T>

    type Instruction<'T when 'T : comparison> = { address         : 'T;
                                                  disassemble     : string;
                                                  thread_id       : uint32;
                                                  opcode          : byte[];
                                                  read_registers  : RegisterMap<'T>;
                                                  write_registers : RegisterMap<'T>;
                                                  read_addresses  : MemoryMap<'T>;
                                                  write_addresses : MemoryMap<'T> }

    type TraceInfo<'T when 'T : comparison> = { machine_info : MachineInfo;
                                                instructions : seq<Instruction<'T>> }
