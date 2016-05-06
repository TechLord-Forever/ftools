namespace cfgrecon

  // [<AutoOpen>]
  module Machine =
    type Architecture =
      | X86
      | X86_64

    type MemoryMap<'T when 'T : comparison> = Map<'T, uint8>
    type RegisterMap<'T> = Map<string, 'T>

    type Instruction<'T when 'T : comparison> = { address         : 'T;
                                                  disassemble     : string;
                                                  thread_id       : uint32;
                                                  opcode          : byte[];
                                                  read_registers  : RegisterMap<'T>;
                                                  write_registers : RegisterMap<'T>;
                                                  load_addresses  : MemoryMap<'T>;
                                                  store_addresses : MemoryMap<'T> }

    type TraceInfo<'T when 'T : comparison> = { arch : Architecture;
                                                instructions : seq<Instruction<'T>> }
