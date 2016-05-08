namespace cfgrecon

  // [<AutoOpen>]
  module Machine =
    [<RequireQualifiedAccess>]
    type Architecture =
      | X86
      | X86_64

    [<RequireQualifiedAccess>]
    type MemoryMap<'T when 'T : comparison> = Map<'T, uint8>

    [<RequireQualifiedAccess>]
    type RegisterMap<'T> = Map<string, 'T>

    [<RequireQualifiedAccess>]
    type Instruction<'T when 'T : comparison> = { address         : 'T;
                                                  disassemble     : string;
                                                  thread_id       : uint32;
                                                  opcode          : byte[];
                                                  read_registers  : RegisterMap<'T>;
                                                  write_registers : RegisterMap<'T>;
                                                  load_addresses  : MemoryMap<'T>;
                                                  store_addresses : MemoryMap<'T> }

    [<RequireQualifiedAccess>]
    type TraceInfo<'T when 'T : comparison> = { arch : Architecture;
                                                instructions : seq<Instruction<'T>> }

    let inline instruction_to_string (ins:Instruction<'T>) =
      Printf.sprintf "0x%x %s"  ins.address ins.disassemble
