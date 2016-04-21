namespace cfgrecon
  module Instruction =
    type MemoryMap<'T when 'T : comparison> = Map<'T, uint8>
    type RegisterMap<'T>                    = Map<string, 'T>

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


