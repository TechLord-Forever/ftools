namespace cfgrecon
  module Instruction =
    type Addrint = uint32
    type MemoryMap = Map<Addrint, Addrint>
    type RegisterMap = Map<string, Addrint>
    type Instruction = { address : Addrint;
                         read_registers : RegisterMap;
                         written_registers : MemoryMap;
                         read_addresses : MemoryMap;
                         written_addresses : MemoryMap }

    let make_instruction ins_addr read_regs written_regs read_addrs written_addrs =
       { address = ins_addr;
         read_registers = read_regs;
         written_registers = written_regs;
         read_addresses = read_addrs;
         written_addresses = written_addrs }

    let get_read_register_value reg_name ins =
      match ins with
        | { address = _;
            read_registers = regs;
            written_registers = _;
            read_addresses = _;
            written_addresses = _ } -> Map.find reg_name regs

    let get_written_register_value reg_name ins =
      match ins with
        | { address = _;
            read_registers = _;
            written_registers = regs;
            read_addresses = _;
            written_addresses = _ } -> Map.find reg_name regs

    let get_read_memory_value mem_addr ins =
      match ins with
        | { address = _;
            read_registers = _;
            written_registers = _;
            read_addresses = addrs;
            written_addresses = _ } -> Map.find mem_addr addrs

    let get_written_memory_value mem_addr ins =
      match ins with
        | { address = _;
            read_registers = _;
            written_registers = _;
            read_addresses = _;
            written_addresses = addrs } -> Map.find mem_addr addrs
