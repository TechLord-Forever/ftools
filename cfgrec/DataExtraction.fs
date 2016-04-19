namespace cfgrecon
  module DataExtraction =

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

    type header_t =
      { architecture: architecture_t;
        address_size: typeid_address_t }

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
