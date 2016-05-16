namespace cfgrecon
  module ProtobufExtraction =
    // architecture_t
    type private Architecture =
      | Unknown = 0
      | X86     = 1
      | X86_64  = 2

    // address_t
    type private Address () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_value = ref (Value_32 Unchecked.defaultof<uint32>)

      let decode_v32_callback raw_field =
        let ref_v32 = ref (uint32 0)
        // Froto.Core.Encoding.Serializer.hydrateSInt32 ref_v32 raw_field;
        Froto.Core.Encoding.Serializer.hydrateUInt32 ref_v32 raw_field
        m_value := Value_32 (!ref_v32)

      let decode_v64_callback raw_field =
        let ref_v64 = ref (uint64 0)
        // Froto.Core.Encoding.Serializer.hydrateSInt64 ref_v64 raw_field;
        Froto.Core.Encoding.Serializer.hydrateUInt64 ref_v64 raw_field
        m_value := Value_64 (!ref_v64)

      let m_decoder_ring = Map.ofList [ 1, decode_v32_callback
                                        2, decode_v64_callback ]
      (* end of primary constructor *)

      member x.Value
        with get() = !m_value
        and set(v) = m_value := v

      override x.Clear () =
        m_value := (Value_32 Unchecked.defaultof<uint32>)

      override x.Encode zc_buffer =
        match !m_value with
          // | Value_32 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt32 1) zc_buffer
          // | Value_64 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt64 2) zc_buffer
          | Value_32 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateVarint 1) zc_buffer
          | Value_64 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateVarint 2) zc_buffer
          // | _ -> failwith "invalid value"

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = Address()
        self.Merge(buffer) |> ignore
        self
    and private UnionInt =
      | Value_32 of uint32
      | Value_64 of uint64
      // | None

    // register_t
    type private Register () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_name = ref Unchecked.defaultof<string>
      let m_value = ref Unchecked.defaultof<Address>
      // let m_value = ref (Address())

      // let name_decoder raw_field =
      //   let ref_name = ref (Unchecked.defaultof<string>)
      //   Froto.Core.Encoding.Serializer.hydrateString ref_name raw_field
      //   Printf.printfn "decoding register name"
      //   m_name := !ref_name

      let m_decoder_ring =
        Map.ofList [ 1, m_name |> Froto.Core.Encoding.Serializer.hydrateString
                     2, m_value |> Froto.Core.Encoding.Serializer.hydrateMessage (Address.FromArraySegment) ]
      (* end of primary constructor *)

      member x.Name
        with get() = !m_name
        and set(v) = m_name := v

      member x.Value
        with get () = !m_value
        and set(v) = m_value := v

      override x.Clear () =
        m_name := Unchecked.defaultof<string>;
        m_value := Address()

      override x.Encode zc_buffer =
        let encode =
          (!m_name |> Froto.Core.Encoding.Serializer.dehydrateString 1) >>
          (!m_value |> Froto.Core.Encoding.Serializer.dehydrateMessage 2)
        encode zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = Register()
        self.Merge(buffer) |> ignore
        self

    // memory_t
    type private Memory () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_address = ref (Address())
      let m_value = ref Unchecked.defaultof<uint32>

      let m_decoder_ring =
        Map.ofList [ 1, m_address |> Froto.Core.Encoding.Serializer.hydrateMessage (Address.FromArraySegment)
                     2, m_value |> Froto.Core.Encoding.Serializer.hydrateUInt32 ]
                     // 2, m_value |> Froto.Core.Encoding.Serializer.hydrateSInt32 ]
      (* end of primary constructor *)

      member x.Address
        with get() = !m_address
        and set(v) = m_address := v

      member x.Value
        with get() = !m_value
        and set(v) = m_value := v

      override x.Clear () =
        m_address := Address();
        m_value := Unchecked.defaultof<uint32>

      override x.Encode zc_buffer =
        let encode =
          (!m_address |> Froto.Core.Encoding.Serializer.dehydrateMessage 1) >>
          (!m_value |> Froto.Core.Encoding.Serializer.dehydrateVarint 2)
          // (!m_value |> Froto.Core.Encoding.Serializer.dehydrateSInt32 2)
        encode zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = Memory()
        ignore <| self.Merge(buffer)
        self

    // concrete_info_t
    type private ConcreteInfo () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      // let m_value = ref None
      let m_value = ref <| ReadRegister (Register())

      // false => read, true => write
      let decode_register_callback read_or_write =
        fun raw_field ->
          let ref_register = ref (Register())
          Froto.Core.Encoding.Serializer.hydrateMessage (Register.FromArraySegment) ref_register raw_field;
          if read_or_write then
            m_value := ReadRegister (!ref_register)
          else
            m_value := WriteRegister (!ref_register)

      // false => load, true => store
      let decode_memory_callback load_or_store =
        fun raw_field ->
          let ref_memory = ref (Memory())
          Froto.Core.Encoding.Serializer.hydrateMessage (Memory.FromArraySegment) ref_memory raw_field;
          if load_or_store then
            m_value := LoadMemory (!ref_memory)
          else
            m_value := StoreMemory (!ref_memory)

      let m_decoder_ring =
        Map.ofList [ 1, decode_register_callback false
                     2, decode_register_callback true
                     3, decode_memory_callback false
                     4, decode_memory_callback true ]
      (* end of primary constructor *)

      member x.Value
        with get() = !m_value
        and set(v) = m_value := v

      override x.Clear () =
        m_value := ReadRegister (Register())

      override x.Encode zc_buffer =
        match !m_value with
          | ReadRegister v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 1) zc_buffer
          | WriteRegister v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 2) zc_buffer
          | LoadMemory v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 3) zc_buffer
          | StoreMemory v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 4) zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer:System.ArraySegment<byte>) =
        let self = ConcreteInfo()
        ignore <| self.Merge(buffer)
        self
    and private UnionInfo =
      | ReadRegister of Register
      | WriteRegister of Register
      | LoadMemory of Memory
      | StoreMemory of Memory

    // instruction_t
    type private Instruction () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_thread_id = ref Unchecked.defaultof<uint32>
      let m_address = ref (Address())
      let m_opcode = ref Unchecked.defaultof<byte[]>
      let m_disassemble = ref Unchecked.defaultof<string>
      let m_c_info = ref List.empty<ConcreteInfo>

      // let thread_id_decoder raw_field =
      //   let ref_thread_id = ref (Unchecked.defaultof<uint32>)
      //   Froto.Core.Encoding.Serializer.hydrateUInt32 ref_thread_id raw_field
      //   Printf.printfn "decoding thread id"
      //   m_thread_id := !ref_thread_id

      // let address_decoder raw_field =
      //   let ref_address = ref (Unchecked.defaultof<Address>)
      //   Froto.Core.Encoding.Serializer.hydrateMessage (Address.FromArraySegment) ref_address raw_field
      //   Printf.printfn "decoding address"
      //   m_address := !ref_address

      // let opcode_decoder raw_field =
      //   let ref_opcode = ref (Unchecked.defaultof<byte[]>)
      //   Froto.Core.Encoding.Serializer.hydrateBytes ref_opcode raw_field
      //   Printf.printfn "decoding opcode"
      //   m_opcode := !ref_opcode

      // let disassemble_decoder raw_field =
      //   let ref_disassemble = ref (Unchecked.defaultof<string>)
      //   Froto.Core.Encoding.Serializer.hydrateString ref_disassemble raw_field
      //   Printf.printfn "decoding disassemble: %s" !ref_disassemble
      //   m_disassemble := !ref_disassemble

      // let concrete_info_decoder raw_field =
      //   let ref_concrete_infos = ref (Unchecked.defaultof<ConcreteInfo list>)
      //   Froto.Core.Encoding.Serializer.hydrateRepeated (Froto.Core.Encoding.Serializer.hydrateMessage ConcreteInfo.FromArraySegment) ref_concrete_infos raw_field
      //   Printf.printfn "decoding concrete info"
      //   m_c_info := !ref_concrete_infos

      let m_decoder_ring =
        Map.ofList [ 1, // thread_id_decoder
                     m_thread_id |> Froto.Core.Encoding.Serializer.hydrateUInt32
                     2, // address_decoder
                     m_address |> Froto.Core.Encoding.Serializer.hydrateMessage (Address.FromArraySegment)
                     3, // opcode_decoder
                     m_opcode |> Froto.Core.Encoding.Serializer.hydrateBytes
                     4, // disassemble_decoder
                     m_disassemble |> Froto.Core.Encoding.Serializer.hydrateString
                     5, // concrete_info_decoder
                     m_c_info |> Froto.Core.Encoding.Serializer.hydrateRepeated (Froto.Core.Encoding.Serializer.hydrateMessage (ConcreteInfo.FromArraySegment))
                     ]
      (* end of primary constructor *)

      member x.ThreadId
        with get() = !m_thread_id
        and set(v) = m_thread_id := v

      member x.Address
        with get() = !m_address
        and set(v) = m_address := v

      member x.Opcode
        with get() = !m_opcode
        and set(v) = m_opcode := v

      member x.Disassemble
        with get() = !m_disassemble
        and set(v) = m_disassemble := v

      member x.ConcreteInfo
        with get() = !m_c_info
        and set(v) = m_c_info := v

      override x.Clear () =
        m_thread_id   := Unchecked.defaultof<uint32>;
        m_address     := Address();
        m_opcode      := Unchecked.defaultof<byte[]>;
        m_disassemble := Unchecked.defaultof<string>;
        m_c_info      := List.empty<ConcreteInfo>

      override x.Encode zc_buffer =
        let encode =
          (Froto.Core.Encoding.Serializer.dehydrateVarint 1 !m_thread_id) >>
          (Froto.Core.Encoding.Serializer.dehydrateMessage 2 !m_address) >>
          (Froto.Core.Encoding.Serializer.dehydrateBytes 3 <| System.ArraySegment(!m_opcode)) >>
          (Froto.Core.Encoding.Serializer.dehydrateString 4 !m_disassemble) >>
          (Froto.Core.Encoding.Serializer.dehydrateRepeated Froto.Core.Encoding.Serializer.dehydrateMessage 5 !m_c_info)
        encode zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer:System.ArraySegment<byte>) =
        let self = Instruction()
        ignore <| self.Merge(buffer)
        self

    // header_t
    type private Header () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_arch = ref Architecture.X86

      let m_decoder_ring = Map.ofList [ 1, m_arch |> Froto.Core.Encoding.Serializer.hydrateEnum ]
      (* end of primary constructor *)

      member x.Architecture
        with get() = !m_arch
        and set(v) = m_arch := v

      override x.Clear () = m_arch := Architecture.X86

      override x.Encode zc_buffer =
        Froto.Core.Encoding.Serializer.dehydrateDefaultedVarint Architecture.X86 1 !m_arch zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer:System.ArraySegment<byte>) =
        let self = Header()
        ignore <| self.Merge(buffer)
        self

    // chunk_t
    type private Chunk () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_insts = ref List.empty<Instruction>

      // let instructions_decoder raw_data =
      //   let ref_instructions = ref Unchecked.defaultof<Instruction list>
      //   Froto.Core.Encoding.Serializer.hydrateRepeated (Froto.Core.Encoding.Serializer.hydrateMessage Instruction.FromArraySegment) ref_instructions raw_data
      //   Printf.printfn "%d instruction(s) decoded" <| List.length !ref_instructions
      //   m_insts := !ref_instructions

      let m_decoder_ring =
        Map.ofList [ 1, m_insts |> Froto.Core.Encoding.Serializer.hydrateRepeated (Froto.Core.Encoding.Serializer.hydrateMessage Instruction.FromArraySegment) ]
      (* end of primary constructor *)

      member x.Instructions
        with get() = !m_insts
        and set(v) = m_insts := v

      override x.Clear () = m_insts := List.empty<Instruction>

      override x.Encode zc_buffer =
        Froto.Core.Encoding.Serializer.dehydrateRepeated Froto.Core.Encoding.Serializer.dehydrateMessage 1 !m_insts zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer:System.ArraySegment<byte>) =
        let self = Chunk()
        ignore <| self.Merge(buffer)
        self

    let private read_data_block (reader:System.IO.BinaryReader) =
      let block_size = int <| reader.ReadUInt32()
      // Printf.printfn "block size: %d" block_size
      reader.ReadBytes block_size

    let extract_machine_architecture (reader:System.IO.BinaryReader) : Machine.Architecture option =
      try
        Printf.printfn "extract machine architecture"
        let header_block = read_data_block reader
        let header_segment = System.ArraySegment(header_block)
        let header = Header.FromArraySegment(header_segment)
        match header.Architecture with
          | Architecture.X86 -> Some Machine.Architecture.X86
          | Architecture.X86_64 -> Some Machine.Architecture.X86_64
          | _ -> None
      with
        | _ -> None

    let private convert_to_explicit_address<'T> (addr:Address) =
      match addr.Value with
        | Value_64 v -> uint64 v |> unbox<'T>
        | Value_32 v -> uint32 v |> unbox<'T>
    // let inline convert_to_explicit_address (addr:Address) = FSharpPlus.Operators.explicit addr

    let private convert_to_explicit_instruction<'T when 'T : comparison> (ins:Instruction) : Machine.Instruction<'T> =
      let explicit_thread_id   = ins.ThreadId
      let explicit_address     = convert_to_explicit_address ins.Address
      let explicit_opcode      = ins.Opcode
      let explicit_disassemble = ins.Disassemble
      let conc_info_list       = ins.ConcreteInfo
      let mem_load_map         = ref Map.empty
      let mem_store_map        = ref Map.empty
      let reg_read_map         = ref Map.empty
      let reg_write_map        = ref Map.empty
      for conc_info in conc_info_list do
        match conc_info.Value with
          | ReadRegister read_reg   -> reg_read_map  := Map.ofList ((read_reg.Name, convert_to_explicit_address(read_reg.Value))::(Map.toList !reg_read_map))
          | WriteRegister write_reg -> reg_write_map := Map.ofList ((write_reg.Name, convert_to_explicit_address(write_reg.Value))::(Map.toList !reg_write_map))
          | LoadMemory load_mem     -> mem_load_map  := Map.ofList ((convert_to_explicit_address(load_mem.Address), uint8 load_mem.Value)::(Map.toList !mem_load_map))
          | StoreMemory store_mem   -> mem_store_map := Map.ofList ((convert_to_explicit_address(store_mem.Address), uint8 store_mem.Value)::(Map.toList !mem_store_map))
      { address = explicit_address; disassemble = explicit_disassemble; thread_id = explicit_thread_id; opcode = explicit_opcode;
        read_registers = !reg_read_map; write_registers = !reg_write_map; load_addresses = !mem_load_map; store_addresses = !mem_store_map }

    let extract_instructions<'T when 'T : comparison> (reader:System.IO.BinaryReader) : Machine.Instruction<'T> list  =
      let extracted_inss = ref List.empty
      let should_continue_parsing = ref true
      while !should_continue_parsing do
        try
          let chunk_block = read_data_block reader
          let chunk_segment = System.ArraySegment(chunk_block)
          let chunk = Chunk.FromArraySegment(chunk_segment)
          // let chunk = Chunk()
          // ignore (chunk.DeserializeLengthDelimited <| Froto.Core.ZeroCopyBuffer(chunk_segment))
          let chunk_inss = chunk.Instructions
          // Printf.printfn "chunk has %d instruction(s)" <| List.length chunk_inss
          extracted_inss := chunk_inss |> List.append !extracted_inss
        with
          | :? System.IO.EndOfStreamException -> should_continue_parsing := false
      // Printf.printfn "%d instruction(s) parsed" <| List.length !extracted_inss
      List.map convert_to_explicit_instruction<'T> !extracted_inss
