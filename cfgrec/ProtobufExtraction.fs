namespace cfgrecon
  module ProtobufExtraction =

    // architecture_t
    type Architecture =
      X86 = 0 | X86_64 = 1

    // address_t
    type Address () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      // let m_value = ref (Value_32 Unchecked.defaultof<int32>)
      let m_value = ref None

      let decode_v32_callback =
        fun raw_field ->
          let ref_v32 = ref (int32 0)
          Froto.Core.Encoding.Serializer.hydrateSInt32 ref_v32 raw_field;
          m_value := Value_32 (!ref_v32)

      let decode_v64_callback =
        fun raw_field ->
          let ref_v64 = ref (int64 0)
          Froto.Core.Encoding.Serializer.hydrateSInt64 ref_v64 raw_field;
          m_value := Value_64 (!ref_v64)

      let m_decoder_ring = Map.ofList [ 1, decode_v32_callback
                                        2, decode_v64_callback ]
      (* end of primary constructor *)

      member x.Value with get() = !m_value and set(v) = m_value := v

      override x.Clear () =
        m_value := (Value_32 Unchecked.defaultof<int32>)

      override x.Encode zc_buffer =
        match !m_value with
          | Value_32 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt32 1) zc_buffer
          | Value_64 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt64 2) zc_buffer
          | None -> failwith "invalid value"

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = Address()
        self.Merge(buffer) |> ignore
        self

    and UnionInt =
      | Value_32 of int32
      | Value_64 of int64
      | None

    // register_t
    type Register () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_name = ref Unchecked.defaultof<string>
      let m_value = ref (Address())

      let m_decoder_ring =
        Map.ofList [ 1, m_name |> Froto.Core.Encoding.Serializer.hydrateString
                     2, m_value |> Froto.Core.Encoding.Serializer.hydrateMessage (Address.FromArraySegment) ]
      (* end of primary constructor *)

      member x.Name with get() = !m_name and set(v) = m_name := v
      member x.Value with get () = !m_value and set(v) = m_value := v

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
    type Memory () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_address = ref (Address())
      let m_value = ref Unchecked.defaultof<int32>

      let m_decoder_ring =
        Map.ofList [ 1, m_address |> Froto.Core.Encoding.Serializer.hydrateMessage (Address.FromArraySegment)
                     2, m_value |> Froto.Core.Encoding.Serializer.hydrateSInt32 ]
      (* end of primary constructor *)

      member x.Address with get() = !m_address and set(v) = m_address := v
      member x.Value with get() = !m_value and set(v) = m_value := v

      override x.Clear () =
        m_address := Address();
        m_value := Unchecked.defaultof<int32>

      override x.Encode zc_buffer =
        let encode =
          (!m_address |> Froto.Core.Encoding.Serializer.dehydrateMessage 1) >>
          (!m_value |> Froto.Core.Encoding.Serializer.dehydrateSInt32 2)
        encode zc_buffer

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = Memory()
        ignore <| self.Merge(buffer)
        self

    // concrete_info_t
    type ConcreteInfo () =
      inherit Froto.Core.Encoding.MessageBase()

      (* begin of primary constructor *)
      let m_value = ref None

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

      member x.Value with get() = !m_value and set(v) = m_value := v

      override x.Clear () =
        m_value := None

      override x.Encode zc_buffer =
        match !m_value with
          | ReadRegister v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 1) zc_buffer
          | WriteRegister v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 2) zc_buffer
          | LoadMemory v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 3) zc_buffer
          | StoreMemory v -> (v |> Froto.Core.Encoding.Serializer.dehydrateMessage 4) zc_buffer
          | None -> failwith "invalid value"

      override x.DecoderRing = m_decoder_ring

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = ConcreteInfo()
        ignore <| self.Merge(buffer)
        self

    and UnionInfo =
      | ReadRegister of Register
      | WriteRegister of Register
      | LoadMemory of Memory
      | StoreMemory of Memory
      | None
