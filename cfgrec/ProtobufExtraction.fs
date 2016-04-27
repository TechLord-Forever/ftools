namespace cfgrecon
  module ProtobufExtraction =

    // architecture_t
    type Architecture =
      X86 = 0 | X86_64 = 1

    // typeid_address_t
    type TypeIdOfAddress =
      Bit32 = 0 | Bit64 = 1

    type UnionInt =
      Value_32 of int32 | Value_64 of int64


    let m_value32 = ref (int32 0)
    let decode = m_value32 |> Froto.Core.Encoding.Serializer.hydrateSInt32

    // value_address_t
    type ValueOfAddress () =
      inherit Froto.Core.Encoding.MessageBase()

      let m_value = ref (Value_32 0)

      // let decode_v32 raw_field =
      //   let ref_v32 = ref (int32 0)
      //   Froto.Core.Encoding.Serializer.hydrateSInt32 ref_v32 raw_field;
      //   // ref_union_int := Value_32 (!ref_v32)
      //   m_value := Value_32 (!ref_v32)

      let decode_v64 ref_union_int raw_field =
        let ref_v64 = ref (int64 0)
        Froto.Core.Encoding.Serializer.hydrateSInt64 ref_v64 raw_field;
        ref_union_int := Value_64 (!ref_v64)

      member x.Value with get() = !m_value and set(v) = m_value := v

      override x.Clear () =
        m_value := (Value_32 0)

      override x.Encode zc_buffer =
        match !m_value with
          | Value_32 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt32 1) zc_buffer
          | Value_64 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt64 2) zc_buffer

      override x.DecoderRing =
        let decode_v32_callback =
          fun raw_field ->
            let ref_v32 = ref (int32 0)
            Froto.Core.Encoding.Serializer.hydrateSInt32 ref_v32 raw_field
            m_value := Value_32 (!ref_v32)
        let decode_v64_callback =
          fun raw_field ->
            let ref_v64 = ref (int64 0)
            Froto.Core.Encoding.Serializer.hydrateSInt64 ref_v64 raw_field
            m_value := Value_64 (!ref_v64)
        Map.ofList [ 1, decode_v32_callback
                     2, decode_v64_callback ]

      static member FromArraySegment (buffer : System.ArraySegment<byte>) =
        let self = ValueOfAddress()
        self.Merge(buffer) |> ignore
        self

    // address_t
    type Address () =
      inherit Froto.Core.Encoding.MessageBase()

      let m_typeid = ref (TypeIdOfAddress.Bit32)
      let m_value = ref (ValueOfAddress())

      member x.Typeid with get() = !m_typeid and set(v) = m_typeid := v
      member x.Value with get() = !m_value and set(v) = m_value := v

      override x.Clear () =
        m_typeid := TypeIdOfAddress.Bit32
        m_value := ValueOfAddress()

      override x.Encode zc_buffer =
        let encode =
          (!m_typeid |> Froto.Core.Encoding.Serializer.)
