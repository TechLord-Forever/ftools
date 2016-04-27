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

    // value_address_t
    type ValueOfAddress () =
      inherit Froto.Core.Encoding.MessageBase()

      let m_value = ref (Value_32 0)

      member x.Value with get() = !m_value and set(v) = m_value := v

      override x.Clear () =
        m_value := (Value_32 0)

      override x.Encode zc_buffer =
        match !m_value with
          | Value_32 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt32 1) zc_buffer
          | Value_64 v -> (v |> Froto.Core.Encoding.Serializer.dehydrateSInt64 2) zc_buffer


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
