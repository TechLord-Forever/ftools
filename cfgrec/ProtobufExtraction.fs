namespace cfgrecon
  module ProtobufExtraction =

    // architecture_t
    type Architecture =
      X86 = 0 | X86_64 = 1

    // typeid_address_t
    type TypeIdOfAddress =
      Bit32 = 0 | Bit64 = 1

    // value_address_t
    type ValueOfAddress () =
      inherit Froto.Core.Encoding.MessageBase()

      let m_value_32 = ref (uint32 0)
      let m_value_64 = ref (uint64 0)

      override x.Clear() =
        m_value_32 := (uint32 0)
        m_value_64 := (uint64 0)

      override x.Encode(zc_buffer) =
        let encode =
          (!m_value_32 |> Froto.Core.Encoding.Serializer.de)
      
