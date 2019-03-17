1. Constant
[Start][Serialize]Constant
  [Start][Serialize]tpe
    [Start][Serialize]Type [toString=SBoolean; typeCode=0x01]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SBoolean; typeCode=0x01]
  [End][Serialize]tpe
  [Start][Serialize]value
    [Start Real][Serialize]Put Boolean
    [End Real][Serialize]Put Boolean
  [End][Serialize]value
[End][Serialize]Constant

2. ConstantPlaceholder
[Start][Serialize]ConstantPlaceholder
  [Start][Serialize]id
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
  [End][Serialize]id
[End][Serialize]ConstantPlaceholder
3. Tuple
[Start][Serialize]Tuple
  [Start][Serialize]length
    [Start Real][Serialize]Put UByte
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End Real][Serialize]Put UByte
  [End][Serialize]length
  [Start][Serialize]items*
    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put UShort
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End Real][Serialize]Put UShort
            [Start Real][Serialize]Put Bytes
            [End Real][Serialize]Put Bytes
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
    [Start][Serialize]Value [toString=SLong; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SLong; typeCode=0x05]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SLong; typeCode=0x05]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put Long
              [Start][Serialize]ZigZagLong
              [End][Serialize]ZigZagLong
              [Start Real][Serialize]Put ULong
                [Start][Serialize]VLQLong
                [End][Serialize]VLQLong
              [End Real][Serialize]Put ULong
            [End Real][Serialize]Put Long
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SLong; opCode=0x00]
  [End][Serialize]items*
[End][Serialize]Tuple
4. SelectField
[Start][Serialize]SelectField
  [Start][Serialize]input
    [Start][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
          [Start][Serialize]SimpleTransformer
            [Start][Serialize]input
              [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                    [Start][Serialize]CaseObject
                    [End][Serialize]CaseObject
                  [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBox; opCode=0xA7]
            [End][Serialize]input
          [End][Serialize]SimpleTransformer
        [End][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
  [End][Serialize]input
  [Start][Serialize]fieldIndex
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]fieldIndex
[End][Serialize]SelectField
5. Relation2
[Start][Serialize]Relation2
  [Start][Serialize][Not BooleanConstants]
    [Start][Serialize]left
      [Start][Serialize]Value [toString=SInt; opCode=0xA3]
        [Start][Serialize][Non-constant]
          [Start][Serialize]opCode
            [Start Real][Serialize]Put Byte
            [End Real][Serialize]Put Byte
          [End][Serialize]opCode
          [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
            [Start][Serialize]CaseObject
            [End][Serialize]CaseObject
          [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
        [End][Serialize][Non-constant]
      [End][Serialize]Value [toString=SInt; opCode=0xA3]
    [End][Serialize]left
    [Start][Serialize]right
      [Start][Serialize]Value [toString=SInt; opCode=0x9A]
        [Start][Serialize][Non-constant]
          [Start][Serialize]opCode
            [Start Real][Serialize]Put Byte
            [End Real][Serialize]Put Byte
          [End][Serialize]opCode
          [Start][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
            [Start][Serialize]TwoArguments
              [Start][Serialize]left
                [Start][Serialize]Value [toString=SInt; opCode=0x8C]
                  [Start][Serialize][Non-constant]
                    [Start][Serialize]opCode
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]opCode
                    [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                      [Start][Serialize]SelectField
                        [Start][Serialize]input
                          [Start][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                [Start][Serialize]SimpleTransformer
                                  [Start][Serialize]input
                                    [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                          [Start][Serialize]CaseObject
                                          [End][Serialize]CaseObject
                                        [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SBox; opCode=0xA7]
                                  [End][Serialize]input
                                [End][Serialize]SimpleTransformer
                              [End][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                        [End][Serialize]input
                        [Start][Serialize]fieldIndex
                          [Start Real][Serialize]Put Byte
                          [End Real][Serialize]Put Byte
                        [End][Serialize]fieldIndex
                      [End][Serialize]SelectField
                    [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                  [End][Serialize][Non-constant]
                [End][Serialize]Value [toString=SInt; opCode=0x8C]
              [End][Serialize]left
              [Start][Serialize]right
                [Start][Serialize]Value [toString=SInt; opCode=0x00]
                  [Start][Serialize][Constant with store]
                    [Start][Serialize]placeholder.opCode
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]placeholder.opCode
                    [Start][Serialize]constantPlaceholder
                      [Start][Serialize]ConstantPlaceholder
                        [Start][Serialize]id
                          [Start Real][Serialize]Put UInt
                            [Start Real][Serialize]Put ULong
                              [Start][Serialize]VLQLong
                              [End][Serialize]VLQLong
                            [End Real][Serialize]Put ULong
                          [End Real][Serialize]Put UInt
                        [End][Serialize]id
                      [End][Serialize]ConstantPlaceholder
                    [End][Serialize]constantPlaceholder
                  [End][Serialize][Constant with store]
                [End][Serialize]Value [toString=SInt; opCode=0x00]
              [End][Serialize]right
            [End][Serialize]TwoArguments
          [End][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
        [End][Serialize][Non-constant]
      [End][Serialize]Value [toString=SInt; opCode=0x9A]
    [End][Serialize]right
  [End][Serialize][Not BooleanConstants]
[End][Serialize]Relation2
6. Quadruple
[Start][Serialize]Quadruple
  [Start][Serialize]first
    [Start][Serialize]Value [toString=SBoolean; opCode=0x8F]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x8F; Serializer=Relation2Serializer(-113,sigmastate.serialization.ValueSerializer$$$Lambda$168/0x00000008401b5040@2a19a0fe)]
          [Start][Serialize]Relation2
            [Start][Serialize][Not BooleanConstants]
              [Start][Serialize]left
                [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                  [Start][Serialize][Non-constant]
                    [Start][Serialize]opCode
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]opCode
                    [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                      [Start][Serialize]CaseObject
                      [End][Serialize]CaseObject
                    [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                  [End][Serialize][Non-constant]
                [End][Serialize]Value [toString=SInt; opCode=0xA3]
              [End][Serialize]left
              [Start][Serialize]right
                [Start][Serialize]Value [toString=SInt; opCode=0x00]
                  [Start][Serialize][Constant with store]
                    [Start][Serialize]placeholder.opCode
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]placeholder.opCode
                    [Start][Serialize]constantPlaceholder
                      [Start][Serialize]ConstantPlaceholder
                        [Start][Serialize]id
                          [Start Real][Serialize]Put UInt
                            [Start Real][Serialize]Put ULong
                              [Start][Serialize]VLQLong
                              [End][Serialize]VLQLong
                            [End Real][Serialize]Put ULong
                          [End Real][Serialize]Put UInt
                        [End][Serialize]id
                      [End][Serialize]ConstantPlaceholder
                    [End][Serialize]constantPlaceholder
                  [End][Serialize][Constant with store]
                [End][Serialize]Value [toString=SInt; opCode=0x00]
              [End][Serialize]right
            [End][Serialize][Not BooleanConstants]
          [End][Serialize]Relation2
        [End][Serialize]Body [opCode=0x8F; Serializer=Relation2Serializer(-113,sigmastate.serialization.ValueSerializer$$$Lambda$168/0x00000008401b5040@2a19a0fe)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SBoolean; opCode=0x8F]
  [End][Serialize]first
  [Start][Serialize]second
    [Start][Serialize]Value [toString=SLong; opCode=0x9A]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
          [Start][Serialize]TwoArguments
            [Start][Serialize]left
              [Start][Serialize]Value [toString=SLong; opCode=0x00]
                [Start][Serialize][Constant with store]
                  [Start][Serialize]placeholder.opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]placeholder.opCode
                  [Start][Serialize]constantPlaceholder
                    [Start][Serialize]ConstantPlaceholder
                      [Start][Serialize]id
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]id
                    [End][Serialize]ConstantPlaceholder
                  [End][Serialize]constantPlaceholder
                [End][Serialize][Constant with store]
              [End][Serialize]Value [toString=SLong; opCode=0x00]
            [End][Serialize]left
            [Start][Serialize]right
              [Start][Serialize]Value [toString=SLong; opCode=0x9C]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
                    [Start][Serialize]TwoArguments
                      [Start][Serialize]left
                        [Start][Serialize]Value [toString=SLong; opCode=0x00]
                          [Start][Serialize][Constant with store]
                            [Start][Serialize]placeholder.opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]placeholder.opCode
                            [Start][Serialize]constantPlaceholder
                              [Start][Serialize]ConstantPlaceholder
                                [Start][Serialize]id
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]id
                              [End][Serialize]ConstantPlaceholder
                            [End][Serialize]constantPlaceholder
                          [End][Serialize][Constant with store]
                        [End][Serialize]Value [toString=SLong; opCode=0x00]
                      [End][Serialize]left
                      [Start][Serialize]right
                        [Start][Serialize]Value [toString=SLong; opCode=0x7E]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x7E; Serializer=NumericCastSerializer(126,sigmastate.serialization.ValueSerializer$$$Lambda$222/0x0000000840427040@7cfb8e98)]
                              [Start][Serialize]NumericCast
                                [Start][Serialize]input
                                  [Start][Serialize]Value [toString=SInt; opCode=0x99]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x99; Serializer=TwoArgumentsSerializer(-103,sigmastate.serialization.ValueSerializer$$$Lambda$180/0x0000000840403040@4b5ad306)]
                                        [Start][Serialize]TwoArguments
                                          [Start][Serialize]left
                                            [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                              [Start][Serialize][Constant with store]
                                                [Start][Serialize]placeholder.opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]placeholder.opCode
                                                [Start][Serialize]constantPlaceholder
                                                  [Start][Serialize]ConstantPlaceholder
                                                    [Start][Serialize]id
                                                      [Start Real][Serialize]Put UInt
                                                        [Start Real][Serialize]Put ULong
                                                          [Start][Serialize]VLQLong
                                                          [End][Serialize]VLQLong
                                                        [End Real][Serialize]Put ULong
                                                      [End Real][Serialize]Put UInt
                                                    [End][Serialize]id
                                                  [End][Serialize]ConstantPlaceholder
                                                [End][Serialize]constantPlaceholder
                                              [End][Serialize][Constant with store]
                                            [End][Serialize]Value [toString=SInt; opCode=0x00]
                                          [End][Serialize]left
                                          [Start][Serialize]right
                                            [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                                              [Start][Serialize][Non-constant]
                                                [Start][Serialize]opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]opCode
                                                [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                  [Start][Serialize]CaseObject
                                                  [End][Serialize]CaseObject
                                                [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                              [End][Serialize][Non-constant]
                                            [End][Serialize]Value [toString=SInt; opCode=0xA3]
                                          [End][Serialize]right
                                        [End][Serialize]TwoArguments
                                      [End][Serialize]Body [opCode=0x99; Serializer=TwoArgumentsSerializer(-103,sigmastate.serialization.ValueSerializer$$$Lambda$180/0x0000000840403040@4b5ad306)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SInt; opCode=0x99]
                                [End][Serialize]input
                                [Start][Serialize]tpe
                                  [Start][Serialize]Type [toString=SLong; typeCode=0x05]
                                    [Start Real][Serialize]Put Byte
                                    [End Real][Serialize]Put Byte
                                  [End][Serialize]Type [toString=SLong; typeCode=0x05]
                                [End][Serialize]tpe
                              [End][Serialize]NumericCast
                            [End][Serialize]Body [opCode=0x7E; Serializer=NumericCastSerializer(126,sigmastate.serialization.ValueSerializer$$$Lambda$222/0x0000000840427040@7cfb8e98)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SLong; opCode=0x7E]
                      [End][Serialize]right
                    [End][Serialize]TwoArguments
                  [End][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SLong; opCode=0x9C]
            [End][Serialize]right
          [End][Serialize]TwoArguments
        [End][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SLong; opCode=0x9A]
  [End][Serialize]second
  [Start][Serialize]third
    [Start][Serialize]Value [toString=SLong; opCode=0x95]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
          [Start][Serialize]Quadruple
            [Start][Serialize]first
              [Start][Serialize]Value [toString=SBoolean; opCode=0x8F]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x8F; Serializer=Relation2Serializer(-113,sigmastate.serialization.ValueSerializer$$$Lambda$168/0x00000008401b5040@2a19a0fe)]
                    [Start][Serialize]Relation2
                      [Start][Serialize][Not BooleanConstants]
                        [Start][Serialize]left
                          [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                [Start][Serialize]CaseObject
                                [End][Serialize]CaseObject
                              [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=SInt; opCode=0xA3]
                        [End][Serialize]left
                        [Start][Serialize]right
                          [Start][Serialize]Value [toString=SInt; opCode=0x00]
                            [Start][Serialize][Constant with store]
                              [Start][Serialize]placeholder.opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]placeholder.opCode
                              [Start][Serialize]constantPlaceholder
                                [Start][Serialize]ConstantPlaceholder
                                  [Start][Serialize]id
                                    [Start Real][Serialize]Put UInt
                                      [Start Real][Serialize]Put ULong
                                        [Start][Serialize]VLQLong
                                        [End][Serialize]VLQLong
                                      [End Real][Serialize]Put ULong
                                    [End Real][Serialize]Put UInt
                                  [End][Serialize]id
                                [End][Serialize]ConstantPlaceholder
                              [End][Serialize]constantPlaceholder
                            [End][Serialize][Constant with store]
                          [End][Serialize]Value [toString=SInt; opCode=0x00]
                        [End][Serialize]right
                      [End][Serialize][Not BooleanConstants]
                    [End][Serialize]Relation2
                  [End][Serialize]Body [opCode=0x8F; Serializer=Relation2Serializer(-113,sigmastate.serialization.ValueSerializer$$$Lambda$168/0x00000008401b5040@2a19a0fe)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBoolean; opCode=0x8F]
            [End][Serialize]first
            [Start][Serialize]second
              [Start][Serialize]Value [toString=SLong; opCode=0x9A]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                    [Start][Serialize]TwoArguments
                      [Start][Serialize]left
                        [Start][Serialize]Value [toString=SLong; opCode=0x00]
                          [Start][Serialize][Constant with store]
                            [Start][Serialize]placeholder.opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]placeholder.opCode
                            [Start][Serialize]constantPlaceholder
                              [Start][Serialize]ConstantPlaceholder
                                [Start][Serialize]id
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]id
                              [End][Serialize]ConstantPlaceholder
                            [End][Serialize]constantPlaceholder
                          [End][Serialize][Constant with store]
                        [End][Serialize]Value [toString=SLong; opCode=0x00]
                      [End][Serialize]left
                      [Start][Serialize]right
                        [Start][Serialize]Value [toString=SLong; opCode=0x9C]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
                              [Start][Serialize]TwoArguments
                                [Start][Serialize]left
                                  [Start][Serialize]Value [toString=SLong; opCode=0x00]
                                    [Start][Serialize][Constant with store]
                                      [Start][Serialize]placeholder.opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]placeholder.opCode
                                      [Start][Serialize]constantPlaceholder
                                        [Start][Serialize]ConstantPlaceholder
                                          [Start][Serialize]id
                                            [Start Real][Serialize]Put UInt
                                              [Start Real][Serialize]Put ULong
                                                [Start][Serialize]VLQLong
                                                [End][Serialize]VLQLong
                                              [End Real][Serialize]Put ULong
                                            [End Real][Serialize]Put UInt
                                          [End][Serialize]id
                                        [End][Serialize]ConstantPlaceholder
                                      [End][Serialize]constantPlaceholder
                                    [End][Serialize][Constant with store]
                                  [End][Serialize]Value [toString=SLong; opCode=0x00]
                                [End][Serialize]left
                                [Start][Serialize]right
                                  [Start][Serialize]Value [toString=SLong; opCode=0x7E]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x7E; Serializer=NumericCastSerializer(126,sigmastate.serialization.ValueSerializer$$$Lambda$222/0x0000000840427040@7cfb8e98)]
                                        [Start][Serialize]NumericCast
                                          [Start][Serialize]input
                                            [Start][Serialize]Value [toString=SInt; opCode=0x99]
                                              [Start][Serialize][Non-constant]
                                                [Start][Serialize]opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]opCode
                                                [Start][Serialize]Body [opCode=0x99; Serializer=TwoArgumentsSerializer(-103,sigmastate.serialization.ValueSerializer$$$Lambda$180/0x0000000840403040@4b5ad306)]
                                                  [Start][Serialize]TwoArguments
                                                    [Start][Serialize]left
                                                      [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                                        [Start][Serialize][Constant with store]
                                                          [Start][Serialize]placeholder.opCode
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]placeholder.opCode
                                                          [Start][Serialize]constantPlaceholder
                                                            [Start][Serialize]ConstantPlaceholder
                                                              [Start][Serialize]id
                                                                [Start Real][Serialize]Put UInt
                                                                  [Start Real][Serialize]Put ULong
                                                                    [Start][Serialize]VLQLong
                                                                    [End][Serialize]VLQLong
                                                                  [End Real][Serialize]Put ULong
                                                                [End Real][Serialize]Put UInt
                                                              [End][Serialize]id
                                                            [End][Serialize]ConstantPlaceholder
                                                          [End][Serialize]constantPlaceholder
                                                        [End][Serialize][Constant with store]
                                                      [End][Serialize]Value [toString=SInt; opCode=0x00]
                                                    [End][Serialize]left
                                                    [Start][Serialize]right
                                                      [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                                                        [Start][Serialize][Non-constant]
                                                          [Start][Serialize]opCode
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]opCode
                                                          [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                            [Start][Serialize]CaseObject
                                                            [End][Serialize]CaseObject
                                                          [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                        [End][Serialize][Non-constant]
                                                      [End][Serialize]Value [toString=SInt; opCode=0xA3]
                                                    [End][Serialize]right
                                                  [End][Serialize]TwoArguments
                                                [End][Serialize]Body [opCode=0x99; Serializer=TwoArgumentsSerializer(-103,sigmastate.serialization.ValueSerializer$$$Lambda$180/0x0000000840403040@4b5ad306)]
                                              [End][Serialize][Non-constant]
                                            [End][Serialize]Value [toString=SInt; opCode=0x99]
                                          [End][Serialize]input
                                          [Start][Serialize]tpe
                                            [Start][Serialize]Type [toString=SLong; typeCode=0x05]
                                              [Start Real][Serialize]Put Byte
                                              [End Real][Serialize]Put Byte
                                            [End][Serialize]Type [toString=SLong; typeCode=0x05]
                                          [End][Serialize]tpe
                                        [End][Serialize]NumericCast
                                      [End][Serialize]Body [opCode=0x7E; Serializer=NumericCastSerializer(126,sigmastate.serialization.ValueSerializer$$$Lambda$222/0x0000000840427040@7cfb8e98)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SLong; opCode=0x7E]
                                [End][Serialize]right
                              [End][Serialize]TwoArguments
                            [End][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SLong; opCode=0x9C]
                      [End][Serialize]right
                    [End][Serialize]TwoArguments
                  [End][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SLong; opCode=0x9A]
            [End][Serialize]second
            [Start][Serialize]third
              [Start][Serialize]Value [toString=SLong; opCode=0x95]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                    [Start][Serialize]Quadruple
                      [Start][Serialize]first
                        [Start][Serialize]Value [toString=SBoolean; opCode=0x8F]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x8F; Serializer=Relation2Serializer(-113,sigmastate.serialization.ValueSerializer$$$Lambda$168/0x00000008401b5040@2a19a0fe)]
                              [Start][Serialize]Relation2
                                [Start][Serialize][Not BooleanConstants]
                                  [Start][Serialize]left
                                    [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                          [Start][Serialize]CaseObject
                                          [End][Serialize]CaseObject
                                        [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SInt; opCode=0xA3]
                                  [End][Serialize]left
                                  [Start][Serialize]right
                                    [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                      [Start][Serialize][Constant with store]
                                        [Start][Serialize]placeholder.opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]placeholder.opCode
                                        [Start][Serialize]constantPlaceholder
                                          [Start][Serialize]ConstantPlaceholder
                                            [Start][Serialize]id
                                              [Start Real][Serialize]Put UInt
                                                [Start Real][Serialize]Put ULong
                                                  [Start][Serialize]VLQLong
                                                  [End][Serialize]VLQLong
                                                [End Real][Serialize]Put ULong
                                              [End Real][Serialize]Put UInt
                                            [End][Serialize]id
                                          [End][Serialize]ConstantPlaceholder
                                        [End][Serialize]constantPlaceholder
                                      [End][Serialize][Constant with store]
                                    [End][Serialize]Value [toString=SInt; opCode=0x00]
                                  [End][Serialize]right
                                [End][Serialize][Not BooleanConstants]
                              [End][Serialize]Relation2
                            [End][Serialize]Body [opCode=0x8F; Serializer=Relation2Serializer(-113,sigmastate.serialization.ValueSerializer$$$Lambda$168/0x00000008401b5040@2a19a0fe)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SBoolean; opCode=0x8F]
                      [End][Serialize]first
                      [Start][Serialize]second
                        [Start][Serialize]Value [toString=SLong; opCode=0x9C]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
                              [Start][Serialize]TwoArguments
                                [Start][Serialize]left
                                  [Start][Serialize]Value [toString=SLong; opCode=0x00]
                                    [Start][Serialize][Constant with store]
                                      [Start][Serialize]placeholder.opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]placeholder.opCode
                                      [Start][Serialize]constantPlaceholder
                                        [Start][Serialize]ConstantPlaceholder
                                          [Start][Serialize]id
                                            [Start Real][Serialize]Put UInt
                                              [Start Real][Serialize]Put ULong
                                                [Start][Serialize]VLQLong
                                                [End][Serialize]VLQLong
                                              [End Real][Serialize]Put ULong
                                            [End Real][Serialize]Put UInt
                                          [End][Serialize]id
                                        [End][Serialize]ConstantPlaceholder
                                      [End][Serialize]constantPlaceholder
                                    [End][Serialize][Constant with store]
                                  [End][Serialize]Value [toString=SLong; opCode=0x00]
                                [End][Serialize]left
                                [Start][Serialize]right
                                  [Start][Serialize]Value [toString=SLong; opCode=0x7E]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x7E; Serializer=NumericCastSerializer(126,sigmastate.serialization.ValueSerializer$$$Lambda$222/0x0000000840427040@7cfb8e98)]
                                        [Start][Serialize]NumericCast
                                          [Start][Serialize]input
                                            [Start][Serialize]Value [toString=SInt; opCode=0x99]
                                              [Start][Serialize][Non-constant]
                                                [Start][Serialize]opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]opCode
                                                [Start][Serialize]Body [opCode=0x99; Serializer=TwoArgumentsSerializer(-103,sigmastate.serialization.ValueSerializer$$$Lambda$180/0x0000000840403040@4b5ad306)]
                                                  [Start][Serialize]TwoArguments
                                                    [Start][Serialize]left
                                                      [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                                        [Start][Serialize][Constant with store]
                                                          [Start][Serialize]placeholder.opCode
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]placeholder.opCode
                                                          [Start][Serialize]constantPlaceholder
                                                            [Start][Serialize]ConstantPlaceholder
                                                              [Start][Serialize]id
                                                                [Start Real][Serialize]Put UInt
                                                                  [Start Real][Serialize]Put ULong
                                                                    [Start][Serialize]VLQLong
                                                                    [End][Serialize]VLQLong
                                                                  [End Real][Serialize]Put ULong
                                                                [End Real][Serialize]Put UInt
                                                              [End][Serialize]id
                                                            [End][Serialize]ConstantPlaceholder
                                                          [End][Serialize]constantPlaceholder
                                                        [End][Serialize][Constant with store]
                                                      [End][Serialize]Value [toString=SInt; opCode=0x00]
                                                    [End][Serialize]left
                                                    [Start][Serialize]right
                                                      [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                                                        [Start][Serialize][Non-constant]
                                                          [Start][Serialize]opCode
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]opCode
                                                          [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                            [Start][Serialize]CaseObject
                                                            [End][Serialize]CaseObject
                                                          [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                        [End][Serialize][Non-constant]
                                                      [End][Serialize]Value [toString=SInt; opCode=0xA3]
                                                    [End][Serialize]right
                                                  [End][Serialize]TwoArguments
                                                [End][Serialize]Body [opCode=0x99; Serializer=TwoArgumentsSerializer(-103,sigmastate.serialization.ValueSerializer$$$Lambda$180/0x0000000840403040@4b5ad306)]
                                              [End][Serialize][Non-constant]
                                            [End][Serialize]Value [toString=SInt; opCode=0x99]
                                          [End][Serialize]input
                                          [Start][Serialize]tpe
                                            [Start][Serialize]Type [toString=SLong; typeCode=0x05]
                                              [Start Real][Serialize]Put Byte
                                              [End Real][Serialize]Put Byte
                                            [End][Serialize]Type [toString=SLong; typeCode=0x05]
                                          [End][Serialize]tpe
                                        [End][Serialize]NumericCast
                                      [End][Serialize]Body [opCode=0x7E; Serializer=NumericCastSerializer(126,sigmastate.serialization.ValueSerializer$$$Lambda$222/0x0000000840427040@7cfb8e98)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SLong; opCode=0x7E]
                                [End][Serialize]right
                              [End][Serialize]TwoArguments
                            [End][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SLong; opCode=0x9C]
                      [End][Serialize]second
                      [Start][Serialize]third
                        [Start][Serialize]Value [toString=SLong; opCode=0x00]
                          [Start][Serialize][Constant with store]
                            [Start][Serialize]placeholder.opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]placeholder.opCode
                            [Start][Serialize]constantPlaceholder
                              [Start][Serialize]ConstantPlaceholder
                                [Start][Serialize]id
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]id
                              [End][Serialize]ConstantPlaceholder
                            [End][Serialize]constantPlaceholder
                          [End][Serialize][Constant with store]
                        [End][Serialize]Value [toString=SLong; opCode=0x00]
                      [End][Serialize]third
                    [End][Serialize]Quadruple
                  [End][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SLong; opCode=0x95]
            [End][Serialize]third
          [End][Serialize]Quadruple
        [End][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SLong; opCode=0x95]
  [End][Serialize]third
[End][Serialize]Quadruple
7. TwoArguments
[Start][Serialize]TwoArguments
  [Start][Serialize]left
    [Start][Serialize]Value [toString=SInt; opCode=0x8C]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
          [Start][Serialize]SelectField
            [Start][Serialize]input
              [Start][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                    [Start][Serialize]SimpleTransformer
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                              [Start][Serialize]CaseObject
                              [End][Serialize]CaseObject
                            [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SBox; opCode=0xA7]
                      [End][Serialize]input
                    [End][Serialize]SimpleTransformer
                  [End][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
            [End][Serialize]input
            [Start][Serialize]fieldIndex
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]fieldIndex
          [End][Serialize]SelectField
        [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SInt; opCode=0x8C]
  [End][Serialize]left
  [Start][Serialize]right
    [Start][Serialize]Value [toString=SInt; opCode=0x00]
      [Start][Serialize][Constant with store]
        [Start][Serialize]placeholder.opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]placeholder.opCode
        [Start][Serialize]constantPlaceholder
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]constantPlaceholder
      [End][Serialize][Constant with store]
    [End][Serialize]Value [toString=SInt; opCode=0x00]
  [End][Serialize]right
[End][Serialize]TwoArguments
8. ProveDHTuple
[Start][Serialize]ProveDHTuple
  [Start][Serialize][SGroupElementConstants]
    [Start][Serialize]constCodePrefix
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]constCodePrefix
    [Start][Serialize]SGroupElementType
      [Start][Serialize]Type [toString=SGroupElement; typeCode=0x07]
        [Start Real][Serialize]Put Byte
        [End Real][Serialize]Put Byte
      [End][Serialize]Type [toString=SGroupElement; typeCode=0x07]
    [End][Serialize]SGroupElementType
    [Start][Serialize]gv_data
      [Start][Serialize]GroupElementSerializer
        [Start Real][Serialize]Put Bytes
        [End Real][Serialize]Put Bytes
      [End][Serialize]GroupElementSerializer
    [End][Serialize]gv_data
    [Start][Serialize]hv_data
      [Start][Serialize]GroupElementSerializer
        [Start Real][Serialize]Put Bytes
        [End Real][Serialize]Put Bytes
      [End][Serialize]GroupElementSerializer
    [End][Serialize]hv_data
    [Start][Serialize]uv_data
      [Start][Serialize]GroupElementSerializer
        [Start Real][Serialize]Put Bytes
        [End Real][Serialize]Put Bytes
      [End][Serialize]GroupElementSerializer
    [End][Serialize]uv_data
    [Start][Serialize]vv_data
      [Start][Serialize]GroupElementSerializer
        [Start Real][Serialize]Put Bytes
        [End Real][Serialize]Put Bytes
      [End][Serialize]GroupElementSerializer
    [End][Serialize]vv_data
  [End][Serialize][SGroupElementConstants]
[End][Serialize]ProveDHTuple
9. ProveDlog
[Start][Serialize]ProveDlog
  [Start][Serialize]value
    [Start][Serialize]Value [toString=SGroupElement; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SGroupElement; typeCode=0x07]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SGroupElement; typeCode=0x07]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start][Serialize]GroupElementSerializer
              [Start Real][Serialize]Put Bytes
              [End Real][Serialize]Put Bytes
            [End][Serialize]GroupElementSerializer
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SGroupElement; opCode=0x00]
  [End][Serialize]value
[End][Serialize]ProveDlog
10. CaseObject
[Start][Serialize]CaseObject
[End][Serialize]CaseObject
11. SigmaPropIsProven
[Start][Serialize]SigmaPropIsProven
  [Start][Serialize]input
    [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
          [Start][Serialize]SigmaTransformer
            [Start][Serialize]items.length
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]items.length
            [Start][Serialize]items*
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                    [Start][Serialize]BoolToSigmaProp
                      [Start][Serialize]value
                        [Start][Serialize]Value [toString=SBoolean; opCode=0x92]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x92; Serializer=Relation2Serializer(-110,sigmastate.serialization.ValueSerializer$$$Lambda$167/0x00000008401b5840@52066604)]
                              [Start][Serialize]Relation2
                                [Start][Serialize][Not BooleanConstants]
                                  [Start][Serialize]left
                                    [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                          [Start][Serialize]CaseObject
                                          [End][Serialize]CaseObject
                                        [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SInt; opCode=0xA3]
                                  [End][Serialize]left
                                  [Start][Serialize]right
                                    [Start][Serialize]Value [toString=SInt; opCode=0x9A]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                                          [Start][Serialize]TwoArguments
                                            [Start][Serialize]left
                                              [Start][Serialize]Value [toString=SInt; opCode=0x8C]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                    [Start][Serialize]SelectField
                                                      [Start][Serialize]input
                                                        [Start][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                                                          [Start][Serialize][Non-constant]
                                                            [Start][Serialize]opCode
                                                              [Start Real][Serialize]Put Byte
                                                              [End Real][Serialize]Put Byte
                                                            [End][Serialize]opCode
                                                            [Start][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                                              [Start][Serialize]SimpleTransformer
                                                                [Start][Serialize]input
                                                                  [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                                                        [Start][Serialize]CaseObject
                                                                        [End][Serialize]CaseObject
                                                                      [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=SBox; opCode=0xA7]
                                                                [End][Serialize]input
                                                              [End][Serialize]SimpleTransformer
                                                            [End][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                                          [End][Serialize][Non-constant]
                                                        [End][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                                                      [End][Serialize]input
                                                      [Start][Serialize]fieldIndex
                                                        [Start Real][Serialize]Put Byte
                                                        [End Real][Serialize]Put Byte
                                                      [End][Serialize]fieldIndex
                                                    [End][Serialize]SelectField
                                                  [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=SInt; opCode=0x8C]
                                            [End][Serialize]left
                                            [Start][Serialize]right
                                              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                                [Start][Serialize][Constant with store]
                                                  [Start][Serialize]placeholder.opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]placeholder.opCode
                                                  [Start][Serialize]constantPlaceholder
                                                    [Start][Serialize]ConstantPlaceholder
                                                      [Start][Serialize]id
                                                        [Start Real][Serialize]Put UInt
                                                          [Start Real][Serialize]Put ULong
                                                            [Start][Serialize]VLQLong
                                                            [End][Serialize]VLQLong
                                                          [End Real][Serialize]Put ULong
                                                        [End Real][Serialize]Put UInt
                                                      [End][Serialize]id
                                                    [End][Serialize]ConstantPlaceholder
                                                  [End][Serialize]constantPlaceholder
                                                [End][Serialize][Constant with store]
                                              [End][Serialize]Value [toString=SInt; opCode=0x00]
                                            [End][Serialize]right
                                          [End][Serialize]TwoArguments
                                        [End][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SInt; opCode=0x9A]
                                  [End][Serialize]right
                                [End][Serialize][Not BooleanConstants]
                              [End][Serialize]Relation2
                            [End][Serialize]Body [opCode=0x92; Serializer=Relation2Serializer(-110,sigmastate.serialization.ValueSerializer$$$Lambda$167/0x00000008401b5840@52066604)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SBoolean; opCode=0x92]
                      [End][Serialize]value
                    [End][Serialize]BoolToSigmaProp
                  [End][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                [Start][Serialize][Constant with store]
                  [Start][Serialize]placeholder.opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]placeholder.opCode
                  [Start][Serialize]constantPlaceholder
                    [Start][Serialize]ConstantPlaceholder
                      [Start][Serialize]id
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]id
                    [End][Serialize]ConstantPlaceholder
                  [End][Serialize]constantPlaceholder
                [End][Serialize][Constant with store]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
            [End][Serialize]items*
          [End][Serialize]SigmaTransformer
        [End][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
  [End][Serialize]input
[End][Serialize]SigmaPropIsProven
12. SigmaPropBytes
[Start][Serialize]SigmaPropBytes
  [Start][Serialize]input
    [Start][Serialize]Value [toString=SSigmaProp; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SSigmaProp; opCode=0x73]
  [End][Serialize]input
[End][Serialize]SigmaPropBytes
13. ConcreteCollectionBooleanConstant
[Start][Serialize]ConcreteCollectionBooleanConstant
  [Start][Serialize]items.size
    [Start Real][Serialize]Put UShort
      [Start Real][Serialize]Put UInt
        [Start Real][Serialize]Put ULong
          [Start][Serialize]VLQLong
          [End][Serialize]VLQLong
        [End Real][Serialize]Put ULong
      [End Real][Serialize]Put UInt
    [End Real][Serialize]Put UShort
  [End][Serialize]items.size
  [Start][Serialize]items
    [Start Real][Serialize]Put Bits
    [End Real][Serialize]Put Bits
  [End][Serialize]items
[End][Serialize]ConcreteCollectionBooleanConstant
14. ConcreteCollection
[Start][Serialize]ConcreteCollection
  [Start][Serialize]items.size
    [Start Real][Serialize]Put UShort
      [Start Real][Serialize]Put UInt
        [Start Real][Serialize]Put ULong
          [Start][Serialize]VLQLong
          [End][Serialize]VLQLong
        [End Real][Serialize]Put ULong
      [End Real][Serialize]Put UInt
    [End Real][Serialize]Put UShort
  [End][Serialize]items.size
  [Start][Serialize]tpe.elemType
    [Start][Serialize]Type [toString=SByte; typeCode=0x02]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SByte; typeCode=0x02]
  [End][Serialize]tpe.elemType
  [Start][Serialize]items*
    [Start][Serialize]Value [toString=SByte; opCode=0x72]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
          [Start][Serialize]ValUse
            [Start][Serialize]valId
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]valId
          [End][Serialize]ValUse
        [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SByte; opCode=0x72]
  [End][Serialize]items*
[End][Serialize]ConcreteCollection
15. LogicalTransformer
[Start][Serialize]LogicalTransformer
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SBoolean]; opCode=0x83]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
          [Start][Serialize]ConcreteCollection
            [Start][Serialize]items.size
              [Start Real][Serialize]Put UShort
                [Start Real][Serialize]Put UInt
                  [Start Real][Serialize]Put ULong
                    [Start][Serialize]VLQLong
                    [End][Serialize]VLQLong
                  [End Real][Serialize]Put ULong
                [End Real][Serialize]Put UInt
              [End Real][Serialize]Put UShort
            [End][Serialize]items.size
            [Start][Serialize]tpe.elemType
              [Start][Serialize]Type [toString=SBoolean; typeCode=0x01]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SBoolean; typeCode=0x01]
            [End][Serialize]tpe.elemType
            [Start][Serialize]items*
              [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                    [Start][Serialize]Relation2
                      [Start][Serialize][Not BooleanConstants]
                        [Start][Serialize]left
                          [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                [Start][Serialize]CaseObject
                                [End][Serialize]CaseObject
                              [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=SInt; opCode=0xA3]
                        [End][Serialize]left
                        [Start][Serialize]right
                          [Start][Serialize]Value [toString=SInt; opCode=0x8C]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                [Start][Serialize]SelectField
                                  [Start][Serialize]input
                                    [Start][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                          [Start][Serialize]SimpleTransformer
                                            [Start][Serialize]input
                                              [Start][Serialize]Value [toString=SBox; opCode=0xB2]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0xB2; Serializer=ByIndexSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$220/0x0000000840425440@3bcbb589)]
                                                    [Start][Serialize]ByIndex
                                                      [Start][Serialize]input
                                                        [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                                                          [Start][Serialize][Non-constant]
                                                            [Start][Serialize]opCode
                                                              [Start Real][Serialize]Put Byte
                                                              [End Real][Serialize]Put Byte
                                                            [End][Serialize]opCode
                                                            [Start][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                                                              [Start][Serialize]CaseObject
                                                              [End][Serialize]CaseObject
                                                            [End][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                                                          [End][Serialize][Non-constant]
                                                        [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                                                      [End][Serialize]input
                                                      [Start][Serialize]index
                                                        [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                                          [Start][Serialize][Constant with store]
                                                            [Start][Serialize]placeholder.opCode
                                                              [Start Real][Serialize]Put Byte
                                                              [End Real][Serialize]Put Byte
                                                            [End][Serialize]placeholder.opCode
                                                            [Start][Serialize]constantPlaceholder
                                                              [Start][Serialize]ConstantPlaceholder
                                                                [Start][Serialize]id
                                                                  [Start Real][Serialize]Put UInt
                                                                    [Start Real][Serialize]Put ULong
                                                                      [Start][Serialize]VLQLong
                                                                      [End][Serialize]VLQLong
                                                                    [End Real][Serialize]Put ULong
                                                                  [End Real][Serialize]Put UInt
                                                                [End][Serialize]id
                                                              [End][Serialize]ConstantPlaceholder
                                                            [End][Serialize]constantPlaceholder
                                                          [End][Serialize][Constant with store]
                                                        [End][Serialize]Value [toString=SInt; opCode=0x00]
                                                      [End][Serialize]index
                                                      [Start][Serialize]default
                                                        [Start Real][Serialize]Put Option
                                                        [End Real][Serialize]Put Option
                                                      [End][Serialize]default
                                                    [End][Serialize]ByIndex
                                                  [End][Serialize]Body [opCode=0xB2; Serializer=ByIndexSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$220/0x0000000840425440@3bcbb589)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=SBox; opCode=0xB2]
                                            [End][Serialize]input
                                          [End][Serialize]SimpleTransformer
                                        [End][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                                  [End][Serialize]input
                                  [Start][Serialize]fieldIndex
                                    [Start Real][Serialize]Put Byte
                                    [End Real][Serialize]Put Byte
                                  [End][Serialize]fieldIndex
                                [End][Serialize]SelectField
                              [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=SInt; opCode=0x8C]
                        [End][Serialize]right
                      [End][Serialize][Not BooleanConstants]
                    [End][Serialize]Relation2
                  [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBoolean; opCode=0x93]
              [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                    [Start][Serialize]Relation2
                      [Start][Serialize][Not BooleanConstants]
                        [Start][Serialize]left
                          [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xC2]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xC2; Serializer=SimpleTransformerSerializer(-62,sigmastate.serialization.ValueSerializer$$$Lambda$201/0x000000084041a040@53d102a2)]
                                [Start][Serialize]SimpleTransformer
                                  [Start][Serialize]input
                                    [Start][Serialize]Value [toString=SBox; opCode=0xB2]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0xB2; Serializer=ByIndexSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$220/0x0000000840425440@3bcbb589)]
                                          [Start][Serialize]ByIndex
                                            [Start][Serialize]input
                                              [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                                                    [Start][Serialize]CaseObject
                                                    [End][Serialize]CaseObject
                                                  [End][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                                            [End][Serialize]input
                                            [Start][Serialize]index
                                              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                                [Start][Serialize][Constant with store]
                                                  [Start][Serialize]placeholder.opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]placeholder.opCode
                                                  [Start][Serialize]constantPlaceholder
                                                    [Start][Serialize]ConstantPlaceholder
                                                      [Start][Serialize]id
                                                        [Start Real][Serialize]Put UInt
                                                          [Start Real][Serialize]Put ULong
                                                            [Start][Serialize]VLQLong
                                                            [End][Serialize]VLQLong
                                                          [End Real][Serialize]Put ULong
                                                        [End Real][Serialize]Put UInt
                                                      [End][Serialize]id
                                                    [End][Serialize]ConstantPlaceholder
                                                  [End][Serialize]constantPlaceholder
                                                [End][Serialize][Constant with store]
                                              [End][Serialize]Value [toString=SInt; opCode=0x00]
                                            [End][Serialize]index
                                            [Start][Serialize]default
                                              [Start Real][Serialize]Put Option
                                              [End Real][Serialize]Put Option
                                            [End][Serialize]default
                                          [End][Serialize]ByIndex
                                        [End][Serialize]Body [opCode=0xB2; Serializer=ByIndexSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$220/0x0000000840425440@3bcbb589)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SBox; opCode=0xB2]
                                  [End][Serialize]input
                                [End][Serialize]SimpleTransformer
                              [End][Serialize]Body [opCode=0xC2; Serializer=SimpleTransformerSerializer(-62,sigmastate.serialization.ValueSerializer$$$Lambda$201/0x000000084041a040@53d102a2)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=Coll[SByte]; opCode=0xC2]
                        [End][Serialize]left
                        [Start][Serialize]right
                          [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x74]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0x74; Serializer=sigmastate.serialization.SubstConstantsSerializer$@6c45ee6e]
                                [Start][Serialize]SubstConstants
                                  [Start][Serialize]scriptBytes
                                    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
                                      [Start][Serialize][Constant with store]
                                        [Start][Serialize]placeholder.opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]placeholder.opCode
                                        [Start][Serialize]constantPlaceholder
                                          [Start][Serialize]ConstantPlaceholder
                                            [Start][Serialize]id
                                              [Start Real][Serialize]Put UInt
                                                [Start Real][Serialize]Put ULong
                                                  [Start][Serialize]VLQLong
                                                  [End][Serialize]VLQLong
                                                [End Real][Serialize]Put ULong
                                              [End Real][Serialize]Put UInt
                                            [End][Serialize]id
                                          [End][Serialize]ConstantPlaceholder
                                        [End][Serialize]constantPlaceholder
                                      [End][Serialize][Constant with store]
                                    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
                                  [End][Serialize]scriptBytes
                                  [Start][Serialize]positions
                                    [Start][Serialize]Value [toString=Coll[SInt]; opCode=0x00]
                                      [Start][Serialize][Constant with store]
                                        [Start][Serialize]placeholder.opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]placeholder.opCode
                                        [Start][Serialize]constantPlaceholder
                                          [Start][Serialize]ConstantPlaceholder
                                            [Start][Serialize]id
                                              [Start Real][Serialize]Put UInt
                                                [Start Real][Serialize]Put ULong
                                                  [Start][Serialize]VLQLong
                                                  [End][Serialize]VLQLong
                                                [End Real][Serialize]Put ULong
                                              [End Real][Serialize]Put UInt
                                            [End][Serialize]id
                                          [End][Serialize]ConstantPlaceholder
                                        [End][Serialize]constantPlaceholder
                                      [End][Serialize][Constant with store]
                                    [End][Serialize]Value [toString=Coll[SInt]; opCode=0x00]
                                  [End][Serialize]positions
                                  [Start][Serialize]newValues
                                    [Start][Serialize]Value [toString=Coll[SSigmaProp]; opCode=0x83]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
                                          [Start][Serialize]ConcreteCollection
                                            [Start][Serialize]items.size
                                              [Start Real][Serialize]Put UShort
                                                [Start Real][Serialize]Put UInt
                                                  [Start Real][Serialize]Put ULong
                                                    [Start][Serialize]VLQLong
                                                    [End][Serialize]VLQLong
                                                  [End Real][Serialize]Put ULong
                                                [End Real][Serialize]Put UInt
                                              [End Real][Serialize]Put UShort
                                            [End][Serialize]items.size
                                            [Start][Serialize]tpe.elemType
                                              [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                                                [Start Real][Serialize]Put Byte
                                                [End Real][Serialize]Put Byte
                                              [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                                            [End][Serialize]tpe.elemType
                                            [Start][Serialize]items*
                                              [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                                                [Start][Serialize][Constant with store]
                                                  [Start][Serialize]placeholder.opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]placeholder.opCode
                                                  [Start][Serialize]constantPlaceholder
                                                    [Start][Serialize]ConstantPlaceholder
                                                      [Start][Serialize]id
                                                        [Start Real][Serialize]Put UInt
                                                          [Start Real][Serialize]Put ULong
                                                            [Start][Serialize]VLQLong
                                                            [End][Serialize]VLQLong
                                                          [End Real][Serialize]Put ULong
                                                        [End Real][Serialize]Put UInt
                                                      [End][Serialize]id
                                                    [End][Serialize]ConstantPlaceholder
                                                  [End][Serialize]constantPlaceholder
                                                [End][Serialize][Constant with store]
                                              [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                                            [End][Serialize]items*
                                          [End][Serialize]ConcreteCollection
                                        [End][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=Coll[SSigmaProp]; opCode=0x83]
                                  [End][Serialize]newValues
                                [End][Serialize]SubstConstants
                              [End][Serialize]Body [opCode=0x74; Serializer=sigmastate.serialization.SubstConstantsSerializer$@6c45ee6e]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=Coll[SByte]; opCode=0x74]
                        [End][Serialize]right
                      [End][Serialize][Not BooleanConstants]
                    [End][Serialize]Relation2
                  [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBoolean; opCode=0x93]
              [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                    [Start][Serialize]Relation2
                      [Start][Serialize][Not BooleanConstants]
                        [Start][Serialize]left
                          [Start][Serialize]Value [toString=SInt; opCode=0xB1]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xB1; Serializer=SimpleTransformerSerializer(-79,sigmastate.serialization.ValueSerializer$$$Lambda$199/0x0000000840418840@6b3e12b5)]
                                [Start][Serialize]SimpleTransformer
                                  [Start][Serialize]input
                                    [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                                          [Start][Serialize]CaseObject
                                          [End][Serialize]CaseObject
                                        [End][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                                  [End][Serialize]input
                                [End][Serialize]SimpleTransformer
                              [End][Serialize]Body [opCode=0xB1; Serializer=SimpleTransformerSerializer(-79,sigmastate.serialization.ValueSerializer$$$Lambda$199/0x0000000840418840@6b3e12b5)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=SInt; opCode=0xB1]
                        [End][Serialize]left
                        [Start][Serialize]right
                          [Start][Serialize]Value [toString=SInt; opCode=0x00]
                            [Start][Serialize][Constant with store]
                              [Start][Serialize]placeholder.opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]placeholder.opCode
                              [Start][Serialize]constantPlaceholder
                                [Start][Serialize]ConstantPlaceholder
                                  [Start][Serialize]id
                                    [Start Real][Serialize]Put UInt
                                      [Start Real][Serialize]Put ULong
                                        [Start][Serialize]VLQLong
                                        [End][Serialize]VLQLong
                                      [End Real][Serialize]Put ULong
                                    [End Real][Serialize]Put UInt
                                  [End][Serialize]id
                                [End][Serialize]ConstantPlaceholder
                              [End][Serialize]constantPlaceholder
                            [End][Serialize][Constant with store]
                          [End][Serialize]Value [toString=SInt; opCode=0x00]
                        [End][Serialize]right
                      [End][Serialize][Not BooleanConstants]
                    [End][Serialize]Relation2
                  [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBoolean; opCode=0x93]
            [End][Serialize]items*
          [End][Serialize]ConcreteCollection
        [End][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SBoolean]; opCode=0x83]
  [End][Serialize]input
[End][Serialize]LogicalTransformer
16. TaggedVariable
[Start][Serialize]TaggedVariable
  [Start][Serialize]varId
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]varId
  [Start][Serialize]tpe
    [Start][Serialize]Type [toString=SLong; typeCode=0x05]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SLong; typeCode=0x05]
  [End][Serialize]tpe
[End][Serialize]TaggedVariable
17. GetVar
[Start][Serialize]GetVar
  [Start][Serialize]varId
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]varId
  [Start][Serialize]tpe.elemType
    [Start][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
  [End][Serialize]tpe.elemType
[End][Serialize]GetVar
18. MapCollection
[Start][Serialize]MapCollection
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA4]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xA4; Serializer=CaseObjectSerialization(-92,Inputs)]
          [Start][Serialize]CaseObject
          [End][Serialize]CaseObject
        [End][Serialize]Body [opCode=0xA4; Serializer=CaseObjectSerialization(-92,Inputs)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA4]
  [End][Serialize]input
  [Start][Serialize]mapper
    [Start][Serialize]Value [toString=(SBox) => SLong; opCode=0xD9]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
          [Start][Serialize]FuncValue
            [Start][Serialize]args.length
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]args.length
            [Start][Serialize](args.idx, args.tpe)*
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
              [Start][Serialize]Type [toString=SBox; typeCode=0x63]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SBox; typeCode=0x63]
            [End][Serialize](args.idx, args.tpe)*
            [Start][Serialize]body
              [Start][Serialize]Value [toString=SLong; opCode=0xDA]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xDA; Serializer=ApplySerializer(sigmastate.serialization.ValueSerializer$$$Lambda$227/0x000000084042b040@79308a2)]
                    [Start][Serialize]Apply
                      [Start][Serialize]func
                        [Start][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                              [Start][Serialize]ValUse
                                [Start][Serialize]valId
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]valId
                              [End][Serialize]ValUse
                            [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
                      [End][Serialize]func
                      [Start][Serialize]args
                        [Start][Serialize]Values
                          [Start][Serialize]length
                            [Start Real][Serialize]Put UInt
                              [Start Real][Serialize]Put ULong
                                [Start][Serialize]VLQLong
                                [End][Serialize]VLQLong
                              [End Real][Serialize]Put ULong
                            [End Real][Serialize]Put UInt
                          [End][Serialize]length
                          [Start][Serialize]values*
                            [Start][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
                              [Start][Serialize][Non-constant]
                                [Start][Serialize]opCode
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]opCode
                                [Start][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
                                  [Start][Serialize]MapCollection
                                    [Start][Serialize]input
                                      [Start][Serialize]Value [toString=Coll[(Coll[SByte],SLong)]; opCode=0xE4]
                                        [Start][Serialize][Non-constant]
                                          [Start][Serialize]opCode
                                            [Start Real][Serialize]Put Byte
                                            [End Real][Serialize]Put Byte
                                          [End][Serialize]opCode
                                          [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                            [Start][Serialize]SimpleTransformer
                                              [Start][Serialize]input
                                                [Start][Serialize]Value [toString=Option[Coll[(Coll[SByte],SLong)]]; opCode=0xC6]
                                                  [Start][Serialize][Non-constant]
                                                    [Start][Serialize]opCode
                                                      [Start Real][Serialize]Put Byte
                                                      [End Real][Serialize]Put Byte
                                                    [End][Serialize]opCode
                                                    [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                                      [Start][Serialize]ExtractRegisterAs
                                                        [Start][Serialize]input
                                                          [Start][Serialize]Value [toString=SBox; opCode=0x72]
                                                            [Start][Serialize][Non-constant]
                                                              [Start][Serialize]opCode
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                              [End][Serialize]opCode
                                                              [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                [Start][Serialize]ValUse
                                                                  [Start][Serialize]valId
                                                                    [Start Real][Serialize]Put UInt
                                                                      [Start Real][Serialize]Put ULong
                                                                        [Start][Serialize]VLQLong
                                                                        [End][Serialize]VLQLong
                                                                      [End Real][Serialize]Put ULong
                                                                    [End Real][Serialize]Put UInt
                                                                  [End][Serialize]valId
                                                                [End][Serialize]ValUse
                                                              [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                            [End][Serialize][Non-constant]
                                                          [End][Serialize]Value [toString=SBox; opCode=0x72]
                                                        [End][Serialize]input
                                                        [Start][Serialize]registerId.number
                                                          [Start Real][Serialize]Put Byte
                                                          [End Real][Serialize]Put Byte
                                                        [End][Serialize]registerId.number
                                                        [Start][Serialize]tpe.elemType
                                                          [Start][Serialize]Type [toString=Coll[(Coll[SByte],SLong)]; typeCode=0x0C]
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]Type [toString=Coll[(Coll[SByte],SLong)]; typeCode=0x0C]
                                                        [End][Serialize]tpe.elemType
                                                      [End][Serialize]ExtractRegisterAs
                                                    [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                                  [End][Serialize][Non-constant]
                                                [End][Serialize]Value [toString=Option[Coll[(Coll[SByte],SLong)]]; opCode=0xC6]
                                              [End][Serialize]input
                                            [End][Serialize]SimpleTransformer
                                          [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                        [End][Serialize][Non-constant]
                                      [End][Serialize]Value [toString=Coll[(Coll[SByte],SLong)]; opCode=0xE4]
                                    [End][Serialize]input
                                    [Start][Serialize]mapper
                                      [Start][Serialize]Value [toString=((Coll[SByte],SLong)) => SLong; opCode=0xD9]
                                        [Start][Serialize][Non-constant]
                                          [Start][Serialize]opCode
                                            [Start Real][Serialize]Put Byte
                                            [End Real][Serialize]Put Byte
                                          [End][Serialize]opCode
                                          [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                                            [Start][Serialize]FuncValue
                                              [Start][Serialize]args.length
                                                [Start Real][Serialize]Put UInt
                                                  [Start Real][Serialize]Put ULong
                                                    [Start][Serialize]VLQLong
                                                    [End][Serialize]VLQLong
                                                  [End Real][Serialize]Put ULong
                                                [End Real][Serialize]Put UInt
                                              [End][Serialize]args.length
                                              [Start][Serialize](args.idx, args.tpe)*
                                                [Start Real][Serialize]Put UInt
                                                  [Start Real][Serialize]Put ULong
                                                    [Start][Serialize]VLQLong
                                                    [End][Serialize]VLQLong
                                                  [End Real][Serialize]Put ULong
                                                [End Real][Serialize]Put UInt
                                                [Start][Serialize]Type [toString=(Coll[SByte],SLong); typeCode=0x60]
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]Type [toString=(Coll[SByte],SLong); typeCode=0x60]
                                              [End][Serialize](args.idx, args.tpe)*
                                              [Start][Serialize]body
                                                [Start][Serialize]Value [toString=SLong; opCode=0x95]
                                                  [Start][Serialize][Non-constant]
                                                    [Start][Serialize]opCode
                                                      [Start Real][Serialize]Put Byte
                                                      [End Real][Serialize]Put Byte
                                                    [End][Serialize]opCode
                                                    [Start][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                                                      [Start][Serialize]Quadruple
                                                        [Start][Serialize]first
                                                          [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                            [Start][Serialize][Non-constant]
                                                              [Start][Serialize]opCode
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                              [End][Serialize]opCode
                                                              [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                                [Start][Serialize]Relation2
                                                                  [Start][Serialize][Not BooleanConstants]
                                                                    [Start][Serialize]left
                                                                      [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x8C]
                                                                        [Start][Serialize][Non-constant]
                                                                          [Start][Serialize]opCode
                                                                            [Start Real][Serialize]Put Byte
                                                                            [End Real][Serialize]Put Byte
                                                                          [End][Serialize]opCode
                                                                          [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                            [Start][Serialize]SelectField
                                                                              [Start][Serialize]input
                                                                                [Start][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                                  [Start][Serialize][Non-constant]
                                                                                    [Start][Serialize]opCode
                                                                                      [Start Real][Serialize]Put Byte
                                                                                      [End Real][Serialize]Put Byte
                                                                                    [End][Serialize]opCode
                                                                                    [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                      [Start][Serialize]ValUse
                                                                                        [Start][Serialize]valId
                                                                                          [Start Real][Serialize]Put UInt
                                                                                            [Start Real][Serialize]Put ULong
                                                                                              [Start][Serialize]VLQLong
                                                                                              [End][Serialize]VLQLong
                                                                                            [End Real][Serialize]Put ULong
                                                                                          [End Real][Serialize]Put UInt
                                                                                        [End][Serialize]valId
                                                                                      [End][Serialize]ValUse
                                                                                    [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                  [End][Serialize][Non-constant]
                                                                                [End][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                              [End][Serialize]input
                                                                              [Start][Serialize]fieldIndex
                                                                                [Start Real][Serialize]Put Byte
                                                                                [End Real][Serialize]Put Byte
                                                                              [End][Serialize]fieldIndex
                                                                            [End][Serialize]SelectField
                                                                          [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                        [End][Serialize][Non-constant]
                                                                      [End][Serialize]Value [toString=Coll[SByte]; opCode=0x8C]
                                                                    [End][Serialize]left
                                                                    [Start][Serialize]right
                                                                      [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x73]
                                                                        [Start][Serialize][Non-constant]
                                                                          [Start][Serialize]opCode
                                                                            [Start Real][Serialize]Put Byte
                                                                            [End Real][Serialize]Put Byte
                                                                          [End][Serialize]opCode
                                                                          [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                            [Start][Serialize]ConstantPlaceholder
                                                                              [Start][Serialize]id
                                                                                [Start Real][Serialize]Put UInt
                                                                                  [Start Real][Serialize]Put ULong
                                                                                    [Start][Serialize]VLQLong
                                                                                    [End][Serialize]VLQLong
                                                                                  [End Real][Serialize]Put ULong
                                                                                [End Real][Serialize]Put UInt
                                                                              [End][Serialize]id
                                                                            [End][Serialize]ConstantPlaceholder
                                                                          [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                        [End][Serialize][Non-constant]
                                                                      [End][Serialize]Value [toString=Coll[SByte]; opCode=0x73]
                                                                    [End][Serialize]right
                                                                  [End][Serialize][Not BooleanConstants]
                                                                [End][Serialize]Relation2
                                                              [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                            [End][Serialize][Non-constant]
                                                          [End][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                        [End][Serialize]first
                                                        [Start][Serialize]second
                                                          [Start][Serialize]Value [toString=SLong; opCode=0x8C]
                                                            [Start][Serialize][Non-constant]
                                                              [Start][Serialize]opCode
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                              [End][Serialize]opCode
                                                              [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                [Start][Serialize]SelectField
                                                                  [Start][Serialize]input
                                                                    [Start][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                      [Start][Serialize][Non-constant]
                                                                        [Start][Serialize]opCode
                                                                          [Start Real][Serialize]Put Byte
                                                                          [End Real][Serialize]Put Byte
                                                                        [End][Serialize]opCode
                                                                        [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                          [Start][Serialize]ValUse
                                                                            [Start][Serialize]valId
                                                                              [Start Real][Serialize]Put UInt
                                                                                [Start Real][Serialize]Put ULong
                                                                                  [Start][Serialize]VLQLong
                                                                                  [End][Serialize]VLQLong
                                                                                [End Real][Serialize]Put ULong
                                                                              [End Real][Serialize]Put UInt
                                                                            [End][Serialize]valId
                                                                          [End][Serialize]ValUse
                                                                        [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                      [End][Serialize][Non-constant]
                                                                    [End][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                  [End][Serialize]input
                                                                  [Start][Serialize]fieldIndex
                                                                    [Start Real][Serialize]Put Byte
                                                                    [End Real][Serialize]Put Byte
                                                                  [End][Serialize]fieldIndex
                                                                [End][Serialize]SelectField
                                                              [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                            [End][Serialize][Non-constant]
                                                          [End][Serialize]Value [toString=SLong; opCode=0x8C]
                                                        [End][Serialize]second
                                                        [Start][Serialize]third
                                                          [Start][Serialize]Value [toString=SLong; opCode=0x73]
                                                            [Start][Serialize][Non-constant]
                                                              [Start][Serialize]opCode
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                              [End][Serialize]opCode
                                                              [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                [Start][Serialize]ConstantPlaceholder
                                                                  [Start][Serialize]id
                                                                    [Start Real][Serialize]Put UInt
                                                                      [Start Real][Serialize]Put ULong
                                                                        [Start][Serialize]VLQLong
                                                                        [End][Serialize]VLQLong
                                                                      [End Real][Serialize]Put ULong
                                                                    [End Real][Serialize]Put UInt
                                                                  [End][Serialize]id
                                                                [End][Serialize]ConstantPlaceholder
                                                              [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                            [End][Serialize][Non-constant]
                                                          [End][Serialize]Value [toString=SLong; opCode=0x73]
                                                        [End][Serialize]third
                                                      [End][Serialize]Quadruple
                                                    [End][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                                                  [End][Serialize][Non-constant]
                                                [End][Serialize]Value [toString=SLong; opCode=0x95]
                                              [End][Serialize]body
                                            [End][Serialize]FuncValue
                                          [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                                        [End][Serialize][Non-constant]
                                      [End][Serialize]Value [toString=((Coll[SByte],SLong)) => SLong; opCode=0xD9]
                                    [End][Serialize]mapper
                                  [End][Serialize]MapCollection
                                [End][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
                              [End][Serialize][Non-constant]
                            [End][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
                          [End][Serialize]values*
                        [End][Serialize]Values
                      [End][Serialize]args
                    [End][Serialize]Apply
                  [End][Serialize]Body [opCode=0xDA; Serializer=ApplySerializer(sigmastate.serialization.ValueSerializer$$$Lambda$227/0x000000084042b040@79308a2)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SLong; opCode=0xDA]
            [End][Serialize]body
          [End][Serialize]FuncValue
        [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=(SBox) => SLong; opCode=0xD9]
  [End][Serialize]mapper
[End][Serialize]MapCollection
19. BooleanTransformer
[Start][Serialize]BooleanTransformer
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA4]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xA4; Serializer=CaseObjectSerialization(-92,Inputs)]
          [Start][Serialize]CaseObject
          [End][Serialize]CaseObject
        [End][Serialize]Body [opCode=0xA4; Serializer=CaseObjectSerialization(-92,Inputs)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA4]
  [End][Serialize]input
  [Start][Serialize]condition
    [Start][Serialize]Value [toString=(SBox) => SBoolean; opCode=0xD9]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
          [Start][Serialize]FuncValue
            [Start][Serialize]args.length
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]args.length
            [Start][Serialize](args.idx, args.tpe)*
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
              [Start][Serialize]Type [toString=SBox; typeCode=0x63]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SBox; typeCode=0x63]
            [End][Serialize](args.idx, args.tpe)*
            [Start][Serialize]body
              [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                    [Start][Serialize]Relation2
                      [Start][Serialize][Not BooleanConstants]
                        [Start][Serialize]left
                          [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xC5]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xC5; Serializer=SimpleTransformerSerializer(-59,sigmastate.serialization.ValueSerializer$$$Lambda$204/0x000000084041c040@7ebc5be6)]
                                [Start][Serialize]SimpleTransformer
                                  [Start][Serialize]input
                                    [Start][Serialize]Value [toString=SBox; opCode=0x72]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                          [Start][Serialize]ValUse
                                            [Start][Serialize]valId
                                              [Start Real][Serialize]Put UInt
                                                [Start Real][Serialize]Put ULong
                                                  [Start][Serialize]VLQLong
                                                  [End][Serialize]VLQLong
                                                [End Real][Serialize]Put ULong
                                              [End Real][Serialize]Put UInt
                                            [End][Serialize]valId
                                          [End][Serialize]ValUse
                                        [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SBox; opCode=0x72]
                                  [End][Serialize]input
                                [End][Serialize]SimpleTransformer
                              [End][Serialize]Body [opCode=0xC5; Serializer=SimpleTransformerSerializer(-59,sigmastate.serialization.ValueSerializer$$$Lambda$204/0x000000084041c040@7ebc5be6)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=Coll[SByte]; opCode=0xC5]
                        [End][Serialize]left
                        [Start][Serialize]right
                          [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xC5]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xC5; Serializer=SimpleTransformerSerializer(-59,sigmastate.serialization.ValueSerializer$$$Lambda$204/0x000000084041c040@7ebc5be6)]
                                [Start][Serialize]SimpleTransformer
                                  [Start][Serialize]input
                                    [Start][Serialize]Value [toString=SBox; opCode=0x73]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                          [Start][Serialize]ConstantPlaceholder
                                            [Start][Serialize]id
                                              [Start Real][Serialize]Put UInt
                                                [Start Real][Serialize]Put ULong
                                                  [Start][Serialize]VLQLong
                                                  [End][Serialize]VLQLong
                                                [End Real][Serialize]Put ULong
                                              [End Real][Serialize]Put UInt
                                            [End][Serialize]id
                                          [End][Serialize]ConstantPlaceholder
                                        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SBox; opCode=0x73]
                                  [End][Serialize]input
                                [End][Serialize]SimpleTransformer
                              [End][Serialize]Body [opCode=0xC5; Serializer=SimpleTransformerSerializer(-59,sigmastate.serialization.ValueSerializer$$$Lambda$204/0x000000084041c040@7ebc5be6)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=Coll[SByte]; opCode=0xC5]
                        [End][Serialize]right
                      [End][Serialize][Not BooleanConstants]
                    [End][Serialize]Relation2
                  [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBoolean; opCode=0x93]
            [End][Serialize]body
          [End][Serialize]FuncValue
        [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=(SBox) => SBoolean; opCode=0xD9]
  [End][Serialize]condition
[End][Serialize]BooleanTransformer
20. Fold
[Start][Serialize]Fold
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SLong]; opCode=0x72]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
          [Start][Serialize]ValUse
            [Start][Serialize]valId
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]valId
          [End][Serialize]ValUse
        [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SLong]; opCode=0x72]
  [End][Serialize]input
  [Start][Serialize]zero
    [Start][Serialize]Value [toString=SLong; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SLong; opCode=0x73]
  [End][Serialize]zero
  [Start][Serialize]foldOp
    [Start][Serialize]Value [toString=((SLong,SLong)) => SLong; opCode=0xD9]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
          [Start][Serialize]FuncValue
            [Start][Serialize]args.length
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]args.length
            [Start][Serialize](args.idx, args.tpe)*
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
              [Start][Serialize]Type [toString=(SLong,SLong); typeCode=0x60]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=(SLong,SLong); typeCode=0x60]
            [End][Serialize](args.idx, args.tpe)*
            [Start][Serialize]body
              [Start][Serialize]Value [toString=SLong; opCode=0x9A]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                    [Start][Serialize]TwoArguments
                      [Start][Serialize]left
                        [Start][Serialize]Value [toString=SLong; opCode=0x8C]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                              [Start][Serialize]SelectField
                                [Start][Serialize]input
                                  [Start][Serialize]Value [toString=(SLong,SLong); opCode=0x72]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                        [Start][Serialize]ValUse
                                          [Start][Serialize]valId
                                            [Start Real][Serialize]Put UInt
                                              [Start Real][Serialize]Put ULong
                                                [Start][Serialize]VLQLong
                                                [End][Serialize]VLQLong
                                              [End Real][Serialize]Put ULong
                                            [End Real][Serialize]Put UInt
                                          [End][Serialize]valId
                                        [End][Serialize]ValUse
                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=(SLong,SLong); opCode=0x72]
                                [End][Serialize]input
                                [Start][Serialize]fieldIndex
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]fieldIndex
                              [End][Serialize]SelectField
                            [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SLong; opCode=0x8C]
                      [End][Serialize]left
                      [Start][Serialize]right
                        [Start][Serialize]Value [toString=SLong; opCode=0x8C]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                              [Start][Serialize]SelectField
                                [Start][Serialize]input
                                  [Start][Serialize]Value [toString=(SLong,SLong); opCode=0x72]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                        [Start][Serialize]ValUse
                                          [Start][Serialize]valId
                                            [Start Real][Serialize]Put UInt
                                              [Start Real][Serialize]Put ULong
                                                [Start][Serialize]VLQLong
                                                [End][Serialize]VLQLong
                                              [End Real][Serialize]Put ULong
                                            [End Real][Serialize]Put UInt
                                          [End][Serialize]valId
                                        [End][Serialize]ValUse
                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=(SLong,SLong); opCode=0x72]
                                [End][Serialize]input
                                [Start][Serialize]fieldIndex
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]fieldIndex
                              [End][Serialize]SelectField
                            [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SLong; opCode=0x8C]
                      [End][Serialize]right
                    [End][Serialize]TwoArguments
                  [End][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SLong; opCode=0x9A]
            [End][Serialize]body
          [End][Serialize]FuncValue
        [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=((SLong,SLong)) => SLong; opCode=0xD9]
  [End][Serialize]foldOp
[End][Serialize]Fold
21. SimpleTransformer
[Start][Serialize]SimpleTransformer
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xC2]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xC2; Serializer=SimpleTransformerSerializer(-62,sigmastate.serialization.ValueSerializer$$$Lambda$201/0x000000084041a040@53d102a2)]
          [Start][Serialize]SimpleTransformer
            [Start][Serialize]input
              [Start][Serialize]Value [toString=SBox; opCode=0x72]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                    [Start][Serialize]ValUse
                      [Start][Serialize]valId
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]valId
                    [End][Serialize]ValUse
                  [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBox; opCode=0x72]
            [End][Serialize]input
          [End][Serialize]SimpleTransformer
        [End][Serialize]Body [opCode=0xC2; Serializer=SimpleTransformerSerializer(-62,sigmastate.serialization.ValueSerializer$$$Lambda$201/0x000000084041a040@53d102a2)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SByte]; opCode=0xC2]
  [End][Serialize]input
[End][Serialize]SimpleTransformer
22. OptionGetOrElse
[Start][Serialize]OptionGetOrElse
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Option[SByte]; opCode=0xC6]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
          [Start][Serialize]ExtractRegisterAs
            [Start][Serialize]input
              [Start][Serialize]Value [toString=SBox; opCode=0xB2]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xB2; Serializer=ByIndexSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$220/0x0000000840425440@3bcbb589)]
                    [Start][Serialize]ByIndex
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                              [Start][Serialize]CaseObject
                              [End][Serialize]CaseObject
                            [End][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
                      [End][Serialize]input
                      [Start][Serialize]index
                        [Start][Serialize]Value [toString=SInt; opCode=0x73]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                              [Start][Serialize]ConstantPlaceholder
                                [Start][Serialize]id
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]id
                              [End][Serialize]ConstantPlaceholder
                            [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SInt; opCode=0x73]
                      [End][Serialize]index
                      [Start][Serialize]default
                        [Start Real][Serialize]Put Option
                        [End Real][Serialize]Put Option
                      [End][Serialize]default
                    [End][Serialize]ByIndex
                  [End][Serialize]Body [opCode=0xB2; Serializer=ByIndexSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$220/0x0000000840425440@3bcbb589)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBox; opCode=0xB2]
            [End][Serialize]input
            [Start][Serialize]registerId.number
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]registerId.number
            [Start][Serialize]tpe.elemType
              [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SByte; typeCode=0x02]
            [End][Serialize]tpe.elemType
          [End][Serialize]ExtractRegisterAs
        [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Option[SByte]; opCode=0xC6]
  [End][Serialize]input
  [Start][Serialize]default
    [Start][Serialize]Value [toString=SByte; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SByte; opCode=0x73]
  [End][Serialize]default
[End][Serialize]OptionGetOrElse
23. DeserializeContext
[Start][Serialize]DeserializeContext
  [Start][Serialize]tpe
    [Start][Serialize]Type [toString=SBoolean; typeCode=0x01]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SBoolean; typeCode=0x01]
  [End][Serialize]tpe
  [Start][Serialize]id
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]id
[End][Serialize]DeserializeContext
24. DeserializeRegister

25. ExtractRegisterAs
[Start][Serialize]ExtractRegisterAs
  [Start][Serialize]input
    [Start][Serialize]Value [toString=SBox; opCode=0xA7]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
          [Start][Serialize]CaseObject
          [End][Serialize]CaseObject
        [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SBox; opCode=0xA7]
  [End][Serialize]input
  [Start][Serialize]registerId.number
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]registerId.number
  [Start][Serialize]tpe.elemType
    [Start][Serialize]Type [toString=SInt; typeCode=0x04]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SInt; typeCode=0x04]
  [End][Serialize]tpe.elemType
[End][Serialize]ExtractRegisterAs
26. Filter
[Start][Serialize]Filter
  [Start][Serialize]id
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]id
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SInt]; opCode=0x83]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
          [Start][Serialize]ConcreteCollection
            [Start][Serialize]items.size
              [Start Real][Serialize]Put UShort
                [Start Real][Serialize]Put UInt
                  [Start Real][Serialize]Put ULong
                    [Start][Serialize]VLQLong
                    [End][Serialize]VLQLong
                  [End Real][Serialize]Put ULong
                [End Real][Serialize]Put UInt
              [End Real][Serialize]Put UShort
            [End][Serialize]items.size
            [Start][Serialize]tpe.elemType
              [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SInt; typeCode=0x04]
            [End][Serialize]tpe.elemType
            [Start][Serialize]items*
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
              [Start][Serialize]Value [toString=SInt; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SInt; typeCode=0x04]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Int
                        [Start][Serialize]ZigZagInt
                        [End][Serialize]ZigZagInt
                        [Start Real][Serialize]Put ULong
                          [Start][Serialize]VLQLong
                          [End][Serialize]VLQLong
                        [End Real][Serialize]Put ULong
                      [End Real][Serialize]Put Int
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SInt; opCode=0x00]
            [End][Serialize]items*
          [End][Serialize]ConcreteCollection
        [End][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SInt]; opCode=0x83]
  [End][Serialize]input
  [Start][Serialize]condition
    [Start][Serialize]Value [toString=SBoolean; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SBoolean; typeCode=0x01]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SBoolean; typeCode=0x01]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put Boolean
            [End Real][Serialize]Put Boolean
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SBoolean; opCode=0x00]
  [End][Serialize]condition
[End][Serialize]Filter
27. Slice
[Start][Serialize]Slice
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xCB]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xCB; Serializer=SimpleTransformerSerializer(-53,sigmastate.serialization.ValueSerializer$$$Lambda$208/0x000000084041f040@9d3d54e)]
          [Start][Serialize]SimpleTransformer
            [Start][Serialize]input
              [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xE4]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                    [Start][Serialize]SimpleTransformer
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=Option[Coll[SByte]]; opCode=0xE3]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xE3; Serializer=GetVarSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$194/0x000000084040d040@23ee2ccf)]
                              [Start][Serialize]GetVar
                                [Start][Serialize]varId
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]varId
                                [Start][Serialize]tpe.elemType
                                  [Start][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
                                    [Start Real][Serialize]Put Byte
                                    [End Real][Serialize]Put Byte
                                  [End][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
                                [End][Serialize]tpe.elemType
                              [End][Serialize]GetVar
                            [End][Serialize]Body [opCode=0xE3; Serializer=GetVarSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$194/0x000000084040d040@23ee2ccf)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=Option[Coll[SByte]]; opCode=0xE3]
                      [End][Serialize]input
                    [End][Serialize]SimpleTransformer
                  [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=Coll[SByte]; opCode=0xE4]
            [End][Serialize]input
          [End][Serialize]SimpleTransformer
        [End][Serialize]Body [opCode=0xCB; Serializer=SimpleTransformerSerializer(-53,sigmastate.serialization.ValueSerializer$$$Lambda$208/0x000000084041f040@9d3d54e)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SByte]; opCode=0xCB]
  [End][Serialize]input
  [Start][Serialize]from
    [Start][Serialize]Value [toString=SInt; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SInt; opCode=0x73]
  [End][Serialize]from
  [Start][Serialize]until
    [Start][Serialize]Value [toString=SInt; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SInt; opCode=0x73]
  [End][Serialize]until
[End][Serialize]Slice
28. AtLeast
[Start][Serialize]AtLeast
  [Start][Serialize]bound
    [Start][Serialize]Value [toString=SInt; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SInt; typeCode=0x04]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SInt; typeCode=0x04]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put Int
              [Start][Serialize]ZigZagInt
              [End][Serialize]ZigZagInt
              [Start Real][Serialize]Put ULong
                [Start][Serialize]VLQLong
                [End][Serialize]VLQLong
              [End Real][Serialize]Put ULong
            [End Real][Serialize]Put Int
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SInt; opCode=0x00]
  [End][Serialize]bound
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SSigmaProp]; opCode=0x83]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
          [Start][Serialize]ConcreteCollection
            [Start][Serialize]items.size
              [Start Real][Serialize]Put UShort
                [Start Real][Serialize]Put UInt
                  [Start Real][Serialize]Put ULong
                    [Start][Serialize]VLQLong
                    [End][Serialize]VLQLong
                  [End Real][Serialize]Put ULong
                [End Real][Serialize]Put UInt
              [End Real][Serialize]Put UShort
            [End][Serialize]items.size
            [Start][Serialize]tpe.elemType
              [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
            [End][Serialize]tpe.elemType
            [Start][Serialize]items*
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start][Serialize]Value [toString=SBoolean; opCode=0xCD]
                        [Start][Serialize][Non-constant]
                          [Start][Serialize]opCode
                            [Start Real][Serialize]Put Byte
                            [End Real][Serialize]Put Byte
                          [End][Serialize]opCode
                          [Start][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
                            [Start][Serialize]ProveDlog
                              [Start][Serialize]value
                                [Start][Serialize]Value [toString=SGroupElement; opCode=0x00]
                                  [Start][Serialize][Constant without store]
                                    [Start][Serialize]Constant
                                      [Start][Serialize]tpe
                                        [Start][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                                      [End][Serialize]tpe
                                      [Start][Serialize]value
                                        [Start][Serialize]GroupElementSerializer
                                          [Start Real][Serialize]Put Bytes
                                          [End Real][Serialize]Put Bytes
                                        [End][Serialize]GroupElementSerializer
                                      [End][Serialize]value
                                    [End][Serialize]Constant
                                  [End][Serialize][Constant without store]
                                [End][Serialize]Value [toString=SGroupElement; opCode=0x00]
                              [End][Serialize]value
                            [End][Serialize]ProveDlog
                          [End][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
                        [End][Serialize][Non-constant]
                      [End][Serialize]Value [toString=SBoolean; opCode=0xCD]
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start][Serialize]Value [toString=SBoolean; opCode=0xCD]
                        [Start][Serialize][Non-constant]
                          [Start][Serialize]opCode
                            [Start Real][Serialize]Put Byte
                            [End Real][Serialize]Put Byte
                          [End][Serialize]opCode
                          [Start][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
                            [Start][Serialize]ProveDlog
                              [Start][Serialize]value
                                [Start][Serialize]Value [toString=SGroupElement; opCode=0x00]
                                  [Start][Serialize][Constant without store]
                                    [Start][Serialize]Constant
                                      [Start][Serialize]tpe
                                        [Start][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                                      [End][Serialize]tpe
                                      [Start][Serialize]value
                                        [Start][Serialize]GroupElementSerializer
                                          [Start Real][Serialize]Put Bytes
                                          [End Real][Serialize]Put Bytes
                                        [End][Serialize]GroupElementSerializer
                                      [End][Serialize]value
                                    [End][Serialize]Constant
                                  [End][Serialize][Constant without store]
                                [End][Serialize]Value [toString=SGroupElement; opCode=0x00]
                              [End][Serialize]value
                            [End][Serialize]ProveDlog
                          [End][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
                        [End][Serialize][Non-constant]
                      [End][Serialize]Value [toString=SBoolean; opCode=0xCD]
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start][Serialize]Value [toString=SBoolean; opCode=0xCD]
                        [Start][Serialize][Non-constant]
                          [Start][Serialize]opCode
                            [Start Real][Serialize]Put Byte
                            [End Real][Serialize]Put Byte
                          [End][Serialize]opCode
                          [Start][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
                            [Start][Serialize]ProveDlog
                              [Start][Serialize]value
                                [Start][Serialize]Value [toString=SGroupElement; opCode=0x00]
                                  [Start][Serialize][Constant without store]
                                    [Start][Serialize]Constant
                                      [Start][Serialize]tpe
                                        [Start][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                                      [End][Serialize]tpe
                                      [Start][Serialize]value
                                        [Start][Serialize]GroupElementSerializer
                                          [Start Real][Serialize]Put Bytes
                                          [End Real][Serialize]Put Bytes
                                        [End][Serialize]GroupElementSerializer
                                      [End][Serialize]value
                                    [End][Serialize]Constant
                                  [End][Serialize][Constant without store]
                                [End][Serialize]Value [toString=SGroupElement; opCode=0x00]
                              [End][Serialize]value
                            [End][Serialize]ProveDlog
                          [End][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
                        [End][Serialize][Non-constant]
                      [End][Serialize]Value [toString=SBoolean; opCode=0xCD]
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
            [End][Serialize]items*
          [End][Serialize]ConcreteCollection
        [End][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SSigmaProp]; opCode=0x83]
  [End][Serialize]input
[End][Serialize]AtLeast
29. ByIndex
[Start][Serialize]ByIndex
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
          [Start][Serialize]CaseObject
          [End][Serialize]CaseObject
        [End][Serialize]Body [opCode=0xA5; Serializer=CaseObjectSerialization(-91,Outputs)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA5]
  [End][Serialize]input
  [Start][Serialize]index
    [Start][Serialize]Value [toString=SInt; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SInt; opCode=0x73]
  [End][Serialize]index
  [Start][Serialize]default
    [Start Real][Serialize]Put Option
    [End Real][Serialize]Put Option
  [End][Serialize]default
[End][Serialize]ByIndex

30. Append
[Start][Serialize]Append
  [Start][Serialize]input
    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put UShort
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End Real][Serialize]Put UShort
            [Start Real][Serialize]Put Bytes
            [End Real][Serialize]Put Bytes
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
  [End][Serialize]input
  [Start][Serialize]col2
    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put UShort
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End Real][Serialize]Put UShort
            [Start Real][Serialize]Put Bytes
            [End Real][Serialize]Put Bytes
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
  [End][Serialize]col2
[End][Serialize]Append

31. NumericCast
[Start][Serialize]NumericCast
  [Start][Serialize]input
    [Start][Serialize]Value [toString=SByte; opCode=0x9C]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
          [Start][Serialize]TwoArguments
            [Start][Serialize]left
              [Start][Serialize]Value [toString=SByte; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SByte; typeCode=0x02]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SByte; opCode=0x00]
            [End][Serialize]left
            [Start][Serialize]right
              [Start][Serialize]Value [toString=SByte; opCode=0x00]
                [Start][Serialize][Constant without store]
                  [Start][Serialize]Constant
                    [Start][Serialize]tpe
                      [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]Type [toString=SByte; typeCode=0x02]
                    [End][Serialize]tpe
                    [Start][Serialize]value
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]value
                  [End][Serialize]Constant
                [End][Serialize][Constant without store]
              [End][Serialize]Value [toString=SByte; opCode=0x00]
            [End][Serialize]right
          [End][Serialize]TwoArguments
        [End][Serialize]Body [opCode=0x9C; Serializer=TwoArgumentsSerializer(-100,sigmastate.serialization.ValueSerializer$$$Lambda$181/0x0000000840403840@1ec88aa1)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SByte; opCode=0x9C]
  [End][Serialize]input
  [Start][Serialize]tpe
    [Start][Serialize]Type [toString=SBigInt; typeCode=0x06]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SBigInt; typeCode=0x06]
  [End][Serialize]tpe
[End][Serialize]NumericCast

32. ValDef
[Start][Serialize]ValDef
  [Start][Serialize]id
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
  [End][Serialize]id
  [Start][Serialize]rhs
    [Start][Serialize]Value [toString=SByte; opCode=0xE4]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
          [Start][Serialize]SimpleTransformer
            [Start][Serialize]input
              [Start][Serialize]Value [toString=Option[SByte]; opCode=0xC6]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                    [Start][Serialize]ExtractRegisterAs
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                              [Start][Serialize]CaseObject
                              [End][Serialize]CaseObject
                            [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SBox; opCode=0xA7]
                      [End][Serialize]input
                      [Start][Serialize]registerId.number
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]registerId.number
                      [Start][Serialize]tpe.elemType
                        [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                          [Start Real][Serialize]Put Byte
                          [End Real][Serialize]Put Byte
                        [End][Serialize]Type [toString=SByte; typeCode=0x02]
                      [End][Serialize]tpe.elemType
                    [End][Serialize]ExtractRegisterAs
                  [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=Option[SByte]; opCode=0xC6]
            [End][Serialize]input
          [End][Serialize]SimpleTransformer
        [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SByte; opCode=0xE4]
  [End][Serialize]rhs
[End][Serialize]ValDef

33. BlockValue
[Start][Serialize]BlockValue
  [Start][Serialize]items.length
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
  [End][Serialize]items.length
  [Start][Serialize]items*
    [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD6]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD6; Serializer=ValDefSerializer(-42)]
          [Start][Serialize]ValDef
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
            [Start][Serialize]rhs
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0xE4]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                    [Start][Serialize]SimpleTransformer
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=Option[SSigmaProp]; opCode=0xC6]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                              [Start][Serialize]ExtractRegisterAs
                                [Start][Serialize]input
                                  [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                        [Start][Serialize]CaseObject
                                        [End][Serialize]CaseObject
                                      [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SBox; opCode=0xA7]
                                [End][Serialize]input
                                [Start][Serialize]registerId.number
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]registerId.number
                                [Start][Serialize]tpe.elemType
                                  [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                                    [Start Real][Serialize]Put Byte
                                    [End Real][Serialize]Put Byte
                                  [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                                [End][Serialize]tpe.elemType
                              [End][Serialize]ExtractRegisterAs
                            [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=Option[SSigmaProp]; opCode=0xC6]
                      [End][Serialize]input
                    [End][Serialize]SimpleTransformer
                  [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0xE4]
            [End][Serialize]rhs
          [End][Serialize]ValDef
        [End][Serialize]Body [opCode=0xD6; Serializer=ValDefSerializer(-42)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SSigmaProp; opCode=0xD6]
    [Start][Serialize]Value [toString=SByte; opCode=0xD6]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD6; Serializer=ValDefSerializer(-42)]
          [Start][Serialize]ValDef
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
            [Start][Serialize]rhs
              [Start][Serialize]Value [toString=SByte; opCode=0xE4]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                    [Start][Serialize]SimpleTransformer
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=Option[SByte]; opCode=0xE3]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xE3; Serializer=GetVarSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$194/0x000000084040d040@23ee2ccf)]
                              [Start][Serialize]GetVar
                                [Start][Serialize]varId
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]varId
                                [Start][Serialize]tpe.elemType
                                  [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                                    [Start Real][Serialize]Put Byte
                                    [End Real][Serialize]Put Byte
                                  [End][Serialize]Type [toString=SByte; typeCode=0x02]
                                [End][Serialize]tpe.elemType
                              [End][Serialize]GetVar
                            [End][Serialize]Body [opCode=0xE3; Serializer=GetVarSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$194/0x000000084040d040@23ee2ccf)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=Option[SByte]; opCode=0xE3]
                      [End][Serialize]input
                    [End][Serialize]SimpleTransformer
                  [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SByte; opCode=0xE4]
            [End][Serialize]rhs
          [End][Serialize]ValDef
        [End][Serialize]Body [opCode=0xD6; Serializer=ValDefSerializer(-42)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SByte; opCode=0xD6]
    [Start][Serialize]Value [toString=SByte; opCode=0xD6]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD6; Serializer=ValDefSerializer(-42)]
          [Start][Serialize]ValDef
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
            [Start][Serialize]rhs
              [Start][Serialize]Value [toString=SByte; opCode=0xE4]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                    [Start][Serialize]SimpleTransformer
                      [Start][Serialize]input
                        [Start][Serialize]Value [toString=Option[SByte]; opCode=0xC6]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                              [Start][Serialize]ExtractRegisterAs
                                [Start][Serialize]input
                                  [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                        [Start][Serialize]CaseObject
                                        [End][Serialize]CaseObject
                                      [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SBox; opCode=0xA7]
                                [End][Serialize]input
                                [Start][Serialize]registerId.number
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]registerId.number
                                [Start][Serialize]tpe.elemType
                                  [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                                    [Start Real][Serialize]Put Byte
                                    [End Real][Serialize]Put Byte
                                  [End][Serialize]Type [toString=SByte; typeCode=0x02]
                                [End][Serialize]tpe.elemType
                              [End][Serialize]ExtractRegisterAs
                            [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=Option[SByte]; opCode=0xC6]
                      [End][Serialize]input
                    [End][Serialize]SimpleTransformer
                  [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SByte; opCode=0xE4]
            [End][Serialize]rhs
          [End][Serialize]ValDef
        [End][Serialize]Body [opCode=0xD6; Serializer=ValDefSerializer(-42)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SByte; opCode=0xD6]
  [End][Serialize]items*
  [Start][Serialize]result
    [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEB]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xEB; Serializer=SigmaTransformerSerializer(-21,sigmastate.serialization.ValueSerializer$$$Lambda$231/0x000000084042d040@165614f5)]
          [Start][Serialize]SigmaTransformer
            [Start][Serialize]items.length
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]items.length
            [Start][Serialize]items*
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                    [Start][Serialize]SigmaTransformer
                      [Start][Serialize]items.length
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]items.length
                      [Start][Serialize]items*
                        [Start][Serialize]Value [toString=SSigmaProp; opCode=0x72]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                              [Start][Serialize]ValUse
                                [Start][Serialize]valId
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]valId
                              [End][Serialize]ValUse
                            [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SSigmaProp; opCode=0x72]
                        [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                              [Start][Serialize]BoolToSigmaProp
                                [Start][Serialize]value
                                  [Start][Serialize]Value [toString=SBoolean; opCode=0x91]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x91; Serializer=Relation2Serializer(-111,sigmastate.serialization.ValueSerializer$$$Lambda$166/0x00000008401b6840@5b0902b4)]
                                        [Start][Serialize]Relation2
                                          [Start][Serialize][Not BooleanConstants]
                                            [Start][Serialize]left
                                              [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                    [Start][Serialize]CaseObject
                                                    [End][Serialize]CaseObject
                                                  [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=SInt; opCode=0xA3]
                                            [End][Serialize]left
                                            [Start][Serialize]right
                                              [Start][Serialize]Value [toString=SInt; opCode=0xE4]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                                    [Start][Serialize]SimpleTransformer
                                                      [Start][Serialize]input
                                                        [Start][Serialize]Value [toString=Option[SInt]; opCode=0xC6]
                                                          [Start][Serialize][Non-constant]
                                                            [Start][Serialize]opCode
                                                              [Start Real][Serialize]Put Byte
                                                              [End Real][Serialize]Put Byte
                                                            [End][Serialize]opCode
                                                            [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                                              [Start][Serialize]ExtractRegisterAs
                                                                [Start][Serialize]input
                                                                  [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                                                        [Start][Serialize]CaseObject
                                                                        [End][Serialize]CaseObject
                                                                      [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=SBox; opCode=0xA7]
                                                                [End][Serialize]input
                                                                [Start][Serialize]registerId.number
                                                                  [Start Real][Serialize]Put Byte
                                                                  [End Real][Serialize]Put Byte
                                                                [End][Serialize]registerId.number
                                                                [Start][Serialize]tpe.elemType
                                                                  [Start][Serialize]Type [toString=SInt; typeCode=0x04]
                                                                    [Start Real][Serialize]Put Byte
                                                                    [End Real][Serialize]Put Byte
                                                                  [End][Serialize]Type [toString=SInt; typeCode=0x04]
                                                                [End][Serialize]tpe.elemType
                                                              [End][Serialize]ExtractRegisterAs
                                                            [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                                          [End][Serialize][Non-constant]
                                                        [End][Serialize]Value [toString=Option[SInt]; opCode=0xC6]
                                                      [End][Serialize]input
                                                    [End][Serialize]SimpleTransformer
                                                  [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=SInt; opCode=0xE4]
                                            [End][Serialize]right
                                          [End][Serialize][Not BooleanConstants]
                                        [End][Serialize]Relation2
                                      [End][Serialize]Body [opCode=0x91; Serializer=Relation2Serializer(-111,sigmastate.serialization.ValueSerializer$$$Lambda$166/0x00000008401b6840@5b0902b4)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SBoolean; opCode=0x91]
                                [End][Serialize]value
                              [End][Serialize]BoolToSigmaProp
                            [End][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                      [End][Serialize]items*
                    [End][Serialize]SigmaTransformer
                  [End][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                    [Start][Serialize]SigmaTransformer
                      [Start][Serialize]items.length
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]items.length
                      [Start][Serialize]items*
                        [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                              [Start][Serialize]BoolToSigmaProp
                                [Start][Serialize]value
                                  [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                        [Start][Serialize]Relation2
                                          [Start][Serialize][Not BooleanConstants]
                                            [Start][Serialize]left
                                              [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xCB]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0xCB; Serializer=SimpleTransformerSerializer(-53,sigmastate.serialization.ValueSerializer$$$Lambda$208/0x000000084041f040@9d3d54e)]
                                                    [Start][Serialize]SimpleTransformer
                                                      [Start][Serialize]input
                                                        [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xB3]
                                                          [Start][Serialize][Non-constant]
                                                            [Start][Serialize]opCode
                                                              [Start Real][Serialize]Put Byte
                                                              [End Real][Serialize]Put Byte
                                                            [End][Serialize]opCode
                                                            [Start][Serialize]Body [opCode=0xB3; Serializer=AppendSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$221/0x0000000840426840@2f04993d)]
                                                              [Start][Serialize]Append
                                                                [Start][Serialize]input
                                                                  [Start][Serialize]Value [toString=Coll[SByte]; opCode=0xE4]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                                                        [Start][Serialize]SimpleTransformer
                                                                          [Start][Serialize]input
                                                                            [Start][Serialize]Value [toString=Option[Coll[SByte]]; opCode=0xE3]
                                                                              [Start][Serialize][Non-constant]
                                                                                [Start][Serialize]opCode
                                                                                  [Start Real][Serialize]Put Byte
                                                                                  [End Real][Serialize]Put Byte
                                                                                [End][Serialize]opCode
                                                                                [Start][Serialize]Body [opCode=0xE3; Serializer=GetVarSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$194/0x000000084040d040@23ee2ccf)]
                                                                                  [Start][Serialize]GetVar
                                                                                    [Start][Serialize]varId
                                                                                      [Start Real][Serialize]Put Byte
                                                                                      [End Real][Serialize]Put Byte
                                                                                    [End][Serialize]varId
                                                                                    [Start][Serialize]tpe.elemType
                                                                                      [Start][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
                                                                                        [Start Real][Serialize]Put Byte
                                                                                        [End Real][Serialize]Put Byte
                                                                                      [End][Serialize]Type [toString=Coll[SByte]; typeCode=0x0C]
                                                                                    [End][Serialize]tpe.elemType
                                                                                  [End][Serialize]GetVar
                                                                                [End][Serialize]Body [opCode=0xE3; Serializer=GetVarSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$194/0x000000084040d040@23ee2ccf)]
                                                                              [End][Serialize][Non-constant]
                                                                            [End][Serialize]Value [toString=Option[Coll[SByte]]; opCode=0xE3]
                                                                          [End][Serialize]input
                                                                        [End][Serialize]SimpleTransformer
                                                                      [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=Coll[SByte]; opCode=0xE4]
                                                                [End][Serialize]input
                                                                [Start][Serialize]col2
                                                                  [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x83]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
                                                                        [Start][Serialize]ConcreteCollection
                                                                          [Start][Serialize]items.size
                                                                            [Start Real][Serialize]Put UShort
                                                                              [Start Real][Serialize]Put UInt
                                                                                [Start Real][Serialize]Put ULong
                                                                                  [Start][Serialize]VLQLong
                                                                                  [End][Serialize]VLQLong
                                                                                [End Real][Serialize]Put ULong
                                                                              [End Real][Serialize]Put UInt
                                                                            [End Real][Serialize]Put UShort
                                                                          [End][Serialize]items.size
                                                                          [Start][Serialize]tpe.elemType
                                                                            [Start][Serialize]Type [toString=SByte; typeCode=0x02]
                                                                              [Start Real][Serialize]Put Byte
                                                                              [End Real][Serialize]Put Byte
                                                                            [End][Serialize]Type [toString=SByte; typeCode=0x02]
                                                                          [End][Serialize]tpe.elemType
                                                                          [Start][Serialize]items*
                                                                            [Start][Serialize]Value [toString=SByte; opCode=0x72]
                                                                              [Start][Serialize][Non-constant]
                                                                                [Start][Serialize]opCode
                                                                                  [Start Real][Serialize]Put Byte
                                                                                  [End Real][Serialize]Put Byte
                                                                                [End][Serialize]opCode
                                                                                [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                  [Start][Serialize]ValUse
                                                                                    [Start][Serialize]valId
                                                                                      [Start Real][Serialize]Put UInt
                                                                                        [Start Real][Serialize]Put ULong
                                                                                          [Start][Serialize]VLQLong
                                                                                          [End][Serialize]VLQLong
                                                                                        [End Real][Serialize]Put ULong
                                                                                      [End Real][Serialize]Put UInt
                                                                                    [End][Serialize]valId
                                                                                  [End][Serialize]ValUse
                                                                                [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                              [End][Serialize][Non-constant]
                                                                            [End][Serialize]Value [toString=SByte; opCode=0x72]
                                                                          [End][Serialize]items*
                                                                        [End][Serialize]ConcreteCollection
                                                                      [End][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=Coll[SByte]; opCode=0x83]
                                                                [End][Serialize]col2
                                                              [End][Serialize]Append
                                                            [End][Serialize]Body [opCode=0xB3; Serializer=AppendSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$221/0x0000000840426840@2f04993d)]
                                                          [End][Serialize][Non-constant]
                                                        [End][Serialize]Value [toString=Coll[SByte]; opCode=0xB3]
                                                      [End][Serialize]input
                                                    [End][Serialize]SimpleTransformer
                                                  [End][Serialize]Body [opCode=0xCB; Serializer=SimpleTransformerSerializer(-53,sigmastate.serialization.ValueSerializer$$$Lambda$208/0x000000084041f040@9d3d54e)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=Coll[SByte]; opCode=0xCB]
                                            [End][Serialize]left
                                            [Start][Serialize]right
                                              [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
                                                [Start][Serialize][Constant with store]
                                                  [Start][Serialize]placeholder.opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]placeholder.opCode
                                                  [Start][Serialize]constantPlaceholder
                                                    [Start][Serialize]ConstantPlaceholder
                                                      [Start][Serialize]id
                                                        [Start Real][Serialize]Put UInt
                                                          [Start Real][Serialize]Put ULong
                                                            [Start][Serialize]VLQLong
                                                            [End][Serialize]VLQLong
                                                          [End Real][Serialize]Put ULong
                                                        [End Real][Serialize]Put UInt
                                                      [End][Serialize]id
                                                    [End][Serialize]ConstantPlaceholder
                                                  [End][Serialize]constantPlaceholder
                                                [End][Serialize][Constant with store]
                                              [End][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
                                            [End][Serialize]right
                                          [End][Serialize][Not BooleanConstants]
                                        [End][Serialize]Relation2
                                      [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SBoolean; opCode=0x93]
                                [End][Serialize]value
                              [End][Serialize]BoolToSigmaProp
                            [End][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                        [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEB]
                          [Start][Serialize][Non-constant]
                            [Start][Serialize]opCode
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]opCode
                            [Start][Serialize]Body [opCode=0xEB; Serializer=SigmaTransformerSerializer(-21,sigmastate.serialization.ValueSerializer$$$Lambda$231/0x000000084042d040@165614f5)]
                              [Start][Serialize]SigmaTransformer
                                [Start][Serialize]items.length
                                  [Start Real][Serialize]Put UInt
                                    [Start Real][Serialize]Put ULong
                                      [Start][Serialize]VLQLong
                                      [End][Serialize]VLQLong
                                    [End Real][Serialize]Put ULong
                                  [End Real][Serialize]Put UInt
                                [End][Serialize]items.length
                                [Start][Serialize]items*
                                  [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                                        [Start][Serialize]SigmaTransformer
                                          [Start][Serialize]items.length
                                            [Start Real][Serialize]Put UInt
                                              [Start Real][Serialize]Put ULong
                                                [Start][Serialize]VLQLong
                                                [End][Serialize]VLQLong
                                              [End Real][Serialize]Put ULong
                                            [End Real][Serialize]Put UInt
                                          [End][Serialize]items.length
                                          [Start][Serialize]items*
                                            [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                                              [Start][Serialize][Constant with store]
                                                [Start][Serialize]placeholder.opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]placeholder.opCode
                                                [Start][Serialize]constantPlaceholder
                                                  [Start][Serialize]ConstantPlaceholder
                                                    [Start][Serialize]id
                                                      [Start Real][Serialize]Put UInt
                                                        [Start Real][Serialize]Put ULong
                                                          [Start][Serialize]VLQLong
                                                          [End][Serialize]VLQLong
                                                        [End Real][Serialize]Put ULong
                                                      [End Real][Serialize]Put UInt
                                                    [End][Serialize]id
                                                  [End][Serialize]ConstantPlaceholder
                                                [End][Serialize]constantPlaceholder
                                              [End][Serialize][Constant with store]
                                            [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
                                            [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                                              [Start][Serialize][Non-constant]
                                                [Start][Serialize]opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]opCode
                                                [Start][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                                                  [Start][Serialize]BoolToSigmaProp
                                                    [Start][Serialize]value
                                                      [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                        [Start][Serialize][Non-constant]
                                                          [Start][Serialize]opCode
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]opCode
                                                          [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                            [Start][Serialize]Relation2
                                                              [Start][Serialize][Not BooleanConstants]
                                                                [Start][Serialize]left
                                                                  [Start][Serialize]Value [toString=SByte; opCode=0x72]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                        [Start][Serialize]ValUse
                                                                          [Start][Serialize]valId
                                                                            [Start Real][Serialize]Put UInt
                                                                              [Start Real][Serialize]Put ULong
                                                                                [Start][Serialize]VLQLong
                                                                                [End][Serialize]VLQLong
                                                                              [End Real][Serialize]Put ULong
                                                                            [End Real][Serialize]Put UInt
                                                                          [End][Serialize]valId
                                                                        [End][Serialize]ValUse
                                                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=SByte; opCode=0x72]
                                                                [End][Serialize]left
                                                                [Start][Serialize]right
                                                                  [Start][Serialize]Value [toString=SByte; opCode=0x72]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                        [Start][Serialize]ValUse
                                                                          [Start][Serialize]valId
                                                                            [Start Real][Serialize]Put UInt
                                                                              [Start Real][Serialize]Put ULong
                                                                                [Start][Serialize]VLQLong
                                                                                [End][Serialize]VLQLong
                                                                              [End Real][Serialize]Put ULong
                                                                            [End Real][Serialize]Put UInt
                                                                          [End][Serialize]valId
                                                                        [End][Serialize]ValUse
                                                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=SByte; opCode=0x72]
                                                                [End][Serialize]right
                                                              [End][Serialize][Not BooleanConstants]
                                                            [End][Serialize]Relation2
                                                          [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                        [End][Serialize][Non-constant]
                                                      [End][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                    [End][Serialize]value
                                                  [End][Serialize]BoolToSigmaProp
                                                [End][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                                              [End][Serialize][Non-constant]
                                            [End][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                                          [End][Serialize]items*
                                        [End][Serialize]SigmaTransformer
                                      [End][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
                                  [Start][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
                                    [Start][Serialize][Non-constant]
                                      [Start][Serialize]opCode
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]opCode
                                      [Start][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                                        [Start][Serialize]SigmaTransformer
                                          [Start][Serialize]items.length
                                            [Start Real][Serialize]Put UInt
                                              [Start Real][Serialize]Put ULong
                                                [Start][Serialize]VLQLong
                                                [End][Serialize]VLQLong
                                              [End Real][Serialize]Put ULong
                                            [End Real][Serialize]Put UInt
                                          [End][Serialize]items.length
                                          [Start][Serialize]items*
                                            [Start][Serialize]Value [toString=SSigmaProp; opCode=0x72]
                                              [Start][Serialize][Non-constant]
                                                [Start][Serialize]opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]opCode
                                                [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                  [Start][Serialize]ValUse
                                                    [Start][Serialize]valId
                                                      [Start Real][Serialize]Put UInt
                                                        [Start Real][Serialize]Put ULong
                                                          [Start][Serialize]VLQLong
                                                          [End][Serialize]VLQLong
                                                        [End Real][Serialize]Put ULong
                                                      [End Real][Serialize]Put UInt
                                                    [End][Serialize]valId
                                                  [End][Serialize]ValUse
                                                [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                              [End][Serialize][Non-constant]
                                            [End][Serialize]Value [toString=SSigmaProp; opCode=0x72]
                                            [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                                              [Start][Serialize][Non-constant]
                                                [Start][Serialize]opCode
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]opCode
                                                [Start][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                                                  [Start][Serialize]BoolToSigmaProp
                                                    [Start][Serialize]value
                                                      [Start][Serialize]Value [toString=SBoolean; opCode=0x94]
                                                        [Start][Serialize][Non-constant]
                                                          [Start][Serialize]opCode
                                                            [Start Real][Serialize]Put Byte
                                                            [End Real][Serialize]Put Byte
                                                          [End][Serialize]opCode
                                                          [Start][Serialize]Body [opCode=0x94; Serializer=Relation2Serializer(-108,sigmastate.serialization.ValueSerializer$$$Lambda$171/0x0000000840156040@333398f)]
                                                            [Start][Serialize]Relation2
                                                              [Start][Serialize][Not BooleanConstants]
                                                                [Start][Serialize]left
                                                                  [Start][Serialize]Value [toString=SByte; opCode=0x72]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                        [Start][Serialize]ValUse
                                                                          [Start][Serialize]valId
                                                                            [Start Real][Serialize]Put UInt
                                                                              [Start Real][Serialize]Put ULong
                                                                                [Start][Serialize]VLQLong
                                                                                [End][Serialize]VLQLong
                                                                              [End Real][Serialize]Put ULong
                                                                            [End Real][Serialize]Put UInt
                                                                          [End][Serialize]valId
                                                                        [End][Serialize]ValUse
                                                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=SByte; opCode=0x72]
                                                                [End][Serialize]left
                                                                [Start][Serialize]right
                                                                  [Start][Serialize]Value [toString=SByte; opCode=0x72]
                                                                    [Start][Serialize][Non-constant]
                                                                      [Start][Serialize]opCode
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]opCode
                                                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                        [Start][Serialize]ValUse
                                                                          [Start][Serialize]valId
                                                                            [Start Real][Serialize]Put UInt
                                                                              [Start Real][Serialize]Put ULong
                                                                                [Start][Serialize]VLQLong
                                                                                [End][Serialize]VLQLong
                                                                              [End Real][Serialize]Put ULong
                                                                            [End Real][Serialize]Put UInt
                                                                          [End][Serialize]valId
                                                                        [End][Serialize]ValUse
                                                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                    [End][Serialize][Non-constant]
                                                                  [End][Serialize]Value [toString=SByte; opCode=0x72]
                                                                [End][Serialize]right
                                                              [End][Serialize][Not BooleanConstants]
                                                            [End][Serialize]Relation2
                                                          [End][Serialize]Body [opCode=0x94; Serializer=Relation2Serializer(-108,sigmastate.serialization.ValueSerializer$$$Lambda$171/0x0000000840156040@333398f)]
                                                        [End][Serialize][Non-constant]
                                                      [End][Serialize]Value [toString=SBoolean; opCode=0x94]
                                                    [End][Serialize]value
                                                  [End][Serialize]BoolToSigmaProp
                                                [End][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
                                              [End][Serialize][Non-constant]
                                            [End][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
                                          [End][Serialize]items*
                                        [End][Serialize]SigmaTransformer
                                      [End][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                                    [End][Serialize][Non-constant]
                                  [End][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
                                [End][Serialize]items*
                              [End][Serialize]SigmaTransformer
                            [End][Serialize]Body [opCode=0xEB; Serializer=SigmaTransformerSerializer(-21,sigmastate.serialization.ValueSerializer$$$Lambda$231/0x000000084042d040@165614f5)]
                          [End][Serialize][Non-constant]
                        [End][Serialize]Value [toString=SSigmaProp; opCode=0xEB]
                      [End][Serialize]items*
                    [End][Serialize]SigmaTransformer
                  [End][Serialize]Body [opCode=0xEA; Serializer=SigmaTransformerSerializer(-22,sigmastate.serialization.ValueSerializer$$$Lambda$230/0x000000084042c040@10ded6a9)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0xEA]
            [End][Serialize]items*
          [End][Serialize]SigmaTransformer
        [End][Serialize]Body [opCode=0xEB; Serializer=SigmaTransformerSerializer(-21,sigmastate.serialization.ValueSerializer$$$Lambda$231/0x000000084042d040@165614f5)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SSigmaProp; opCode=0xEB]
  [End][Serialize]result
[End][Serialize]BlockValue

34. ValUse
[Start][Serialize]ValUse
  [Start][Serialize]valId
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
  [End][Serialize]valId
[End][Serialize]ValUse

35. FuncValue
[Start][Serialize]FuncValue
  [Start][Serialize]args.length
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
  [End][Serialize]args.length
  [Start][Serialize](args.idx, args.tpe)*
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
    [Start][Serialize]Type [toString=SBox; typeCode=0x63]
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]Type [toString=SBox; typeCode=0x63]
  [End][Serialize](args.idx, args.tpe)*
  [Start][Serialize]body
    [Start][Serialize]Value [toString=SLong; opCode=0xDA]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xDA; Serializer=ApplySerializer(sigmastate.serialization.ValueSerializer$$$Lambda$227/0x000000084042b040@79308a2)]
          [Start][Serialize]Apply
            [Start][Serialize]func
              [Start][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                    [Start][Serialize]ValUse
                      [Start][Serialize]valId
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]valId
                    [End][Serialize]ValUse
                  [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
            [End][Serialize]func
            [Start][Serialize]args
              [Start][Serialize]Values
                [Start][Serialize]length
                  [Start Real][Serialize]Put UInt
                    [Start Real][Serialize]Put ULong
                      [Start][Serialize]VLQLong
                      [End][Serialize]VLQLong
                    [End Real][Serialize]Put ULong
                  [End Real][Serialize]Put UInt
                [End][Serialize]length
                [Start][Serialize]values*
                  [Start][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
                    [Start][Serialize][Non-constant]
                      [Start][Serialize]opCode
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]opCode
                      [Start][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
                        [Start][Serialize]MapCollection
                          [Start][Serialize]input
                            [Start][Serialize]Value [toString=Coll[(Coll[SByte],SLong)]; opCode=0xE4]
                              [Start][Serialize][Non-constant]
                                [Start][Serialize]opCode
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]opCode
                                [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                  [Start][Serialize]SimpleTransformer
                                    [Start][Serialize]input
                                      [Start][Serialize]Value [toString=Option[Coll[(Coll[SByte],SLong)]]; opCode=0xC6]
                                        [Start][Serialize][Non-constant]
                                          [Start][Serialize]opCode
                                            [Start Real][Serialize]Put Byte
                                            [End Real][Serialize]Put Byte
                                          [End][Serialize]opCode
                                          [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                            [Start][Serialize]ExtractRegisterAs
                                              [Start][Serialize]input
                                                [Start][Serialize]Value [toString=SBox; opCode=0x72]
                                                  [Start][Serialize][Non-constant]
                                                    [Start][Serialize]opCode
                                                      [Start Real][Serialize]Put Byte
                                                      [End Real][Serialize]Put Byte
                                                    [End][Serialize]opCode
                                                    [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                      [Start][Serialize]ValUse
                                                        [Start][Serialize]valId
                                                          [Start Real][Serialize]Put UInt
                                                            [Start Real][Serialize]Put ULong
                                                              [Start][Serialize]VLQLong
                                                              [End][Serialize]VLQLong
                                                            [End Real][Serialize]Put ULong
                                                          [End Real][Serialize]Put UInt
                                                        [End][Serialize]valId
                                                      [End][Serialize]ValUse
                                                    [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                  [End][Serialize][Non-constant]
                                                [End][Serialize]Value [toString=SBox; opCode=0x72]
                                              [End][Serialize]input
                                              [Start][Serialize]registerId.number
                                                [Start Real][Serialize]Put Byte
                                                [End Real][Serialize]Put Byte
                                              [End][Serialize]registerId.number
                                              [Start][Serialize]tpe.elemType
                                                [Start][Serialize]Type [toString=Coll[(Coll[SByte],SLong)]; typeCode=0x0C]
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                  [Start Real][Serialize]Put Byte
                                                  [End Real][Serialize]Put Byte
                                                [End][Serialize]Type [toString=Coll[(Coll[SByte],SLong)]; typeCode=0x0C]
                                              [End][Serialize]tpe.elemType
                                            [End][Serialize]ExtractRegisterAs
                                          [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                        [End][Serialize][Non-constant]
                                      [End][Serialize]Value [toString=Option[Coll[(Coll[SByte],SLong)]]; opCode=0xC6]
                                    [End][Serialize]input
                                  [End][Serialize]SimpleTransformer
                                [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                              [End][Serialize][Non-constant]
                            [End][Serialize]Value [toString=Coll[(Coll[SByte],SLong)]; opCode=0xE4]
                          [End][Serialize]input
                          [Start][Serialize]mapper
                            [Start][Serialize]Value [toString=((Coll[SByte],SLong)) => SLong; opCode=0xD9]
                              [Start][Serialize][Non-constant]
                                [Start][Serialize]opCode
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]opCode
                                [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                                  [Start][Serialize]FuncValue
                                    [Start][Serialize]args.length
                                      [Start Real][Serialize]Put UInt
                                        [Start Real][Serialize]Put ULong
                                          [Start][Serialize]VLQLong
                                          [End][Serialize]VLQLong
                                        [End Real][Serialize]Put ULong
                                      [End Real][Serialize]Put UInt
                                    [End][Serialize]args.length
                                    [Start][Serialize](args.idx, args.tpe)*
                                      [Start Real][Serialize]Put UInt
                                        [Start Real][Serialize]Put ULong
                                          [Start][Serialize]VLQLong
                                          [End][Serialize]VLQLong
                                        [End Real][Serialize]Put ULong
                                      [End Real][Serialize]Put UInt
                                      [Start][Serialize]Type [toString=(Coll[SByte],SLong); typeCode=0x60]
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                        [Start Real][Serialize]Put Byte
                                        [End Real][Serialize]Put Byte
                                      [End][Serialize]Type [toString=(Coll[SByte],SLong); typeCode=0x60]
                                    [End][Serialize](args.idx, args.tpe)*
                                    [Start][Serialize]body
                                      [Start][Serialize]Value [toString=SLong; opCode=0x95]
                                        [Start][Serialize][Non-constant]
                                          [Start][Serialize]opCode
                                            [Start Real][Serialize]Put Byte
                                            [End Real][Serialize]Put Byte
                                          [End][Serialize]opCode
                                          [Start][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                                            [Start][Serialize]Quadruple
                                              [Start][Serialize]first
                                                [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                  [Start][Serialize][Non-constant]
                                                    [Start][Serialize]opCode
                                                      [Start Real][Serialize]Put Byte
                                                      [End Real][Serialize]Put Byte
                                                    [End][Serialize]opCode
                                                    [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                      [Start][Serialize]Relation2
                                                        [Start][Serialize][Not BooleanConstants]
                                                          [Start][Serialize]left
                                                            [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x8C]
                                                              [Start][Serialize][Non-constant]
                                                                [Start][Serialize]opCode
                                                                  [Start Real][Serialize]Put Byte
                                                                  [End Real][Serialize]Put Byte
                                                                [End][Serialize]opCode
                                                                [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                  [Start][Serialize]SelectField
                                                                    [Start][Serialize]input
                                                                      [Start][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                        [Start][Serialize][Non-constant]
                                                                          [Start][Serialize]opCode
                                                                            [Start Real][Serialize]Put Byte
                                                                            [End Real][Serialize]Put Byte
                                                                          [End][Serialize]opCode
                                                                          [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                            [Start][Serialize]ValUse
                                                                              [Start][Serialize]valId
                                                                                [Start Real][Serialize]Put UInt
                                                                                  [Start Real][Serialize]Put ULong
                                                                                    [Start][Serialize]VLQLong
                                                                                    [End][Serialize]VLQLong
                                                                                  [End Real][Serialize]Put ULong
                                                                                [End Real][Serialize]Put UInt
                                                                              [End][Serialize]valId
                                                                            [End][Serialize]ValUse
                                                                          [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                        [End][Serialize][Non-constant]
                                                                      [End][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                    [End][Serialize]input
                                                                    [Start][Serialize]fieldIndex
                                                                      [Start Real][Serialize]Put Byte
                                                                      [End Real][Serialize]Put Byte
                                                                    [End][Serialize]fieldIndex
                                                                  [End][Serialize]SelectField
                                                                [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                              [End][Serialize][Non-constant]
                                                            [End][Serialize]Value [toString=Coll[SByte]; opCode=0x8C]
                                                          [End][Serialize]left
                                                          [Start][Serialize]right
                                                            [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
                                                              [Start][Serialize][Constant with store]
                                                                [Start][Serialize]placeholder.opCode
                                                                  [Start Real][Serialize]Put Byte
                                                                  [End Real][Serialize]Put Byte
                                                                [End][Serialize]placeholder.opCode
                                                                [Start][Serialize]constantPlaceholder
                                                                  [Start][Serialize]ConstantPlaceholder
                                                                    [Start][Serialize]id
                                                                      [Start Real][Serialize]Put UInt
                                                                        [Start Real][Serialize]Put ULong
                                                                          [Start][Serialize]VLQLong
                                                                          [End][Serialize]VLQLong
                                                                        [End Real][Serialize]Put ULong
                                                                      [End Real][Serialize]Put UInt
                                                                    [End][Serialize]id
                                                                  [End][Serialize]ConstantPlaceholder
                                                                [End][Serialize]constantPlaceholder
                                                              [End][Serialize][Constant with store]
                                                            [End][Serialize]Value [toString=Coll[SByte]; opCode=0x00]
                                                          [End][Serialize]right
                                                        [End][Serialize][Not BooleanConstants]
                                                      [End][Serialize]Relation2
                                                    [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                  [End][Serialize][Non-constant]
                                                [End][Serialize]Value [toString=SBoolean; opCode=0x93]
                                              [End][Serialize]first
                                              [Start][Serialize]second
                                                [Start][Serialize]Value [toString=SLong; opCode=0x8C]
                                                  [Start][Serialize][Non-constant]
                                                    [Start][Serialize]opCode
                                                      [Start Real][Serialize]Put Byte
                                                      [End Real][Serialize]Put Byte
                                                    [End][Serialize]opCode
                                                    [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                      [Start][Serialize]SelectField
                                                        [Start][Serialize]input
                                                          [Start][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                            [Start][Serialize][Non-constant]
                                                              [Start][Serialize]opCode
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                              [End][Serialize]opCode
                                                              [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                [Start][Serialize]ValUse
                                                                  [Start][Serialize]valId
                                                                    [Start Real][Serialize]Put UInt
                                                                      [Start Real][Serialize]Put ULong
                                                                        [Start][Serialize]VLQLong
                                                                        [End][Serialize]VLQLong
                                                                      [End Real][Serialize]Put ULong
                                                                    [End Real][Serialize]Put UInt
                                                                  [End][Serialize]valId
                                                                [End][Serialize]ValUse
                                                              [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                            [End][Serialize][Non-constant]
                                                          [End][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                        [End][Serialize]input
                                                        [Start][Serialize]fieldIndex
                                                          [Start Real][Serialize]Put Byte
                                                          [End Real][Serialize]Put Byte
                                                        [End][Serialize]fieldIndex
                                                      [End][Serialize]SelectField
                                                    [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                  [End][Serialize][Non-constant]
                                                [End][Serialize]Value [toString=SLong; opCode=0x8C]
                                              [End][Serialize]second
                                              [Start][Serialize]third
                                                [Start][Serialize]Value [toString=SLong; opCode=0x00]
                                                  [Start][Serialize][Constant with store]
                                                    [Start][Serialize]placeholder.opCode
                                                      [Start Real][Serialize]Put Byte
                                                      [End Real][Serialize]Put Byte
                                                    [End][Serialize]placeholder.opCode
                                                    [Start][Serialize]constantPlaceholder
                                                      [Start][Serialize]ConstantPlaceholder
                                                        [Start][Serialize]id
                                                          [Start Real][Serialize]Put UInt
                                                            [Start Real][Serialize]Put ULong
                                                              [Start][Serialize]VLQLong
                                                              [End][Serialize]VLQLong
                                                            [End Real][Serialize]Put ULong
                                                          [End Real][Serialize]Put UInt
                                                        [End][Serialize]id
                                                      [End][Serialize]ConstantPlaceholder
                                                    [End][Serialize]constantPlaceholder
                                                  [End][Serialize][Constant with store]
                                                [End][Serialize]Value [toString=SLong; opCode=0x00]
                                              [End][Serialize]third
                                            [End][Serialize]Quadruple
                                          [End][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                                        [End][Serialize][Non-constant]
                                      [End][Serialize]Value [toString=SLong; opCode=0x95]
                                    [End][Serialize]body
                                  [End][Serialize]FuncValue
                                [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                              [End][Serialize][Non-constant]
                            [End][Serialize]Value [toString=((Coll[SByte],SLong)) => SLong; opCode=0xD9]
                          [End][Serialize]mapper
                        [End][Serialize]MapCollection
                      [End][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
                    [End][Serialize][Non-constant]
                  [End][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
                [End][Serialize]values*
              [End][Serialize]Values
            [End][Serialize]args
          [End][Serialize]Apply
        [End][Serialize]Body [opCode=0xDA; Serializer=ApplySerializer(sigmastate.serialization.ValueSerializer$$$Lambda$227/0x000000084042b040@79308a2)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SLong; opCode=0xDA]
  [End][Serialize]body
[End][Serialize]FuncValue

36. Apply
[Start][Serialize]Apply
  [Start][Serialize]func
    [Start][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
          [Start][Serialize]ValUse
            [Start][Serialize]valId
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]valId
          [End][Serialize]ValUse
        [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
  [End][Serialize]func
  [Start][Serialize]args
    [Start][Serialize]Values
      [Start][Serialize]length
        [Start Real][Serialize]Put UInt
          [Start Real][Serialize]Put ULong
            [Start][Serialize]VLQLong
            [End][Serialize]VLQLong
          [End Real][Serialize]Put ULong
        [End Real][Serialize]Put UInt
      [End][Serialize]length
      [Start][Serialize]values*
        [Start][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
          [Start][Serialize][Non-constant]
            [Start][Serialize]opCode
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]opCode
            [Start][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
              [Start][Serialize]MapCollection
                [Start][Serialize]input
                  [Start][Serialize]Value [toString=Coll[SBox]; opCode=0xA4]
                    [Start][Serialize][Non-constant]
                      [Start][Serialize]opCode
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]opCode
                      [Start][Serialize]Body [opCode=0xA4; Serializer=CaseObjectSerialization(-92,Inputs)]
                        [Start][Serialize]CaseObject
                        [End][Serialize]CaseObject
                      [End][Serialize]Body [opCode=0xA4; Serializer=CaseObjectSerialization(-92,Inputs)]
                    [End][Serialize][Non-constant]
                  [End][Serialize]Value [toString=Coll[SBox]; opCode=0xA4]
                [End][Serialize]input
                [Start][Serialize]mapper
                  [Start][Serialize]Value [toString=(SBox) => SLong; opCode=0xD9]
                    [Start][Serialize][Non-constant]
                      [Start][Serialize]opCode
                        [Start Real][Serialize]Put Byte
                        [End Real][Serialize]Put Byte
                      [End][Serialize]opCode
                      [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                        [Start][Serialize]FuncValue
                          [Start][Serialize]args.length
                            [Start Real][Serialize]Put UInt
                              [Start Real][Serialize]Put ULong
                                [Start][Serialize]VLQLong
                                [End][Serialize]VLQLong
                              [End Real][Serialize]Put ULong
                            [End Real][Serialize]Put UInt
                          [End][Serialize]args.length
                          [Start][Serialize](args.idx, args.tpe)*
                            [Start Real][Serialize]Put UInt
                              [Start Real][Serialize]Put ULong
                                [Start][Serialize]VLQLong
                                [End][Serialize]VLQLong
                              [End Real][Serialize]Put ULong
                            [End Real][Serialize]Put UInt
                            [Start][Serialize]Type [toString=SBox; typeCode=0x63]
                              [Start Real][Serialize]Put Byte
                              [End Real][Serialize]Put Byte
                            [End][Serialize]Type [toString=SBox; typeCode=0x63]
                          [End][Serialize](args.idx, args.tpe)*
                          [Start][Serialize]body
                            [Start][Serialize]Value [toString=SLong; opCode=0xDA]
                              [Start][Serialize][Non-constant]
                                [Start][Serialize]opCode
                                  [Start Real][Serialize]Put Byte
                                  [End Real][Serialize]Put Byte
                                [End][Serialize]opCode
                                [Start][Serialize]Body [opCode=0xDA; Serializer=ApplySerializer(sigmastate.serialization.ValueSerializer$$$Lambda$227/0x000000084042b040@79308a2)]
                                  [Start][Serialize]Apply
                                    [Start][Serialize]func
                                      [Start][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
                                        [Start][Serialize][Non-constant]
                                          [Start][Serialize]opCode
                                            [Start Real][Serialize]Put Byte
                                            [End Real][Serialize]Put Byte
                                          [End][Serialize]opCode
                                          [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                            [Start][Serialize]ValUse
                                              [Start][Serialize]valId
                                                [Start Real][Serialize]Put UInt
                                                  [Start Real][Serialize]Put ULong
                                                    [Start][Serialize]VLQLong
                                                    [End][Serialize]VLQLong
                                                  [End Real][Serialize]Put ULong
                                                [End Real][Serialize]Put UInt
                                              [End][Serialize]valId
                                            [End][Serialize]ValUse
                                          [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                        [End][Serialize][Non-constant]
                                      [End][Serialize]Value [toString=(Coll[SLong]) => SLong; opCode=0x72]
                                    [End][Serialize]func
                                    [Start][Serialize]args
                                      [Start][Serialize]Values
                                        [Start][Serialize]length
                                          [Start Real][Serialize]Put UInt
                                            [Start Real][Serialize]Put ULong
                                              [Start][Serialize]VLQLong
                                              [End][Serialize]VLQLong
                                            [End Real][Serialize]Put ULong
                                          [End Real][Serialize]Put UInt
                                        [End][Serialize]length
                                        [Start][Serialize]values*
                                          [Start][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
                                            [Start][Serialize][Non-constant]
                                              [Start][Serialize]opCode
                                                [Start Real][Serialize]Put Byte
                                                [End Real][Serialize]Put Byte
                                              [End][Serialize]opCode
                                              [Start][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
                                                [Start][Serialize]MapCollection
                                                  [Start][Serialize]input
                                                    [Start][Serialize]Value [toString=Coll[(Coll[SByte],SLong)]; opCode=0xE4]
                                                      [Start][Serialize][Non-constant]
                                                        [Start][Serialize]opCode
                                                          [Start Real][Serialize]Put Byte
                                                          [End Real][Serialize]Put Byte
                                                        [End][Serialize]opCode
                                                        [Start][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                                          [Start][Serialize]SimpleTransformer
                                                            [Start][Serialize]input
                                                              [Start][Serialize]Value [toString=Option[Coll[(Coll[SByte],SLong)]]; opCode=0xC6]
                                                                [Start][Serialize][Non-constant]
                                                                  [Start][Serialize]opCode
                                                                    [Start Real][Serialize]Put Byte
                                                                    [End Real][Serialize]Put Byte
                                                                  [End][Serialize]opCode
                                                                  [Start][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                                                    [Start][Serialize]ExtractRegisterAs
                                                                      [Start][Serialize]input
                                                                        [Start][Serialize]Value [toString=SBox; opCode=0x72]
                                                                          [Start][Serialize][Non-constant]
                                                                            [Start][Serialize]opCode
                                                                              [Start Real][Serialize]Put Byte
                                                                              [End Real][Serialize]Put Byte
                                                                            [End][Serialize]opCode
                                                                            [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                              [Start][Serialize]ValUse
                                                                                [Start][Serialize]valId
                                                                                  [Start Real][Serialize]Put UInt
                                                                                    [Start Real][Serialize]Put ULong
                                                                                      [Start][Serialize]VLQLong
                                                                                      [End][Serialize]VLQLong
                                                                                    [End Real][Serialize]Put ULong
                                                                                  [End Real][Serialize]Put UInt
                                                                                [End][Serialize]valId
                                                                              [End][Serialize]ValUse
                                                                            [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                          [End][Serialize][Non-constant]
                                                                        [End][Serialize]Value [toString=SBox; opCode=0x72]
                                                                      [End][Serialize]input
                                                                      [Start][Serialize]registerId.number
                                                                        [Start Real][Serialize]Put Byte
                                                                        [End Real][Serialize]Put Byte
                                                                      [End][Serialize]registerId.number
                                                                      [Start][Serialize]tpe.elemType
                                                                        [Start][Serialize]Type [toString=Coll[(Coll[SByte],SLong)]; typeCode=0x0C]
                                                                          [Start Real][Serialize]Put Byte
                                                                          [End Real][Serialize]Put Byte
                                                                          [Start Real][Serialize]Put Byte
                                                                          [End Real][Serialize]Put Byte
                                                                          [Start Real][Serialize]Put Byte
                                                                          [End Real][Serialize]Put Byte
                                                                        [End][Serialize]Type [toString=Coll[(Coll[SByte],SLong)]; typeCode=0x0C]
                                                                      [End][Serialize]tpe.elemType
                                                                    [End][Serialize]ExtractRegisterAs
                                                                  [End][Serialize]Body [opCode=0xC6; Serializer=ExtractRegisterAsSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$216/0x0000000840424440@71789580)]
                                                                [End][Serialize][Non-constant]
                                                              [End][Serialize]Value [toString=Option[Coll[(Coll[SByte],SLong)]]; opCode=0xC6]
                                                            [End][Serialize]input
                                                          [End][Serialize]SimpleTransformer
                                                        [End][Serialize]Body [opCode=0xE4; Serializer=SimpleTransformerSerializer(-28,sigmastate.serialization.ValueSerializer$$$Lambda$211/0x0000000840421840@5bf4764d)]
                                                      [End][Serialize][Non-constant]
                                                    [End][Serialize]Value [toString=Coll[(Coll[SByte],SLong)]; opCode=0xE4]
                                                  [End][Serialize]input
                                                  [Start][Serialize]mapper
                                                    [Start][Serialize]Value [toString=((Coll[SByte],SLong)) => SLong; opCode=0xD9]
                                                      [Start][Serialize][Non-constant]
                                                        [Start][Serialize]opCode
                                                          [Start Real][Serialize]Put Byte
                                                          [End Real][Serialize]Put Byte
                                                        [End][Serialize]opCode
                                                        [Start][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                                                          [Start][Serialize]FuncValue
                                                            [Start][Serialize]args.length
                                                              [Start Real][Serialize]Put UInt
                                                                [Start Real][Serialize]Put ULong
                                                                  [Start][Serialize]VLQLong
                                                                  [End][Serialize]VLQLong
                                                                [End Real][Serialize]Put ULong
                                                              [End Real][Serialize]Put UInt
                                                            [End][Serialize]args.length
                                                            [Start][Serialize](args.idx, args.tpe)*
                                                              [Start Real][Serialize]Put UInt
                                                                [Start Real][Serialize]Put ULong
                                                                  [Start][Serialize]VLQLong
                                                                  [End][Serialize]VLQLong
                                                                [End Real][Serialize]Put ULong
                                                              [End Real][Serialize]Put UInt
                                                              [Start][Serialize]Type [toString=(Coll[SByte],SLong); typeCode=0x60]
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                                [Start Real][Serialize]Put Byte
                                                                [End Real][Serialize]Put Byte
                                                              [End][Serialize]Type [toString=(Coll[SByte],SLong); typeCode=0x60]
                                                            [End][Serialize](args.idx, args.tpe)*
                                                            [Start][Serialize]body
                                                              [Start][Serialize]Value [toString=SLong; opCode=0x95]
                                                                [Start][Serialize][Non-constant]
                                                                  [Start][Serialize]opCode
                                                                    [Start Real][Serialize]Put Byte
                                                                    [End Real][Serialize]Put Byte
                                                                  [End][Serialize]opCode
                                                                  [Start][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                                                                    [Start][Serialize]Quadruple
                                                                      [Start][Serialize]first
                                                                        [Start][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                                          [Start][Serialize][Non-constant]
                                                                            [Start][Serialize]opCode
                                                                              [Start Real][Serialize]Put Byte
                                                                              [End Real][Serialize]Put Byte
                                                                            [End][Serialize]opCode
                                                                            [Start][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                                              [Start][Serialize]Relation2
                                                                                [Start][Serialize][Not BooleanConstants]
                                                                                  [Start][Serialize]left
                                                                                    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x8C]
                                                                                      [Start][Serialize][Non-constant]
                                                                                        [Start][Serialize]opCode
                                                                                          [Start Real][Serialize]Put Byte
                                                                                          [End Real][Serialize]Put Byte
                                                                                        [End][Serialize]opCode
                                                                                        [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                                          [Start][Serialize]SelectField
                                                                                            [Start][Serialize]input
                                                                                              [Start][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                                                [Start][Serialize][Non-constant]
                                                                                                  [Start][Serialize]opCode
                                                                                                    [Start Real][Serialize]Put Byte
                                                                                                    [End Real][Serialize]Put Byte
                                                                                                  [End][Serialize]opCode
                                                                                                  [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                                    [Start][Serialize]ValUse
                                                                                                      [Start][Serialize]valId
                                                                                                        [Start Real][Serialize]Put UInt
                                                                                                          [Start Real][Serialize]Put ULong
                                                                                                            [Start][Serialize]VLQLong
                                                                                                            [End][Serialize]VLQLong
                                                                                                          [End Real][Serialize]Put ULong
                                                                                                        [End Real][Serialize]Put UInt
                                                                                                      [End][Serialize]valId
                                                                                                    [End][Serialize]ValUse
                                                                                                  [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                                [End][Serialize][Non-constant]
                                                                                              [End][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                                            [End][Serialize]input
                                                                                            [Start][Serialize]fieldIndex
                                                                                              [Start Real][Serialize]Put Byte
                                                                                              [End Real][Serialize]Put Byte
                                                                                            [End][Serialize]fieldIndex
                                                                                          [End][Serialize]SelectField
                                                                                        [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                                      [End][Serialize][Non-constant]
                                                                                    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x8C]
                                                                                  [End][Serialize]left
                                                                                  [Start][Serialize]right
                                                                                    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x73]
                                                                                      [Start][Serialize][Non-constant]
                                                                                        [Start][Serialize]opCode
                                                                                          [Start Real][Serialize]Put Byte
                                                                                          [End Real][Serialize]Put Byte
                                                                                        [End][Serialize]opCode
                                                                                        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                                          [Start][Serialize]ConstantPlaceholder
                                                                                            [Start][Serialize]id
                                                                                              [Start Real][Serialize]Put UInt
                                                                                                [Start Real][Serialize]Put ULong
                                                                                                  [Start][Serialize]VLQLong
                                                                                                  [End][Serialize]VLQLong
                                                                                                [End Real][Serialize]Put ULong
                                                                                              [End Real][Serialize]Put UInt
                                                                                            [End][Serialize]id
                                                                                          [End][Serialize]ConstantPlaceholder
                                                                                        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                                      [End][Serialize][Non-constant]
                                                                                    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x73]
                                                                                  [End][Serialize]right
                                                                                [End][Serialize][Not BooleanConstants]
                                                                              [End][Serialize]Relation2
                                                                            [End][Serialize]Body [opCode=0x93; Serializer=Relation2Serializer(-109,sigmastate.serialization.ValueSerializer$$$Lambda$170/0x0000000840157040@1d730606)]
                                                                          [End][Serialize][Non-constant]
                                                                        [End][Serialize]Value [toString=SBoolean; opCode=0x93]
                                                                      [End][Serialize]first
                                                                      [Start][Serialize]second
                                                                        [Start][Serialize]Value [toString=SLong; opCode=0x8C]
                                                                          [Start][Serialize][Non-constant]
                                                                            [Start][Serialize]opCode
                                                                              [Start Real][Serialize]Put Byte
                                                                              [End Real][Serialize]Put Byte
                                                                            [End][Serialize]opCode
                                                                            [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                              [Start][Serialize]SelectField
                                                                                [Start][Serialize]input
                                                                                  [Start][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                                    [Start][Serialize][Non-constant]
                                                                                      [Start][Serialize]opCode
                                                                                        [Start Real][Serialize]Put Byte
                                                                                        [End Real][Serialize]Put Byte
                                                                                      [End][Serialize]opCode
                                                                                      [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                        [Start][Serialize]ValUse
                                                                                          [Start][Serialize]valId
                                                                                            [Start Real][Serialize]Put UInt
                                                                                              [Start Real][Serialize]Put ULong
                                                                                                [Start][Serialize]VLQLong
                                                                                                [End][Serialize]VLQLong
                                                                                              [End Real][Serialize]Put ULong
                                                                                            [End Real][Serialize]Put UInt
                                                                                          [End][Serialize]valId
                                                                                        [End][Serialize]ValUse
                                                                                      [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                                                                                    [End][Serialize][Non-constant]
                                                                                  [End][Serialize]Value [toString=(Coll[SByte],SLong); opCode=0x72]
                                                                                [End][Serialize]input
                                                                                [Start][Serialize]fieldIndex
                                                                                  [Start Real][Serialize]Put Byte
                                                                                  [End Real][Serialize]Put Byte
                                                                                [End][Serialize]fieldIndex
                                                                              [End][Serialize]SelectField
                                                                            [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                                                          [End][Serialize][Non-constant]
                                                                        [End][Serialize]Value [toString=SLong; opCode=0x8C]
                                                                      [End][Serialize]second
                                                                      [Start][Serialize]third
                                                                        [Start][Serialize]Value [toString=SLong; opCode=0x73]
                                                                          [Start][Serialize][Non-constant]
                                                                            [Start][Serialize]opCode
                                                                              [Start Real][Serialize]Put Byte
                                                                              [End Real][Serialize]Put Byte
                                                                            [End][Serialize]opCode
                                                                            [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                              [Start][Serialize]ConstantPlaceholder
                                                                                [Start][Serialize]id
                                                                                  [Start Real][Serialize]Put UInt
                                                                                    [Start Real][Serialize]Put ULong
                                                                                      [Start][Serialize]VLQLong
                                                                                      [End][Serialize]VLQLong
                                                                                    [End Real][Serialize]Put ULong
                                                                                  [End Real][Serialize]Put UInt
                                                                                [End][Serialize]id
                                                                              [End][Serialize]ConstantPlaceholder
                                                                            [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                                                                          [End][Serialize][Non-constant]
                                                                        [End][Serialize]Value [toString=SLong; opCode=0x73]
                                                                      [End][Serialize]third
                                                                    [End][Serialize]Quadruple
                                                                  [End][Serialize]Body [opCode=0x95; Serializer=QuadrupleSerializer(-107,sigmastate.serialization.ValueSerializer$$$Lambda$176/0x0000000840400040@e154848)]
                                                                [End][Serialize][Non-constant]
                                                              [End][Serialize]Value [toString=SLong; opCode=0x95]
                                                            [End][Serialize]body
                                                          [End][Serialize]FuncValue
                                                        [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                                                      [End][Serialize][Non-constant]
                                                    [End][Serialize]Value [toString=((Coll[SByte],SLong)) => SLong; opCode=0xD9]
                                                  [End][Serialize]mapper
                                                [End][Serialize]MapCollection
                                              [End][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
                                            [End][Serialize][Non-constant]
                                          [End][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
                                        [End][Serialize]values*
                                      [End][Serialize]Values
                                    [End][Serialize]args
                                  [End][Serialize]Apply
                                [End][Serialize]Body [opCode=0xDA; Serializer=ApplySerializer(sigmastate.serialization.ValueSerializer$$$Lambda$227/0x000000084042b040@79308a2)]
                              [End][Serialize][Non-constant]
                            [End][Serialize]Value [toString=SLong; opCode=0xDA]
                          [End][Serialize]body
                        [End][Serialize]FuncValue
                      [End][Serialize]Body [opCode=0xD9; Serializer=FuncValueSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$226/0x000000084042a040@120350eb)]
                    [End][Serialize][Non-constant]
                  [End][Serialize]Value [toString=(SBox) => SLong; opCode=0xD9]
                [End][Serialize]mapper
              [End][Serialize]MapCollection
            [End][Serialize]Body [opCode=0xAD; Serializer=MapCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$195/0x000000084040e040@352e5a82)]
          [End][Serialize][Non-constant]
        [End][Serialize]Value [toString=Coll[SLong]; opCode=0xAD]
      [End][Serialize]values*
    [End][Serialize]Values
  [End][Serialize]args
[End][Serialize]Apply

37. MethodCall
[Start][Serialize]MethodCall
  [Start][Serialize]method.objType.typeId
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]method.objType.typeId
  [Start][Serialize]method.methodId
    [Start Real][Serialize]Put Byte
    [End Real][Serialize]Put Byte
  [End][Serialize]method.methodId
  [Start][Serialize]obj
    [Start][Serialize]Value [toString=SInt; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SInt; typeCode=0x04]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SInt; typeCode=0x04]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put Int
              [Start][Serialize]ZigZagInt
              [End][Serialize]ZigZagInt
              [Start Real][Serialize]Put ULong
                [Start][Serialize]VLQLong
                [End][Serialize]VLQLong
              [End Real][Serialize]Put ULong
            [End Real][Serialize]Put Int
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SInt; opCode=0x00]
  [End][Serialize]obj
[End][Serialize]MethodCall

38. SigmaTransformer
[Start][Serialize]SigmaTransformer
  [Start][Serialize]items.length
    [Start Real][Serialize]Put UInt
      [Start Real][Serialize]Put ULong
        [Start][Serialize]VLQLong
        [End][Serialize]VLQLong
      [End Real][Serialize]Put ULong
    [End Real][Serialize]Put UInt
  [End][Serialize]items.length
  [Start][Serialize]items*
    [Start][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
          [Start][Serialize]BoolToSigmaProp
            [Start][Serialize]value
              [Start][Serialize]Value [toString=SBoolean; opCode=0x92]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x92; Serializer=Relation2Serializer(-110,sigmastate.serialization.ValueSerializer$$$Lambda$167/0x00000008401b5840@52066604)]
                    [Start][Serialize]Relation2
                      [Start][Serialize][Not BooleanConstants]
                        [Start][Serialize]left
                          [Start][Serialize]Value [toString=SInt; opCode=0xA3]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                                [Start][Serialize]CaseObject
                                [End][Serialize]CaseObject
                              [End][Serialize]Body [opCode=0xA3; Serializer=CaseObjectSerialization(-93,Height)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=SInt; opCode=0xA3]
                        [End][Serialize]left
                        [Start][Serialize]right
                          [Start][Serialize]Value [toString=SInt; opCode=0x9A]
                            [Start][Serialize][Non-constant]
                              [Start][Serialize]opCode
                                [Start Real][Serialize]Put Byte
                                [End Real][Serialize]Put Byte
                              [End][Serialize]opCode
                              [Start][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                                [Start][Serialize]TwoArguments
                                  [Start][Serialize]left
                                    [Start][Serialize]Value [toString=SInt; opCode=0x8C]
                                      [Start][Serialize][Non-constant]
                                        [Start][Serialize]opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]opCode
                                        [Start][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                          [Start][Serialize]SelectField
                                            [Start][Serialize]input
                                              [Start][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                                                [Start][Serialize][Non-constant]
                                                  [Start][Serialize]opCode
                                                    [Start Real][Serialize]Put Byte
                                                    [End Real][Serialize]Put Byte
                                                  [End][Serialize]opCode
                                                  [Start][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                                    [Start][Serialize]SimpleTransformer
                                                      [Start][Serialize]input
                                                        [Start][Serialize]Value [toString=SBox; opCode=0xA7]
                                                          [Start][Serialize][Non-constant]
                                                            [Start][Serialize]opCode
                                                              [Start Real][Serialize]Put Byte
                                                              [End Real][Serialize]Put Byte
                                                            [End][Serialize]opCode
                                                            [Start][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                                              [Start][Serialize]CaseObject
                                                              [End][Serialize]CaseObject
                                                            [End][Serialize]Body [opCode=0xA7; Serializer=CaseObjectSerialization(-89,Self)]
                                                          [End][Serialize][Non-constant]
                                                        [End][Serialize]Value [toString=SBox; opCode=0xA7]
                                                      [End][Serialize]input
                                                    [End][Serialize]SimpleTransformer
                                                  [End][Serialize]Body [opCode=0xC7; Serializer=SimpleTransformerSerializer(-57,sigmastate.serialization.ValueSerializer$$$Lambda$205/0x000000084041d040@5669c5fb)]
                                                [End][Serialize][Non-constant]
                                              [End][Serialize]Value [toString=(SInt,Coll[SByte]); opCode=0xC7]
                                            [End][Serialize]input
                                            [Start][Serialize]fieldIndex
                                              [Start Real][Serialize]Put Byte
                                              [End Real][Serialize]Put Byte
                                            [End][Serialize]fieldIndex
                                          [End][Serialize]SelectField
                                        [End][Serialize]Body [opCode=0x8C; Serializer=SelectFieldSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$165/0x00000008401b7040@56113384)]
                                      [End][Serialize][Non-constant]
                                    [End][Serialize]Value [toString=SInt; opCode=0x8C]
                                  [End][Serialize]left
                                  [Start][Serialize]right
                                    [Start][Serialize]Value [toString=SInt; opCode=0x00]
                                      [Start][Serialize][Constant with store]
                                        [Start][Serialize]placeholder.opCode
                                          [Start Real][Serialize]Put Byte
                                          [End Real][Serialize]Put Byte
                                        [End][Serialize]placeholder.opCode
                                        [Start][Serialize]constantPlaceholder
                                          [Start][Serialize]ConstantPlaceholder
                                            [Start][Serialize]id
                                              [Start Real][Serialize]Put UInt
                                                [Start Real][Serialize]Put ULong
                                                  [Start][Serialize]VLQLong
                                                  [End][Serialize]VLQLong
                                                [End Real][Serialize]Put ULong
                                              [End Real][Serialize]Put UInt
                                            [End][Serialize]id
                                          [End][Serialize]ConstantPlaceholder
                                        [End][Serialize]constantPlaceholder
                                      [End][Serialize][Constant with store]
                                    [End][Serialize]Value [toString=SInt; opCode=0x00]
                                  [End][Serialize]right
                                [End][Serialize]TwoArguments
                              [End][Serialize]Body [opCode=0x9A; Serializer=TwoArgumentsSerializer(-102,sigmastate.serialization.ValueSerializer$$$Lambda$184/0x0000000840406040@340b9973)]
                            [End][Serialize][Non-constant]
                          [End][Serialize]Value [toString=SInt; opCode=0x9A]
                        [End][Serialize]right
                      [End][Serialize][Not BooleanConstants]
                    [End][Serialize]Relation2
                  [End][Serialize]Body [opCode=0x92; Serializer=Relation2Serializer(-110,sigmastate.serialization.ValueSerializer$$$Lambda$167/0x00000008401b5840@52066604)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SBoolean; opCode=0x92]
            [End][Serialize]value
          [End][Serialize]BoolToSigmaProp
        [End][Serialize]Body [opCode=0xD1; Serializer=BoolToSigmaPropSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$232/0x000000084042d840@4a194c39)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SSigmaProp; opCode=0xD1]
    [Start][Serialize]Value [toString=SSigmaProp; opCode=0x00]
      [Start][Serialize][Constant with store]
        [Start][Serialize]placeholder.opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]placeholder.opCode
        [Start][Serialize]constantPlaceholder
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]constantPlaceholder
      [End][Serialize][Constant with store]
    [End][Serialize]Value [toString=SSigmaProp; opCode=0x00]
  [End][Serialize]items*
[End][Serialize]SigmaTransformer

39. BoolToSigmaProp
[Start][Serialize]BoolToSigmaProp
  [Start][Serialize]value
    [Start][Serialize]Value [toString=SBoolean; opCode=0x94]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x94; Serializer=Relation2Serializer(-108,sigmastate.serialization.ValueSerializer$$$Lambda$171/0x0000000840156040@333398f)]
          [Start][Serialize]Relation2
            [Start][Serialize][Not BooleanConstants]
              [Start][Serialize]left
                [Start][Serialize]Value [toString=SByte; opCode=0x72]
                  [Start][Serialize][Non-constant]
                    [Start][Serialize]opCode
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]opCode
                    [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                      [Start][Serialize]ValUse
                        [Start][Serialize]valId
                          [Start Real][Serialize]Put UInt
                            [Start Real][Serialize]Put ULong
                              [Start][Serialize]VLQLong
                              [End][Serialize]VLQLong
                            [End Real][Serialize]Put ULong
                          [End Real][Serialize]Put UInt
                        [End][Serialize]valId
                      [End][Serialize]ValUse
                    [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                  [End][Serialize][Non-constant]
                [End][Serialize]Value [toString=SByte; opCode=0x72]
              [End][Serialize]left
              [Start][Serialize]right
                [Start][Serialize]Value [toString=SByte; opCode=0x72]
                  [Start][Serialize][Non-constant]
                    [Start][Serialize]opCode
                      [Start Real][Serialize]Put Byte
                      [End Real][Serialize]Put Byte
                    [End][Serialize]opCode
                    [Start][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                      [Start][Serialize]ValUse
                        [Start][Serialize]valId
                          [Start Real][Serialize]Put UInt
                            [Start Real][Serialize]Put ULong
                              [Start][Serialize]VLQLong
                              [End][Serialize]VLQLong
                            [End Real][Serialize]Put ULong
                          [End Real][Serialize]Put UInt
                        [End][Serialize]valId
                      [End][Serialize]ValUse
                    [End][Serialize]Body [opCode=0x72; Serializer=ValUseSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$225/0x0000000840429840@6e685e6c)]
                  [End][Serialize][Non-constant]
                [End][Serialize]Value [toString=SByte; opCode=0x72]
              [End][Serialize]right
            [End][Serialize][Not BooleanConstants]
          [End][Serialize]Relation2
        [End][Serialize]Body [opCode=0x94; Serializer=Relation2Serializer(-108,sigmastate.serialization.ValueSerializer$$$Lambda$171/0x0000000840156040@333398f)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=SBoolean; opCode=0x94]
  [End][Serialize]value
[End][Serialize]BoolToSigmaProp

40. ModQ
[Start][Serialize]ModQ
  [Start][Serialize]input
    [Start][Serialize]Value [toString=SBigInt; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SBigInt; typeCode=0x06]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SBigInt; typeCode=0x06]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put UShort
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End Real][Serialize]Put UShort
            [Start Real][Serialize]Put Bytes
            [End Real][Serialize]Put Bytes
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SBigInt; opCode=0x00]
  [End][Serialize]input
[End][Serialize]ModQ

41. ModQArithOp
[Start][Serialize]ModQArithOp
  [Start][Serialize]left
    [Start][Serialize]Value [toString=SBigInt; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SBigInt; typeCode=0x06]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SBigInt; typeCode=0x06]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put UShort
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End Real][Serialize]Put UShort
            [Start Real][Serialize]Put Bytes
            [End Real][Serialize]Put Bytes
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SBigInt; opCode=0x00]
  [End][Serialize]left
  [Start][Serialize]right
    [Start][Serialize]Value [toString=SBigInt; opCode=0x00]
      [Start][Serialize][Constant without store]
        [Start][Serialize]Constant
          [Start][Serialize]tpe
            [Start][Serialize]Type [toString=SBigInt; typeCode=0x06]
              [Start Real][Serialize]Put Byte
              [End Real][Serialize]Put Byte
            [End][Serialize]Type [toString=SBigInt; typeCode=0x06]
          [End][Serialize]tpe
          [Start][Serialize]value
            [Start Real][Serialize]Put UShort
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End Real][Serialize]Put UShort
            [Start Real][Serialize]Put Bytes
            [End Real][Serialize]Put Bytes
          [End][Serialize]value
        [End][Serialize]Constant
      [End][Serialize][Constant without store]
    [End][Serialize]Value [toString=SBigInt; opCode=0x00]
  [End][Serialize]right
[End][Serialize]ModQArithOp

42. SubstConstants
[Start][Serialize]SubstConstants
  [Start][Serialize]scriptBytes
    [Start][Serialize]Value [toString=Coll[SByte]; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SByte]; opCode=0x73]
  [End][Serialize]scriptBytes
  [Start][Serialize]positions
    [Start][Serialize]Value [toString=Coll[SInt]; opCode=0x73]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
          [Start][Serialize]ConstantPlaceholder
            [Start][Serialize]id
              [Start Real][Serialize]Put UInt
                [Start Real][Serialize]Put ULong
                  [Start][Serialize]VLQLong
                  [End][Serialize]VLQLong
                [End Real][Serialize]Put ULong
              [End Real][Serialize]Put UInt
            [End][Serialize]id
          [End][Serialize]ConstantPlaceholder
        [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SInt]; opCode=0x73]
  [End][Serialize]positions
  [Start][Serialize]newValues
    [Start][Serialize]Value [toString=Coll[SSigmaProp]; opCode=0x83]
      [Start][Serialize][Non-constant]
        [Start][Serialize]opCode
          [Start Real][Serialize]Put Byte
          [End Real][Serialize]Put Byte
        [End][Serialize]opCode
        [Start][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
          [Start][Serialize]ConcreteCollection
            [Start][Serialize]items.size
              [Start Real][Serialize]Put UShort
                [Start Real][Serialize]Put UInt
                  [Start Real][Serialize]Put ULong
                    [Start][Serialize]VLQLong
                    [End][Serialize]VLQLong
                  [End Real][Serialize]Put ULong
                [End Real][Serialize]Put UInt
              [End Real][Serialize]Put UShort
            [End][Serialize]items.size
            [Start][Serialize]tpe.elemType
              [Start][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
                [Start Real][Serialize]Put Byte
                [End Real][Serialize]Put Byte
              [End][Serialize]Type [toString=SSigmaProp; typeCode=0x08]
            [End][Serialize]tpe.elemType
            [Start][Serialize]items*
              [Start][Serialize]Value [toString=SSigmaProp; opCode=0x73]
                [Start][Serialize][Non-constant]
                  [Start][Serialize]opCode
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]opCode
                  [Start][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                    [Start][Serialize]ConstantPlaceholder
                      [Start][Serialize]id
                        [Start Real][Serialize]Put UInt
                          [Start Real][Serialize]Put ULong
                            [Start][Serialize]VLQLong
                            [End][Serialize]VLQLong
                          [End Real][Serialize]Put ULong
                        [End Real][Serialize]Put UInt
                      [End][Serialize]id
                    [End][Serialize]ConstantPlaceholder
                  [End][Serialize]Body [opCode=0x73; Serializer=ConstantPlaceholderSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$163/0x00000008401ea040@1e92c3b6)]
                [End][Serialize][Non-constant]
              [End][Serialize]Value [toString=SSigmaProp; opCode=0x73]
            [End][Serialize]items*
          [End][Serialize]ConcreteCollection
        [End][Serialize]Body [opCode=0x83; Serializer=ConcreteCollectionSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$190/0x000000084040a040@542e560f)]
      [End][Serialize][Non-constant]
    [End][Serialize]Value [toString=Coll[SSigmaProp]; opCode=0x83]
  [End][Serialize]newValues
[End][Serialize]SubstConstants


43. [Start][Serialize]Value [toString=SBoolean; opCode=0xCD]
  [Start][Serialize][Non-constant]
    [Start][Serialize]opCode
      [Start Real][Serialize]Put Byte
      [End Real][Serialize]Put Byte
    [End][Serialize]opCode
    [Start][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
      [Start][Serialize]ProveDlog
        [Start][Serialize]value
          [Start][Serialize]Value [toString=SGroupElement; opCode=0x00]
            [Start][Serialize][Constant without store]
              [Start][Serialize]Constant
                [Start][Serialize]tpe
                  [Start][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                    [Start Real][Serialize]Put Byte
                    [End Real][Serialize]Put Byte
                  [End][Serialize]Type [toString=SGroupElement; typeCode=0x07]
                [End][Serialize]tpe
                [Start][Serialize]value
                  [Start][Serialize]GroupElementSerializer
                    [Start Real][Serialize]Put Bytes
                    [End Real][Serialize]Put Bytes
                  [End][Serialize]GroupElementSerializer
                [End][Serialize]value
              [End][Serialize]Constant
            [End][Serialize][Constant without store]
          [End][Serialize]Value [toString=SGroupElement; opCode=0x00]
        [End][Serialize]value
      [End][Serialize]ProveDlog
    [End][Serialize]Body [opCode=0xCD; Serializer=ProveDlogSerializer(sigmastate.serialization.ValueSerializer$$$Lambda$188/0x0000000840408840@373ebf74)]
  [End][Serialize][Non-constant]
[End][Serialize]Value [toString=SBoolean; opCode=0xCD]


44. 