package org.ergoplatform.sdk

import debox.cfor
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import org.ergoplatform.ErgoBox
import org.ergoplatform.sdk.utils.SerializationUtils.{parseString, serializeString}
import org.ergoplatform.settings.ErgoAlgos
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.CryptoConstants
import sigmastate.eval._
import sigmastate.exceptions.SerializerException
import sigmastate.lang.{DeserializationSigmaBuilder, SigmaParser, StdSigmaBuilder}
import sigmastate.serialization._
import sigmastate.util.safeNewArray
import sigmastate.utils.{Helpers, SigmaByteReader, SigmaByteWriter}
import special.sigma._

import java.util.Objects
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try

case class Parameter(
  name: String,
  description: String,
  constantIndex: Int
)

object Parameter {

  /** Immutable empty IndexedSeq, can be used to save allocations in many places. */
  val EmptySeq: IndexedSeq[Parameter] = Array.empty[Parameter]

  /** HOTSPOT: don't beautify this code */
  object serializer extends SigmaSerializer[Parameter, Parameter] {
    override def serialize(data: Parameter, w: SigmaByteWriter): Unit = {
      serializeString(data.name, w)
      serializeString(data.description, w)
      w.putUInt(data.constantIndex)
    }

    override def parse(r: SigmaByteReader): Parameter = {
      val name = parseString(r)
      val description = parseString(r)
      val constantIndex = r.getUInt().toInt
      Parameter(name, description, constantIndex)
    }
  }

  implicit val encoder: Encoder[Parameter] = Encoder.instance({ p =>
    Json.obj(
      "name" -> Json.fromString(p.name),
      "description" -> Json.fromString(p.description),
      "constantIndex" -> Json.fromInt(p.constantIndex)
    )
  })

  implicit val decoder: Decoder[Parameter] = Decoder.instance({ cursor =>
    for {
      name <- cursor.downField("name").as[String]
      description <- cursor.downField("description").as[String]
      constantIndex <- cursor.downField("constantIndex").as[Int]
    } yield new Parameter(name, description, constantIndex)
  })
}

case class ContractTemplate(
  treeVersion: Option[Byte],
  name: String,
  description: String,
  constTypes: IndexedSeq[SType],
  constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
  parameters: IndexedSeq[Parameter],
  expressionTree: SigmaPropValue
) {

  validate()

  private def validate(): Unit = {
    require(constValues.isEmpty || constValues.get.size == constTypes.size,
      s"constValues must be empty or of same length as constTypes. Got ${constValues.get.size}, expected ${constTypes.size}")
    require(parameters.size <= constTypes.size, "number of parameters must be <= number of constants")

    // Validate that no parameter is duplicated i.e. points to the same position & also to a valid constant.
    // Also validate that no two parameters exist with the same name.
    val paramNames = mutable.Set[String]()
    val paramIndices = this.parameters.map(p => {
      require(constTypes.isDefinedAt(p.constantIndex),
        s"parameter constantIndex must be in range [0, ${constTypes.size})")
      require(!paramNames.contains(p.name),
        s"parameter names must be unique. Found duplicate parameters with name ${p.name}")
      paramNames += p.name
      p.constantIndex
    }).toSet
    require(paramIndices.size == parameters.size, "multiple parameters point to the same constantIndex")

    // Validate that any constValues[i] = None has a parameter.
    if (constValues.isEmpty) {
      require(parameters.size == constTypes.size,
        "all the constants must be provided via parameter since constValues == None")
    } else {
      cfor(0)(_ < constTypes.size, _ + 1) { i =>
        require(constValues.get(i).isDefined || paramIndices.contains(i),
          s"constantIndex ${i} does not have a default value and absent from parameter as well")
      }
    }
  }

  def applyTemplate(version: Option[Byte], paramValues: Map[String, Values.Constant[SType]]): Values.ErgoTree = {
    require(treeVersion.isDefined || version.isDefined, "ErgoTreeVersion must be provided to generate the ErgoTree.")
    val nConsts = constTypes.size
    val requiredParameterNames =
      if (constValues.isEmpty)
        this.parameters.map(p => p.name)
      else this.parameters
        .filter(p => constValues.get(p.constantIndex).isEmpty)
        .map(p => p.name)
    requiredParameterNames.foreach(name => require(
      paramValues.contains(name),
      s"value for parameter ${name} was not provided while it does not have a default value."))

    val paramIndices = this.parameters.map(p => p.constantIndex).toSet
    val constants = safeNewArray[Constant[SType]](nConsts)
    cfor(0)(_ < nConsts, _ + 1) { i =>
      if (paramIndices.contains(i) && paramValues.contains(parameters(i).name)) {
        val paramValue = paramValues(parameters(i).name)
        require(paramValue.tpe == constTypes(i),
          s"parameter type mismatch, expected ${constTypes(i)}, got ${paramValue.tpe}")
        constants(i) = StdSigmaBuilder.mkConstant(paramValue.value, constTypes(i))
      } else {
        constants(i) = StdSigmaBuilder.mkConstant(constValues.get(i).get, constTypes(i))
      }
    }

    val usedErgoTreeVersion = if (version.isDefined) version.get else treeVersion.get
    ErgoTree(
      (ErgoTree.ConstantSegregationHeader | usedErgoTreeVersion).toByte,
      constants,
      this.expressionTree
    )
  }

  override def hashCode(): Int = Objects.hash(treeVersion, name, description, constTypes, constValues, parameters, expressionTree)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) ||
    ((obj.asInstanceOf[AnyRef] != null) && (obj match {
      case other: ContractTemplate =>
        other.treeVersion == treeVersion && other.name == name && other.description == description && other.constTypes == constTypes && other.constValues == constValues && other.parameters == parameters && other.expressionTree == expressionTree
      case _ => false
    }))
}

object ContractTemplate {
  def apply(name: String,
            description: String,
            constTypes: IndexedSeq[SType],
            constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
            parameters: IndexedSeq[Parameter],
            expressionTree: SigmaPropValue): ContractTemplate = {
    new ContractTemplate(None, name, description, constTypes, constValues, parameters, expressionTree)
  }

  object serializer extends SigmaSerializer[ContractTemplate, ContractTemplate] {

    override def serialize(data: ContractTemplate, w: SigmaByteWriter): Unit = {
      w.putOption(data.treeVersion)(_.putUByte(_))
      serializeString(data.name, w)
      serializeString(data.description, w)

      val nConstants = data.constTypes.length
      w.putUInt(nConstants)
      cfor(0)(_ < nConstants, _ + 1) { i =>
        TypeSerializer.serialize(data.constTypes(i), w)
      }
      w.putOption(data.constValues)((_, values) => {
        cfor(0)(_ < nConstants, _ + 1) { i =>
          w.putOption(values(i))((_, const) =>
            DataSerializer.serialize(const, data.constTypes(i), w))
        }
      })

      val nParameters = data.parameters.length
      w.putUInt(nParameters)
      cfor(0)(_ < nParameters, _ + 1) { i =>
        Parameter.serializer.serialize(data.parameters(i), w)
      }

      val expressionTreeWriter = SigmaSerializer.startWriter()
      ValueSerializer.serialize(data.expressionTree, expressionTreeWriter)
      val expressionBytes = expressionTreeWriter.toBytes
      w.putUInt(expressionBytes.length)
      w.putBytes(expressionBytes)
    }

    override def parse(r: SigmaByteReader): ContractTemplate = {
      val treeVersion = r.getOption(r.getUByte().toByte)
      val name = parseString(r)
      val description = parseString(r)

      val nConstants = r.getUInt().toInt
      val constTypes: IndexedSeq[SType] = {
        if (nConstants > 0) {
          val res = safeNewArray[SType](nConstants)
          cfor(0)(_ < nConstants, _ + 1) { i =>
            res(i) = TypeSerializer.deserialize(r)
          }
          res
        } else {
          SType.EmptySeq
        }
      }
      val constValues: Option[IndexedSeq[Option[SType#WrappedType]]] = r.getOption((() => {
        if (nConstants > 0) {
          val res = safeNewArray[Option[SType#WrappedType]](nConstants)
          cfor(0)(_ < nConstants, _ + 1) { i =>
            res(i) = r.getOption((() => DataSerializer.deserialize(constTypes(i), r))())
          }
          res
        } else {
          Array.empty[Option[SType#WrappedType]]
        }
      })())

      val nParameters = r.getUInt().toInt
      val parameters: IndexedSeq[Parameter] = {
        if (nParameters > 0) {
          val res = safeNewArray[Parameter](nParameters)
          cfor(0)(_ < nParameters, _ + 1) { i =>
            res(i) = Parameter.serializer.parse(r)
          }
          res
        } else {
          Parameter.EmptySeq
        }
      }

      // Populate constants in constantStore so that the expressionTree can be deserialized.
      val constants = constTypes.indices.map(i => {
        val t = constTypes(i)
        DeserializationSigmaBuilder.mkConstant(defaultOf(t), t)
      })
      constants.foreach(c => r.constantStore.put(c)(DeserializationSigmaBuilder))

      val _ = r.getUInt().toInt
      val expressionTree = ValueSerializer.deserialize(r)
      if (!expressionTree.tpe.isSigmaProp) {
        throw SerializerException(
          s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${expressionTree.tpe}")
      }

      ContractTemplate(
        treeVersion, name, description,
        constTypes, constValues, parameters,
        expressionTree.toSigmaProp)
    }
  }

  object jsonEncoder extends JsonCodecs {

    implicit val sTypeEncoder: Encoder[SType] = Encoder.instance({ tpe =>
      Json.fromString(tpe.toTermString)
    })

    implicit val sTypeDecoder: Decoder[SType] = Decoder.instance({ implicit cursor =>
      fromTry(Try.apply(SigmaParser.parseType(cursor.value.asString.get)))
    })

    implicit val encoder: Encoder[ContractTemplate] = Encoder.instance({ ct =>
      val expressionTreeWriter = SigmaSerializer.startWriter()
      ValueSerializer.serialize(ct.expressionTree, expressionTreeWriter)

      Json.obj(
        "treeVersion" -> ct.treeVersion.asJson,
        "name" -> Json.fromString(ct.name),
        "description" -> Json.fromString(ct.description),
        "constTypes" -> ct.constTypes.asJson,
        "constValues" -> (
          if (ct.constValues.isEmpty) Json.Null
          else ct.constValues.get.indices.map(i => ct.constValues.get(i) match {
            case Some(const) => DataJsonEncoder.encodeData(const, ct.constTypes(i))
            case None => Json.Null
          }).asJson),
        "parameters" -> ct.parameters.asJson,
        "expressionTree" -> expressionTreeWriter.toBytes.asJson
      )
    })

    implicit val decoder: Decoder[ContractTemplate] = Decoder.instance({ implicit cursor =>
      val constTypesResult = cursor.downField("constTypes").as[IndexedSeq[SType]]
      val expressionTreeBytesResult = cursor.downField("expressionTree").as[Array[Byte]]
      (constTypesResult, expressionTreeBytesResult) match {
        case (Right(constTypes), Right(expressionTreeBytes)) =>
          val constValuesOpt = {
            val constValuesJson = cursor.downField("constValues").focus.get
            if (constValuesJson != Json.Null) {
              val jsonValues = constValuesJson.asArray.get
              Some(jsonValues.indices.map(
                i => if (jsonValues(i) == Json.Null) None
                else Some(DataJsonEncoder.decodeData(jsonValues(i), constTypes(i)))))
            } else {
              None
            }
          }

          // Populate synthetic constants in the constant store for deserialization of expression tree.
          val r = SigmaSerializer.startReader(expressionTreeBytes)
          val constants = constTypes.indices.map(i => {
            val t = constTypes(i)
            DeserializationSigmaBuilder.mkConstant(defaultOf(t), t)
          })
          constants.foreach(c => r.constantStore.put(c)(DeserializationSigmaBuilder))

          for {
            treeVersion <- cursor.downField("treeVersion").as[Option[Byte]]
            name <- cursor.downField("name").as[String]
            description <- cursor.downField("description").as[String]
            parameters <- cursor.downField("parameters").as[IndexedSeq[Parameter]]
          } yield new ContractTemplate(
            treeVersion,
            name,
            description,
            constTypes,
            constValuesOpt,
            parameters,
            ValueSerializer.deserialize(r).toSigmaProp)
        case _ => Left(DecodingFailure("Failed to decode contract template", cursor.history))
      }
    })
  }

  /** Synthetic default value for the type.
    * Used for deserializing contract templates.
    */
  private def defaultOf[T <: SType](tpe: T): T#WrappedType = {
    val syntheticBox = new ErgoBox(
      0L,
      new ErgoTree(
        0.toByte,
        Vector(),
        Right(CSigmaProp(TrivialProp(false)))
      ),
      Colls.emptyColl,
      Map(),
      ModifierId @@ ("synthetic_transaction_id"),
      0.toShort,
      0
    )
    val syntheticPreHeader = CPreHeader(
      0.toByte,
      Helpers.decodeBytes("1c597f88969600d2fffffdc47f00d8ffc555a9e85001000001c505ff80ff8f7f"),
      -755484979487531112L,
      9223372036854775807L,
      11,
      Helpers.decodeGroupElement("0227a58e9b2537103338c237c52c1213bf44bdb344fa07d9df8ab826cca26ca08f"),
      Helpers.decodeBytes("007f00")
    )
    val syntheticAvlTree = AvlTreeData(
      ADDigest @@ (ErgoAlgos.decodeUnsafe("54d23dd080006bdb56800100356080935a80ffb77e90b800057f00661601807f17")),
      AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = false),
      2147483647,
      None
    )

    val res = (tpe match {
      case SBoolean => false
      case SByte => 0.toByte
      case SShort => 0.toShort
      case SInt => 0
      case SLong => 0.toLong
      case SBigInt => BigInt(0)
      case SGroupElement => CGroupElement(CryptoConstants.dlogGroup.identity)
      case SSigmaProp => CSigmaProp(TrivialProp(false))
      case SBox => CostingBox(syntheticBox)
      case c: SCollectionType[_] => SCollectionType(c.elemType)
      case _: SOption[_] => None
      case _: STuple => STuple(SInt, SLong)
      case _: SFunc => SFunc(SBoolean, NoType)
      case SContext => CostingDataContext(
        _dataInputs = Colls.emptyColl,
        headers = Colls.emptyColl,
        preHeader = syntheticPreHeader,
        inputs = Colls.emptyColl,
        outputs = Colls.emptyColl,
        height = 1,
        selfBox = CostingBox(syntheticBox),
        selfIndex = 0,
        lastBlockUtxoRootHash = CAvlTree(syntheticAvlTree),
        _minerPubKey = Helpers.decodeBytes("0227a58e9b2537103338c237c52c1213bf44bdb344fa07d9df8ab826cca26ca08f"),
        vars = Colls.emptyColl,
        activatedScriptVersion = 0.toByte,
        currentErgoTreeVersion = 0.toByte
      )
      case SAvlTree => CAvlTree(syntheticAvlTree)
      case SGlobal => CostingSigmaDslBuilder
      case SHeader => CHeader(
        Colls.fromArray(Blake2b256("Header.id")),
        0,
        Colls.fromArray(Blake2b256("Header.parentId")),
        Colls.fromArray(Blake2b256("ADProofsRoot")),
        CAvlTree(syntheticAvlTree),
        Colls.fromArray(Blake2b256("transactionsRoot")),
        timestamp = 0,
        nBits = 0,           
        height = 0,
        extensionRoot = Colls.fromArray(Blake2b256("transactionsRoot")),
        minerPk = SigmaDsl.groupGenerator,
        powOnetimePk = SigmaDsl.groupGenerator,
        powNonce = Colls.fromArray(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)),
        powDistance = SigmaDsl.BigInt(BigInt("0").bigInteger),
        votes = Colls.fromArray(Array[Byte](0, 1, 2))
      )
      case SPreHeader => syntheticPreHeader
      case SUnit => ()
      case _ => sys.error(s"Unknown type $tpe")
    }).asInstanceOf[T#WrappedType]
    res
  }
}