declare module "sigmastate-js/main" {
  type SigmaCompilerNamedConstantsMap = { [key: string]: Value };
  type HexString = string;

  class ErgoTree {
    toHex(): HexString;
    bytes(): { u: Uint8Array };
    header(): number;
    version(): number;
    isConstantSegregation(): boolean;
    hasSize(): boolean;
    constants(): Value[];
    toString(): string;
  }

  class ErgoTreeObj {
    static fromHex(value: HexString): ErgoTree;
  }

  class Type {
    name: string;
    toString(): string;
  }

  class TypeObj {
    static Byte: Type;
    static Short: Type;
    static Int: Type;
    static Long: Type;
    static BigInt: Type;
    static GroupElement: Type;
    static SigmaProp: Type;
    static Box: Type;
    static AvlTree: Type;
    static Context: Type;
    static Header: Type;
    static PreHeader: Type;
    static SigmaDslBuilder: Type;
    static pairType(left: Type, right: Type): Type;
    static collType(elemType: Type): Type;
  }

  class Value<T = unknown> {
    data: T;
    tpe: Type;
    toHex(): HexString;
  }

  class ValueObj {
    static ofByte(value: number): Value<number>;
    static ofShort(value: number): Value<number>;
    static ofInt(value: number): Value<number>;
    static ofLong(value: bigint): Value<bigint>;
    static ofBigInt(value: bigint): Value<bigint>;
    static pairOf<R, L>(left: Value<R>, right: Value<L>): Value<[R, L]>;
    static collOf<T>(items: T[], type: Type): Value<T[]>;
    static fromHex<T>(hex: HexString): Value<T>;
  }

  class SigmaCompiler {
    compile(
      namedConstants: SigmaCompilerNamedConstantsMap,
      segregateConstants: boolean,
      additionalHeaderFlags: number,
      ergoScript: string
    ): ErgoTree;
  }

  class SigmaCompilerObj {
    static forMainnet(): SigmaCompiler;
    static forTestnet(): SigmaCompiler;
  }
}
