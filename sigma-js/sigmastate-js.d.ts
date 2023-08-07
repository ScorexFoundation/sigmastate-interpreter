declare module "sigmastate-js/main" {
  type SigmaCompilerNamedConstantsMap = { [key: string]: Value };
  type HexString = string;
  type ByteArray = { u: Int8Array };

  export declare class ErgoTree {
    toHex(): HexString;
    bytes(): ByteArray;
    header(): number;
    version(): number;
    isConstantSegregation(): boolean;
    hasSize(): boolean;
    constants(): Value[];
    template(): ByteArray;
    templateHex(): HexString;
    toString(): string;
  }

  export declare class ErgoTreeObj {
    static fromHex(value: HexString): ErgoTree;
  }

  export declare class Type {
    name: string;
    toString(): string;
  }

  export declare class TypeObj {
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

  export declare class Value<T = unknown> {
    data: T;
    tpe: Type;
    toHex(): HexString;
  }

  export declare class ValueObj {
    static ofByte(value: number): Value<number>;
    static ofShort(value: number): Value<number>;
    static ofInt(value: number): Value<number>;
    static ofLong(value: bigint): Value<bigint>;
    static ofBigInt(value: bigint): Value<bigint>;
    static pairOf<R, L>(left: Value<R>, right: Value<L>): Value<[R, L]>;
    static collOf<T>(items: T[], type: Type): Value<T[]>;
    static fromHex<T>(hex: HexString): Value<T>;
  }

  export declare class SigmaCompiler {
    compile(
      namedConstants: SigmaCompilerNamedConstantsMap,
      segregateConstants: boolean,
      additionalHeaderFlags: number,
      ergoScript: string
    ): ErgoTree;
  }

  export declare class SigmaCompilerObj {
    static forMainnet(): SigmaCompiler;
    static forTestnet(): SigmaCompiler;
  }
}
