const { TypeObj, ValueObj, ErgoTree, ErgoTreeObj } = require("sigmastate-js");

describe("Smoke tests for API exporting", () => {
  it("Should export ErgoTree object", () => {
    expect(ErgoTree).not.toBeUndefined();
  });
});

describe("Smoke tests for ErgoTree", () => {
  it("Should create fromHex", () => {
    var hex = "100604000e000400040005000500d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202";
    var tree = ErgoTreeObj.fromHex(hex)
    expect(tree.toString()).not.toBeUndefined();
    expect(tree.toHex()).toEqual(hex)
  });
});

describe("Smoke tests for Types", () => {
  it("Should create primitive types", () => {
    expect(TypeObj.Byte.name).toEqual("Byte");
    expect(TypeObj.Short.name).toEqual("Short");
    expect(TypeObj.Int.name).toEqual("Int");
    expect(TypeObj.Long.name).toEqual("Long");
  });
  it("Should create complex types", () => {
    expect(TypeObj.pairType(TypeObj.Int, TypeObj.Long).name).toEqual("(Int, Long)");
    expect(TypeObj.collType(TypeObj.Int).name).toEqual("Coll[Int]");
    expect(TypeObj.collType(TypeObj.pairType(TypeObj.Int, TypeObj.Long)).name)
        .toEqual("Coll[(Int, Long)]");
  });
});

function testRange(factory, min, max) {
  expect(factory(max).data).toEqual(max);
  expect(() => factory(max + 1).data).toThrow();
  expect(factory(-1).data).toEqual(-1);
  expect(factory(min).data).toEqual(min);
  expect(() => factory(min - 1).data).toThrow();
}

describe("Smoke tests for Values", () => {
  it("Should create values of primitive types", () => {
    expect(ValueObj.ofByte(0).data).toEqual(0);
    expect(ValueObj.ofByte(0).tpe).toEqual(TypeObj.Byte);
    testRange(function(v) { return ValueObj.ofByte(v); }, -128, 127);
    testRange(function(v) { return ValueObj.ofShort(v); }, -32768, 32767);
    testRange(function(v) { return ValueObj.ofInt(v); }, -0x7FFFFFFF - 1, 0x7FFFFFFF);
    testRange(function(v) { return ValueObj.ofLong(v); }, -0x8000000000000000n, 0x7fffffffffffffffn);
  });

  it("Should create values of complex types", () => {
    let pair = ValueObj.pairOf(ValueObj.ofByte(10), ValueObj.ofLong(20n));
    expect(pair.data).toEqual([10, 20n]);
    expect(pair.tpe.name).toEqual("(Byte, Long)");

    let coll = ValueObj.collOf([-10, 0, 10], TypeObj.Byte)
    expect(coll.tpe.name).toEqual("Coll[Byte]");
  });

  let longHex = "05e012";
  let collHex = "1a0203010203020a14";
  let pairHex = "3e050a28"

  it("Long Value.toHex", () => {
    let v = ValueObj.ofLong(1200n)
    expect(v.toHex()).toEqual(longHex)
  });

  it("Coll Value.toHex", () => {
    let arr = [ [1, 2, 3], [10, 20] ]
    let t = TypeObj.collType(TypeObj.Byte)
    let collV = ValueObj.collOf(arr, t)

    expect(collV.tpe.name).toEqual("Coll[Coll[Byte]]");
    expect(collV.toHex()).toEqual(collHex)
  });

  it("Pair Value.toHex", () => {
    let fst = ValueObj.ofByte(10)
    let snd = ValueObj.ofLong(20)
    let pair = ValueObj.pairOf(fst, snd)
    expect(pair.tpe.name).toEqual("(Byte, Long)");
    expect(pair.toHex()).toEqual(pairHex)
  });

  it("Long Value.fromHex", () => {
    let v = ValueObj.fromHex(longHex)
    expect(v.data).toEqual(1200n)
    expect(v.tpe.name).toEqual("Long")
  });

  it("Coll Value.fromHex", () => {
    let coll = ValueObj.fromHex(collHex)
    expect(coll.tpe.name).toEqual("Coll[Coll[Byte]]");
    expect(coll.toHex()).toEqual(collHex)
  });

  it("Pair Value.fromHex", () => {
    let p = ValueObj.fromHex(pairHex)
    expect(p.tpe.name).toEqual("(Byte, Long)");
    expect(p.toHex()).toEqual(pairHex)
  });
});
