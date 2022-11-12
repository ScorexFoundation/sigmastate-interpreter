const {
  Types
} = require("../../../interpreter/js/target/scala-2.13/interpreter-fastopt/common.js");
const {
  Values
} = require("../../../interpreter/js/target/scala-2.13/interpreter-fastopt/core.js");
const {
  ErgoTree, ErgoTrees
} = require("../../../interpreter/js/target/scala-2.13/interpreter-fastopt/ergotree.js");

describe("Smoke tests for API exporting", () => {
  it("Should export ErgoTree object", () => {
    expect(ErgoTree).not.toBeUndefined();
  });
});

describe("Smoke tests for ErgoTree", () => {
  it("Should create fromHex", () => {
    var hex = "100604000e000400040005000500d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202";
    var tree = ErgoTrees.fromHex(hex)
    expect(tree.toString()).not.toBeUndefined();
    expect(tree.toHex()).toEqual(hex)
  });
});

describe("Smoke tests for Types", () => {
  it("Should create primitive types", () => {
    expect(Types.Byte.name).toEqual("Byte");
    expect(Types.Short.name).toEqual("Short");
    expect(Types.Int.name).toEqual("Int");
    expect(Types.Long.name).toEqual("Long");
  });
  it("Should create complex types", () => {
    expect(Types.pairType(Types.Int, Types.Long).name).toEqual("(Int, Long)");
    expect(Types.collType(Types.Int).name).toEqual("Coll[Int]");
    expect(Types.collType(Types.pairType(Types.Int, Types.Long)).name)
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
    expect(Values.ofByte(0).data).toEqual(0);
    expect(Values.ofByte(0).tpe).toEqual(Types.Byte);
    testRange(function(v) { return Values.ofByte(v); }, -128, 127);
    testRange(function(v) { return Values.ofShort(v); }, -32768, 32767);
    testRange(function(v) { return Values.ofInt(v); }, -0x7FFFFFFF - 1, 0x7FFFFFFF);
    testRange(function(v) { return Values.ofLong(v); }, -0x8000000000000000n, 0x7fffffffffffffffn);
  });

  it("Should create values of complex types", () => {
  });

  var hex = "05e012";
  it("Value.toHex", () => {
    var v = Values.ofLong(1200n)
    expect(v.toHex()).toEqual(hex)
  });

  it("Value.fromHex", () => {
    var v = Values.fromHex(hex)
    expect(v.data).toEqual(1200n)
    expect(v.tpe.name).toEqual("Long")
  });
});
