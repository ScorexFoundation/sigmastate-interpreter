const { TypeObj } = require("sigmastate-js");

describe("Smoke tests for Types", () => {

  it("Should create primitive types", () => {
    expect(TypeObj.Byte.name).toEqual("Byte");
    expect(TypeObj.Short.name).toEqual("Short");
    expect(TypeObj.Int.name).toEqual("Int");
    expect(TypeObj.Long.name).toEqual("Long");
    expect(TypeObj.BigInt.name).toEqual("BigInt");
    expect(TypeObj.GroupElement.name).toEqual("GroupElement");
    expect(TypeObj.SigmaProp.name).toEqual("SigmaProp");
    expect(TypeObj.Box.name).toEqual("Box");
    expect(TypeObj.AvlTree.name).toEqual("AvlTree");
    expect(TypeObj.Context.name).toEqual("Context");
    expect(TypeObj.Header.name).toEqual("Header");
    expect(TypeObj.PreHeader.name).toEqual("PreHeader");
    expect(TypeObj.SigmaDslBuilder.name).toEqual("SigmaDslBuilder");
  });

  it("Should create complex types", () => {
    expect(TypeObj.pairType(TypeObj.Int, TypeObj.Long).name).toEqual("(Int, Long)");
    expect(TypeObj.collType(TypeObj.Int).name).toEqual("Coll[Int]");
    expect(TypeObj.collType(TypeObj.pairType(TypeObj.Int, TypeObj.Long)).name)
        .toEqual("Coll[(Int, Long)]");
  });
});
