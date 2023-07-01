const { Types, Values, ErgoTree, SigmaCompiler, SigmaCompilerObj } = require("sigmastate-js");

describe("Smoke tests for API exporting", () => {
  let compiler = SigmaCompilerObj.forMainnet();

  it("Should create SigmaCompiler", () => {
    expect(compiler).not.toBeUndefined();
  });

  it("SigmaCompiler should compile", () => {
    let treeWithSegregation = compiler.compile({}, true, 0, "sigmaProp(HEIGHT > 100)");
    expect(treeWithSegregation).not.toBeUndefined();
    expect(treeWithSegregation.toHex()).toEqual("100104c801d191a37300")

    let treeWithoutSegregation = compiler.compile({}, false, 0, "sigmaProp(HEIGHT > 100)");
    expect(treeWithoutSegregation.toHex()).toEqual("00d191a304c801")
  });
});

