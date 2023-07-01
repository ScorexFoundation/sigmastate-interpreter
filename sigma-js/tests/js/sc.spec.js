const { Types, Values, ErgoTree, SigmaCompiler, SigmaCompilerObj } = require("sigmastate-js");

describe("Smoke tests for API exporting", () => {
  it("Should create SigmaCompiler", () => {
    let compiler = SigmaCompilerObj.create(0);
    expect(compiler).not.toBeUndefined();
  });
});

