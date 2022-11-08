const {
  ErgoTree,
  ErgoTreeSerializer
} = require("../../target/scala-2.13/interpreter-opt/main.js");

describe("Smoke tests for API exporting", () => {
  it("Should export ErgoTree object", () => {
    expect(ErgoTree).not.toBeUndefined();
  });

  it("Should export ErgoTreeSerializer object", () => {
    expect(ErgoTreeSerializer).not.toBeUndefined();
  });

});
