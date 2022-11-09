const {
  ErgoTree,
  ErgoTreeSerializer
} = require("../../../interpreter/js/target/scala-2.13/interpreter-opt/main.js");

describe("Smoke tests for API exporting", () => {
  it("Should export ErgoTree object", () => {
    expect(ErgoTree).not.toBeUndefined();
  });

  it("Should export ErgoTreeSerializer object", () => {
    expect(ErgoTreeSerializer).not.toBeUndefined();
  });

});

describe("Smoke tests for ErgoTree", () => {
  it("Should create fromHex", () => {
    var hex = "100604000e000400040005000500d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202";
    expect(ErgoTree.fromHex(hex).toString()).not.toBeUndefined();
  });

});
