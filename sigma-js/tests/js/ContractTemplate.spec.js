const { ContractTemplate, ContractTemplateObj } = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
  it("Should export ContractTempate object", () => {
    expect(ContractTemplate).not.toBeUndefined();
  });
});

describe("ContractTemplate", () => {
  let addrStr = "9iJd9drp1KR3R7HLi7YmQbB5sJ5HFKZoPb5MxGepamggJs5vDHm";
  let p2sStr = "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB9"

  // it("roundtrip for P2PK", () => {
  //   let addr = AddressObj.fromString(addrStr);
  //   expect(addr.isP2PK()).toEqual(true)
  //   expect(addr.isP2S()).toEqual(false)
  //
  //   expect(addr.toString()).not.toBeUndefined();
  //   expect(addr.toString()).toEqual(addrStr)
  //   expect(addr.asP2PK()).not.toBeUndefined();
  // });

});

