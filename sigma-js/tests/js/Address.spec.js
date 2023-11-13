const { Address, AddressObj } = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
  it("Should export Address object", () => {
    expect(Address).not.toBeUndefined();
  });
});

describe("Address", () => {
  let addrStr = "9iJd9drp1KR3R7HLi7YmQbB5sJ5HFKZoPb5MxGepamggJs5vDHm";
  let p2sStr = "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB9"

  it("roundtrip for P2PK", () => {
    let addr = AddressObj.fromString(addrStr);
    expect(addr.isP2PK()).toEqual(true)
    expect(addr.isP2S()).toEqual(false)

    expect(addr.toString()).not.toBeUndefined();
    expect(addr.toString()).toEqual(addrStr)
  });

  it("roundtrip for P2S", () => {
    let addr = AddressObj.fromString(p2sStr);
    expect(addr.isP2S()).toEqual(true)
    expect(addr.isP2PK()).toEqual(false)

    expect(addr.toString()).not.toBeUndefined();
    expect(addr.toString()).toEqual(p2sStr)
  });

  it("toSigmaPropOpt", () => {
    let addr = AddressObj.fromString(addrStr);

    expect(addr.isMainnet()).toEqual(true)
    expect(addr.toSigmaPropOpt()).not.toBeUndefined()
  });

});

