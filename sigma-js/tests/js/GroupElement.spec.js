const { GroupElementObj, ErgoTree} = require("sigmastate-js/main");

let pointAsn1Hex = "02c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5";

describe("GroupElement", () => {
    it("should implement toHex/fromHex", () => {
        let ge = GroupElementObj.fromHex(pointAsn1Hex)
        expect(ge.toHex()).toEqual(pointAsn1Hex)
    });
});