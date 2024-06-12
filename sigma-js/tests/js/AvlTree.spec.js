const { AvlTree$ } = require("sigmastate-js/main");

let digestHex = "9032fbb45d18df53bf29e054769ffbdc420476cf0a64652c83e7bef8a01fad3d1a";

describe("AvlTree", () => {
    it("should implement fromDigest", () => {
        let tree = AvlTree$.fromDigest(digestHex)
        expect(tree.digest).toEqual(digestHex)
        expect(tree.digest.length).toEqual(AvlTree$.DigestSize * 2)
        expect(tree.insertAllowed).toEqual(true)
        expect(tree.updateAllowed).toEqual(true)
        expect(tree.removeAllowed).toEqual(true)
        expect(tree.keyLength).toEqual(AvlTree$.DigestSize - 1)
        expect(tree.valueLengthOpt).toBeUndefined()
    });
});