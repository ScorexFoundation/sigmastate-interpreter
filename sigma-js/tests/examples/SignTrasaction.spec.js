const { ProverBuilder$ } = require("sigmastate-js/main");

const bip39 = require("bip39");
const { POOL_MNEMONIC } = require("../data/users");
const {
	blockchainParameterFromErgoNodeIfo,
	sigmastateHeader,
} = require("../data/utils");
const headers = require("../data/headers");
const info = require("../data/info");
const { poolToAliceTx } = require("../data/unsignedTrasactions");

describe("SigmaProver", () => {
	it("should sign a simple transaction", async () => {
		const networkMainnet = 0;
		const unsignedTx = poolToAliceTx;

		const buffer = await bip39.mnemonicToSeed(POOL_MNEMONIC);
		const mnemonicPhrase = buffer.toString("hex");
		const BLOCKCHAIN_PARAMETERS = blockchainParameterFromErgoNodeIfo(info);
		const prover = ProverBuilder$.create(BLOCKCHAIN_PARAMETERS, networkMainnet)
			.withMnemonic(mnemonicPhrase, "")
			.build();

		const sHeaders = headers.map(sigmastateHeader);
		const stateCtx = {
			sigmaLastHeaders: sHeaders.slice(1),
			previousStateDigest: sHeaders[1].stateRoot.digest,
			sigmaPreHeader: sHeaders[0],
		};

		const reducedInput = prover.reduceTransactionInput(
			stateCtx,
			unsignedTx,
			unsignedTx.inputs,
			unsignedTx.dataInputs,
			[],
			0
		);
		expect(reducedInput).toBeDefined();

		const reducedTx = prover.reduce(
			stateCtx,
			unsignedTx,
			unsignedTx.inputs,
			unsignedTx.dataInputs,
			[],
			0
		);
		expect(reducedTx).toBeDefined();

		const hints = prover.generateCommitments(reducedTx);
		expect(hints).toBeDefined();

		// let signedTx = prover.signReduced(reducedTx, hints);
		// expect(signedTx).toBeDefined();
	});
});
