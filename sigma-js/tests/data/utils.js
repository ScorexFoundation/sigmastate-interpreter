const { AvlTree$, GroupElement$ } = require('sigmastate-js/main');

function blockchainParameterFromErgoNodeIfo(info){
    return{
        storageFeeFactor: info.params.storageFeeFactor,
        minValuePerByte: info.params.minValuePerByte,
        maxBlockSize: info.params.maxBlockSize,
        tokenAccessCost: info.params.tokenAccessCost,
        inputCost: info.params.inputCost,
        dataInputCost: info.params.dataInputCost,
        outputCost: info.params.outputCost,
        maxBlockCost: info.params.maxBlockCost,
        softForkStartingHeight: 100,
        softForkVotesCollected: 50,
        blockVersion: info.params.blockVersion,
    }
};

function sigmastateHeader(header){
    return {
		id: header.id,
		version: header.version,
		parentId: header.parentId,
		ADProofsRoot: header.adProofsRoot,
		stateRoot: AvlTree$.fromDigest(header.stateRoot),
		transactionsRoot: header.transactionsRoot,
		timestamp: header.timestamp,
		nBits: header.nBits,
		height: header.height,
		extensionRoot: header.extensionHash,
		minerPk: GroupElement$.fromPointHex(header.powSolutions.pk),
		powOnetimePk: GroupElement$.fromPointHex(header.powSolutions.w),
		powNonce: header.powSolutions.n,
		powDistance: BigInt(header.powSolutions.d),
		votes: header.votes,
	};
}

module.exports = {
	blockchainParameterFromErgoNodeIfo,
	sigmastateHeader
}