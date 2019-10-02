# Goal:
Verify properties of a contract starting from simple (Crowdfunding) to more complex (ICO).

# Workflow:
Write the contract in SigmaDls.
During the compilation:
* Our macros generate ErgoTree;
* [Stainless](https://github.com/epfl-lara/stainless) verifies contract properties;

# Plan:

## Crowdfunding Contract:
- [x] some properties of the Crowdfunding contract are already proven, see [source](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/1f65d90cdab0388acc73bfa43797e39d3d9e32bc/contract-verification/src/main/scala/sigmastate/verification/contract/CrowdFundingContractVerification.scala#L34-L76)
- [ ] find and prove more properties.

## ICO contract
### Rewrite ICO contracts:
- [ ] simplify/reduce the proof, the requirement is too complex [here](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/1f65d90cdab0388acc73bfa43797e39d3d9e32bc/contract-verification/src/main/scala/sigmastate/verification/contract/ICOContractVerification.scala#L208-L208) 
- [ ] eliminate the use of `Option.getOrElse`;
### Prove ICO contract properties:
- [x] some properties of the ICO funding contract are already proven, see [source](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/1f65d90cdab0388acc73bfa43797e39d3d9e32bc/contract-verification/src/main/scala/sigmastate/verification/contract/ICOContractVerification.scala#L197-L238)
- [ ] find and prove more properties for ICO funding contract.
- [ ] find and prove properties for ICO issuance contract.
- [ ] find and prove properties for ICO withdrawal contract.

## Generate ErgoTree from SigmaDsl. 