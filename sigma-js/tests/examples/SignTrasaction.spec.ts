import { ProverBuilder$ } from "sigmastate-js/main";
import {
  OutputBuilder,
  SAFE_MIN_BOX_VALUE,
  TransactionBuilder,
} from "@fleet-sdk/core";
import bip39 from "bip39";
import { ALICE_ADDRESS, POOL_ADDRESS, POOL_MNEMONIC } from "../data/users";
import {
  blockchainParameterFromErgoNodeIfo,
  sigmastateHeader,
} from "../data/utils";
import {headers} from "../data/headers";
import {info} from "../data/info";
import { poolUtxo } from "../data/utxo";

/**
 * in this example we take a box from POOL_ADDRESS
 * and send SAFE_MIN_BOX_VALUE to ALICE_ADDRESS
 * change goes back to POOL_ADDRESS
 */
describe("SigmaProver", () => {
  it("should generate commitments", async () => {
    const networkMainnet = 0;

    const output = new OutputBuilder(SAFE_MIN_BOX_VALUE, ALICE_ADDRESS);
    const unsignedTx = new TransactionBuilder(info.height)
      .from(poolUtxo)
      .to(output)
      .sendChangeTo(POOL_ADDRESS)
      .payFee(SAFE_MIN_BOX_VALUE)
      .build()
      .toEIP12Object();

    const buffer = await bip39.mnemonicToSeed(POOL_MNEMONIC);
    const mnemonicPhrase = buffer.toString("hex");
    const BLOCKCHAIN_PARAMETERS = blockchainParameterFromErgoNodeIfo(info);
    const prover = ProverBuilder$.create(
      BLOCKCHAIN_PARAMETERS,
      networkMainnet
    )
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
  });
});
