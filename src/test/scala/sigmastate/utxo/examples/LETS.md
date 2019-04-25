A Local Exchange Trading System On Top Of Ergo
==============================================

 A Local Exchange Trading System (LETS) is a local mutual credit association in which members are allowed to create common credit money individually, with all the deals in the system written into a common ledger. As an example, assume that Alice with zero balance is willing to buy a liter of raw milk from Bob, who also has zero balance. First, they agree on a price, which in our example is 2 Euro (as Alice and Bob live in Ireland). After the deal is written into a ledger, Alice's balance becomes -2 (minus two) Euro, and Bob's balance becomes 2 Euro. Bob may now spend his 2 Euro, for example, on home-made beer from Charlie. Often, such systems impose limits on negative balances, and sometimes even on positive ones, in order to promote exchanges in the community.
 
 Historically, such systems become popular during times of economic crisis.  The first such system was established by Michael Linton in a Canadian town stuck in depression back in 1981. Local exchange trading systems were extremely popular during 1998-2002 Argentine Great Depression. Most LETS groups range from 50 to 250 members, with paper-based credit notes and a ledger maintained by a core committee. However, paper-based LETS currencies have some problems, such as counterfeit notes, possible rogue behavior of system managers, and so on. So blockchain-based LETS could be superior to the old systems.
 
 In this article we show how LETS could be implemented on top of Ergo. To the best of our knowledge, this is the first implementation of such kind of community currency on top of a blockchain. Our reference implementation is simple and consists of two contracts, namely, a *management contract* and an *exchange contract*. We skip Ergo preliminaries, for which we refer the reader to the ICO article and ErgoScript tutorials. Nevertheless, we introduce a couple of new terms as follows. A token generated with quantity equal to one is called a *singleton token*. Similarly, a box containing a singleton token is called a *singleton box*.
 
 The management contract defines the members of the LETS system and is present in a singleton box called the *management box*. The contract allows adding new members at the rate of one member per one transaction. Note that the box does not store the actual members. Instead it stores only a small digest of an authenticated data structure built on top of the members directory. A member is associated with a singleton token issued in an *add* transaction which adds the member to the directory. This transaction creates a new member's box containing the member's singleton token. The member's box is protected by the exchange contract described next and its register R4 stores the credit balance, which is initially zero. The add transaction must also consume an existing management box and generate a new one with the updated directory. A proof of correct directory transformation should also be provided in the transaction. 
 
 The management contract box is usually controlled by a committee, which could evolve over time. To support that, we store the committee data in register R5 and an add transaction can modify this data as well. For instance, assume that the transaction adds a new committee member along with a new LETS member. The input to the transaction is a management box requiring 2-out-of-3 signatures and the output is the member's box along with a management box that requires 3-out-of-4 signatures. In this case contents of the R5 register in the input and the output management boxes would differ.
 
 The management contract code in ErgoScript is provided below. Please note that  "userContractHash" is the hash of the exchange contract script. 
 
    val selfOut = OUTPUTS(0)
 
    // Management script
    val managementScript = selfOut.R5[SigmaProp].get
 
    // The management script template is replicating self, and management script is satisfied
    val scriptCorrect = (selfOut.propositionBytes == SELF.propositionBytes) && managementScript
 
    // A spending transaction is creating boxes for directory, user, fee.
    val outsSizeCorrect = OUTPUTS.size == 3
 
    // Checks that the management label token is replicating self
    val outTokenCorrect = (selfOut.tokens.size == 1) && (selfOut.tokens(0)._1 == letsToken)
 
    // Checks that new token is issued, and its amount is correct
    // OUTPUTS(0) tokens already checked via outtokenCorrect
    val issuedTokenId = INPUTS(0).id
    val userOut = OUTPUTS(1)
    val correctTokenAmounts =
      (userOut.tokens.size == 1 &&
        userOut.tokens(0)._1 == issuedTokenId &&
        userOut.tokens(0)._2 == 1 &&
        OUTPUTS(2).tokens.size == 0 &&
        outTokenCorrect)
 
    // Checks that the new user has been created with the zero balance
    val zeroUserBalance  = userOut.R4[Long].get == 0
 
    val properUserScript = blake2b256(userOut.propositionBytes) == userContractHash
 
    // Checks that the new token identifier has been added to the directory
    val selfTree = SELF.R4[AvlTree].get
    val toAdd: Coll[(Coll[Byte], Coll[Byte])] = Coll((issuedTokenId, Coll[Byte]()))
    val proof = getVar[Coll[Byte]](1).get
    val modifiedTree = selfTree.insert(toAdd, proof).get
    val expectedTree = selfOut.R4[AvlTree].get
    val treeCorrect = modifiedTree == expectedTree
 
    correctTokenAmounts && scriptCorrect && treeCorrect && zeroUserBalance && properUserScript       
 
 
 The exchange contract script protecting a member's box is provided below. The contract is pretty straightforward and has the following logic. The spending transaction for an exchange contract box, called an *exchange transaction* has at least two inputs such that the first two inputs are protected by the exchange contract script and contain LETS member tokens. To check that a singleton token in an input indeed belong to the LETS system, the exchange transaction also provides the management contract box _M_ as the first read-only data input along with a proof that the token belongs to the directory authenticated via R4 register of _M_. 
 
    // Minimal balance allowed for LETS trader
    val minBalance = -20000
 
    val lookupProof = getVar[Coll[Byte]](1).get
 
    // The read-only box which contains directory of LETS members
    val treeHolderBox = CONTEXT.dataInputs(0)
    val properTree = treeHolderBox.tokens(0)._1 == letsToken
    val membersTree = treeHolderBox.R4[AvlTree].get
 
    // A spending transaction is taking two boxes of LETS members willing to make a deal,
    // and returns boxes with modified balances.
    val participant0 = INPUTS(0)
    val participant1 = INPUTS(1)
    val participantOut0 = OUTPUTS(0)
    val participantOut1 = OUTPUTS(1)
 
    //Check that members are indeed belong to the LETS
    val token0 = participant0.tokens(0)._1
    val token1 = participant1.tokens(0)._1
    val memberTokens = Coll(token0, token1)
    val membersExist = membersTree.getMany(memberTokens, lookupProof).forall({ (o: Option[Coll[Byte]]) => o.isDefined })
 
    // Check that LETS member balance changes during the deal are correct
    val initialBalance0 = participant0.R4[Long].get
    val initialBalance1 = participant1.R4[Long].get
    val finishBalance0  = participantOut0.R4[Long].get
    val finishBalance1  = participantOut1.R4[Long].get
    val diff0 = finishBalance0 - initialBalance0
    val diff1 = finishBalance1 - initialBalance1
    val diffCorrect = diff0 == -diff1
    val balancesCorrect = (finishBalance0 > minBalance) && (finishBalance1 > minBalance) && diffCorrect
 
    // Check that member boxes save their scripts.
    // todo: optimization could be made here
    val script0Saved = participantOut0.propositionBytes == participant0.propositionBytes
    val script1Saved = participantOut1.propositionBytes == participant1.propositionBytes
    val scriptsSaved = script0Saved && script1Saved
 
    // Member-specific box protection
    val selfPubKey = SELF.R5[SigmaProp].get
 
    selfPubKey && properTree && membersExist && diffCorrect && scriptsSaved
    
 Note that both contracts can be enhanced to get new systems with more complex features. So hopefully some day this article will be continued!
