A Local Exchange Trading System On Top Of Ergo
==============================================

 A local exchange trading system (LETS) is a local association which members allowed to create
 credit money individually, and all the deals in the system are written into a common ledger. 
 As an example, assume that Alice with zero balance is willing to buy a liter of raw milk from Bob.
 First, they agree on a price, for example, assume that the price is about 2 Euro (as Alice and Bob 
 are living in Ireland). Then after the deal written into a ledger, Alice's balance becomes -2 (minus 
 two) Euro, and Bob's balance becomes 2 Euro. Then Bob may spend his 2 Euro, for example, on  
 home-made beer from Charlie. Often, such systems impose limits on negative balances, and sometimes 
 even on positive ones, to promote exchange in the community.
 
 Historically, such systems become popular during crisis times. For example, in Argentina, 2001,
 ... . However, paper-based LETS currencies have shown some problems, such as counterfeit notes, 
 possible rogue behavior of system managers, and so on. 
 
 In this article we show how LETS could be implemented on top of Ergo. Our reference implementation 
 is simple and consists of two contracts, namely, a management contract and an exchange contract.
 We skip Ergo preliminaries, so please read ICO article
 
 Also, we are going to introduce a couple of new terms in following sentences.
 If some token is issued with amount equal to one, we call it the singleton token. Similarly, 
 a box which contains the singleton token is called the singleton box.
 
 The management contract is controlling a singleton box which holds members of the LETS system. 
 The contract allows to add new members, with the pace of one member per one transaction. The box
 is not storing members though, only a small digest of authenticated data structure built on top of
 the members directory. A member is associated with a singleton token issued in a transaction which
 is adding the member to the directory. The transaction creates a new member's box which contains
 the member's singleton token. The member's box is protected the exchange contract. Also, the newly
 created member's box has initial balance written down into the R4 register, and the balance is 
 equal to zero in our example.
 
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
 
 
 The exchange script 
 
 
 