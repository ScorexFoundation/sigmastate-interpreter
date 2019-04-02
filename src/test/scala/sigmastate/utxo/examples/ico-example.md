An ICO Example On Top of Ergo.
==============================

We cover such complex example as an ICO (Initial Coin Offering) here covering several important novel features of the Ergo Platform. 

## Part 1. Preliminaries


A cryptocurrency protocol needs to specify what a spending transaction actually spends. There are two possibilities here. 
In Bitcoin, a transaction is spending one-time digital coins and creates new ones. In Nxt, Ethereum, or Waves, a transaction is deducing
a transaction amount from long-living account or establishing a new one~(with possible side-effects on the way, like contract execution in Waves or Ethereum). Ergo is similar to Bitcoin, it is spending one-time coins (Ergo transaction also can have data inputs which are not to be spent, rather, they are providing information from current set of unspent coins).

It is not trivial to do an ICO in model of one-time coins, as, in opposite to account-based cryptocurrencies, there is no explicit 
persistent storage here. However, Ergo brings spending transaction into execution context of a script which is protecting a coin. 
With this small change it becomes possible to express dependencies between transaction outputs and inputs. In turn, by setting dependencies we can execute even arbitrarily complex Turing-complete programs on top of blockchain (see ...). In this article we will define a concrete scenario of a multi-stage contract, as for ICO we need for different stages (funding, token issuance, withdrawal).

Now imagine an ICO for thousands of participants. Unlike Ethereum, Ergo does not provide possibility to store and carry over though an execution trace large sets of data. Rather, it allows to store only about 40-bytes header of a data structure, represented as key -> value dictionary, authenticated similarly to Merkle tree. To access some elements in the dictionary, or to modify it, a spending transaction which is triggering protecting script execution should provide lookup or modification proofs. This gives possibility for a contract to authenticate potentially huge datasets without requiring much memory to store contract state. However, storing space in the state (of active contracts) would mean bigger transactions, but this problem is easier from scalability point of view, and scalability is a top priority for Ergo. 

Please 

## Part 2. The ICO Contract

There could be many possible scenarios associated with the Initial Coin Offering (ICO). In this article we are working with the ICO contract steps briefly described below with details provided further:
	- first, funding epoch takes place. It starts with a project coin authenticating an empty dictionary. The dictionary is intended for holding (investor, balance) pairs, where the investor is associated with script of its withdrawal coin. For the balance, we assume that 1 token is equal to 1 Ergo during the ICO. During the funding epoch, it is possible to put Ergs into the project coin.
	For that, a spending transaction for the project coin provides inputs which holding investor withdrawal scripts. Investor scripts and input values should be added to the tree. There could be many chained funding transactions.  
	- second, the funding period should be finished with closing the tree holding investors data. An authenticated tree could have different modification operations allowed individually: inserts, deletes, updates, or all the operations could be disallowed (so the tree could be in the read-only mode). Also, this transaction creates tokens of the ICO project which will be withdrawn in the next stage. The project can withdraw Ergs at this stage.  
	- third, investors are withdrawing their tokens. For that, a spending transaction creates outputs with guarding conditions and token values from the tree. The tree should be cleared from withdrawn pairs. There could be many chained funding transactions.

This three stages should be linked together, and form logical order. To fulfill these goals, we are using the same coin, 

## Part 3. The ICO Contract Details

### The funding stage

### The issuance stage



### The withdrawal stage 

## Part 4. Contractual Money 
