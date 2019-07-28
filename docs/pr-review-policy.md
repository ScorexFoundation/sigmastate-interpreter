## Pull Request Review Policy

### Motivation

ErgoScript (working name Sigma) is used in Ergo to validate box spending conditions.
This is part of network consensus protocol and thus critical for long-term survivability 
of Ergo blockchain.

From Sigma development point of view, consensus critical part of Ergo
is BlockCandidate validation code. More concretely `ErgoTransaciton.validateStateful` method.

To achieve network consensus, block validation should be the same on all the nodes. 
If some nodes have different validation results, this may lead to a network fork 
(which we call hard-fork here, or HF for short).

To prevent accidental hard forking changes in the code the development process should have
barriers to prevent them from creeping in to the mainnet releases.

It is not possible to prove NO HF with testing alone. 
In addition, other software development practices should be employed to decrease 
the unfortunate probability of hard forking changes. In particular more 
formalized development process can reduce risks.

The developer of Sigma need to understand and know internal working 
of the code in `ErgoTransaciton.validateStateful` 
method and analyse the impact of all his/her changes on this method behavior. 

This part of Ergo should be well known for all sigma developers.
Everything which is used in `ErgoTransaciton.validateStateful` is part of consensus 
and hence `red` zone.

### PR Requirements

Before the first mainnet has been released we tried to keep sigma release versions in sync with ergo versions. 
Now it doesn’t look possible, since ergo will have more frequent changes.
For example (at the time of writing) `v3.0.1` из next release branch in sigma and `v3.0.5` is next in ergo.
Thus between ergo and sigma we should keep only first two numbers of the version in sync.
So when ergo will switch to `v3.1.x`, sigma will also switch to `3.1.x`, but the last digit may differ.

When creating new PRs, `master` branch should not be used directly in PRs as a base (or target branch).

Use the following branches:
- [v3.0.1](https://github.com/ScorexFoundation/sigmastate-interpreter/tree/v3.0.1) - if you 
  can PROVE that you don’t have changes leading to hard-fork 
- [v4.0](https://github.com/ScorexFoundation/sigmastate-interpreter/tree/v4.0) - if there are 
  hard-fork changes

Because of the danger of hard forks, the requirement for new developments are 
much stronger than in typical software development projects.

The only way a new code can go to mainnet is after the following has been done:
1) the new PR is based on `v3.x` branch
2) the new PR passes all tests in `v3.x` branch (including ergo-unit, ergo-it and spam tests)
3) the PR is reviewed and merged in `v3.x` branch 
4) at least 2 approving reviews are presented (@aslesarenko and @kushti are strictly required)
5) the PR is merged into `v3.x` branch
6) the commits are merged into master as part of `v3.x` branch and released after that

This strict requirements are only for `v3.x` branches, because we need to guarantee 
that we don’t accidentally introduce hard-forks.

### PR Review Requirements

During review all the changes should be examined for hard fork. In case of any suspicious change 
the conversation should be opened during PR review and addressed by PR authors.

*NOTE: PR authors shouldn't resolve opened conversations 
(by pressing `Resolve conversation` button), this can be done by reviewers, after the 
questions are resolved.*

This is responsibility of a reviewer to identify suspicious changes.
The responsibility of the author is to dispel those suspicions by providing a NO-HF-PROOF comment
in the source code (not in github PR comment).

Reviewers should request NO-HF-PROOF comment for ALL suspicious changes
in the code. The template for comment is as the following:
```scala
/* NO HF PROOF: 
  Changed: the `if (spendingTransaction == null)` condition was removed
  Motivation: to improve performance of hotspot code
  Safety:
  It is safe to remove this spendingTransaction == null check because v3.0.5 version of ergo
  never create ErgoLikeContext instance with spendingTransaction = null.
  Examined ergo code: ErgoTransaction.validateStatefull and all code paths started from it.
*/
```
The proof should be based on the analysis of the code.

Upon receiving of NO-HF-PROOF comment, reviewers should verify the NO-HF-PROOF themselves and never assume the author did it correctly.

Because of this added complexity of v3.x review process the developer 
is strongly advised to minimize changes to a minimum which is absolutely required. 

If the change is semantic the author need to have strong reasons, 
which should be motivated in the NO-HF-PROOF comment.

Even if suspicious change is approved, there is still a chance that some 
negative impact has been ovelooked. 
Thus, by default, we allow only one such change per release, to simplify root cause analysis 
in case of any errors.

### PR Review Checklist

1. PR is based on `v3.x` branch
2. ergoBranch is correct and PR passes all tests (including ergo-unit, ergo-it and spam tests)
3. Every change is motivated
4. Every change preserves consensus
5. Every suspicious change comes with NO-HF-PROOF
6. Every NO-HF-PROOF is verified

