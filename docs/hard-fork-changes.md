## A list of hard-fork changes

Please describe here all changes which may lead to hard fork (HF for short).

**Note for reviewers: Pull requests targeted to the next HF branch (e.g. v4.x) should be rejected, 
if they contain HF change, which is not described here**.

### Hard-fork changes in v4.0 (since v3.0.0)

1. This version v4.0.x is based on special/v0.6.x and inherit its hard-fork changes 
  [see corresponding file](https://github.com/scalan/special/blob/v0.6.0/docs/hard-fork-changes.md).
  
2. CheckAppendInFoldLoop validation rule added.

3. Costing rule for Append is changed. The same script will have different cost in this version.
   Hence some transactions may become invalid.
   
4. ModQ operations serializers removed from serializers table, because these operations
are not fully implemented.
This means old v3.0 nodes can successfully serialize but throw exception later.
New v4.0 nodes will throw during deserialization. For this reason this opCodes 
cannot be used, even after new operations are fully implemented via soft-fork.
Thus this is better to be fixed as part of upcoming hard-fork.

5. Complexity of Fold.opCode changed 4034 -> 8034 (i.e. increased for better approximations of the costs)

6. Append operation cost also increased (see `("Append", "(Coll[IV],Coll[IV]) => Coll[IV]", appendCost)`)